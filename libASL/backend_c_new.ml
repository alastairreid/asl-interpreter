(****************************************************************
 * ASL to C backend
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C backend *)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module V = Value
open Runtime
open Asl_utils
open Format_utils
open Identset
open Builtin_idents
open Utils

module type RuntimeLib = Runtime.RuntimeLib
let runtime = ref (module Runtime_fallback.Runtime : RuntimeLib)

let runtimes = ["ac"; "c23"; "fallback"; "sc"]

let is_cxx = ref false

let set_runtime (rt : string) : unit =
  runtime := if rt = "ac" then (module Runtime_ac.Runtime : RuntimeLib)
             else if rt = "c23" then (module Runtime_c23.Runtime : RuntimeLib)
             else if rt = "sc" then (module Runtime_sc.Runtime : RuntimeLib)
             else (module Runtime_fallback.Runtime : RuntimeLib);
  is_cxx := (rt = "ac") || (rt = "sc")

let include_line_info : bool ref = ref false
let new_ffi : bool ref = ref false

let wrap_extern (add_wrapper : bool) (fmt : PP.formatter) (f : PP.formatter -> 'a) : 'a =
  if add_wrapper then (
    PP.fprintf fmt "@.#ifdef __cplusplus@.";
    PP.fprintf fmt "extern \"C\" {@.";
    PP.fprintf fmt "#endif@,@.";
    let r = f fmt in
    PP.fprintf fmt "@.#ifdef __cplusplus@.";
    PP.fprintf fmt "}@.";
    PP.fprintf fmt "#endif@,@.";
    r
  ) else (
    f fmt
  )

(* Pass arguments bigger than this using const & (C++ syntax)
 * Also, structs and arrays are also passed by reference no matter
 * how large they are.
 *
 * Don't use 'const &' for any arguments if set to 0.
 *)
let const_ref_limit : int ref = ref 0

let use_const_ref (loc : Loc.t) (x : AST.ty) : bool =
  if !const_ref_limit = 0 then
    false
  else
    ( match x with
    | Type_Integer _ -> true
    | Type_Bits (Expr_Lit (VInt n), _) ->
        let n' = Z.to_int n in
        n' > !const_ref_limit
    | Type_Constructor (tc, _) ->
        not (
            Ident.equal tc boolean_ident
            || Ident.equal tc string_ident
        )
    | Type_Array (ix_ty, ty) -> true
    | Type_Tuple tys -> true
    | _ ->
        let msg = Format.asprintf "use_const_ref: unexpected type" in
        let pp fmt = FMT.ty fmt x in
        raise (Error.Unimplemented (loc, msg, pp))
    )

let commasep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    ~pp_sep:(fun fmt' _ -> PP.pp_print_string fmt' ", ")
    pp
    fmt
    xs

let cutsep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    pp
    fmt
    xs

(** List of all the reserved words in C *)
let reserved_c = [
  "auto";
  "break";
  "case";
  "char";
  "const";
  "continue";
  "default";
  "do";
  "double";
  "else";
  "enum";
  "extern";
  "float";
  "for";
  "goto";
  "if";
  "int";
  "long";
  "register";
  "return";
  "short";
  "signed";
  "sizeof";
  "static";
  "struct";
  "switch";
  "typedef";
  "union";
  "unsigned";
  "void";
  "volatile";
  "while"
]

(** List of all the reserved words in C++ *)
let reserved_cpp = [
  "alignas";
  "alignof";
  "and";
  "and_eq";
  "asm";
  "auto";
  "bitand";
  "bitor";
  "bool";
  "break";
  "case";
  "catch";
  "char";
  "char8_t";
  "char16_t";
  "char32_t";
  "class";
  "compl";
  "concept";
  "consteval";
  "constexpr";
  "constinit";
  "const_cast";
  "continue";
  "co_await";
  "co_return";
  "co_yield";
  "decltype";
  "default";
  "delete";
  "do";
  "double";
  "dynamic_cast";
  "else";
  "enum";
  "explicit";
  "export";
  "extern";
  "false";
  "float";
  "for";
  "friend";
  "goto";
  "if";
  "inline";
  "int";
  "long";
  "mutable";
  "namespace";
  "new";
  "noexcept";
  "not";
  "not_eq";
  "nullptr";
  "operator";
  "or";
  "or_eq";
  "private";
  "protected";
  "public";
  "register";
  "reinterpret_cast";
  "requires";
  "return";
  "short";
  "signed";
  "sizeof";
  "static";
  "static_assert";
  "static_cast";
  "struct";
  "switch";
  "template";
  "this";
  "thread_local";
  "throw";
  "true";
  "try";
  "typedef";
  "typeid";
  "typename";
  "union";
  "unsigned";
  "using";
  "virtual";
  "void";
  "volatile";
  "wchar_t";
  "xor";
  "xor_eq"
]

let reserved_idents =
  reserved_c @ reserved_cpp
  |> Ident.mk_idents
  |> IdentSet.of_list

let delimiter (fmt : PP.formatter) (s : string) : unit =
  PP.pp_print_string fmt s

let ident_str (fmt : PP.formatter) (x : string) : unit =
  PP.pp_print_string fmt x

let ident (fmt : PP.formatter) (x : Ident.t) : unit =
  (* Rename any identifiers that match C/C++ reserved words *)
  if IdentSet.mem x reserved_idents
  then ident_str fmt ("__asl_" ^ Ident.name_with_tag x)
  else ident_str fmt (Ident.name_with_tag x)

(* C delimiters *)

let semicolon (fmt : PP.formatter) : unit = PP.pp_print_string fmt ";"

(* Support for handling enumerations in the Foreign Function Interface *)

let enumerated_types : (Ident.t list) Bindings.t ref = ref Bindings.empty

let add_enumerated_type (tc : Ident.t) (es : Ident.t list) : unit =
  enumerated_types := Bindings.add tc es !enumerated_types

let is_enumerated_type (x : Ident.t) : bool =
  Bindings.mem x !enumerated_types


(* Support for handling return types in the Foreign Function Interface *)

let return_types : ((Ident.t * AST.ty) list) Bindings.t ref = ref Bindings.empty

let add_return_type (tc : Ident.t) (es : (Ident.t * AST.ty) list) : unit =
  return_types := Bindings.add tc es !return_types

let fields_of_return_type (x : AST.ty) : (Ident.t * AST.ty) list option =
  ( match x with
  | Type_Constructor (tc, []) -> Bindings.find_opt tc !return_types
  | _ -> None
  )

(* Support for distributing state (e.g., registers and memory) into struct that
 * are then accessed via a named pointer.
 * Typically used to separate state into thread-local, processor-local,
 * cluster-local, package-local, etc. structs
 *)
let var_ptrs : (string * string) Bindings.t ref = ref Bindings.empty

let pointer (fmt : PP.formatter) (x : Ident.t) : unit =
  ( match Bindings.find_opt x !var_ptrs with
  | Some (_, ptr) -> PP.fprintf fmt "%s->" ptr
  | _ -> ()
  )

(* Any global state can register an initializer to be run on a global variable.
 * This is currently only used for RAM blocks.
 *)
let initializers : (string * string) list ref = ref []

let add_initializer (struct_name : string) (i : string) : unit =
  initializers := !initializers @ [(struct_name, i)]

(* list of all exception tycons - used to decide whether to insert a tag in
 * Expr_RecordInit
 *)
let exception_tcs : Ident.t list ref = ref []

(** Supply of goto labels for exception implementation *)
let catch_labels = new Asl_utils.nameSupply "catch"

module Catcher = struct
  type t = { mutable label : Ident.t option }

  let create (_ : unit) : t = { label = None }
  let get_label (x : t) : Ident.t = Option.get x.label
  let is_active (x : t) : bool = Option.is_some x.label
  let activate (x : t) : unit = x.label <- Some catch_labels#fresh
end

let catcher_stack : Catcher.t list ref = ref []

let current_catch_label (_ : unit) : Ident.t =
  match !catcher_stack with
  | c :: _ ->
      if not (Catcher.is_active c) then Catcher.activate c;
      Catcher.get_label c
  | [] ->
      raise
        (InternalError (Unknown, "No topmost catcher", (fun _ -> ()), __LOC__))

let with_catch_label (f : Catcher.t -> unit) : unit =
  let prev = !catcher_stack in
  let catcher = Catcher.create () in
  catcher_stack := catcher :: prev;
  f catcher;
  catcher_stack := prev

let rethrow_stmt (fmt : PP.formatter) : unit =
  PP.fprintf fmt "if (ASL_exception._exc.ASL_tag != ASL_no_exception) goto %a;"
    ident (current_catch_label ())

let rethrow_expr (fmt : PP.formatter) (f : unit -> unit) : unit =
  PP.fprintf fmt "({ ";
  if !is_cxx then begin
    PP.fprintf fmt "auto __r = "
  end else begin
    (* __auto_type is a gcc extension (also supported by clang) *)
    PP.fprintf fmt "__auto_type __r = "
  end;
  f ();
  PP.fprintf fmt "; ";
  rethrow_stmt fmt;
  PP.fprintf fmt " __r; })"

let const_expr (loc : Loc.t) (x : AST.expr) : V.value =
  ( match x with
  | Expr_Lit v -> v
  | Expr_TApply (f, _, [Expr_Lit (VIntN w)], _) when Ident.equal f cvt_sintN_int -> VInt w.v
  | _ ->
      let msg = Format.asprintf "const_expr: not literal constant '%a'" FMT.expr x in
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, msg, pp))
  )

let const_int_expr (loc : Loc.t) (x : AST.expr) : int =
  let v = const_expr loc x in
  ( match v with
  | VInt i -> Z.to_int i
  | _ ->
      let msg = Format.asprintf "const_int_expr: integer expected '%a'" V.pp_value v in
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, msg, pp))
  )

let const_int_exprs (loc : Loc.t) (xs : AST.expr list) : int list =
  List.map (const_int_expr loc) xs

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | VInt v    -> Runtime.int_literal fmt v
  | VIntN v   -> Runtime.sintN_literal fmt v
  | VBits v   -> Runtime.bits_literal fmt v
  | VString v -> PP.pp_print_string fmt ("\"" ^ String.escaped v ^ "\"")
  | _ -> raise (InternalError (loc, "valueLit", (fun fmt -> Value.pp_value fmt x), __LOC__))
  )

let rec apply (loc : Loc.t) (fmt : PP.formatter) (f : unit -> unit) (args : AST.expr list) : unit =
  f ();
  parens fmt (fun _ -> exprs loc fmt args)

and unop (loc : Loc.t) (fmt : PP.formatter) (op : string) (x : AST.expr) : unit =
  PP.fprintf fmt "(%s %a)"
    op
    (expr loc) x

and binop (loc : Loc.t) (fmt : PP.formatter) (op : string) (x : AST.expr) (y : AST.expr) : unit =
  PP.fprintf fmt "(%a %s %a)"
    (expr loc) x
    op
    (expr loc) y

and mk_expr (loc : Loc.t) : AST.expr -> rt_expr = Runtime.mk_rt_expr (expr loc)

and funcall (loc : Loc.t) (fmt : PP.formatter) (f : Ident.t) (tes : AST.expr list) (args : AST.expr list) =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match (const_int_exprs loc tes, args) with

  (* Boolean builtin functions *)
  | ([], [x;y]) when Ident.equal f eq_bool      -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.equal f equiv_bool   -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.equal f ne_bool      -> binop loc fmt "!=" x y
  | ([], [x;y]) when Ident.equal f and_bool     -> binop loc fmt "&&" x y
  | ([], [x;y]) when Ident.equal f or_bool      -> binop loc fmt "||" x y
  | ([], [x])   when Ident.equal f not_bool     -> unop loc fmt "!" x
  | ([], [x;y]) when Ident.equal f implies_bool -> cond loc fmt x y Asl_utils.asl_true

  (* Enumeration builtin functions *)
  (* The reason we need direct checks against eq_enum and ne_enum is because
     the identifier eq_enum with tag 0 doesn't have a root. And we need this
     function identifier in the xform_case transform right now *)
  | ([], [x;y]) when Ident.equal f eq_enum -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.equal f ne_enum -> binop loc fmt "!=" x y
  | ([], [x;y]) when Ident.root_equal f ~root:eq_enum -> binop loc fmt "==" x y
  | ([], [x;y]) when Ident.root_equal f ~root:ne_enum -> binop loc fmt "!=" x y

  (* Integer builtin functions *)
  | ([], [x;y]) when Ident.equal f add_int -> Runtime.add_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x])   when Ident.equal f neg_int ->
      ( match x with
      | Expr_Lit (VInt l) -> Runtime.int_literal fmt (Z.neg l)
      | _ -> Runtime.neg_int fmt (mk_expr loc x)
      )
  | ([], [x;y]) when Ident.equal f sub_int -> Runtime.sub_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f shl_int -> Runtime.shl_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f shr_int -> Runtime.shr_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f mul_int -> Runtime.mul_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f zdiv_int -> Runtime.zdiv_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f zrem_int -> Runtime.zrem_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f exact_div_int -> Runtime.exact_div_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f fdiv_int -> Runtime.fdiv_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f frem_int -> Runtime.frem_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f eq_int -> Runtime.eq_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f ne_int -> Runtime.ne_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f ge_int -> Runtime.ge_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f gt_int -> Runtime.gt_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f le_int -> Runtime.le_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f lt_int -> Runtime.lt_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x;y]) when Ident.equal f align_int -> Runtime.align_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x])   when Ident.equal f is_pow2_int -> Runtime.is_pow2_int fmt (mk_expr loc x)
  | ([], [x;y]) when Ident.equal f mod_pow2_int -> Runtime.mod_pow2_int fmt (mk_expr loc x) (mk_expr loc y)
  | ([], [x])   when Ident.equal f pow2_int -> Runtime.pow2_int fmt (mk_expr loc x)
  | ([], [x])   when Ident.equal f print_int_dec -> Runtime.print_int_dec fmt (mk_expr loc x)
  | ([], [x])   when Ident.equal f print_int_hex -> Runtime.print_int_hex fmt (mk_expr loc x)

  (* Bounded integer builtin functions *)
  | ([n], [x;y]) when Ident.equal f add_sintN -> Runtime.add_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x])   when Ident.equal f neg_sintN -> Runtime.neg_sintN fmt n (mk_expr loc x)
  | ([n], [x;y]) when Ident.equal f sub_sintN -> Runtime.sub_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f shl_sintN -> Runtime.shl_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f shr_sintN -> Runtime.shr_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f mul_sintN -> Runtime.mul_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f zdiv_sintN -> Runtime.zdiv_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f zrem_sintN -> Runtime.zrem_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f exact_div_sintN -> Runtime.exact_div_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f fdiv_sintN -> Runtime.fdiv_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f frem_sintN -> Runtime.frem_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f eq_sintN -> Runtime.eq_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f ne_sintN -> Runtime.ne_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f ge_sintN -> Runtime.ge_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f gt_sintN -> Runtime.gt_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f le_sintN -> Runtime.le_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f lt_sintN -> Runtime.lt_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x;y]) when Ident.equal f align_sintN -> Runtime.align_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x])   when Ident.equal f is_pow2_sintN -> Runtime.is_pow2_sintN fmt n (mk_expr loc x)
  | ([n], [x;y]) when Ident.equal f mod_pow2_sintN -> Runtime.mod_pow2_sintN fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n], [x])   when Ident.equal f pow2_sintN -> Runtime.pow2_sintN fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f cvt_bits_ssintN -> Runtime.cvt_bits_ssintN fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f cvt_bits_usintN -> Runtime.cvt_bits_usintN fmt n (mk_expr loc x)
  | ([m;n],[x;_])  when Ident.equal f cvt_sintN_bits -> Runtime.cvt_sintN_bits fmt m n (mk_expr loc x)
  | ([m;n], [x;_]) when Ident.equal f resize_sintN -> Runtime.resize_sintN fmt m n (mk_expr loc x)
  | ([n],[x])      when Ident.equal f cvt_sintN_int -> Runtime.cvt_sintN_int fmt n (mk_expr loc x)
  | ([n],[x;_])    when Ident.equal f cvt_int_sintN -> Runtime.cvt_int_sintN fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f print_sintN_dec -> Runtime.print_sintN_dec fmt n (mk_expr loc x)
  | ([n], [x])   when Ident.equal f print_sintN_hex -> Runtime.print_sintN_hex fmt n (mk_expr loc x)

  (* Real builtin functions *)
  | (_, _) when Ident.in_list f [add_real;
    cvt_int_real;
    divide_real;
    eq_real ;
    ge_real;
    gt_real;
    le_real;
    lt_real;
    mul_real;
    ne_real ;
    neg_real;
    pow2_real;
    round_down_real;
    round_tozero_real;
    round_up_real ;
    sqrt_real;
    sub_real] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "real builtin function", pp))

  (* Bitvector builtin functions *)
  | ([n],   [x;y]) when Ident.equal f eq_bits -> Runtime.eq_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f ne_bits -> Runtime.ne_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f add_bits -> Runtime.add_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f sub_bits -> Runtime.sub_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f mul_bits -> Runtime.mul_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f and_bits -> Runtime.and_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f or_bits -> Runtime.or_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f xor_bits -> Runtime.xor_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x])   when Ident.equal f not_bits -> Runtime.not_bits fmt n (mk_expr loc x)
  | ([n],   [x;y]) when Ident.equal f lsl_bits -> Runtime.lsl_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f lsr_bits -> Runtime.lsr_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x;y]) when Ident.equal f asr_bits -> Runtime.asr_bits fmt n (mk_expr loc x) (mk_expr loc y)
  | ([n],   [x])   when Ident.equal f cvt_bits_sint -> Runtime.cvt_bits_sint fmt n (mk_expr loc x)
  | ([n],   [x])   when Ident.equal f cvt_bits_uint -> Runtime.cvt_bits_uint fmt n (mk_expr loc x)
  | ([n],   [x;_]) when Ident.equal f cvt_int_bits -> Runtime.cvt_int_bits fmt n (mk_expr loc x)
  | ([n],   [x;y]) when Ident.equal f mk_mask -> Runtime.mk_mask fmt n (mk_expr loc x)
  | ([n],   [x])   when Ident.equal f zeros_bits -> Runtime.zeros_bits fmt n
  | ([n],   [x])   when Ident.equal f ones_bits -> Runtime.ones_bits fmt n
  | ([m;n], [x;y]) when Ident.equal f append_bits -> Runtime.append_bits fmt m n (mk_expr loc x) (mk_expr loc y)
  | ([m;n], [x;y]) when Ident.equal f replicate_bits -> Runtime.replicate_bits fmt m n (mk_expr loc x) (mk_expr loc y)
  | ([m;n], [x;_]) when Ident.equal f zero_extend_bits -> Runtime.zero_extend_bits fmt m n (mk_expr loc x)
  | ([m;n], [x;_]) when Ident.equal f sign_extend_bits -> Runtime.sign_extend_bits fmt m n (mk_expr loc x)
  | ([n],   [x])   when Ident.equal f print_bits_hex -> Runtime.print_bits_hex fmt n (mk_expr loc x)

  | _ when Ident.in_list f
      [ frem_bits_int;
        in_mask;
        notin_mask
      ] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "bitvector builtin function", pp))

  (* String builtin functions *)
  | _ when Ident.in_list f [
        append_str_str;
        cvt_bits_str;
        cvt_bool_str;
        cvt_int_decstr;
        cvt_int_hexstr;
        cvt_real_str
      ] ->
      PP.fprintf fmt "\"\""
  | _ when Ident.in_list f [ eq_str; ne_str ] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "string builtin function", pp))

  (* RAM builtin functions *)
  | ([a], [_;ram;v]) when Ident.equal f ram_init -> Runtime.ram_init fmt a (mk_expr loc ram) (mk_expr loc v)
  | ([n;a], [_;_;ram;addr]) when Ident.equal f ram_read -> Runtime.ram_read fmt a n (mk_expr loc ram) (mk_expr loc addr)
  | ([n;a], [_;_;ram;addr;v]) when Ident.equal f ram_write -> Runtime.ram_write fmt a n (mk_expr loc ram) (mk_expr loc addr) (mk_expr loc v)

  (* Printing builtin functions *)
  | ([], [x]) when Ident.equal f print_char -> Runtime.print_char fmt (mk_expr loc x)
  | ([], [x]) when Ident.equal f print_str -> Runtime.print_str fmt (mk_expr loc x)

  | ([], [x]) when Ident.equal f asl_end_execution -> Runtime.end_execution fmt (mk_expr loc x)

  (* File builtin functions *)
  | _ when Ident.in_list f [ asl_file_getc; asl_file_open; asl_file_write ] ->
      let pp fmt = FMT.funname fmt f in
      raise (Error.Unimplemented (loc, "file builtin function", pp))

  (* Helper functions with ASL_ prefix *)
  | _ when String.starts_with ~prefix:"ASL_" (Ident.name f) ->
      apply loc fmt (fun _ -> ident_str fmt (Ident.name f)) args

  (* User defined function *)
  | _ -> apply loc fmt (fun _ -> ident fmt f) args
  )

and cond_cont (loc : Loc.t) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr)
    (y : unit -> unit) : unit =
  parens fmt (fun _ ->
      expr loc fmt c;
      nbsp fmt;
      delimiter fmt "?";
      nbsp fmt;
      expr loc fmt x;
      nbsp fmt;
      delimiter fmt ":";
      nbsp fmt;
      y ())

and cond (loc : Loc.t) (fmt : PP.formatter) (c : AST.expr) (x : AST.expr) (y : AST.expr) :
    unit =
  cond_cont loc fmt c x (fun _ -> expr loc fmt y)

and conds (loc : Loc.t) (fmt : PP.formatter) (cts : (AST.expr * AST.expr) list) (e : AST.expr) : unit =
  ( match cts with
  | [] -> expr loc fmt e
  | (c, t) :: cts' -> cond_cont loc fmt c t (fun _ -> conds loc fmt cts' e)
  )

and expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | Expr_Concat _ ->
      raise (InternalError (loc, "Expr_Concat not expected", (fun fmt -> FMT.expr fmt x), __LOC__))
  | Expr_Field (e, f) ->
      PP.fprintf fmt "%a.%a"
        (expr loc) e
        ident f
  | Expr_If (c, t, els, e) ->
      let els1 = List.map (function AST.E_Elsif_Cond (c, e) -> (c, e)) els in
      conds loc fmt ((c, t) :: els1) e
  | Expr_Let (v, t, e, b) ->
      PP.fprintf fmt "({ const ";
      varty loc fmt v t;
      PP.fprintf fmt " = %a; %a; })"
        (expr loc) e
        (expr loc) b
  | Expr_Assert (e1, e2, loc) ->
      PP.fprintf fmt "({ ASL_assert(\"%s\", \"%s\", %a); %a; })"
        (String.escaped (Loc.to_string loc))
        (String.escaped (Utils.to_string2 (Fun.flip FMT.expr e1)))
        (expr loc) e1
        (expr loc) e2;
  | Expr_Lit v -> valueLit loc fmt v
  | Expr_RecordInit (tc, [], fas) ->
      if List.mem tc !exception_tcs then begin
        PP.fprintf fmt "(ASL_exception_t){ ._%a={ .ASL_tag = tag_%a, " ident tc ident tc;
        commasep
          (fun fmt' (f, e) -> PP.fprintf fmt' ".%a = %a" ident f (expr loc) e)
          fmt
          fas;
        PP.fprintf fmt " }}"
      end else begin
        PP.fprintf fmt "(%a){ " ident tc;
        commasep
          (fun fmt' (f, e) -> PP.fprintf fmt' ".%a = %a" ident f (expr loc) e)
          fmt
          fas;
        PP.fprintf fmt " }"
      end
  | Expr_Slices (Type_Bits (n,_), e, [Slice_LoWd (lo, wd)]) ->
      let module Runtime = (val (!runtime) : RuntimeLib) in
      Runtime.get_slice fmt (const_int_expr loc n) (const_int_expr loc wd) (mk_expr loc e) (fun fmt -> index_expr loc fmt lo)
  | Expr_Slices (Type_Integer _, e, [Slice_LoWd (lo, wd)]) ->
      let module Runtime = (val (!runtime) : RuntimeLib) in
      Runtime.get_slice_int fmt (const_int_expr loc wd) (mk_expr loc e) (mk_expr loc lo)
  | Expr_WithChanges (t, e, cs) ->
      let tmp1 = Ident.mk_ident "__tmp1" in
      let rt_tmp1 fmt = Ident.pp fmt tmp1 in
      PP.fprintf fmt "({ ";
      varty loc fmt tmp1 t;
      PP.fprintf fmt " = %a; " (expr loc) e;
      List.iteri (fun i (c, e) ->
        let c : AST.change = c in
        ( match c with
        | Change_Field f ->
            PP.fprintf fmt "%a.%a = %a; "
              Ident.pp tmp1
              Ident.pp f
              (expr loc) e
        | Change_Slices ss ->
            let tmp2 = Ident.mk_ident "__tmp2" in
            let rt_tmp2 fmt = Ident.pp fmt tmp2 in
            PP.fprintf fmt "{ ";
            varty loc fmt tmp2 t;
            PP.fprintf fmt " = %a; " (expr loc) e;
            let offset = ref 0 in
            List.iter (fun s ->
              ( match (t, s) with
              | (Type_Bits (n,_), AST.Slice_LoWd (lo, wd)) ->
                  let n' = const_int_expr loc n in
                  let offset' fmt = PP.fprintf fmt "%d" !offset in
                  let lo' fmt = index_expr loc fmt lo in
                  let wd' = const_int_expr loc wd in
                  let r fmt = Runtime.get_slice fmt n' wd' rt_tmp2 offset' in
                  Runtime.set_slice fmt n' wd' rt_tmp1 lo' r;
                  offset := !offset + wd'
              | _ ->
                  let msg = "expr with {...}: " in
                  let pp fmt = PP.fprintf fmt "%a %a" FMT.ty t FMT.slice s in
                  raise (Error.Unimplemented (loc, msg, pp))
              )
            ) ss;
            PP.fprintf fmt "} "
        )
      ) cs;
      PP.fprintf fmt "__tmp1; })";
  | Expr_TApply (f, tes, es, throws) ->
      if throws <> NoThrow then
        rethrow_expr fmt (fun _ -> funcall loc fmt f tes es)
      else
        funcall loc fmt f tes es
  | Expr_Var v ->
      if Ident.equal v true_ident then ident_str fmt "true"
      else if Ident.equal v false_ident then ident_str fmt "false"
      else begin
        pointer fmt v;
        ident fmt v
      end
  | Expr_Array (a, i) ->
      PP.fprintf fmt "%a[%a]"
        (expr loc) a
        (index_expr loc) i
  | Expr_In (e, Pat_Lit (VMask m)) ->
      Runtime.in_bits fmt m.n (mk_expr loc e) m
  | Expr_AsConstraint (e, _)
  | Expr_AsType (e, _) ->
      expr loc fmt e
  | Expr_ArrayInit _
  | Expr_Binop _
  | Expr_Fields _
  | Expr_In _
  | Expr_RecordInit _
  | Expr_Slices _
  | Expr_Tuple _
  | Expr_Unknown _
  | Expr_Unop _
  | Expr_UApply _
    ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

and exprs (loc : Loc.t) (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep (expr loc) fmt es

(* The same as expr except that it guarantees that the result is a legal type
 * to use as a C/C++ array index.
 *)
and index_expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | Expr_TApply (f, _, [x'], _) when Ident.equal f cvt_sintN_int ->
      Runtime.ffi_asl2c_sintN_small fmt (mk_expr loc x')
  | _ ->
      Runtime.ffi_asl2c_integer_small fmt (mk_expr loc x)
  )

and varty ?(const_ref = false) (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (x : AST.ty) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match x with
  | Type_Bits (n, _) ->
    if const_ref then PP.fprintf fmt "const ";
    Runtime.ty_bits fmt (const_int_expr loc n);
    nbsp fmt;
    if const_ref then PP.fprintf fmt "&";
    ident fmt v
  | Type_Constructor (tc, []) ->
      ( match tc with
      | i when Ident.equal i boolean_ident ->
        PP.fprintf fmt "bool %a"
          ident v
      | i when Ident.equal i string_ident ->
        PP.fprintf fmt "const char *%a"
          ident v
      | _ ->
        if const_ref then
          PP.fprintf fmt "const %a &%a"
            ident tc
            ident v
        else
          PP.fprintf fmt "%a %a"
            ident tc
            ident v
      )
  | Type_Constructor (i, [n]) when Ident.equal i Builtin_idents.sintN ->
    Runtime.ty_sintN fmt (const_int_expr loc n);
    nbsp fmt;
    ident fmt v
  | Type_Constructor (i, [_]) when Ident.equal i Builtin_idents.ram ->
    Runtime.ty_ram fmt;
    nbsp fmt;
    ident fmt v
  | Type_Integer _ ->
    Runtime.ty_int fmt;
    nbsp fmt;
    ident fmt v
  | Type_Array (Index_Enum tc, ety) ->
    varty ~const_ref:const_ref loc fmt v ety;
    PP.fprintf fmt "[%a]" ident tc
  | Type_Array (Index_Int sz, ety) ->
    varty ~const_ref:const_ref loc fmt v ety;
    PP.fprintf fmt "[%d]" (const_int_expr loc sz)
  | Type_Constructor (_, _)
  | Type_OfExpr _
  | Type_Tuple _
  ->
      let pp fmt = FMT.ty fmt x in
      raise (Error.Unimplemented (loc, "type", pp))
  )

and varoty (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (ot : AST.ty option) : unit =
  match ot with
  | None -> raise (InternalError (loc, "expected identifier to have a type", (fun fmt -> FMT.varname fmt v), __LOC__))
  | Some t -> varty loc fmt v t

let pattern (loc : Loc.t) (fmt : PP.formatter) (x : AST.pattern) : unit =
  ( match x with
  | Pat_Lit (VInt c) ->
    if not (Z.fits_int64 c) then begin
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "large (> 64 bit) integer pattern", pp))
    end;
    PP.fprintf fmt "%sLL" (Z.format "%d" c)
  | Pat_Lit (VBits c) ->
    if c.n > 64 then begin
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "large (> 64 bit) bitvector pattern", pp))
    end;
    PP.fprintf fmt "0x%sULL" (Z.format "%x" c.v)
  | Pat_Lit v -> valueLit loc fmt v
  | Pat_Const v ->
      if Ident.equal v true_ident then ident_str fmt "true"
      else if Ident.equal v false_ident then ident_str fmt "false"
      else ident fmt v
  | Pat_Range _ | Pat_Set _ | Pat_Single _
  | Pat_Tuple _ | Pat_Wildcard ->
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "pattern", pp))
  )

let rec lexpr (loc : Loc.t) (fmt : PP.formatter) (x : AST.lexpr) : unit =
  ( match x with
  | LExpr_Var v ->
    pointer fmt v;
    ident fmt v
  | LExpr_Array (a, i) ->
    PP.fprintf fmt "%a[%a]"
      (lexpr loc) a
      (index_expr loc) i
  | LExpr_Field (l, f) ->
    PP.fprintf fmt "%a.%a"
      (lexpr loc) l
      ident f
  | LExpr_Wildcard
  | LExpr_BitTuple _
  | LExpr_Fields _
  | LExpr_ReadWrite _
  | LExpr_Slices _
  | LExpr_Tuple _
  | LExpr_Write _ ->
      let pp fmt = FMT.lexpr fmt x in
      raise (Error.Unimplemented (loc, "l-expression", pp))
  )

let mk_lexpr (loc : Loc.t) (l : AST.lexpr) : rt_expr = fun fmt -> lexpr loc fmt l

let lslice (loc : Loc.t) (fmt : PP.formatter) (t : AST.ty) (l : AST.lexpr) (r : AST.expr) (s : AST.slice) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  ( match (t, s) with
  | (Type_Bits (n, _), Slice_LoWd (lo, wd)) ->
      Runtime.set_slice fmt (const_int_expr loc n) (const_int_expr loc wd) (mk_lexpr loc l) (fun fmt -> index_expr loc fmt lo) (mk_expr loc r)
  | (Type_Integer _, Slice_LoWd (lo, wd)) ->
      Runtime.set_slice_int fmt (const_int_expr loc wd) (mk_lexpr loc l) (fun fmt -> index_expr loc fmt lo) (mk_expr loc r)
  | _ -> raise (InternalError (loc, "Only Slice_LoWd is supported", (fun fmt -> FMT.lexpr fmt l), __LOC__))
  )

let lexpr_assign (loc : Loc.t) (fmt : PP.formatter) (x : AST.lexpr) (r : AST.expr) : unit =
  ( match x with
  | LExpr_Slices (t, l, ss) ->
      List.iter (lslice loc fmt t l r) ss;
  | LExpr_Wildcard ->
      PP.fprintf fmt "(void)%a;"
        (expr loc) r
  | _ ->
      PP.fprintf fmt "%a = %a;@,"
        (lexpr loc) x
        (expr loc) r
  )

let rec declitem (loc : Loc.t) (fmt : PP.formatter) (x : AST.decl_item) =
  ( match x with
  | DeclItem_Var (v, Some t) ->
      varty loc fmt v t;
      PP.fprintf fmt ";@,"
  | DeclItem_Tuple dis ->
      cutsep (declitem loc) fmt dis;
      cut fmt
  | DeclItem_BitTuple dbs ->
      let pp fmt = FMT.decl_item fmt x in
      raise (Error.Unimplemented (loc, "declitem: bittuple", pp))
  | DeclItem_Var (v, None) ->
      let pp fmt = FMT.varname fmt v in
      raise (Error.Unimplemented (loc, "decl: type of variable unknown", pp))
  | DeclItem_Wildcard _ -> ()
  )

let decl (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, (Type_Constructor (i, [ _ ]) as t), loc)
    when Ident.equal i Builtin_idents.ram ->
      List.iter (fun v ->
          varty loc fmt v t;
          PP.fprintf fmt " __attribute__((cleanup(ASL_ram_free)));@,"
      )
      vs
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      List.iter (fun v ->
          varty loc fmt v t;
          PP.fprintf fmt ";@,"
      )
      vs
  | Stmt_VarDecl (di, i, loc) | Stmt_ConstDecl (di, i, loc) -> declitem loc fmt di
  | _ -> ()
  )

let direction (x : AST.direction) (up : 'a) (down : 'a) : 'a =
  ( match x with
  | Direction_Up -> up
  | Direction_Down -> down
  )

let stmt_line_info (fmt : PP.formatter) (x : AST.stmt) : unit =
  if !include_line_info then
    ( match Asl_utils.stmt_loc x with
    | Range (p, _) ->
        let fname = p.Lexing.pos_fname in
        let line = p.Lexing.pos_lnum in
        PP.fprintf fmt "#line %d \"%s\"@," line fname
    | _ -> ()
    )

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  stmt_line_info fmt x;
  ( match x with
  | Stmt_Assert (e, loc) ->
      PP.fprintf fmt "ASL_assert(\"%s\", \"%s\", %a);"
        (String.escaped (Loc.to_string loc))
        (String.escaped (Utils.to_string2 (Fun.flip FMT.expr e)))
        (expr loc) e
  | Stmt_Assign (l, r, loc) ->
      lexpr_assign loc fmt l r
  | Stmt_Block (ss, _) ->
      PP.fprintf fmt "{%a@,}" indented_block ss
  | Stmt_Case (e, Some ty, alts, ob, loc) ->
      vbox fmt (fun _ ->
          PP.fprintf fmt "switch (";
          ( match ty with
          | Type_Integer _
          -> Runtime.ffi_asl2c_integer_small fmt (mk_expr loc e)
          | Type_Bits (n,_)
          -> Runtime.ffi_asl2c_bits_small 64 fmt (mk_expr loc e)
          | _
          -> expr loc fmt e
          );
          PP.fprintf fmt ") {@,";
          indented fmt (fun _ ->
              map fmt
                (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                  if Option.is_some oc then
                    raise (Error.Unimplemented (loc, "pattern_guard", fun fmt -> ()));
                  List.iter (PP.fprintf fmt "case %a:@," (pattern loc)) ps;
                  Format.fprintf fmt "{%a@,    break;@,}@," indented_block ss)
                alts;

              PP.fprintf fmt "default: {";
              ( match ob with
              | Some (b, bl) ->
                  indented_block fmt b;
              | None ->
                  indented fmt (fun _ ->
                    Format.fprintf fmt "ASL_error_unmatched_case(\"%s\");@,"
                      (String.escaped (Loc.to_string loc))
                  )
              );
              PP.fprintf fmt "@,}"
            );
          PP.fprintf fmt "@,}"
      )
  | Stmt_VarDecl (DeclItem_Var (v, _), i, loc)
  | Stmt_ConstDecl (DeclItem_Var (v, _), i, loc) ->
      Format.fprintf fmt "%a = %a;"
        ident v
        (expr loc) i
  | Stmt_VarDecl (DeclItem_Wildcard _, i, loc)
  | Stmt_ConstDecl (DeclItem_Wildcard _, i, loc) ->
      PP.fprintf fmt "(void)%a;"
        (expr loc) i
  | Stmt_For (v, ty, f, dir, t, b, loc) ->
      PP.fprintf fmt  "for (%a = %a; %a %s %a; %s%a) {%a@,}"
          (fun fmt _ -> varty loc fmt v ty) ()
          (expr loc) f

          ident v
          (direction dir "<=" ">=")
          (expr loc) t

          (direction dir "++" "--")
          ident v
          indented_block b
  | Stmt_FunReturn (e, loc) ->
      PP.fprintf fmt "return %a;" (expr loc) e
  | Stmt_If (c, t, els, (e, el), loc) ->
      vbox fmt (fun _ ->
          PP.fprintf fmt "if (%a) {%a@,}"
            (expr loc) c
            indented_block t;
          map fmt
            (fun (AST.S_Elsif_Cond (c, s, loc)) ->
              PP.fprintf fmt " else if (%a) {%a@,}"
                (expr loc) c
                indented_block s)
            els;
          if e <> [] then begin
            PP.fprintf fmt " else {%a@,}"
              indented_block e
          end)
  | Stmt_While (c, b, loc) ->
      PP.fprintf fmt "while (%a) {%a@,}"
        (expr loc) c
        indented_block b
  | Stmt_Repeat (b, c, pos, loc) ->
      PP.fprintf fmt "do {%a@,} while (!(%a));"
        indented_block b
        (expr loc) c
  | Stmt_ProcReturn loc ->
      PP.fprintf fmt "return;"
  | Stmt_TCall (f, tes, args, throws, loc) ->
      funcall loc fmt f tes args;
      semicolon fmt;
      if throws <> NoThrow then rethrow_stmt fmt
  | Stmt_VarDeclsNoInit (vs, Type_Constructor (i, [ _ ]), loc)
    when Ident.equal i Builtin_idents.ram ->
      cutsep (fun fmt' -> PP.fprintf fmt' "%a = ASL_ram_alloc();" ident) fmt vs
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      (* handled by decl *)
      ()
  | Stmt_Throw (e, loc) ->
      PP.fprintf fmt "ASL_exception = %a;@," (expr loc) e;
      PP.fprintf fmt "goto %a;" ident (current_catch_label ())
  | Stmt_Try (tb, pos, catchers, odefault, loc) ->
      with_catch_label (fun catcher ->
        PP.fprintf fmt "{%a@,}" indented_block tb;
        if Catcher.is_active catcher then
          PP.fprintf fmt "@,%a:" ident (Catcher.get_label catcher);
        cut fmt
      );
      PP.fprintf fmt "if (ASL_exception._exc.ASL_tag == ASL_no_exception) {@,";
      List.iter (function AST.Catcher_Guarded (v, tc, b, loc) ->
          PP.fprintf fmt "} else if (ASL_exception._exc.ASL_tag == tag_%a) {" ident tc;
          indented fmt (fun _ ->
            PP.fprintf fmt "%a %a = ASL_exception._%a;@,"
              ident tc
              ident v
              ident tc;
            PP.fprintf fmt "ASL_exception._exc.ASL_tag = ASL_no_exception;@,";
            PP.fprintf fmt "{%a@,}" indented_block b
            );
          cut fmt
        )
        catchers;
      PP.fprintf fmt "} else {";
      indented fmt (fun _ ->
        ( match odefault with
        | None -> PP.fprintf fmt "goto %a;@," ident (current_catch_label ())
        | Some (s, _) ->
            PP.fprintf fmt "ASL_exception._exc.ASL_tag = ASL_no_exception;@,";
            PP.fprintf fmt "{%a@,}" indented_block s
        ));
      PP.fprintf fmt "@,}"
  | Stmt_VarDecl _
  | Stmt_ConstDecl _
  | Stmt_Case _
  | Stmt_UCall _
  ->
    let pp fmt = FMT.stmt fmt x in
    raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then begin
    indented fmt (fun _ ->
      map fmt (decl fmt) xs;
      cutsep stmt fmt xs)
  end

let formal (loc : Loc.t) (fmt : PP.formatter) (x : Ident.t * AST.ty * AST.expr option) : unit =
  let (v, t, _) = x in
  varty ~const_ref:(use_const_ref loc t) loc fmt v t

let function_header (loc : Loc.t) (fmt : PP.formatter) (f : Ident.t) (fty : AST.function_type) : unit =
  PP.pp_print_option
    ~none:(fun _ _ -> PP.fprintf fmt "void "; ident fmt f)
    (fun _ t -> varty loc fmt f t) fmt fty.rty;
  parens fmt (fun _ -> commasep (formal loc) fmt fty.args)

let function_body (loc : Loc.t) (fmt : PP.formatter) (b : AST.stmt list) (orty : AST.ty option) : unit =
  with_catch_label (fun catcher ->
    braces fmt
      (fun _ ->
         indented_block fmt b;
         cut fmt;

         if Catcher.is_active catcher then (
           PP.fprintf fmt "%a:" ident (Catcher.get_label catcher);
           (* When throwing an exception, we need to return a value with
            * the correct type. This value will never be used so the easy way
            * to create it is to declare a variable, not initialize it and
            * return the variables. This will make the C compiler emit a warning.
            * *)
           indented fmt (fun _ ->
             match orty with
             | None -> PP.fprintf fmt "return;"
             | Some rty ->
               braces fmt (fun _ ->
                 indented fmt (fun _ ->
                   let v = asl_fake_return_value in
                   varty loc fmt v rty;
                   PP.fprintf fmt ";@,return %a;" ident v
                 );
                 cut fmt
               )
             );
           cut fmt
         )
      )
    )

let pp_field (loc : Loc.t) (fmt : PP.formatter) (f : (Ident.t * AST.ty)) : unit =
  let (fname, t) = f in
  varty loc fmt fname t;
  semicolon fmt

let declaration (fmt : PP.formatter) ?(is_extern : bool option) (x : AST.declaration) : unit =
  let is_extern_val = Option.value is_extern ~default:false in
  vbox fmt (fun _ ->
      match x with
      | Decl_BuiltinType (tc, loc) ->
          ( match tc with
          | _ when Ident.in_list tc [
              real_ident;
              string_ident;
              mask_ident;
              ram
            ] -> ()
          | _ ->
              let pp fmt = FMT.tycon fmt tc in
              raise (Error.Unimplemented (Loc.Unknown, "builtin type", pp))
          )
      | Decl_Const (v, oty, e, loc) ->
          PP.fprintf fmt "const ";
          varoty loc fmt v oty;
          if not is_extern_val then (
            PP.fprintf fmt " = ";
            ( match e with
            | Expr_ArrayInit es -> PP.fprintf fmt "{ %a }" (exprs loc) es
            | _ -> expr loc fmt e
            )
          );
          PP.fprintf fmt ";@,@,"
      | Decl_Config (v, ty, i, loc) ->
          varty loc fmt v ty;
          if not is_extern_val then (
            PP.fprintf fmt " = ";
            expr loc fmt i
          );
          PP.fprintf fmt ";@,@,"
      | Decl_Enum (tc, es, loc) ->
          if Ident.equal tc boolean_ident then (
              (* is in C99 stdbool.h *)
          ) else (
            PP.fprintf fmt "typedef enum %a {%a} %a;@,@,"
              ident tc
              (commasep ident) es
              ident tc;
          )
      | Decl_FunDefn (f, fty, b, loc) ->
          function_header loc fmt f fty;
          nbsp fmt;
          function_body loc fmt b fty.rty;
          PP.fprintf fmt "@,@,"
      | Decl_FunType (f, fty, loc) ->
          function_header loc fmt f fty;
          PP.fprintf fmt ";@,@,"
      | Decl_Record (tc, [], fs, loc) ->
          PP.fprintf fmt "typedef struct {";
          indented fmt (fun _ -> cutsep (pp_field loc) fmt fs);
          PP.fprintf fmt "@,} %a;@,@," ident tc
      | Decl_Typedef (tc, [], t, loc) ->
          PP.fprintf fmt "typedef ";
          varty loc fmt tc t;
          PP.fprintf fmt ";@,@,"
      | Decl_Var (v, ty, loc) ->
          (match ty with
          | Type_Constructor (i, [ _ ])
            when Ident.equal i Builtin_idents.ram && not is_extern_val ->
              let ram fmt v = PP.fprintf fmt "ASL_%a" ident v in
              ( match Bindings.find_opt v !var_ptrs with
              | Some (s, ptr) -> (* if RAM is in a struct, initializer function needs to set pointer to the RAM object *)
                PP.fprintf fmt "struct ASL_ram %a;@," ram v;
                varty loc fmt v ty;
                PP.fprintf fmt ";@,";
                add_initializer s (PP.asprintf "p->%a = (struct ASL_ram){ 0 };" ram v);
                add_initializer s (PP.asprintf "p->%a = &p->%a;" ident v ram v)
              | None -> (* if RAM is not in a struct, it can be initialized directly *)
                PP.fprintf fmt "struct ASL_ram %a = (struct ASL_ram){ 0 };@," ram v;
                varty loc fmt v ty;
                PP.fprintf fmt " = &%a;@,@," ram v;
              )
          | _ ->
              varty loc fmt v ty;
              PP.fprintf fmt ";@,"
          );
      | Decl_BuiltinFunction (f, fty, loc) ->
          ()
      | _ ->
          let pp fmt = FMT.declaration fmt x in
          raise (Error.Unimplemented (Loc.Unknown, "declaration", pp))
      )

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

let extern_declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ ->
      map fmt
        (fun x ->
          PP.fprintf fmt "extern @?";
          declaration fmt ~is_extern:true x)
        xs)

let exceptions (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ ->
    let excs = List.filter_map
        ( function
        | AST.Decl_Exception (tc, fs, loc) -> Some (tc, fs, loc)
        | _ -> None
        )
        xs
    in
    exception_tcs := List.map (fun (tc, _, _) -> tc) excs;
    PP.fprintf fmt "typedef enum ASL_exception_tag { ASL_no_exception, %a } ASL_exception_tag_t;@,@,"
      (commasep (fun fmt' (tc, _, _) -> PP.fprintf fmt' "tag_%a" ident tc)) excs;
    List.iter (fun (tc, fs, loc) ->
        PP.fprintf fmt "typedef struct {@,";
        PP.fprintf fmt "    ASL_exception_tag_t ASL_tag;@,";
        indented fmt (fun _ -> cutsep (pp_field loc) fmt fs);
        PP.fprintf fmt "} %a;@,@," ident tc
      )
      excs;
    PP.fprintf fmt "typedef union {\n    %s\n%a\n} ASL_exception_t;@,"
      "struct { ASL_exception_tag_t ASL_tag; } _exc;"
      (PP.pp_print_list (fun fmt (tc, _, _) ->
          PP.fprintf fmt "    %a _%a;"
            ident tc
            ident tc)
      )
      excs;
    PP.fprintf fmt "@,extern ASL_exception_t ASL_exception;@,";
  )

let exceptions_init (fmt : PP.formatter) : unit =
  vbox fmt (fun _ ->
    PP.fprintf fmt "ASL_exception_t ASL_exception =@,";
    PP.fprintf fmt "    (ASL_exception_t){ ._exc = { .ASL_tag = ASL_no_exception } };@,"
  )

(****************************************************************
 * Generating files
 *
 * Generate separate files containing
 * - types, constants and function prototypes
 * - variable definitions
 * - function definitions
 ****************************************************************)

let type_decls (xs : AST.declaration list) : AST.declaration list =
  let mk_type_decl (x : AST.declaration) : AST.declaration option =
    ( match x with
    | Decl_Enum _
    | Decl_Record _
    | Decl_Typedef _
    | Decl_FunType _
      -> Some x

    | Decl_FunDefn _
    | Decl_Const _
    | Decl_Exception _
    | Decl_Var _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_Config _
      -> None
    )
  in
  List.filter_map mk_type_decl xs

let fun_decls (xs : AST.declaration list) : AST.declaration list =
  let is_fun_decl (x : AST.declaration) : bool =
    ( match x with
    | Decl_FunDefn _
      -> true

    | Decl_Var _
    | Decl_Const _
    | Decl_Enum _
    | Decl_Record _
    | Decl_Exception _
    | Decl_Typedef _
    | Decl_FunType _
    | Decl_BuiltinType _
    | Decl_Forward _
    | Decl_BuiltinFunction _
    | Decl_Operator1 _
    | Decl_Operator2 _
    | Decl_Config _
      -> false
    )
  in
  List.filter is_fun_decl xs

(****************************************************************
 * Foreign Function Interface (FFI)
 *
 * This code is responsible for generating wrapper functions that
 * deal with differences between the runtime representations of
 * data values and checking that exported functions do not
 * throw exceptions.
 *
 * Note that this is only intended to support a subset of ASL types
 * - boolean
 * - bits(N)
 *   Either uint{8,16,32,64}_t (when the bitsize matches _exactly_)
 *   Or an array of uint64_t (for all other sizes)
 * - integer (limited to 64-bit ints)
 * - __sint(N) (for N <= 64)
 * - string
 * - enumeration types
 *
 * Omissions include records, etc.
 * This may increase slowly over time but the key to keeping this
 * manageable is to focus on doing the hard, runtime-dependent
 * parts in this tool but to leave some of the work for users to
 * do.
 ****************************************************************)

(****************************************************************
 * The core of this process is data conversion for which we
 * generate the following from the ASL variable name and type
 * - A C variable name
 * - The corresponding C type
 * - Code to convert the ASL type to the C type
 * - Code to convert the C type to the ASL type
 * The type and conversion code are Format functions.
 * Optionally, the C variable can be a pointer to a value.
 * For convenience, the ASL variable and type are also part of the
 * structure.
 ****************************************************************)

type ffi_conversion = {
    asl_name : Ident.t;
    asl_type : AST.ty;
    c_name : Ident.t;
    pp_c_type : (PP.formatter -> unit) option;
    pp_c_decl : PP.formatter -> unit;
    pp_asl_to_c : PP.formatter -> unit;
    pp_c_to_asl : PP.formatter -> unit;
}

let mk_ffi_conversion (loc : Loc.t) (indirect : bool) (c_name : Ident.t) (asl_name : Ident.t) (asl_type : AST.ty) : ffi_conversion =
  let ptr fmt _ = if indirect then PP.fprintf fmt "*" else () in
  let module Runtime = (val (!runtime) : RuntimeLib) in
  let pp_asl_name (fmt : PP.formatter) : unit = ident fmt asl_name in
  let pp_c_name (fmt : PP.formatter) : unit = ptr fmt (); ident fmt c_name in
  let mk_ffi_convert_small_bits (n : int) =
    { asl_name = asl_name;
      asl_type = asl_type;
      c_name = c_name;
      pp_c_type = Some (fun fmt -> PP.fprintf fmt "uint%d_t" n);
      pp_c_decl = (fun fmt ->
        PP.fprintf fmt "uint%d_t %a%a" n ptr () ident c_name);
      pp_asl_to_c = (fun fmt ->
        PP.fprintf fmt "%a%a = %a;"
          ptr ()
          ident c_name
          (Runtime.ffi_asl2c_bits_small n) pp_asl_name);
      pp_c_to_asl = (fun fmt ->
        varty loc fmt asl_name asl_type;
        PP.fprintf fmt " = %a;"
          (Runtime.ffi_c2asl_bits_small n) pp_c_name);
    }
  in
  let mk_ffi_convert_large_bits (n : int) =
    let chunks = (n+63) / 64 in
    { asl_name = asl_name;
      asl_type = asl_type;
      c_name = c_name;
      pp_c_type = None;
      pp_c_decl = (fun fmt ->
        PP.fprintf fmt "uint64_t %a[%d]"
          ident c_name
          chunks);
      pp_asl_to_c = (fun fmt ->
        Runtime.ffi_asl2c_bits_large fmt n pp_c_name pp_asl_name);
      pp_c_to_asl = (fun fmt ->
        Runtime.ffi_c2asl_bits_large fmt n pp_asl_name pp_c_name);
    }
  in
  ( match asl_type with
  | Type_Constructor (tc, []) when Ident.equal tc boolean_ident ->
      (* No conversion needed for boolean type since we use stdbool.h's bool already *)
      { asl_name = asl_name;
        asl_type = asl_type;
        c_name = c_name;
        pp_c_type = Some (fun fmt -> PP.fprintf fmt "bool%a" ptr ());
        pp_c_decl = (fun fmt -> PP.fprintf fmt "bool %a%a" ptr () ident c_name);
        pp_asl_to_c = (fun fmt -> PP.fprintf fmt "%a%a = %a;" ptr () ident c_name ident asl_name);
        pp_c_to_asl = (fun fmt -> PP.fprintf fmt "bool %a = %a%a;" ident asl_name ptr () ident c_name);
      }
  | Type_Constructor (tc, []) when Ident.equal tc string_ident ->
      (* No conversion needed for string type since we use 'const char*' already.
       * Note that any strings generated by ASL code are constants (and should not
       * be freed) and that freeing of any strings passed to ASL from C code need to be
       * managed by the C world.
       *)
      { asl_name = asl_name;
        asl_type = asl_type;
        c_name = c_name;
        pp_c_type = Some (fun fmt -> PP.fprintf fmt "const char*%a" ptr ());
        pp_c_decl = (fun fmt -> PP.fprintf fmt "const char *%a%a" ptr () ident c_name);
        pp_asl_to_c = (fun fmt -> PP.fprintf fmt "%a%a = %a;" ptr () ident c_name ident asl_name);
        pp_c_to_asl = (fun fmt -> PP.fprintf fmt "const char *%a = %a%a;" ident asl_name ptr () ident c_name);
      }
  | Type_Constructor (tc, []) when is_enumerated_type tc ->
      (* No conversion needed for enumerated type since we use an identical enumeration *)
      { asl_name = asl_name;
        asl_type = asl_type;
        c_name = c_name;
        pp_c_type = Some (fun fmt -> PP.fprintf fmt "enum %a%a" ident tc ptr ());
        pp_c_decl = (fun fmt -> PP.fprintf fmt "enum %a %a%a" ident tc ptr () ident c_name);
        pp_asl_to_c = (fun fmt -> PP.fprintf fmt "%a%a = %a;" ptr () ident c_name ident asl_name);
        pp_c_to_asl = (fun fmt -> PP.fprintf fmt "%a %a = %a%a;" ident tc ident asl_name ptr () ident c_name);
      }
  | Type_Integer _ ->
      { asl_name = asl_name;
        asl_type = asl_type;
        c_name = c_name;
        pp_c_type = Some (fun fmt -> PP.fprintf fmt "int%a" ptr ());
        pp_c_decl = (fun fmt -> PP.fprintf fmt "int %a%a" ptr () ident c_name);
        pp_asl_to_c = (fun fmt ->
          PP.fprintf fmt "%a%a = %a;"
            ptr ()
            ident c_name
            Runtime.ffi_asl2c_integer_small pp_asl_name);
        pp_c_to_asl = (fun fmt ->
          varty loc fmt asl_name asl_type;
          PP.fprintf fmt " = %a;"
            Runtime.ffi_c2asl_integer_small pp_c_name);
      }
  | Type_Constructor (tc, [Expr_Lit (VInt n)])
    when Ident.equal tc Builtin_idents.sintN
         && Z.to_int n <= 64
    ->
      { asl_name = asl_name;
        asl_type = asl_type;
        c_name = c_name;
        pp_c_type = Some (fun fmt -> PP.fprintf fmt "int%a" ptr ());
        pp_c_decl = (fun fmt -> PP.fprintf fmt "int %a%a" ptr () ident c_name);
        pp_asl_to_c = (fun fmt ->
          PP.fprintf fmt "%a%a = %a;"
            ptr ()
            ident c_name
            Runtime.ffi_asl2c_sintN_small pp_asl_name);
        pp_c_to_asl = (fun fmt ->
          varty loc fmt asl_name asl_type;
          PP.fprintf fmt " = %a;"
            Runtime.ffi_c2asl_sintN_small pp_c_name);
      }
  | Type_Bits(Expr_Lit (VInt n), _)
    when List.mem (Z.to_int n) [8; 16; 32; 64]
    -> mk_ffi_convert_small_bits (Z.to_int n)
  | Type_Bits(Expr_TApply (f, _, [Expr_Lit (VIntN n)], _), _)
    when Ident.equal f cvt_sintN_int && List.mem (Z.to_int n.v) [8; 16; 32; 64]
    -> mk_ffi_convert_small_bits (Z.to_int n.v)
  | Type_Bits(Expr_Lit (VInt n), _)
    -> mk_ffi_convert_large_bits (Z.to_int n)
  | Type_Bits(Expr_TApply (f, _, [Expr_Lit (VIntN n)], _), _)
    when Ident.equal f cvt_sintN_int
    -> mk_ffi_convert_large_bits (Z.to_int n.v)
  | _ ->
      let msg = PP.asprintf "Type '%a' cannot be used in functions that are imported or exported between ASL and C"
        FMT.ty asl_type
      in
      raise (Error.TypeError (loc, msg))
  )

(****************************************************************
 * The main ingredients of import and export wrappers for a
 * function are what you would expect:
 *
 * On import (allowing ASL to call a C function):
 * - convert the arguments from ASL to C
 * - convert the results from C to ASL
 *
 * On export (allowing C to call an ASL function):
 * - convert the arguments from C to ASL
 * - convert the results from ASL to C
 *
 * The only awkward details are:
 * - functions that return ()/void - don't convert the result
 * - functions that return bits(N) where N is not in {8, 16, 32, 64}
 *   - add them all as out-arguments
 * - functions that return multiple results in a tuple.
 *   By the time they reach this code generator, these have
 *   been transformed into functions that return an ASL record
 *   but, since this is just an artifact of the current code generator,
 *   we do not expose the struct in the interface.
 *   Instead, the function takes some number of additional arguments
 *   which are pointers to write each part of the result to.
 *   (Supporting this is quite awkward because it breaks the pattern
 *   that there is a 1:1 relationship between ASL values and C values.)
 ****************************************************************)

(** FFI function checks *)
let ffi_function_checks (loc : Loc.t) (is_import : bool) (asl_name : Ident.t) (fty : AST.function_type) : unit =
  let direction = if is_import then "Import" else "Export" in
  if not (Utils.is_empty fty.parameters)
    || Option.is_some fty.setter_arg
    || fty.use_array_syntax
    || fty.is_getter_setter
  then begin
      let msg = PP.asprintf "Function '%a' with type '%a' cannot be used in %sed functions"
        ident asl_name
        FMT.function_type fty
        direction
      in
      raise (Error.TypeError (loc, msg))
  end;

  if fty.throws != NoThrow then begin
    if is_import then begin
      PP.printf "Warning: imported function '%a' can throw an exception. These will be ignored.@,"
        ident asl_name
    end else begin
      PP.printf "Warning: exported function '%a' can throw an exception. Exceptions will be treated as errors.@,"
        ident asl_name
    end
  end

(** Generate an ffi wrapper function that converts inputs, calls the function,
 *  and converts outputs.
 *
 * There are five main parts to exporting an ASL function to C
 * - the C function header
 * - convert all argument values from their C representation to their ASL representation
 * - call the ASL function with the ASL arguments
 * - convert all ASL result values from their ASL representation to their C representation
 * - the return statement
 *)
let mk_ffi_export_wrapper
    (loc : Loc.t)
    (asl_name : Ident.t)
    (c_name : Ident.t)
    (fty : AST.function_type)
    : (PP.formatter -> unit) * (PP.formatter -> unit)
    =
  ffi_function_checks loc false asl_name fty;

  let (pp_input_decls, pp_input_cvts, input_args) =
      fty.args
      |> List.map (fun (asl_arg_name, asl_arg_ty, _) ->
           let c_name = Ident.add_prefix asl_arg_name ~prefix:"_" in
           let input = mk_ffi_conversion loc false c_name asl_arg_name asl_arg_ty in
           ( input.pp_c_decl
           , input.pp_c_to_asl
           , input.asl_name
           )
         )
      |> Utils.split3
  in

  let pp_funlist fs fmt = List.iter (fun f -> f fmt; PP.fprintf fmt "@,") fs in
  let pp_stmtlist fs fmt = List.iter (fun f -> f fmt; PP.fprintf fmt ";@,") fs in

  (* Most of the complexity is in dealing with tuple returns *)
  let asl_ret_name = Ident.mk_ident "ret" in
  let (pp_c_ret_type, pp_c_ret_value, pp_output_arg_decls, pp_output_cvts) =
      let pp_void_type fmt = PP.fprintf fmt "void" in
      ( match fty.rty with
      | None -> (pp_void_type, (fun fmt -> ()), [], [])
      | Some ty ->
          let c_name = Ident.add_prefix asl_ret_name ~prefix:"c_" in
          ( match fields_of_return_type ty with
          | Some fs ->
              let (pp_extracts, pp_c_decls, pp_c_names, pp_cvts) =
                fs
                |> List.map (fun (asl_field_name, ty) ->
                     let asl_name = Ident.add_prefix asl_field_name ~prefix:"asl_" in
                     let c_name = Ident.add_prefix asl_field_name ~prefix:"c_" in
                     let field = mk_ffi_conversion loc true c_name asl_name ty in
                     let pp_extract fmt =
                         varty loc fmt asl_name field.asl_type;
                         PP.fprintf fmt " = %a.%a;"
                           Ident.pp asl_ret_name
                           Ident.pp asl_field_name
                     in
                     (pp_extract, field.pp_c_decl, (fun fmt -> Ident.pp fmt field.c_name), field.pp_asl_to_c)
                   )
                |> Utils.split4
              in
              ( pp_void_type
              , (fun fmt -> ())
              , pp_c_decls
              , pp_extracts @ pp_cvts
              )
          | None ->
            let output = mk_ffi_conversion loc false c_name asl_ret_name ty in
            ( match output.pp_c_type with
            | Some pp ->
                ( pp
                , (fun fmt -> Ident.pp fmt output.c_name)
                , []
                , [pp_stmtlist [output.pp_c_decl]; output.pp_asl_to_c]
                )
            | None ->
                (* Functions can't directly return arrays, so add to C arg list *)
                ( pp_void_type
                , (fun fmt -> ())
                , [output.pp_c_decl]
                , [output.pp_asl_to_c]
                )
            )
          )
      )
  in

  let pp_c_function_header fmt _ =
    pp_c_ret_type fmt;
    PP.fprintf fmt " %a(%a)"
      ident c_name
      (commasep (fun fmt pp -> pp fmt)) (pp_input_decls @ pp_output_arg_decls)
  in

  (* generate body of export wrapper *)
  let pp_export_body fmt =
    pp_funlist pp_input_cvts fmt;
    PP.fprintf fmt "ASL_exception._exc.ASL_tag = ASL_no_exception;@,";
    ( match fty.rty with
    | None -> ()
    | Some rty ->
        varty loc fmt asl_ret_name rty;
        PP.fprintf fmt " = "
    );
    PP.fprintf fmt "%a(%a);@,"
      ident asl_name
      (commasep Ident.pp) input_args;
    PP.fprintf fmt "if (ASL_exception._exc.ASL_tag != ASL_no_exception) ASL_error(\"%a\", \"uncaught exception\");@,"
      ident asl_name;
    pp_funlist pp_output_cvts fmt;
    PP.fprintf fmt "return %a;" (fun fmt _ -> pp_c_ret_value fmt) ()
  in

  let pp_proto fmt = PP.fprintf fmt "%a;@." pp_c_function_header ()
  in

  let pp_wrapper fmt =
    PP.fprintf fmt "// Export wrapper for %a@.@." ident c_name;
    wrap_extern true fmt (fun fmt ->
      PP.fprintf fmt "%a {" pp_c_function_header ();
      indented fmt (fun _ -> pp_export_body fmt);
      PP.fprintf fmt "@,}@."
    )
  in

  (pp_proto, pp_wrapper)

(** Generate an ffi wrapper function that converts inputs, calls the function,
 *  and converts outputs.
 *
 * There are five main parts to importing a C function to ASL
 * - the ASL function header
 * - convert all argument values from their ASL representation to their C representation
 * - call the C function with the C arguments
 * - convert all C result values from their C representation to their ASL representation
 * - the return statement
 *)
let mk_ffi_import_wrapper
    (loc : Loc.t)
    (asl_name : Ident.t)
    (c_name : Ident.t)
    (fty : AST.function_type)
    : (PP.formatter -> unit) * (PP.formatter -> unit)
    =

  ffi_function_checks loc true asl_name fty;

  let (pp_input_decls, pp_input_cvts, pp_input_args) =
      fty.args
      |> List.map (fun (asl_arg_name, asl_arg_ty, _) ->
           let c_name = Ident.add_prefix asl_arg_name ~prefix:"_" in
           let input = mk_ffi_conversion loc false c_name asl_arg_name asl_arg_ty in
           let pp_cvt fmt =
                 input.pp_c_decl fmt;
                 PP.fprintf fmt ";@,";
                 input.pp_asl_to_c fmt
           in
           ( input.pp_c_decl
           , pp_cvt
           , (fun fmt -> Ident.pp fmt input.c_name)
           )
         )
      |> Utils.split3
  in

  let pp_funlist fs fmt = List.iter (fun f -> f fmt; PP.fprintf fmt "@,") fs in
  let pp_stmtlist fs fmt = List.iter (fun f -> f fmt; PP.fprintf fmt ";@,") fs in

  (* Most of the complexity is in dealing with tuple returns *)
  let asl_ret_name = Ident.mk_ident "ret" in
  let (pp_c_ret_type, pp_c_ret_decl, pp_asl_ret_value, pp_output_args, pp_output_arg_decls, pp_output_var_decls, pp_output_cvts) =
      let pp_void_type fmt = PP.fprintf fmt "void" in
      ( match fty.rty with
      | None -> (pp_void_type, None, (fun fmt -> ()), [], [], [], [])
      | Some ty ->
          let c_name = Ident.add_prefix asl_ret_name ~prefix:"c_" in
          ( match fields_of_return_type ty with
          | Some fs ->
              let (pp_inserts, pp_c_arg_decls, pp_c_var_decls, pp_c_args, pp_cvts) =
                fs
                |> List.map (fun (asl_field_name, ty) ->
                     let asl_name = Ident.add_prefix asl_field_name ~prefix:"asl_" in
                     let c_name = Ident.add_prefix asl_field_name ~prefix:"c_" in
                     let field = mk_ffi_conversion loc false c_name asl_name ty in
                     let field_indirect = mk_ffi_conversion loc true c_name asl_name ty in
                     let pp_insert fmt =
                         PP.fprintf fmt "%a.%a = %a;"
                           Ident.pp asl_ret_name
                           Ident.pp asl_field_name
                           Ident.pp asl_name
                     in
                     let pp_c_arg fmt = PP.fprintf fmt "&%a" Ident.pp field.c_name in
                     (pp_insert, field_indirect.pp_c_decl, field.pp_c_decl, pp_c_arg, field.pp_c_to_asl)
                   )
                |> Utils.split5
              in
              let pp_decl_ret_var fmt =
                  varty loc fmt asl_ret_name ty;
                  PP.fprintf fmt ";"
              in
              ( pp_void_type
              , None
              , (fun fmt -> Ident.pp fmt asl_ret_name)
              , pp_c_args
              , pp_c_arg_decls
              , pp_c_var_decls
              , pp_cvts @ [pp_decl_ret_var] @ pp_inserts
              )
          | None ->
            let output = mk_ffi_conversion loc false c_name asl_ret_name ty in
            ( match output.pp_c_type with
            | Some pp ->
                ( pp
                , Some output.pp_c_decl
                , (fun fmt -> Ident.pp fmt output.asl_name)
                , []
                , []
                , []
                , [output.pp_c_to_asl]
                )
            | None ->
                (* Functions can't directly return arrays, so add to C arg list *)
                ( pp_void_type
                , None
                , (fun fmt -> Ident.pp fmt output.asl_name)
                , [(fun fmt -> Ident.pp fmt output.c_name)]
                , [output.pp_c_decl]
                , [output.pp_c_decl]
                , [output.pp_c_to_asl]
                )
            )
          )
      )
  in

  let pp_c_function_header fmt _ =
    pp_c_ret_type fmt;
    PP.fprintf fmt " %a(%a)"
      ident c_name
      (commasep (fun fmt pp -> pp fmt)) (pp_input_decls @ pp_output_arg_decls)
  in

  (* generate body of import wrapper *)
  let pp_import_body fmt =
    pp_funlist pp_input_cvts fmt;
    pp_stmtlist pp_output_var_decls fmt;
    ( match pp_c_ret_decl with
    | None -> ()
    | Some pp ->
        pp fmt;
        PP.fprintf fmt " = "
    );
    PP.fprintf fmt "%a(%a);@,"
      ident c_name
      (commasep (fun fmt f -> f fmt)) (pp_input_args @ pp_output_args);
    pp_funlist pp_output_cvts fmt;
    PP.fprintf fmt "return %a;" (fun fmt _ -> pp_asl_ret_value fmt) ()
  in

  let pp_proto fmt = PP.fprintf fmt "%a;@." pp_c_function_header ()
  in

  let pp_wrapper fmt =
    PP.fprintf fmt "// Import wrapper for %a@.@." ident c_name;
    wrap_extern true fmt pp_proto;
    function_header loc fmt asl_name fty;
    PP.fprintf fmt "@,{@,";
    indented fmt (fun _ -> pp_import_body fmt);
    PP.fprintf fmt "@,}@."
  in

  (pp_proto, pp_wrapper)

(* Build ffi wrapper functions *)
let mk_ffi_wrappers (is_import : bool) (decl_map : (AST.declaration list) Bindings.t) (c_names : string list) :
    (PP.formatter -> unit) * (PP.formatter -> unit) =
  let direction = if is_import then "Import" else "Export" in
  let missing : string list ref = ref [] in
  let infos = List.filter_map (fun c_name ->
      let asl_ident = Ident.mk_fident c_name in
      let c_ident = Ident.mk_ident c_name in
      ( match Bindings.find_opt asl_ident decl_map with
      | Some (Decl_FunType (_, fty, loc) :: _) -> Some (c_ident, asl_ident, fty, loc)
      | _ ->
          if not (is_enumerated_type c_ident) then begin
              missing := c_name :: !missing
          end;
          None
      )
    ) c_names
  in
  if not (Utils.is_empty !missing) then begin
    List.iter (PP.eprintf "Error: %sed function '%s' is not defined\n" direction) !missing;
    exit 1
  end;
  let (mk_protos, mk_defns) =
    infos
    |> List.map (fun (c_name, asl_name, fty, loc) ->
         if is_import then
           mk_ffi_import_wrapper loc asl_name c_name fty
         else
           mk_ffi_export_wrapper loc asl_name c_name fty
       )
    |> List.split
  in
  let pp_protos fmt : unit = List.iter (fun f -> f fmt) mk_protos in
  let pp_defns fmt : unit = List.iter (fun f -> f fmt) mk_defns in
  (pp_protos, pp_defns)

let ffi_track_enums (decl_map : (AST.declaration list) Bindings.t) (exports : string list) : unit =
  List.iter (fun c_name ->
    let c_ident = Ident.mk_ident c_name in
    ( match Bindings.find_opt c_ident decl_map with
    | Some (Decl_Enum (tc, es, loc) :: _) ->
      add_enumerated_type tc es
    | _ ->
      ()
    ))
    exports

let ffi_track_return_types (decls : AST.declaration list) : unit =
  List.iter (fun x ->
    ( match x with
    | AST.Decl_Record (tc, [], fs, loc) when Xform_tuples.isReturnTypeName tc ->
      add_return_type tc fs
    | _ ->
      ()
    ))
    decls

(* Generate C enumeration declarations for FFI file *)
let mk_ffi_enums (fmt : PP.formatter) : unit =
  Bindings.bindings !enumerated_types
  |> List.iter (fun (tc, es) -> PP.fprintf fmt "enum %a { %a };@," ident tc (commasep ident) es)

let config_setter_prefix = "ASL_set_config_"
let config_getter_prefix = "ASL_get_config_"

(* Generate ASL functions for reading/writing configuration variables.
 * These will then be exported so that C code can read/write the variables.
 *)
let mk_ffi_config (decls : AST.declaration list) : (string list * AST.declaration list) =
  let configs = List.filter_map (fun x ->
      ( match x with
      | AST.Decl_Config (v, ty, i, loc) -> Some (v, ty, loc)
      | _ -> None
      ))
    decls
  in

  let mk_functions ((v, ty, loc) : (Ident.t * AST.ty * Loc.t)) : (string list * AST.declaration list) =
    let getter_name = config_getter_prefix ^ Ident.name v in
    let getter_id = Ident.mk_fident getter_name in
    let getter_fty : AST.function_type = {
        parameters = [];
        args = [];
        setter_arg = None;
        rty = Some ty;
        use_array_syntax = false;
        is_getter_setter = false;
        throws = NoThrow;
      }
    in
    let getter_body = [ AST.Stmt_FunReturn (Expr_Var v, loc) ] in
    let getter_defn = AST.Decl_FunDefn (getter_id, getter_fty, getter_body, loc) in
    let getter_type = AST.Decl_FunType (getter_id, getter_fty, loc) in

    let setter_name = config_setter_prefix ^ Ident.name v in
    let setter_id = Ident.mk_fident setter_name in
    let setter_arg = Ident.mk_ident "value" in
    let setter_fty : AST.function_type = {
        parameters = [];
        args = [(setter_arg, ty, None)];
        setter_arg = None;
        rty = None;
        use_array_syntax = false;
        is_getter_setter = false;
        throws = NoThrow;
      }
    in
    let setter_body = [ AST.Stmt_Assign (LExpr_Var v, Expr_Var setter_arg, loc) ] in
    let setter_defn = AST.Decl_FunDefn (setter_id, setter_fty, setter_body, loc) in
    let setter_type = AST.Decl_FunType (setter_id, setter_fty, loc) in

    ([getter_name; setter_name], [getter_defn; getter_type; setter_defn; setter_type])
  in

  let (names, decls) = List.map mk_functions configs |> List.split in
  (List.concat names, List.concat decls)

(****************************************************************
 * File writing support
 ****************************************************************)

let get_rt_header (_ : unit) : string list =
  let module Runtime = (val (!runtime) : RuntimeLib) in
  Runtime.file_header

(* the name of the pointer to a given struct *)
let struct_ptr (s : string) : string = s ^ "_ptr"

let state_struct (fmt : PP.formatter) (name : string) (vs : AST.declaration list) : unit =
  PP.fprintf fmt "struct %s {@." name;
  indented fmt (fun _ -> declarations fmt vs);
  PP.fprintf fmt "@.};@.@."

let wrap_multi_include_protection (basename : string) (fmt : PP.formatter) (f : PP.formatter -> 'a) : 'a =
  let macro =
    String.uppercase_ascii basename
    |> String.map (fun c -> if List.mem c [ '.'; '/'; '-' ] then '_' else c)
  in
  PP.fprintf fmt "#ifndef %s@." macro;
  PP.fprintf fmt "#define %s@,@." macro;
  let r = f fmt in
  PP.fprintf fmt "#endif  // %s@." macro;
  r

let emit_c_header (is_cxx : bool) (dirname : string) (basename : string) (f : PP.formatter -> unit) : unit =
  let header_suffix = if is_cxx then ".hpp" else ".h" in
  let basename = basename ^ header_suffix in
  let filename = Filename.concat dirname basename in
  Utils.to_file filename (fun fmt ->
    wrap_multi_include_protection basename fmt f
  )

let emit_c_source (filename : string) ?(index : int option) (includes : string list)
  (f : PP.formatter -> unit) : unit
  =
  let suffix = function None -> "" | Some i -> "_" ^ string_of_int i in
  let code_suffix = if !is_cxx then ".cpp" else ".c" in
  let filename = filename ^ suffix index ^ code_suffix in
  Utils.to_file filename (fun fmt ->
      List.iter (PP.fprintf fmt "%s\n") (get_rt_header ());
      List.iter (PP.fprintf fmt "#include \"%s\"\n") includes;
      f fmt
  )

let regexps_match (res : Str.regexp list) (s : string) : bool =
  List.exists (fun re -> Str.string_match re s 0) res

let generate_files (num_c_files : int) (dirname : string) (basename : string)
    (ffi_prototypes : PP.formatter -> unit)
    (ffi_definitions : PP.formatter -> unit)
    (structs : (Str.regexp list * string) list)
    (ds : AST.declaration list) : unit =

  (* Construct
   * - the global map 'var_ptrs' from global variables to the pointer to be used
   *   to access the variable
   * - the association list 'structs' from struct names to variable declarations
   * - a list of all declarations of mutable and immutable variables not associated with a struct
   *)
  let struct_vars = ref (List.map (fun (_, s) -> (s, ref [])) structs) in
  let global_vars : AST.declaration list ref = ref [] in
  List.iter (fun d ->
      ( match d with
      | AST.Decl_Var (v, _, _) ->
          let name = Ident.name v in
          let entry = List.find_opt (fun (res, _) -> regexps_match res name) structs in
          ( match entry with
          | Some (_, s) ->
              var_ptrs := Bindings.add v (s, struct_ptr s) !var_ptrs;
              let ds = List.assoc s !struct_vars in
              ds := d :: !ds
          | None ->
              global_vars := d :: !global_vars
          )
      | Decl_Const _
      | Decl_Config _
        ->
          global_vars := d :: !global_vars
      | _ -> ()
      ))
    ds;

  let basename_t = basename ^ "_types" in
  emit_c_header !is_cxx dirname basename_t (fun fmt ->
      List.iter (PP.fprintf fmt "%s\n") (get_rt_header ());
      wrap_extern (not !is_cxx) fmt (fun fmt ->
          type_decls ds |> Asl_utils.topological_sort |> List.rev |> declarations fmt;
          Format.fprintf fmt "@,"
      )
  );

  if !new_ffi then begin
      (* Emit *_ffi.h file even if generating C++ for other files *)
      let basename_ffi = basename ^ "_ffi" in
      emit_c_header false dirname basename_ffi (fun fmt ->
          Format.fprintf fmt "#include <stdint.h>@.";
          Format.fprintf fmt "#include <stdbool.h>@.";
          wrap_extern !is_cxx fmt ffi_prototypes;
          Format.fprintf fmt "@."
      )
  end;

  let basename_e = basename ^ "_exceptions" in
  emit_c_header !is_cxx dirname basename_e (fun fmt ->
      List.iter (PP.fprintf fmt "%s\n") (get_rt_header ());
      wrap_extern (not !is_cxx) fmt (fun fmt -> exceptions fmt ds)
  );
  let basename_v = basename ^ "_vars" in
  emit_c_header !is_cxx dirname basename_v (fun fmt ->
      List.iter (PP.fprintf fmt "%s\n") (get_rt_header ());
      extern_declarations fmt !global_vars;
      List.iter (fun (s, ds) -> state_struct fmt s !ds) !struct_vars;
      List.iter
        (fun (s, _) -> Format.fprintf fmt "extern struct %s *%s;@," s (struct_ptr s))
        !struct_vars
  );

  let header_suffix = if !is_cxx then ".hpp" else ".h" in
  let gen_h_filenames =
    List.map (fun s -> s ^ header_suffix) [ basename_t; basename_e; basename_v ]
  in

  let filename_e = Filename.concat dirname basename_e in
  emit_c_source filename_e gen_h_filenames exceptions_init;

  let filename_v = Filename.concat dirname basename_v in
  emit_c_source filename_v gen_h_filenames (fun fmt ->
      declarations fmt !global_vars;
      List.iter
        (fun (s, _) -> Format.fprintf fmt "struct %s *%s;@," s (struct_ptr s))
        !struct_vars;
      List.iter
        (fun (s, _) ->
            Format.fprintf fmt "void ASL_initialize_%s(struct %s *p) {@," s s;
            List.iter
              (fun (s', i) -> if s = s' then Format.fprintf fmt "  %s\n" i;)
              !initializers;
            Format.fprintf fmt "}@,"
        )
        !struct_vars;
  );

  let ds = fun_decls ds in
  let filename_f = Filename.concat dirname (basename ^ "_funs") in
  let emit_funs ?(index : int option) (ds : AST.declaration list) : unit =
    emit_c_source filename_f ?index gen_h_filenames (fun fmt -> declarations fmt ds)
  in
  if num_c_files = 1 then begin
    emit_c_source filename_f gen_h_filenames (fun fmt ->
      declarations fmt ds;
      Format.fprintf fmt "@,";
      ffi_definitions fmt
    )
  end else begin
    let threshold = List.length ds / num_c_files in
    let rec emit_funs_by_chunk (i : int) (acc : AST.declaration list) = function
      (* last chunk *)
      | l when i = num_c_files ->
          emit_c_source filename_f ~index:i gen_h_filenames (fun fmt ->
            declarations fmt (List.rev acc @ l);
            Format.fprintf fmt "@,";
            ffi_definitions fmt
          )
      | h :: t when List.length acc < threshold ->
          emit_funs_by_chunk i (h :: acc) t
      | h :: t ->
          emit_c_source filename_f ~index:i gen_h_filenames (fun fmt -> declarations fmt (List.rev acc));
          emit_funs_by_chunk (i + 1) [ h ] t
      | [] -> emit_funs ~index:i (List.rev acc)
    in
    emit_funs_by_chunk 1 [] ds
  end

(****************************************************************
 * Command: :generate_c
 ****************************************************************)

let _ =
  let opt_dirname = ref "" in
  let opt_num_c_files = ref 1 in
  let opt_basename = ref "asl2c" in
  let opt_split_state = ref false in

  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let decls = !Commands.declarations in
    let (cfg_exports, cfg_funs) = mk_ffi_config decls in
    let decls' = decls @ cfg_funs in
    let decl_map = Asl_utils.decls_map_of decls' in
    let imports = if !new_ffi then Configuration.get_strings "imports" else [] in
    let exports = if !new_ffi then Configuration.get_strings "exports" else [] in

    ffi_track_enums decl_map exports;
    ffi_track_return_types decls';
    let (ffi_import_protos, ffi_import_defns) = mk_ffi_wrappers true decl_map imports in
    let (ffi_export_protos, ffi_export_defns) = mk_ffi_wrappers false decl_map (cfg_exports @ exports) in
    let ffi_protos (fmt : PP.formatter) : unit =
        wrap_extern true fmt (fun fmt ->
          mk_ffi_enums fmt;
          ffi_import_protos fmt;
          ffi_export_protos fmt
        )
    in

    let ffi_defns (fmt: PP.formatter) : unit =
        ffi_import_defns fmt;
        ffi_export_defns fmt
    in

    (* When splitting global state across structs is enabled,
     * the struct map is a file contains entries like
     *   "global": [ "RAM" ],
     *   "thread_local": [ ".*" ]
     * That is, a list of regular expressions associated with struct names.
     *
     * This is used to generate structs that contain entries for global
     * mutable variables that match the regular expressions.
     * And all references to those variables are indirected through a
     * global variable that points to structs of that type.
     * For example:
     *   global_ptr->RAM
     *   thread_local_ptr->RIP
     *   thread_local_ptr->GPR[i]
     * Note that immutable variables are not put in structs
     *)
    let structs = if !opt_split_state then
        let map = Configuration.get_record_entries "split_state" in
        List.map (fun (s, res) -> (List.map Str.regexp res), s) map
      else
        []
    in

    generate_files !opt_num_c_files !opt_dirname !opt_basename ffi_protos ffi_defns structs decls';
    true
  in

  let flags = Arg.align [
        ("--output-dir",   Arg.Set_string opt_dirname,         "<dirname>    Directory for output files");
        ("--basename",     Arg.Set_string opt_basename,        "<basename>   Basename of output files");
        ("--num-c-files",  Arg.Set_int opt_num_c_files,        "<num>        Number of .c files created (default: 1)");
        ("--runtime",      Arg.Symbol (runtimes, set_runtime), "fallback|c23 Select runtime system");
        ("--const-ref",    Arg.Set_int const_ref_limit,        " Use 'const &' for arguments bigger than this");
        ("--generate-cxx", Arg.Set is_cxx,                     " Generate C++ code");
        ("--new-ffi",      Arg.Set   new_ffi,                  " Use new FFI");
        ("--no-new-ffi",   Arg.Clear new_ffi,                  " Do not use new FFI");
        ("--line-info",    Arg.Set include_line_info,          " Insert line number information");
        ("--no-line-info", Arg.Clear include_line_info,        " Do not insert line number information");
        ("--split-state",  Arg.Set opt_split_state,            " Split global variables into structs");
      ]
  in
  Commands.registerCommand "generate_c_new" flags [] [] "Generate C (new)" cmd

(****************************************************************
 * End
 ****************************************************************)
