(****************************************************************
 * ISA file load and parse
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open LibISA
module AST = Isa_ast

let test_function_declaration () : unit =
  (* Create two tempoary files with contents: *)
  let (full_f1,o1) = Filename.open_temp_file "prefix" ".asl" in
  let (full_f2,o2) = Filename.open_temp_file "prefix" ".asl" in

  Out_channel.output_string o1 "function f (x : Integer) => Integer begin return x; end";
  Out_channel.output_string o2 "function main () => () begin let b = f(5); end";
  Out_channel.flush o1;
  Out_channel.flush o2;
  Out_channel.close o1;
  Out_channel.close o2;

  let paths = [Filename.get_temp_dir_name ()] in
  let f1 = Filename.basename full_f1 in
  let f2 = Filename.basename full_f2 in

  let _ = LoadISA.read_files paths [f1; f2] false in
  (* Flip the order and it should still work *)
  let _ = LoadISA.read_files paths [f2; f1] false in
  (* The test passes by not getting any exceptions *)
  ()

let tests () : unit Alcotest.test_case list =
  [
    ("test_function_declaration", `Quick, test_function_declaration)
  ]

let () = Alcotest.run "loadISA"
  [
    ("read_files", tests ())
  ]

(****************************************************************
 * End
 ****************************************************************)
