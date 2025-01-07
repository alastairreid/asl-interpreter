(****************************************************************
 * ELF loader
 *
 * Currently only handles little-endian, 64-bit
 *
 * Copyright Alastair Reid (c) 2019-2020
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(* Turn off warnings about unused values.
 * It is conveninte to have a full set of names for offsets, etc.
 * as defined by ELF even if they are not being used by the current
 * code.
 *)
[@@@warning "-32-34"]

(****************************************************************)
(** {2 ELF-independent helper types and functions}              *)
(****************************************************************)

type uint64 = Int64.t
type sint64 = Int64.t
type uint32 = Int32.t
type sint32 = Int32.t

type uint8 = Int32.t
type uint16 = Int32.t
type sint16 = Int32.t

let get1 (b : bytes) (o : int) : uint8 =
  let x0 = Bytes.get b o in
  Int32.of_int (Char.code x0)

let get2 (b : bytes) (o : int) : uint16 =
  let x0 = Char.code (Bytes.get b o) in
  let x1 = Char.code (Bytes.get b (o + 1)) in
  Int32.of_int (x0 + (x1 lsl 8))

let get4 (b : bytes) (o : int) : uint32 =
  let x0 = get2 b o in
  let x1 = get2 b (o + 2) in
  Int32.add x0 (Int32.shift_left x1 16)

let get8 (b : bytes) (o : int) : uint64 =
  let x0 = Int64.of_int32 (get4 b o) in
  let x1 = Int64.of_int32 (get4 b (o + 4)) in
  Int64.add x0 (Int64.shift_left x1 32)

(****************************************************************)
(** {2 ELF-specific helper functions and offsets}               *)
(****************************************************************)

let byte (b : bytes) (o : int) : char = Bytes.get b o
let addr (b : bytes) (o : int) : uint64 = get8 b o
let half (b : bytes) (o : int) : uint16 = get2 b o
let shalf (b : bytes) (o : int) : sint16 = get2 b o
let off (b : bytes) (o : int) : uint64 = get8 b o
let sword (b : bytes) (o : int) : sint32 = get4 b o
let word (b : bytes) (o : int) : uint32 = get4 b o
let xword (b : bytes) (o : int) : uint64 = get8 b o
let sxword (b : bytes) (o : int) : uint64 = get8 b o

(* offsets of fields in Elf64_Phdr *)
let p_type = 0
let p_flags = p_type + 4
let p_offset = p_flags + 4
let p_vaddr = p_offset + 8
let p_paddr = p_vaddr + 8
let p_filesz = p_paddr + 8
let p_memsz = p_filesz + 8
let p_align = p_memsz + 8

(* offsets of fields in Elf64_Shdr *)
let sh_name = 0
let sh_type = sh_name + 4
let sh_flags = sh_type + 4
let sh_addr = sh_flags + 8
let sh_offset = sh_addr + 8
let sh_size = sh_offset + 8
let sh_link = sh_size + 8
let sh_info = sh_link + 4
let sh_addralign = sh_info + 4
let sh_entsize = sh_addralign + 8

(* offsets of fields in Elf64_Ehdr *)
let e_ident = 0
let e_type = e_ident + 16
let e_machine = e_type + 2
let e_version = e_machine + 2
let e_entry = e_version + 4
let e_phoff = e_entry + 8
let e_shoff = e_phoff + 8
let e_flags = e_shoff + 8
let e_ehsize = e_flags + 4
let e_phentsize = e_ehsize + 2
let e_phnum = e_phentsize + 2
let e_shentsize = e_phnum + 2
let e_shnum = e_shentsize + 2
let e_shstrndx = e_shnum + 2

(* elf header constants *)
let elfCLASSNONE = Char.chr 0
let elfCLASS32 = Char.chr 1
let elfCLASS64 = Char.chr 2
let elfCLASSNUM = Char.chr 3
let elfDATANONE = Char.chr 0
let elfDATA2LSB = Char.chr 1
let elfDATA2MSB = Char.chr 2

(* segment type constants *)
let pt_NULL = Int32.of_int 0
let pt_LOAD = Int32.of_int 1
let pt_DYNAMIC = Int32.of_int 2
let pt_INTERP = Int32.of_int 3
let pt_NOTE = Int32.of_int 4
let pt_SHLIB = Int32.of_int 5
let pt_PHDR = Int32.of_int 6
let pt_TLS = Int32.of_int 7
let pt_LOOS = Int32.of_int 0x60000000
let pt_HIOS = Int32.of_int 0x6fffffff
let pt_LOPROC = Int32.of_int 0x70000000
let pt_HIPROC = Int32.of_int 0x7fffffff
let pt_GNU_EH_FRAME = Int32.of_int 0x6474e550
let pt_GNU_STACK = Int32.add pt_LOOS (Int32.of_int 0x474e551)

(****************************************************************)
(** {2 ELF Loader}                                              *)
(****************************************************************)

(** load block from offset in buffer *)
let load_block (write : uint64 -> char -> unit) (buffer : bytes) (offset : int)
    (addr : uint64) (fsz : uint64) (memsz : uint64) : unit =
  let rec copy (i : uint64) : unit =
    if i < fsz then (
      write (Int64.add addr i) (byte buffer (offset + Int64.to_int i));
      copy (Int64.succ i))
  in
  copy (Int64.of_int 0);
  let rec zero (i : uint64) : unit =
    if i < memsz then (
      write (Int64.add addr i) '\x00';
      zero (Int64.succ i))
  in
  zero fsz

(** load program header from offset in buffer *)
let load_Phdr (write : uint64 -> char -> unit) (buffer : bytes) (offset : int) :
    unit =
  if word buffer (offset + p_type) = pt_LOAD then (
    let o = off buffer (offset + p_offset) in
    let fsz = xword buffer (offset + p_filesz) in
    let paddr = addr buffer (offset + p_paddr) in
    let memsz = xword buffer (offset + p_memsz) in
    if false then
      Printf.printf "Loading program header %Lx %Lx %Lx\n" paddr fsz memsz;
    load_block write buffer (Int64.to_int o) paddr fsz memsz)

(** load ELF file, returning entry address *)
let load_file (name : string) (write : uint64 -> char -> unit) : uint64 =
  let buffer = Utils.read_file name in
  let elf64 = byte buffer (e_ident + 4) = elfCLASS64 in
  let le = byte buffer (e_ident + 5) = elfDATA2LSB in
  assert elf64;
  (* 64-bit only *)
  assert le;
  (* little endian only *)
  let ph = off buffer e_phoff in
  let phnm = half buffer e_phnum in
  let phsz = half buffer e_phentsize in
  let rec loadPHs (i : int) : unit =
    if i < Int32.to_int phnm then (
      load_Phdr write buffer (Int64.to_int ph + (i * Int32.to_int phsz));
      loadPHs (i + 1))
  in
  loadPHs 0;
  addr buffer e_entry

(****************************************************************
 * Command: :elf
 ****************************************************************)

let _ =
  let file = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Printf.printf "Loading ELF file %s.\n" !file;
    let entry = load_file !file cpu.elfwrite8 in
    Printf.printf "Entry point = 0x%Lx\n" entry;
    cpu.setPC entry;
    true
  in
  let args = [
    (file, "file");
  ]
  in
  Commands.registerCommand "elf" [] args [] "Load an ELF file" cmd

(****************************************************************
 * End
 ****************************************************************)
