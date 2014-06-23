(* *************** High-level to low-level language compiler *************** *)
(*                                                                           *)
(* This is the implementation of a compiler described in the paper titled    *)
(* "Secure Compilation to Modern Processors", accepted at CSF 2012.          *)
(*                                                                           *)
(* The compiler will output a list of low-level instructions and a memory    *)
(* descriptor when given an AST-tree of a well-typed high-level object as    *)
(* input. Some example high-level input objects are defined at the end of    *)
(* the file.                                                                 *)
(*                                                                           *)
(* Version: 0.1                                                              *)
(* Author: Pieter Agten <pieter.agten@cs.kuleuven.be>                        *)
(* Date: 12 apr 2012                                                         *)
(*                                                                           *)
(* Copyright (c) 2012, Katholieke Universiteit Leuven                        *)
(* All rights reserved.                                                      *)
(* ************************************************************************* *)


(* **************** AST elements of the high-level language **************** *)
type hl_field_index =
 FI of (int)
;;

type hl_var_index =
 VI of (int)
;;

type hl_method_index =
 MI of (int)
;;

type hl_label_index =
 LI of (int)
;;

type hl_type =
   TUnit
 | TInt
 | TMethod of (hl_type list * hl_type)
;;

type hl_value = 
   VUnit
 | VNull
 | VInt of int
 | VMethod of (hl_method_index)
;;


type hl_field_decl = 
 FDecl of (
  hl_type *            (* field type *)
  hl_field_index *     (* field index *)
  hl_value             (* initial value *)
 )
;;


type hl_var_decl =
 VDecl of (
  hl_type *            (* variable type *)
  hl_var_index *       (* variable index *)
  hl_value             (* initial value *)
 )
;;

type hl_param_decl =
 PDecl of (
  hl_type *            (* parameter type *)
  hl_var_index         (* parameter index *)
 )
;;

type hl_expression = 
   EMovi  of (hl_value)
 | EMov   of (hl_var_index)
 | EAdd   of (hl_var_index * hl_var_index)
 | ESub   of (hl_var_index * hl_var_index)
 | EField of (hl_field_index)
 | ECalli of (hl_method_index * hl_var_index list)
 | ECall  of (hl_var_index * hl_var_index list)
;;

type hl_statement = 
   SVarAssign   of (hl_var_index * hl_expression)
 | SFieldAssign of (hl_field_index * hl_var_index)
 | SJmp         of (hl_label_index)
 | SBeq         of (hl_var_index * hl_var_index * hl_label_index)
 | SBlt         of (hl_var_index * hl_var_index * hl_label_index)
 | SRet         of (hl_var_index)
;;

type hl_line = (
 hl_label_index *      (* line label *)
 hl_statement          (* statement *)
)

type hl_method_decl = 
 MDecl of (
  hl_type *            (* return type *)
  hl_method_index *    (* method index *)
  hl_param_decl list * (* parameters *)
  hl_var_decl list *   (* variable declarations and initializations *)
  hl_line list         (* method body *)
 )
;;

type hl_object_decl = 
 ODecl of (
  hl_field_decl list * (* fields *)
  hl_method_decl list  (* methods *)
 )
;;



(* ***************** Instructions of the low-level language **************** *)
type ll_label = 
  LLabel of (string)
;;

type ll_address =
  LAddr of (int)
;;

type ll_immediate =
   IInt   of (int)
 | IAddr  of (ll_address)
 | ILabelRef of (ll_label)
;;

type ll_register = 
   PC 
 | R0
 | R1
 | R2
 | R3
 | R4
 | R5
 | R6
 | R7
 | R8
 | R9
 | R10
 | R11
 | SP
;;

type ll_instruction = 
   IMovl of (ll_register * ll_register) 
 | IMovs of (ll_register * ll_register) 
 | IMovi of (ll_register * ll_immediate)
 | IAdd  of (ll_register * ll_register) 
 | ISub  of (ll_register * ll_register) 
 | ICmp  of (ll_register * ll_register) 
 | IJmp  of (ll_register) 
 | IJe   of (ll_register) 
 | IJl   of (ll_register) 
 | ICall of (ll_register) 
 | IRet                
 | IData of (ll_immediate)
 | IHalt
 | INop
;;

type ll_line = (
  ll_label option *    (* label *)
  ll_address *         (* memory address *)
  ll_instruction       (* instruction *)
)


type ll_mod_descriptor =
  ModDescriptor of (
    int *              (* number of entry points *)
    int *              (* size of code section *)
    int                (* size of data section *)
  )
;;

type ll_module =
  Module of (
    hl_object_decl *   (* reference to high-level source *)
    ll_line list *     (* module instructions *)
    ll_address *       (* next free address *)
    ll_mod_descriptor  (* module descriptor *)
 )
;;



(* ******* Translation of a high-level object to a low-level module ******** *)
exception Too_Many_Parameters;;


(* Addresses *)
let mod_base_address = 
  LAddr(0x00000000)
;;

let mod_end_address = 
  LAddr(0x7FFFFFFF)
;;

let mod_entry_base_address = 
  mod_base_address
;;

let secure_stack_base_address =
  LAddr(0x5FFFFFFF)
;;

let code_base_address = 
  mod_base_address
;;

let code_end_address = 
  LAddr(0x3FFFFFFF)
;;

let data_base_address = 
  LAddr(0x40000000)
;;

let data_end_address = 
  mod_end_address
;;

let null_address =
  IAddr(LAddr(0xFFFFFFFF))
;;

(* Sizes *)
let code_size =
  match (code_base_address, code_end_address) with 
  | (LAddr(cb), LAddr(ce)) ->
    ce - cb
;;

let data_size =
  match (data_base_address, data_end_address) with 
  | (LAddr(sb), LAddr(se)) ->
    se - sb
;;

(* Label names *)
let stack_base_label = 
  LLabel("stack_base")
;;

let field_label (FI(fi) : hl_field_index) =
  LLabel("field" ^ (string_of_int fi))
;;

let method_entry_point_label (MI(mi) : hl_method_index) =
  LLabel("method" ^ (string_of_int mi))
;;

let method_entry_point_label_suffix (MI(mi) : hl_method_index) (suffix : string) =
  LLabel("method" ^ (string_of_int mi) ^ "_" ^ suffix)
;;

let method_exit_point_label (MI(mi) : hl_method_index) =
  LLabel("method" ^ (string_of_int mi) ^ "_exit")
;;

let method_prologue_label (MI(mi) : hl_method_index) =
  LLabel("method" ^ (string_of_int mi) ^ "_prologue")
;;

let method_epilogue_label (MI(mi) : hl_method_index) =
  LLabel("method" ^ (string_of_int mi) ^ "_epilogue")
;;

let method_label_label (MI(mi) : hl_method_index) (LI(li) : hl_label_index) =
  LLabel("method" ^ (string_of_int mi) ^ "_label" ^ (string_of_int li))
;;

let return_entry_point_label = 
 LLabel("return_entry_point")
;;

let return_entry_point_label_suffix (suffix : string) = 
 LLabel("return_entry_point" ^ "_" ^ suffix)
;;

let callback_helper_label =
  LLabel("callback_helper")
;;

let full_register_cleanup_helper_label = 
  LLabel("register_cleanup_helper")
;;

let return_address_check_helper_label =
  LLabel("rac_helper")
;;

let halt_helper_label =
  LLabel("halt_helper")
;;

let shadow_stack_pointer_field_label =
  LLabel("shadow_stack_pointer")
;;

(* Register for parameter *)
let register_for_param_index (i : int) = 
  match i with
  | 0 -> R4
  | 1 -> R5
  | 2 -> R6
  | 3 -> R7
  | 4 -> R8
  | 5 -> R9
  | 6 -> R10
  | 7 -> R11
  | _ -> raise Too_Many_Parameters
;;

let register_for_param (VI(i) : hl_var_index) = 
  register_for_param_index i
;;


(* ***** Translations ***** *)
(* Translates a high-level value to a corresponding low-level value *)
let translate_value (v : hl_value) =
  match v with
   | VUnit   -> IInt(0)
   | VNull   -> null_address
   | VInt(i) -> IInt(i)
   | VMethod(mi) -> ILabelRef (method_prologue_label mi)
;;

(* Reserves some memory at the current position in the low-level module, for a
   given high-level field. *)
let generate_field (m : ll_module) (fd : hl_field_decl)  =
  match (m, fd) with
  | (Module(src, lines, LAddr(free_addr), d), FDecl(_, index, value)) ->
      let new_line = (Some(field_label index), LAddr(free_addr), IData(translate_value value)) in
      let new_addr = LAddr (free_addr + 1) in
        Module(src, lines @ [new_line], new_addr, d)
;;


(* ***** Common translation helper functions ***** *)
(* Appends a low-level instruction to the low-level module. *)
let append_instruction (m : ll_module) (i : ll_instruction) =
  match m with Module(src, lines, LAddr(free_addr), d) ->
      let new_lines = lines @ [(None, (LAddr free_addr), i)] in
      let new_addr = LAddr(free_addr + 1) in
        Module(src, new_lines, new_addr, d)
;;

(* Generates a label at the current position in the low-level module. *)
let append_label (m : ll_module) (l : ll_label) =
  match m with Module(src, lines, LAddr(free_addr), d) ->
      let new_lines = lines @ [(Some l, (LAddr free_addr), INop)] in
      let new_addr = LAddr(free_addr + 1) in
        Module(src, new_lines, new_addr, d)
;;

(* Generates instructions for loading the address of a given label into a given
 * register at the current position in the low-level module. *)
let generate_load_label (m : ll_module) (l : ll_label) (r : ll_register) =
  append_instruction m (IMovi(r, ILabelRef(l)))
;;

(* Generates instructions for jumping to a given label at the current position
 * in the low-level module. *)
let generate_label_jump (m : ll_module) (l : ll_label) =
  let m0 = generate_load_label m l R3 in
  append_instruction m0 (IJmp(R3))
;;

(* Generates instructions for calling a given label at the current position in
 * the low-level module. *)
let generate_label_call (m : ll_module) (l : ll_label) =
  let m0 = generate_load_label m l R3 in
  append_instruction m0 (ICall(R3))
;;


(* Generates a list of instructions that checks whether the value in a given 
 * register is in- or outside of the module's memory boundaries and jumps to to
 * the label lin if it is inside the memory boundaries or to the label lout
 * otherwise. The given register r cannot be R1 or R3. *)
let generate_address_in_mod_check (m : ll_module) (r : ll_register) (lin : ll_label) (lout : ll_label) =
  List.fold_left append_instruction m 
    [(IMovi(R3, ILabelRef(lout)));         (* Load out jump address into R3 *)
     (IMovi(R1, IAddr(mod_base_address))); (* Load base address into R1     *)
     (ICmp(r, R1));                        (* Jump to the out label if...   *)
     (IJl(R3));                            (* ... r < base address          *)
     (IMovi(R1, IAddr(mod_end_address)));  (* Load end address into R1      *)
     (ICmp(R1,r));                         (* Jump to the out label if...   *)
     (IJl(R3));                            (* end address < r               *)
     (IMovi(R3, ILabelRef(lin)));          (* Load in jump address into R3  *)
     (IJmp(R3))]                           (* Jump to in label              *)
;;

(* Generates a list of instructions that checks whether R0 is equal to a given
 * immediate and jumps to a given label if so. *)
let generate_equals_check (m : ll_module) (i : ll_immediate) (leq : ll_label) =
  List.fold_left append_instruction m 
    [IMovi(R3, ILabelRef(leq));
     IMovi(R1, i);
     ICmp(R0, R1);
     IJe(R3)]
;;

(* Generates a list of instructions that pushes the value in a given register
 * onto the run-time stack. *)
let generate_push_onto_stack (m : ll_module) (r : ll_register) =
  List.fold_left append_instruction m 
    [IMovi(R2, IInt(1));
     ISub(SP, R2);
     IMovs(SP, r)]                          
;;

(* ***** Method entry point generation ***** *)

(* Generates instructions for swapping the value of the SP register with the
 * shadow stack pointer field. *)
let generate_stack_switch (m : ll_module) =
  List.fold_left append_instruction m 
    [IMovi(R2, (ILabelRef shadow_stack_pointer_field_label));
     IMovl(R3, R2);       (* Load value of shadow stack pointer field into R3 *)
     IMovs(R2, SP);       (* Store value of SP in shadow stack pointer field  *)
     IMovi(SP, (IInt 0)); 
     IAdd(SP, R3)]        (* Store value of R3 in SP                          *)
;;


(* Generates instructions for clearing registers R1, R2, R3 and the flags
 * register *)
let generate_working_register_cleanup (m : ll_module) =
  List.fold_left append_instruction m 
    [IMovi(R1, IInt(0));
     IMovi(R2, IInt(0));
     IMovi(R3, IInt(0));
     ICmp(R1, R2)]
;;


(* Generates instructions that load the value at the top of the stack into R0
 * and then call the return_address_check_helper to check whether the value is
 * outside of the bounds of the protected module *)
let generate_return_address_check_call (m : ll_module) =
  List.fold_left append_instruction m 
    [IMovl(R0, SP);
     IMovi(R3, (ILabelRef return_address_check_helper_label));
     ICall(R3)]
;;

(* Returns the next entry point address. That is, this function returns the
 * closest multiple of 128 greater than or equal to the given address *)
let next_entry_point_address (LAddr(a) : ll_address) =
  let new_a = truncate (128.0 *. (ceil ((float_of_int a) /. 128.0))) in 
    LAddr(new_a)
;;




(* Generates a list of instructions forming the entry point for a given
 * high-level method *)
let generate_entry_point (m : ll_module) (md : hl_method_decl) =
  match (m, md) with (Module(src, lines, a, d), MDecl(_, index, _, _, _)) ->
      let m = Module(src, lines, next_entry_point_address a, d) in
      let m = append_label m (method_entry_point_label index) in (* Entry *)
      let m = generate_address_in_mod_check m SP halt_helper_label (method_entry_point_label_suffix index "sp_ok") in
      let m = append_label m (method_entry_point_label_suffix index "sp_ok") in
      let m = generate_stack_switch m in
      let m = generate_label_call m (method_prologue_label index) in
      let m = append_label m (method_exit_point_label index) in (* Exit *)
      let m = generate_stack_switch m in
      let m = generate_return_address_check_call m in
      let m = generate_label_call m full_register_cleanup_helper_label in
      append_instruction m IRet
;;


(* Generates a list of instructions forming the return entry point *)
let generate_return_entry_point (m : ll_module) =
  match m with Module(src, lines, a, d) ->
      let m = Module(src, lines, next_entry_point_address a, d) in
      let m = append_label m return_entry_point_label in
      let m = generate_address_in_mod_check m SP halt_helper_label (return_entry_point_label_suffix "sp_ok") in
      let m = append_label m (return_entry_point_label_suffix "sp_ok") in
      let m = generate_stack_switch m in
      append_instruction m IRet
;;

(* ***** Method code generation ***** *)
(* Generates a list of instructions to initialize a local variable. It assumes
 * the register R0 contains the value 1. *)
let generate_local_var_init (m : ll_module) (var : hl_var_decl) =
  match var with VDecl(_, index, value) ->
    List.fold_left append_instruction m 
      [ISub(SP, R0);
       IMovi(R1, translate_value value);
       IMovs(SP, R1)]
;;

(* Generates a list of instructions to push a parameter contained in a 
 * register onto the stack. It assumes the register R0 contains the value 1. *)
let generate_param_copy_to_stack (m : ll_module) (param : hl_param_decl) =
  match param with
  | PDecl(TUnit, index) ->
    List.fold_left append_instruction m 
      [ISub(SP, R0);
       IMovi(R1, IInt(0));
       IMovs(SP, R1)]
  | PDecl(_, index) ->
    List.fold_left append_instruction m 
      [ISub(SP, R0);
       IMovs(SP, register_for_param index)]
;;

(* Generates a prologue for a given high-level method. *)
let generate_prologue (m : ll_module) (md : hl_method_decl) =
  match md with MDecl(_, index, params, vars, body) ->
    let m0 = append_label m (method_prologue_label index) in
    let m1 = append_instruction m0 (IMovi(R0, IInt(1))) in
    let m2 = List.fold_left generate_param_copy_to_stack m1 params in
    List.fold_left generate_local_var_init m2 vars
;;

(* Generates instructions that load the address of a given local variable into
 * register R3. *)
let generate_var_address (m : ll_module) (VI(vi) : hl_var_index) =
  List.fold_left append_instruction m 
    [IMovi(R3, IInt(0));
     IAdd(R3, SP);
     IMovi(R2, IInt(vi));
     IAdd(R3,R2)]
;;

(* Generates instructions that store the value of a given register into a given
 * local variable. *)
let generate_store_var (m : ll_module) (vi : hl_var_index) (r : ll_register) =
  let m0 = generate_var_address m vi in
  append_instruction m0 (IMovs(R3, r))
;;

(* Generates instructions that load the value of a given local variable into
 * a given register. *)
let generate_load_var (m : ll_module) (vi : hl_var_index) (r : ll_register) =
  let m0 = generate_var_address m vi in
  append_instruction m0 (IMovl(r, R3))
;;

(* Generates instructions for storing the value of a given register into a 
 * given field *)
let generate_store_field (m : ll_module) (fi : hl_field_index) (r : ll_register) =
  List.fold_left append_instruction m 
    [IMovi(R3, ILabelRef(field_label fi));
     IMovs(R3, r)]
;;

(* Generates instructions for loading the value of a given field into a 
 * given register *)
let generate_load_field (m : ll_module) (fi : hl_field_index) (r : ll_register) =
  List.fold_left append_instruction m 
    [IMovi(R3, ILabelRef(field_label fi));
     IMovl(r, R3)]
;;

(* Generates instructions for performing a compare between two given local
 * variables and for loading the address of a given label into a given
 * register. Usefull for performing conditional jumps. *)
let generate_cjump (m : ll_module) (x1 : hl_var_index) (x2 : hl_var_index) (l : ll_label) (r : ll_register) =
  let m0 = generate_load_var   m  x1 R0 in
  let m1 = generate_load_var   m0 x2 R1 in
  let m2 = generate_load_label m1 l r in
  append_instruction m2 (ICmp(R0, R1))
;;

(* Generates instructions for clearing all registers with a given index or 
 * higher. *)
let rec generate_register_clear (m : ll_module) (i : int) =
  try (
      let m = append_instruction m (IMovi(register_for_param_index i, IInt 0)) in
      generate_register_clear m (i + 1))
  with Too_Many_Parameters -> m
;;

(* Generates instructions for copying a given local variable to a register 
 * with a given index. *)
let generate_param_copy_to_register ((m,i) : ll_module * int) (param : hl_var_index) =
  (generate_load_var m param (register_for_param_index i), i+1)
;;

(* Generates instructions for copying a given list of local variables to the
 * parameter registers. *)
let generate_params_copy_to_registers (m : ll_module) (params : hl_var_index list) =
  let copied = List.fold_left generate_param_copy_to_register (m,0) params in
  generate_register_clear (fst copied) (snd copied)
;;

let is_param (VI(i) : hl_var_index) (param : hl_param_decl) =
  match param with PDecl(typ, VI(index)) ->
    i == index
;;

let is_var (VI(i) : hl_var_index) (var : hl_var_decl) =
  match var with VDecl(typ, VI(index), init_value) ->
    i == index
;;

let get_type (md : hl_method_decl) (i : hl_var_index) =
  match md with MDecl(_, _, params, vars, _) ->
    try match (List.find (is_param i) params) with
    | PDecl(typ, _) -> typ with
    | Not_found ->
    match (List.find (is_var i) vars) with
    | VDecl(typ, _, _) -> typ
;;

let is_unit_method_ref (t : hl_type) =
  match t with
  | TMethod(_, ret) -> ret == TUnit
  | _               -> false
;;

(* Generates instructions for calculating the value of a high-level
 * expression. *)
let generate_expression (m : ll_module) (md : hl_method_decl) (e : hl_expression) =
  match e with
  | EMovi (v) -> 
     append_instruction m (IMovi(R0, translate_value v))
  | EMov (x) -> 
     generate_load_var m x R0
  | EAdd (x1, x2) -> 
     let m = generate_load_var m  x1 R0 in
     let m = generate_load_var m x2 R1 in
     append_instruction m (IAdd(R0,R1))
  | ESub (x1, x2) -> 
     let m = generate_load_var m  x1 R0 in
     let m = generate_load_var m x2 R1 in
     append_instruction m (ISub(R0,R1))
  | EField (f) -> 
     generate_load_field m f R0
  | ECalli (mi,p) ->
     let m = generate_load_label m (method_prologue_label mi) R0 in
     let m = generate_params_copy_to_registers m p in
     append_instruction m (ICall(R0))
  | ECall (x,p) -> (* callback *)
     let m = generate_params_copy_to_registers m p in
     let m = generate_load_var m x R0 in
     let m = generate_label_call m callback_helper_label in (* result in R0 *)
     if (is_unit_method_ref (get_type md x)) then 
       append_instruction m (IMovi(R0, IInt(0)))
     else m
;;

(* Generates instructions performing the corresponding operations of a given
 * high-level statement *)
let generate_line_code (mi : hl_method_index) (md : hl_method_decl) (m : ll_module)  ((l, s) : hl_line) =
  let m0 = append_label m (method_label_label mi l) in
    match s with
    | SVarAssign (x, e)   -> 
       let m1 = generate_expression m0 md e in
       generate_store_var m1 x R0
    | SFieldAssign (f, x) -> 
       let m1 = generate_load_var m0 x R0 in
       generate_store_field m1 f R0
    | SJmp (l)            -> 
        generate_label_jump m0 (method_label_label mi l)
    | SBeq (x1, x2, l)    -> 
        let m1 = generate_cjump m0 x1 x2 (method_label_label mi l) R3 in
        append_instruction m1 (IJe(R3))
    | SBlt (x1, x2, l)    -> 
        let m1 = generate_cjump m0 x1 x2 (method_label_label mi l) R3 in
        append_instruction m1 (IJl(R3))
    | SRet (x)            ->
        generate_load_var m0 x R0
;;

(* Generates instructions for deallocating an activation record from the
 * stack. *)
let generate_stack_teardown (m : ll_module) (md : hl_method_decl) =
  match md with MDecl(_, _, params, vars, _) ->
    List.fold_left append_instruction m 
      [IMovi(R1, IInt(List.length params + List.length vars));
       IAdd(SP,R1)]
;;


(* Generates the epilogue for a given high-level method. *)
let generate_epilogue (m : ll_module) (md : hl_method_decl) =
  match md with MDecl(_, index, _, _, _) ->
  let m0 = append_label m (method_epilogue_label index) in
  let m1 = generate_stack_teardown m0 md in
  append_instruction m1 IRet
;;

(* Generates instructions for a given high-level method. *)
let generate_method_code (m : ll_module) (md : hl_method_decl) =
  match md with MDecl(_, index, params, vars, body) ->
      let m0 = generate_prologue m md in
      let m1 = List.fold_left (generate_line_code index md) m0 body in
      generate_epilogue m1 md
;;

(* ***** Helper procedures ***** *)
(* Generates a helper method for checking whether an address in R0 is within
 * the memory bounds of the protected module *)
let generate_return_address_check_helper (m : ll_module) = 
  let m0 = append_label m return_address_check_helper_label in
  let m1 = generate_address_in_mod_check m0 R0 halt_helper_label (LLabel "rac_helper_ok") in
  let m2 = append_label m1 (LLabel "rac_helper_ok") in
  append_instruction m2 IRet
;;



(* Generates a helper method for performing a callback to unprotected code. The
 * address in unprotected code to jump to is assumed to be in R0. *)
let generate_callback_helper (m : ll_module) =
  let m = append_label m callback_helper_label in (* keeps R0 intact *)
  let m = generate_equals_check m null_address halt_helper_label in (* keeps R0 intact *)
  let m = generate_address_in_mod_check m R0 halt_helper_label (LLabel"callback_helper_out") in (* keeps R0 intact *)
  let m = append_label m (LLabel "callback_helper_out") in (* keeps R0 intact *)
  let m = generate_stack_switch m in (* keeps R0 intact *)
  let m = generate_load_label m return_entry_point_label R3 in (* keeps R0 intact *)
  let m = generate_push_onto_stack m R3 in (* keeps R0 intact *)
  let m = generate_working_register_cleanup m in (* keeps R0 intact *)
  append_instruction m (IJmp(R0))
;;

(* Generates a helper method for clearing all register (including the flags
 * register) except R0. *)
let generate_full_register_cleanup_helper (m : ll_module) =
  let m0 = append_label m full_register_cleanup_helper_label in
  List.fold_left append_instruction m0 
    [IMovi(R1, IInt(0));
     IMovi(R2, IInt(0));
     IMovi(R3, IInt(0));
     IMovi(R4, IInt(0));
     IMovi(R5, IInt(0));
     IMovi(R6, IInt(0));
     IMovi(R7, IInt(0));
     IMovi(R8, IInt(0));
     IMovi(R9, IInt(0));
     IMovi(R10, IInt(0));
     IMovi(R11, IInt(0));
     ICmp(R1, R2);
     IRet]
;;

(* Generates a helper method for storing 0 into R0 and then halting the
 * system. *)
let generate_halt_helper (m : ll_module) =
  let m = append_label m halt_helper_label in
  List.fold_left append_instruction m
    [IMovi(R0, IInt(0));
     IHalt]
;;

(* Generates the four helper method defined above. *)
let generate_helper_methods (m : ll_module) =
  let m0 = generate_return_address_check_helper m in
  let m1 = generate_callback_helper m0 in
  let m2 = generate_full_register_cleanup_helper m1 in
  generate_halt_helper m2
;;

(* ***** Fields ***** *)

(* Reserves a memory cell for the shadow stack pointer field. *)
let generate_shadow_stack_pointer_field (m : ll_module) =
  match m with Module(src, lines, LAddr(free_addr), d) ->
    let new_lines = lines @ [(Some shadow_stack_pointer_field_label, LAddr(free_addr), IData(IAddr secure_stack_base_address))] in
    let new_addr = LAddr (free_addr + 1) in
      Module(src, new_lines, new_addr, d)
;;

(* Reserves memory space for all helper fields. *)
let generate_helper_fields (m : ll_module) =
  generate_shadow_stack_pointer_field m
;;

(* Initializes the secure stack. *)
let generate_initial_stack (m : ll_module) =
  match (m, secure_stack_base_address) with (Module(src, lines, LAddr(free_addr), d), LAddr(s)) ->
    let new_lines = lines @ [
      (Some stack_base_label, secure_stack_base_address, IData(ILabelRef(halt_helper_label)))] in
    let new_addr = LAddr(s + 1) in
      Module(src, new_lines, new_addr, d)
;;



(* ***** Object translation ***** *)
(* The 'entry method' of the compiler. Translates a high-level object *)
let translate_object (o : hl_object_decl) =
  match o with
  | ODecl(fields, methods) -> 
    let m0 = Module(o, [], mod_base_address, ModDescriptor((List.length methods) + 1, code_size, data_size)) in
    let m1 = List.fold_left generate_entry_point m0 methods in
    let m2 = generate_return_entry_point m1 in
    let m3 = generate_helper_methods m2 in
    let m4 = List.fold_left generate_method_code m3 methods in
    let m5 = generate_initial_stack m4 in
    let m6 = generate_helper_fields m5 in
    List.fold_left generate_field m6 fields
;;



(* ***** Output formatting functions ***** *)
let reg_name (r : ll_register) =
  match r with
  | PC  -> "PC"
  | R0  -> "R0"
  | R1  -> "R1"
  | R2  -> "R2"
  | R3  -> "R3"
  | R4  -> "R4"
  | R5  -> "R5"
  | R6  -> "R6"
  | R7  -> "R7"
  | R8  -> "R8"
  | R9  -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"
  | SP  -> "SP"

let immediate_to_string (imm : ll_immediate) = 
  match imm with
  | IInt(i) -> string_of_int i
  | IAddr(LAddr(a)) -> Printf.sprintf "0x%08X" a 
  | ILabelRef(LLabel(s)) -> s
;;


let print_instruction (instr : ll_instruction) =
  match instr with
  | IMovl(r1,r2) -> Printf.printf "movl %s %s\n" (reg_name r1) (reg_name r2)
  | IMovs(r1,r2) -> Printf.printf "movs %s %s\n" (reg_name r1) (reg_name r2)
  | IMovi(r,i) -> Printf.printf "movi %s %s\n" (reg_name r) (immediate_to_string i)
  | IAdd(r1,r2) -> Printf.printf "add %s %s\n" (reg_name r1) (reg_name r2)
  | ISub(r1,r2) -> Printf.printf "sub %s %s\n" (reg_name r1) (reg_name r2)
  | ICmp(r1,r2) -> Printf.printf "cmp %s %s\n" (reg_name r1) (reg_name r2)
  | IJmp(r) -> Printf.printf "jmp %s\n" (reg_name r)
  | IJe(r) -> Printf.printf "je %s\n" (reg_name r)
  | IJl(r) -> Printf.printf "jl %s\n" (reg_name r)
  | ICall(r) -> Printf.printf "call %s\n" (reg_name r)
  | IRet  -> Printf.printf "ret\n"
  | IHalt -> Printf.printf "halt\n"
  | INop -> Printf.printf "nop\n"
  | IData(i) -> Printf.printf "data: %s\n" (immediate_to_string i)
;;

let print_label_option (l : ll_label option) = 
  match l with
  | Some (LLabel(s)) -> Printf.printf "%s:\n\t" s
  | None -> Printf.printf "\t"
;;

let print_address (LAddr(a) : ll_address) =
  Printf.printf "0x%08X: " a 
;;

let print_line (l : ll_line) =
  match l with
  | (l, a, instr) -> print_label_option l; print_address a; print_instruction instr
;;

let print_module (m : ll_module) =
  match m with
  | Module(src, lines, _, (ModDescriptor(n, cs, ss))) ->
    Printf.printf "\nModule:\n";
    Printf.printf " Base address: "; print_address mod_base_address; Printf.printf "\n";
    Printf.printf " Code section size: %d\n" cs;
    Printf.printf " Data section size: %d\n" ss;
    Printf.printf " Nb. entry points: %d\n" n;
    List.map print_line lines
;;



(* ***** Some example high-level objects ***** *)
(*
  object o {
    Int i = 0;
    
    Int plus(Int x0) {
      l0: x1 = i;
      l1: x2 = add x0 x1;
      l2: i = x2;
      l3: ret x2;
    }
  }
*)
let fi = FDecl(TInt, FI(0), VInt(0));
and mplus = MDecl(TInt, MI(0),
  [PDecl(TInt, VI(0))],
  [VDecl(TInt, VI(1), VInt(0));
   VDecl(TInt, VI(2), VInt(0))],
  [(LI(0), SVarAssign(VI(1), EField(FI(0))));
   (LI(1), SVarAssign(VI(2), EAdd(VI(0),VI(1))));
   (LI(2), SFieldAssign(FI(0), VI(2)));
   (LI(3), SRet(VI(2)))]);
in print_module (translate_object (ODecl([fi], [mplus])) )
;;


(*
  object o {
    Int f0 = 0;
    
    Int plus2() {
      var Int x0 = 0; Int x1 = 1; Int x2 = 2; Int x3 = 0;
      l0: x0 = add x0 x1;
      l1: blt x0 x2 l0;
      l2: x3 = f0;
      l3: x3 = add x3 x0;
      l4: f0 = x3;
      l5: ret x3;
    }
  }
*)
let fi = FDecl(TInt, FI(0), VInt(0));
and mplus = MDecl(TInt, MI(0),
  [],
  [VDecl(TInt, VI(0), VInt(0));
   VDecl(TInt, VI(1), VInt(1));
   VDecl(TInt, VI(2), VInt(2));
   VDecl(TInt, VI(3), VInt(0));],
  [(LI(0), SVarAssign(VI(0), EAdd(VI(0), VI(1))));
   (LI(1), SBlt(VI(0), VI(2), LI(0)));
   (LI(2), SVarAssign(VI(3), EField(FI(0))));
   (LI(3), SVarAssign(VI(3), EAdd(VI(3), VI(0))));
   (LI(4), SFieldAssign(FI(0), VI(3)));
   (LI(5), SRet(VI(3)))]);
in print_module (translate_object (ODecl([fi], [mplus])) )
;;

(*
  object o {
    Unit m(Unit x0) {
      l0: ret x0;
    }
  }
*)
let m = MDecl(TUnit, MI(0),
  [PDecl(TUnit, VI(0))], [],
  [(LI(0), SRet(VI(0)))]);
in print_module (translate_object (ODecl([], [m])) )
;;

(*
  object o {
    Int m(M<Int -> Unit> x0) {
      var Int x1 = 5; Unit x2 = unit;
      l0: x2 = x0(x1)
      l1: ret x1;
    }
  }
*)
let m = MDecl(TInt, MI(0),
  [PDecl(TMethod([TInt],TUnit), VI(0))],
  [VDecl(TInt, VI(1), VInt(5));
   VDecl(TUnit, VI(2), VUnit)],
  [(LI(0), SVarAssign(VI(2), ECall(VI(0), [VI(1)])));
   (LI(1), SRet(VI(1)))]);
in print_module (translate_object (ODecl([], [m])) )
;;

(*
  object o {
    Int value = 0;
    Int secret = 0;
    
    Int getValue() {
      var x0 = 0;
      l0: x0 = value;
      l1: ret x0;
    }
  }
*)
let fvalue = FDecl(TInt, FI(0), VInt(0));
and fsecret = FDecl(TInt, FI(1), VInt(0));
and mgetValue = MDecl(TInt, MI(0),
  [],
  [VDecl(TInt, VI(0), VInt(0))],
  [(LI(0), SVarAssign(VI(0), EField(FI(0))));
   (LI(1), SRet(VI(0)))]);
in print_module (translate_object (ODecl([fvalue; fsecret], [mgetValue])) )
;;

