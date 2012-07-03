(**********************************************************************
* Copyright 2008-2012 Zach Snow
**********************************************************************)
(**********************************************************************
* This file is part of Parinati.
*
* Parinati is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Parinati is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Parinati.  If not, see <http://www.gnu.org/licenses/>.
**********************************************************************)
type absyn = Program of string * lpsignature * lpmodule

and lpsignature = Signature of lptype list * constant list
and lpmodule = Module of declaration list

and declaration =
  | Term of term
  | Accum of string

and lptype = Type of string * int
and constant = Constant of string * ty

and term =
  | IdTerm of string
  | ApplicationTerm of term * term list
  | AbstractionTerm of string * term
  | ImplicationTerm of term * term
  | ConjunctionTerm of term * term
  | PiTerm of term

and ty =
  | IdType of string
  | VariableType of string
  | ArrowType of ty * ty

(**********************************************************************
*map_name:
* Maps LF names to LP names.  Just remaps names that interfere with
* LP keywords, and replaces invalid characters with 'x'.  There's
* a counter for generated names.
**********************************************************************)
let generate =
  let name_counter = ref 0 in
  fun n ->
    let n' = "lf-" ^ n ^ "-" ^ (string_of_int !name_counter) in
    (incr name_counter;
    n')

let map_name =
  let mapped_names = ref [] in
  
  let add_mapping n n' =
    mapped_names := (n, n') :: !mappedNames
  in
  
  (* A probably-incomplete list of Teyjus/lambdaProlog reserved words. *)
  let reserved =
    [
      "accum_sig"; "accumulate"; "closed"; "end"; "exportdef"; "import";
      "infix"; "infixl"; "infixr"; "kind"; "local"; "localkind";
      "module"; "postfix"; "posfixl"; "prefix"; "prefixr"; "sig";
      "type"; "typeabbrev"; "use_sig"; "useonly"; ":-"; "=>";
      "\\"; "->"; "!";
      
      "::"; "nil"; "+"; "-"; "*"; "/"; "~"; "<"; "="; ">"; "=<"; ">=";
      "pi"; "sigma"; ","; ";"; "&"; "."; "("; ")"; ":"; "["; "]" 
    ]
  in
  
  (* Characters that shouldn't be in regular names; some of them are
     technically legal in some positions, but we'll just strip them all. *)
  let invalid_characters =
    [
      '='; '>'; '<'; '#';
      '?'; '%'; '!'; '@'; '$';
      '^'; '&'; '*'; '('; ')';
      ';'; ':'; '{'; '}'; '[';
      ']'; '|'; '\\'; '/'; '+'
    ]
  in
  
  let explode s =
    let cs = ref [] in
    (String.iter (fun c -> cs := c :: !cs) s;
    List.rev !cs)
  in
  
  let implode cs =
    List.fold_left (fun s c -> s ^ (String.make 1 c)) "" cs
  in
  
  let strip n =
    let ns = explode n in
    let ns' = List.map (fun ch -> if List.mem ch invalid_characters then 'x' else ch) ns in
    implode ns'
  in
  
  fun name ->
    try
      List.assoc name !mapped_names
    with
      Not_found ->
        if List.mem name reserved or List.exists (String.contains name) invalid_characters then
          let name' = generate (strip name) in
          (add_mapping name name';
          name')
        else
          (add_mapping name name;
          name)

(**********************************************************************
*map_domain_name:
* Maps Twelf domain names to LP module names; currently unused as
* Parinati doesn't support domains.
**********************************************************************)
let map_domain_name n =
  Str.global_replace (Str.regexp_string "/") "_" n

(**********************************************************************
*target_type:
* Collects the "target" type of an arrow type; given:
*   A1 -> A2 -> ... An -> T 
* Returns:
*   T
**********************************************************************)
let rec target_type ty =
  match ty with
    | IdType _
    | VariableType _ -> ty
    | ArrowType(_,r) -> targetType r

(**********************************************************************
*argument_types:
* Collects the "argument" types of an arrow type; given:
*   A1 -> A2 -> ... An -> T 
* Returns:
*   [A1, ..., An]
**********************************************************************)
let rec argument_types ty =
  match ty with
    | IdType _
    | VariableType _ -> []
    | ArrowType(l,r) ->
        [l] @ (argumentTypes r)

(**********************************************************************
* Type construction shortcuts.
**********************************************************************)
let id_type s = IdType(s)

let rec arrow_type args t =
  match args with
  | [] -> t
  | arg::args' -> ArrowType(arg, arrow_type args' t)

let predicate_type = id_type "o"

(**********************************************************************
*head_and_argument_terms:
* All terms representing LF base types should have rigid constant
* heads; return this one's head separate from its arguments.
**********************************************************************)
let head_and_argument_terms term =
  let rec get term args =
    match term with
      | IdTerm(s) -> Some (s, args)
      | ApplicationTerm(l,r) -> get l (r @ args)
      | _ -> None
  in
  get term []

(**********************************************************************
* Term construction shortcuts.
**********************************************************************)
let top = IdTerm "true"
let for_all x t = PiTerm(AbstractionTerm(x, t))
let implies l r = ImplicationTerm(l, r)

(**********************************************************************
* Pretty-printing.
**********************************************************************)
let rec string_of_absyn (Program(name, s, m)) =
  (string_of_signature name s, string_of_module name m)

and string_of_signature name (Signature(types, constants)) =
  "% Generated by Parinati (version " ^ Options.version ^ ")\n" ^
  "sig " ^ name ^ ".\n\n" ^
  
  "% Types.\n" ^
  (String.concat "\n" (List.map string_of_lptype types)) ^ "\n\n" ^

  "% Constants.\n" ^
  (String.concat "\n" (List.map string_of_constant constants)) ^ "\n"

(* string_of_lptype *)
and string_of_lptype (Type(name, k)) =
  let rec string_of_kind k =
    match k with
      | 0 -> "type"
      | _ -> "type -> " ^ (string_of_kind (k - 1))
  in
  let n' = map_name name in
  "kind " ^ n' ^ " " ^ (string_of_kind k) ^ ".\n"
  
(**********************************************************************
*string_of_constant:
* Deals with printing context items.
**********************************************************************)
and string_of_constant (Constant(name,t)) =
  let n' = map_name name in
  "type " ^ n' ^ " " ^ (string_of_type t) ^ ".\n"

(**********************************************************************
*string_of_type:
**********************************************************************)
and string_of_type t =
  match t with
    IdType(s) -> (map_name s)
  | VariableType(s) -> s
  | ArrowType(l,r) ->
      "(" ^ (string_of_type l) ^ " -> " ^ (string_of_type r) ^ ")"

(**********************************************************************
*string_of_module:
* Converts a context and a list of judgments to an LP module.
**********************************************************************)
and string_of_module name (Module(decs)) =
  "% Generated by Parinati (version " ^ Options.version ^ ")\n" ^
  "module " ^ name ^ ".\n" ^
  (String.concat "\n" (List.map string_of_declaration decs)) ^ "\n"

(**********************************************************************
*string_of_declaration:
**********************************************************************)
and string_of_declaration j = 
  match j with
      Term(t) -> (string_of_term t) ^ "."
    | Accum(n) -> "accumulate " ^ n ^ "."

(**********************************************************************
*string_of_term:
**********************************************************************)
and string_of_term term =
  match term with
      IdTerm(s) -> (map_name s)
    | ApplicationTerm(head, args) ->
        let sep = if (List.length args) = 0 then "" else " " in
        "(" ^ (string_of_term head) ^ sep ^
        (String.concat " " (List.map string_of_term args)) ^
        ")"
    | AbstractionTerm(v, t) ->
        "(" ^ v ^ "\\ " ^ (string_of_term t) ^ ")"
    | ImplicationTerm(l, r) ->
        "(" ^ (string_of_term l) ^ " => " ^ (string_of_term r) ^ ")"
    | ConjunctionTerm(l, r) ->
        "(" ^ (string_of_term l) ^ ", " ^ (string_of_term r) ^ ")"
    | PiTerm(t) ->
        "(pi " ^ (string_of_term t) ^ ")"

(**********************************************************************
*normalize:
* "Normalize" a term (removing useless 'true' goals and assumptions);
* just for readability.
**********************************************************************)
let is_top t = t = (IdTerm "true")
let normalize t =  
  let rec normalize' t =
    match t with
        IdTerm _
      | ApplicationTerm _ -> t
      | AbstractionTerm(s, t) ->
          let t' = normalize' t in
          if is_top t' then
            top
          else
            AbstractionTerm(s, t')
      | ImplicationTerm(l, r) ->
          let l' = normalize' l in
          let r' = normalize' r in
          if is_top r' then
            top
          else if is_top l' then
            r'
          else
            ImplicationTerm(l', r')
      | ConjunctionTerm(l, r) ->
          let l' = normalize' l in
          let r' = normalize' r in
          if is_top l' then
            r'
          else if is_top r' then
            l'
          else
            ConjunctionTerm(l', r')
      | PiTerm(t) ->
          let t' = normalize' t in
          if is_top t' then
            top
          else
            PiTerm t'
  in
  normalize' t

(**********************************************************************
*elide:
* Given a term, returns it only if it isn't top.
**********************************************************************)
let elide t =
  if t = top then
    None
  else
    Some t
