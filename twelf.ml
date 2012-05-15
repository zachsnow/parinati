(****************************************************************************
*Copyright 2008, 2009 Zach Snow
****************************************************************************)
(****************************************************************************
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
****************************************************************************)
type pos = Errormsg.pos
type absyn = Specification of contextitem list * declaration list

and contextitem = assertion
and assertion = Assertion of term * term * pos

and declaration =
    Solve of string * term * pos
  | Domain of string * pos

and term =
    IdTerm of string * pos
  | AbstractionTerm of string * term * term * pos
  | ApplicationTerm of term * term * pos
  | ImplicationTerm of term * term * pos
  | PiTerm of string * term * term * pos
  | AlephTerm of string * term * term * pos
  | Type of pos

let getContextItemPos (Assertion(_,_,p)) = p

let getDeclarationPos d = match d with
    Solve(_,_,p)
  | Domain(_,p) -> p

let getTermPos t = match t with
    IdTerm(_, p)
  | AbstractionTerm(_,_,_,p)
  | ApplicationTerm(_,_,p)
  | ImplicationTerm(_,_,p)
  | PiTerm(_,_,_,p)
  | AlephTerm(_,_,_,p)
  | Type(p) -> p

(**********************************************************************
*string_of_term:
* prints an LF term in LF syntax.
**********************************************************************)
let rec string_of_term t =
  let needsParens t =
    match t with
      Type(_)
    | IdTerm(_) -> false
    | AbstractionTerm(_)
    | ApplicationTerm(_)
    | ImplicationTerm(_)
    | PiTerm(_)
    | AlephTerm(_) -> true
  in
  let parens t =
    let s = string_of_term t in
    if needsParens t then
      "(" ^ s ^ ")"
    else
      s
  in
  match t with
      IdTerm(t',_) -> t'
    | AbstractionTerm(s,t1,t2,_) ->
        "[" ^ s ^ " : " ^ (string_of_term t1) ^ "] "^ (string_of_term t2)
    | PiTerm(s,t1,t2,_) ->
        "{" ^ s ^ " : " ^ (string_of_term t1) ^ "} "^ (string_of_term t2)
    | AlephTerm(s,t1,t2,_) ->
        "<" ^ s ^ " : " ^ (string_of_term t1) ^ "> "^ (string_of_term t2)
    | ApplicationTerm(t1,t2,_) -> (string_of_term t1) ^ " " ^ (parens t2)
    | ImplicationTerm(t1, t2,_) -> (string_of_term t1) ^ " -> " ^ (string_of_term t2)
    | Type(_) -> "type"

(**********************************************************************
*isUniversal:
* Determines whether the name is possibly a universal variable (i.e.,
* capitalized or '_').
**********************************************************************)
let isUniversal s =
  if s = "" then
    false
  else
    let c = String.get s 0 in
    if (Char.compare c 'A') = 0 ||
      ((Char.compare c 'A') > 0 && (Char.compare c 'Z') < 0) ||
      (Char.compare c 'Z') = 0 then
      true
    else if c = '_' then
      true
    else
      false

let string_of_contextitem (Assertion(p,q,_)) =
  (string_of_term p) ^ " : " ^ (string_of_term q)

(**********************************************************************
*freeVariables
* Given a term, returns a list of all free variables in the term.
**********************************************************************)
let freeVariables t =
  let rec unbound binders t =
    match t with
        IdTerm(n,_) ->
          if (isUniversal n) && not (List.mem n binders) then
            [n]
          else
            []
      | ApplicationTerm(l,r,_)
      | ImplicationTerm(l,r,_) ->
          let lvars = unbound binders l in
          let rvars = unbound binders r in
          lvars @ rvars
      | AbstractionTerm(n, ty, term,_)
      | PiTerm(n, ty, term,_)
      | AlephTerm(n, ty, term,_) ->
          let tyvars = unbound binders ty in
          let termvars = unbound (n::binders) term in
          tyvars @ termvars
      | Type(_) -> []
  in
  unbound [] t

let typecheck (Specification(items, judgments)) =
  let rec checkTerm bvs t =
    match t with
      | IdTerm(n,pos) ->
          if not (List.mem n bvs) then
            (Errormsg.error pos ("unbound variable: " ^ n);
            false)
          else
            true
      | ApplicationTerm(l,r,_)
      | ImplicationTerm(l,r,_) ->
          (checkTerm bvs l) && (checkTerm bvs r)
      | AbstractionTerm(n, ty, term, _)
      | PiTerm(n, ty, term, _)
      | AlephTerm(n, ty, term, _) ->
          (checkTerm bvs ty) && (checkTerm (n::bvs) term)
      | Type(_) -> true
  in
  let checkContextItem bvs ci =
    match ci with
      Assertion(IdTerm(n,_), ty, pos) ->
        if not (checkTerm bvs ty) then
          Errormsg.error pos ("invalid context item type: " ^ (string_of_term ty));
        n :: bvs
    | Assertion(t, _, pos) ->
        (Errormsg.error pos ("invalid context item name: " ^ (string_of_term t));
        bvs)
  in
  let _ = List.fold_left checkContextItem [] items in
  not !Errormsg.anyErrors
