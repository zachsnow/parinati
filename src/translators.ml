(**********************************************************************
*Copyright 2008-2012 Zach Snow
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
(**********************************************************************
*Translators:
* Implementations of the various translations described in
* "Realizing Dependently Typed Logic Programming", Zachary Snow 2010;
* references in this file are to this document.
**********************************************************************)
exception TranslationError

module type TRANSLATOR =
sig
  val translate_positive : Twelf.assertion -> Lp.tern
  val translate_negative : Twelf.term -> Lp.term
end

(*  Name generation.  *)
let counter = ref 0
let generateName () =
  (incr counter;
  "V" ^ (string_of_int (!counter)))

(**********************************************************************
*encodeKind:
* Encodes the LF kind Type as a lambdaProlog type; see Figure 4.1.
**********************************************************************)
let encodeKind k =
  match k with
    | Twelf.Type(_) ->
        if !Options.typeEmbeddingOptimization then
          Lp.predicateType
        else
          Lp.idType "type"
    | _ ->
        (Errormsg.error pos ("invalid kind: " ^ Twelf.string_of_term ty));
        raise TranslationError)

(**********************************************************************
*encodeType:
* Encodes an LF type as a lambdaProlog term; see Figure 4.1.
**********************************************************************)  
let encodeType ty =
  let encodeBaseType s =
    if !Options.typeEmbeddingOptimization then
      Lp.idType (s ^ "-type")
    else
      Lp.idType "object"
  in
  
  match ty with
      Twelf.IdTerm(x,_) -> encodeBaseType x
    | Twelf.Type(_) -> encodeKind ty
    
    | Twelf.PiTerm(_,l,r,_)
    | Twelf.ImplicationTerm(l,r,_) -> Lp.ArrowType(encodeType l, encodeType r)
    
    | Twelf.ApplicationTerm(l,r,_) -> encodeType l  (*  Only care about simple types? *)
    
    | Twelf.AbstractionTerm(_, _, _, pos) ->
        (Errormsg.error pos
          ("invalid type: " ^ (Twelf.string_of_term ty));
        raise TranslationError)

(**********************************************************************
*encodeTerm:
* Encodes an LF term as a lambdaProlog term; see Figure 4.2.
**********************************************************************)
let rec encodeTerm t =
  match t with
      Twelf.IdTerm(s,_) ->
        Lp.IdTerm(s)
    | Twelf.ApplicationTerm(l,r,_) ->
        Lp.ApplicationTerm(encodeTerm l, [encodeTerm r])
    | Twelf.AbstractionTerm(v,_,t,_) ->
        Lp.AbstractionTerm(v, encodeTerm t)
    | _ ->
        let pos = Twelf.get_term_pos t in
        let () = Errormsg.error pos
          ("unable to encode term: " ^ (Twelf.string_of_term t)) in
        raise TranslationError

(**********************************************************************
*is_type:
* A constructor for kind assertions.
**********************************************************************)
let is_type t =
  Lp.ApplicationTerm(Lp.IdTerm("is_type"), [t])

(**********************************************************************
*has_type:
* A constructor for type assertions; the implementation depends on
* the optimizations in effect.
**********************************************************************)
let has_type term ty pos =
  let h = "has_type" in
  
  if !Options.typeEmbeddingOptimization then
    let make_app tyHead tyArgs proofTerm =
      if !Options.noProofTermsOptimization then
        Lp.ApplicationTerm(Lp.IdTerm(tyHead), tyArgs)
      else if !Options.indexingOptimization then
        Lp.ApplicationTerm(Lp.IdTerm(tyHead), tyArgs @ [proofTerm])
      else
        Lp.ApplicationTerm(Lp.IdTerm(tyHead), proofTerm :: tyArgs)
    in
    
    let r = Lp.getTypeHeadAndArgs ty in
    if Util.is_some r then
      let (head, args) = Util.get r in
      make_app head args term
    else
      (Errormsg.error pos
        ("type has invalid head: " ^ (Lp.string_of_term ty));
      raise TranslationError)
  
  else if !Options.noProofTermsOptimization then
    Lp.ApplicationTerm(Lp.IdTerm(h), [ty])
  
  else if !Options.indexingOptimization then
    Lp.ApplicationTerm(Lp.IdTerm(h), [ty; term])
  
  else
    Lp.ApplicationTerm(Lp.IdTerm(h), [term; ty])

(**********************************************************************
* Many of these depend on being given a Translator.translate_positive,
* and so could live in Translate...
**********************************************************************)
let type_of_contextitem c =
    match c with
      | Twelf.Assertion(Twelf.IdTerm(n,_),ty,pos) ->
          if !Options.typeEmbeddingOptimization then
	          (try
	            let lpType = encodeType ty in
	            let targetType = Lp.targetType lpType in
	            match targetType with
	              | Lp.IdType(_) when targetType = Lp.predicateType ->
	                  Some(Lp.Type(n ^ "-type", 0))
	              | _ -> None
	          with
	            TranslationError -> None)
          else
            None
      | _ -> None

let term_of_contextitem translate_positive assertion =
  translate_positive assertion

let term_of_declaration translate_negative dec =
  match dec with
    | Twelf.Solve(n, t, pos) ->
        let vars = Twelf.freeVariables t in
        let returnVar = "_" ^ n in (* Force a universal.  *)
        let vars' =
          List.map
            (fun s -> Lp.IdTerm(s))
            (returnVar :: vars)
        in

        let assertion = Twelf.Assertion(Twelf.IdTerm(returnVar, pos), t, pos) in
        let term = 
          Lp.ImplicationTerm(
            translate_negative assertion,
            Lp.ApplicationTerm(Lp.IdTerm(n), vars')
          )
        in
        Lp.normalize term
    | Twelf.Domain(n, pos) ->
        Errormsg.impossible pos "domains aren't implemented!"

let constant_of_contextitem c =
    match c with
      | Twelf.Assertion(Twelf.IdTerm(n,_),ty,_) ->
          (try
            if !Options.typeEmbeddingOptimization then
              let idf s = Lp.idType (s ^ "-type") in
	            let tf = Lp.predicateType in
		          let lpType = translateType idf tf ty in
		
		          let targetType = Lp.targetType lpType in
		          match targetType with
		            | Lp.IdType(_) when targetType = Lp.predicateType ->
		                if !Options.noProofTermsOptimization then
		                  Some(Lp.Constant(n, lpType))
		                else if !Options.indexingOptimization then
		                  let argumentTypes = Lp.argumentTypes lpType in
		                  let lpType' = Lp.arrowType (argumentTypes @ [idf n]) Lp.predicateType in
		                  Some(Lp.Constant(n, lpType'))
		                else
		                  let lpType' = Lp.arrowType [idf n] lpType in
		                  Some(Lp.Constant(n, lpType'))
		            | Lp.IdType(tf) ->
		                let argumentTypes = Lp.argumentTypes lpType in
		                Some(Lp.Constant(n, Lp.arrowType argumentTypes (Lp.idType tf)))
		            | _ ->
		                (Errormsg.error pos ("invalid target type: " ^ (Lp.string_of_type targetType));
		                Errormsg.error pos ("invalid context item type: " ^ (Twelf.string_of_term ty));
		                None)
            else
              Some(Lp.Constant(n, encodeType ty))
          with
            TranslationError -> None)
      | Twelf.Assertion(_, _, pos) ->
          (Errormsg.error pos
            ("invalid context item: " ^ (Twelf.string_of_contextitem c));
          None)

let constant_of_declaration dec =
  match dec with
    | Twelf.Solve(n,t,_) ->
        let vars = Twelf.freeVariables t in
        let ty = Lp.arrowType (List.map Lp.idType ("Term" :: vars)) Lp.predicateType in
        Some (Lp.Constant(n, ty))
    | Twelf.Domain(_,_) -> None

(**********************************************************************
*OriginalTranslation:
* Just the original translation.
**********************************************************************)
module OriginalTranslation = 
struct
  (********************************************************************
  *translate_positive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translate_positive assertion =
    let translateAbstractionType x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translate_positive r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translate_positive l in
      Lp.forAll x (Lp.implies l' r')
    in

    match assertion with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translateAbstractionType x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstractionType x a p' q pos
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generateName () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstractionType x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          is_type (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encodeTerm term) (encodeTerm ty) pos
      
  (**********************************************************************
  *translate_negative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translate_negative assertion =
    let translateAbstraction x a r pos =
      let l' = translate_positive (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      let r' = translate_negative r in
      Lp.forAll x (Lp.implies l' r')
    in

    match assertion with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            let l = translate_negative (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
            let r = translateAbstraction x a (Twelf.Assertion(p, q, pos)) pos in
            Lp.ConjunctionTerm(l, r)
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let l = translate_negative (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          let r = translateAbstraction x a (Twelf.Assertion(p', q, pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(Twelf.PiTerm(x, a, b, _), ty, pos) ->
          let l = translate_negative (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let r = translateAbstraction x a (Twelf.Assertion(b, ty, pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(a, Twelf.Type(_), pos) ->
          is_type (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encodeTerm term) (encodeTerm ty) pos
end

(**********************************************************************
*SimplifiedTranslation:
* The simplified translation.
**********************************************************************)
module SimplifiedTranslation = 
struct
  (********************************************************************
  *translate:
  * Translates an LF context item or judgment to an LP term.
  ********************************************************************)
  let rec translate assertion =
    (*  translateAbstractionType: *)
    let translateAbstractionType x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translate r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translate l in
      Lp.forAll x (Lp.implies l' r')
    in

    match assertion with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translateAbstractionType x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstractionType x a p' q pos
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generateName () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstractionType x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          is_type (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encodeTerm term) (encodeTerm ty) pos

  let translate_positive = translate
  let translate_negative = translate
end

(******************************************************************
*isRigid:
* Given a term and a list of binders, determines whether the
* term acts as rigid.
* TODO: this is broken!
******************************************************************)
let rec is_rigid term binders =
  match term with
    Twelf.IdTerm(s,_) -> not (List.mem s binders)
  | Twelf.ApplicationTerm(l,_,_) -> is_rigid l binders
  | _ ->
      let pos = Twelf.get_term_pos term in
      (Errormsg.error pos
        ("unable to determine term rigidity: " ^ (Twelf.string_of_term term));
      raise TranslationError)

(******************************************************************
*collectRigidVariables:
* Given a list of binders and a term, returns a list of variables
* that are used rigidly in the term.  This list is a sublist of
* the given binders.
******************************************************************)
let rec collect_rigid_variables binders term =
  let rec collect rhs term =
    match term with
      | Twelf.IdTerm(x,_) ->
          if rhs && List.mem x binders then
            [x]
          else
            []
      | Twelf.ApplicationTerm(l,r,_) ->
          let vars = collect false l in
          if is_rigid l binders then
            vars @ (collect true r)
          else
            vars
      | Twelf.AbstractionTerm(var,_,body,_) ->
          if List.mem var binders then
            []
          else
            collect true body
      | _ ->
          (Errormsg.error Errormsg.none ("unable to collect rigidly used variables from type: " ^ (Twelf.string_of_term term));
          raise TranslationError)
  in
  collect true term

(**********************************************************************
*OptimizedTranslation:
* The optimized translation.
**********************************************************************)
module OptimizedTranslation =
struct
  (********************************************************************
  *translate_positive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translate_positive binders i =
    (*  translateAbstractionType: *)
    let translateAbstraction x a p q pos =
      let binders' = x :: binders in
      let r = Twelf.Assertion(p, q, pos) in
      let (r', rigidVariables) = translate_positive binders' r in  
      if List.mem x rigidVariables then
        (Lp.forAll x r', rigidVariables)
      else
        let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
        let (l', _) = translate_positive binders' l in
        (Lp.forAll x (Lp.implies l' r'), rigidVariables)
    in

    match i with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translateAbstraction x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstraction x a p' q pos
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generateName () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstraction x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          (is_type (encodeTerm a), [])
      | Twelf.Assertion(term, ty, pos) ->
          let rigidVariables = collectRigidVariables binders ty in
          (has_type (encodeTerm term) (encodeTerm ty) pos, rigidVariables)

  (**********************************************************************
  *translate_negative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translate_negative binders j =
    let translate_abstraction x a r pos =
      let (l',_) = translate_positive binders (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      let r' = translate_negative (x :: binders) r in
      Lp.forAll x (Lp.implies l' r')
    in

    match j with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            let l = translate_negative binders (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
            let r = translate_abstraction x a (Twelf.Assertion(p, q, pos)) pos in
            Lp.ConjunctionTerm(l, r)
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let l = translate_negative binders (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          let r = translate_abstraction x a (Twelf.Assertion(p', q, pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(Twelf.PiTerm(x, a, b, _), Twelf.Type(_), pos) ->
          let l = translate_negative binders (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let r = translate_abstraction x a (Twelf.Assertion(b, Twelf.Type(pos), pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(a, Twelf.Type(_), pos) ->
          is_type (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encodeTerm term) (encodeTerm ty) pos

  let translate_positive a =
    let (t, _) = translate_positive [] a in
    t

  let translate_positive a =
    let (t, _) = translate_positive [] a in
    t
end
