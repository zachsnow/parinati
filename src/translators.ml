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
*Translations:
* Implementations of the various translations described in
* "Realizing Dependently Typed Logic Programming", Zachary Snow 2010;
* references in this file are to this document.
**********************************************************************)
exception TranslationError

(*  Name generation for going under LF implications.  *)
let generate_name =
  let counter = ref 0 in
  fun () ->
	  (incr counter;
	  "V" ^ (string_of_int (!counter)))

(**********************************************************************
*encode_kind:
* Encodes the LF kind Type as a lambdaProlog type; see Figure 4.1.
**********************************************************************)
let encode_kind k =
  match k with
    | Twelf.Type(_) ->
        if !Options.type_embedding_optimization then
          Lp.predicate_type
        else
          Lp.id_type "type"
    | _ ->
        let pos = Twelf.get_term_pos k in
        (Errormsg.error pos ("invalid kind: " ^ Twelf.string_of_term k);
        raise TranslationError)

(**********************************************************************
*encode_type:
* Encodes an LF type as a lambdaProlog term; see Figure 4.1.
**********************************************************************)  
let rec encode_type ty =
  let encode_base_type s =
    if !Options.type_embedding_optimization then
      Lp.id_type (s ^ "-type")
    else
      Lp.id_type "object"
  in
  
  match ty with
      Twelf.IdTerm(x,_) -> encode_base_type x
    | Twelf.Type(_) -> encode_kind ty
    
    | Twelf.PiTerm(_,l,r,_)
    | Twelf.ImplicationTerm(l,r,_) -> Lp.ArrowType(encode_type l, encode_type r)
    
    | Twelf.ApplicationTerm(l,r,_) -> encode_type l  (*  Only care about simple types? *)
    
    | Twelf.AbstractionTerm(_, _, _, pos) ->
        (Errormsg.error pos
          ("invalid type: " ^ (Twelf.string_of_term ty));
        raise TranslationError)

(**********************************************************************
*encode_term:
* Encodes an LF term as a lambdaProlog term; see Figure 4.2.
**********************************************************************)
let rec encode_term t =
  match t with
    | Twelf.IdTerm(s,_) ->
        Lp.IdTerm(s)
    | Twelf.ApplicationTerm(l,r,_) ->
        Lp.ApplicationTerm(encode_term l, [encode_term r])
    | Twelf.AbstractionTerm(v,_,t,_) ->
        Lp.AbstractionTerm(v, encode_term t)
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
  
  if !Options.type_embedding_optimization then
    let make_app tyHead tyArgs proofTerm =
      if !Options.proof_terms_optimization then
        Lp.ApplicationTerm(Lp.IdTerm(tyHead), tyArgs)
      else if !Options.indexing_optimization then
        Lp.ApplicationTerm(Lp.IdTerm(tyHead), tyArgs @ [proofTerm])
      else
        Lp.ApplicationTerm(Lp.IdTerm(tyHead), proofTerm :: tyArgs)
    in
    
    let r = Lp.head_and_argument_terms ty in
    if Util.is_some r then
      let (head, args) = Util.get r in
      make_app head args term
    else
      (Errormsg.error pos
        ("type has invalid head: " ^ (Lp.string_of_term ty));
      raise TranslationError)
  
  else if !Options.proof_terms_optimization then
    Lp.ApplicationTerm(Lp.IdTerm(h), [ty])
  
  else if !Options.indexing_optimization then
    Lp.ApplicationTerm(Lp.IdTerm(h), [ty; term])
  
  else
    Lp.ApplicationTerm(Lp.IdTerm(h), [term; ty])

(**********************************************************************
*Translators:
* The essential differences in the translations can be captured by
* how LF assertions are translated positively and negatively.
**********************************************************************)
module type TRANSLATOR =
sig
  val translate_positive : Twelf.assertion -> Lp.term
  val translate_negative : Twelf.assertion -> Lp.term
end


(**********************************************************************
*OriginalTranslator:
* Just the original translation.
**********************************************************************)
module OriginalTranslator = 
struct
  (********************************************************************
  *translate_positive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translate_positive assertion =
    let translate_abstraction_type x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translate_positive r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translate_positive l in
      Lp.for_all x (Lp.implies l' r')
    in

    match assertion with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translate_abstraction_type x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translate_abstraction_type x a p' q pos
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generate_name () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translate_abstraction_type x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          is_type (encode_term a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encode_term term) (encode_term ty) pos
      
  (**********************************************************************
  *translate_negative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translate_negative assertion =
    let translateAbstraction x a r pos =
      let l' = translate_positive (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      let r' = translate_negative r in
      Lp.for_all x (Lp.implies l' r')
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
          is_type (encode_term a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encode_term term) (encode_term ty) pos
end

(**********************************************************************
*SimplifiedTranslator:
* The simplified translation.
**********************************************************************)
module SimplifiedTranslator = 
struct
  (********************************************************************
  *translate:
  * Translates an LF context item or judgment to an LP term.
  ********************************************************************)
  let rec translate assertion =
    (*  translate_abstraction_type: *)
    let translate_abstraction_type x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translate r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translate l in
      Lp.for_all x (Lp.implies l' r')
    in

    match assertion with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translate_abstraction_type x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translate_abstraction_type x a p' q pos
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generate_name () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translate_abstraction_type x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          is_type (encode_term a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encode_term term) (encode_term ty) pos

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
*collect_rigid_variables:
* Given a list of binders and a term, returns a list of variables
* that are used rigidly in the term.  This list is a sublist of
* the given binders.
* TODO: this is broken!
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
*OptimizedTranslator:
* The optimized translation.
**********************************************************************)
module OptimizedTranslator =
struct
  (********************************************************************
  *translate_positive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translate_positive binders assertion =
    (*  translate_abstraction_type: *)
    let translateAbstraction x a p q pos =
      let binders' = x :: binders in
      let r = Twelf.Assertion(p, q, pos) in
      let (r', rigidVariables) = translate_positive binders' r in  
      if List.mem x rigidVariables then
        (Lp.for_all x r', rigidVariables)
      else
        let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
        let (l', _) = translate_positive binders' l in
        (Lp.for_all x (Lp.implies l' r'), rigidVariables)
    in

    match assertion with
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
          let x = generate_name () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstraction x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          (is_type (encode_term a), [])
      | Twelf.Assertion(term, ty, pos) ->
          let rigidVariables = collect_rigid_variables binders ty in
          (has_type (encode_term term) (encode_term ty) pos, rigidVariables)

  (**********************************************************************
  *translate_negative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translate_negative binders j =
    let translate_abstraction x a r pos =
      let (l',_) = translate_positive binders (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      let r' = translate_negative (x :: binders) r in
      Lp.for_all x (Lp.implies l' r')
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
          is_type (encode_term a)
      | Twelf.Assertion(term, ty, pos) ->
          has_type (encode_term term) (encode_term ty) pos

  let translate_positive a =
    let (t, _) = translate_positive [] a in
    t

  let translate_negative a = translate_negative [] a

end

module type TRANSLATION =
sig
  val type_of_contextitem : Twelf.assertion -> Lp.lptype option
  val type_of_declaration : Twelf.declaration -> Lp.lptype option

  val constant_of_contextitem : Twelf.assertion -> Lp.constant option
  val constant_of_declaration : Twelf.declaration -> Lp.constant option
  
  val term_of_contextitem : Twelf.assertion -> Lp.term option
  val term_of_declaration : Twelf.declaration -> Lp.term option
end
  
module Translation(Translator: TRANSLATOR) =
struct
    let type_of_contextitem c =
      if not !Options.type_embedding_optimization then
        None
      else
          match c with
            | Twelf.Assertion(Twelf.IdTerm(n,_),ty,pos) ->
                (try
                   let lp_type = encode_type ty in
                   let target_type = Lp.target_type lp_type in
                   if target_type = Lp.predicate_type then
                     Some(Lp.Type(n ^ "-type", 0))
                   else
                    None
                 with
                   TranslationError -> None)
              | _ -> None
    
    let type_of_declaration assertion =
      None
    
    let term_of_contextitem assertion =
      Lp.elide (Lp.normalize (Translator.translate_positive assertion))
    
    let term_of_declaration dec =
      match dec with
        | Twelf.Solve(n, t, pos) ->
            let vars = Twelf.free_variables t in
            let return_var = "_" ^ n in (* Force a universal.  *)
            let vars' =
              List.map
                (fun s -> Lp.IdTerm(s))
                (return_var :: vars)
            in
    
            let assertion = Twelf.Assertion(Twelf.IdTerm(return_var, pos), t, pos) in
            let term = 
              Lp.ImplicationTerm(
                Translator.translate_negative assertion,
                Lp.ApplicationTerm(Lp.IdTerm(n), vars')
              )
            in
            Lp.elide (Lp.normalize term)
        | Twelf.Domain(n, pos) ->
            Errormsg.impossible pos "domains aren't implemented!"
    
    let constant_of_contextitem c =
        match c with
          | Twelf.Assertion(Twelf.IdTerm(n,_) as n_type,ty,pos) ->
              (try
                if !Options.type_embedding_optimization then
                  let name_type = encode_type n_type in
                  let lp_type = encode_type ty in
                  let target_type = Lp.target_type lp_type in
                  match target_type with
                    | Lp.IdType(_) when target_type = Lp.predicate_type ->
                        if !Options.proof_terms_optimization then
                          Some(Lp.Constant(n, lp_type))
                        else if !Options.indexing_optimization then
                          let argument_types = Lp.argument_types lp_type in
                          let lp_type' = Lp.arrow_type (argument_types @ [name_type]) Lp.predicate_type in
                          Some(Lp.Constant(n, lp_type'))
                        else
                          let lp_type' = Lp.arrow_type [name_type] lp_type in
                          Some(Lp.Constant(n, lp_type'))
                    | Lp.IdType(tf) ->
                        let argument_types = Lp.argument_types lp_type in
                        Some(Lp.Constant(n, Lp.arrow_type argument_types (Lp.id_type tf)))
                    | _ ->
                        (Errormsg.error pos ("invalid target type: " ^ (Lp.string_of_type target_type));
                        Errormsg.error pos ("invalid context item type: " ^ (Twelf.string_of_term ty));
                        None)
                else
                  Some(Lp.Constant(n, encode_type ty))
              with
                TranslationError -> None)
          | Twelf.Assertion(_, _, pos) ->
              (Errormsg.error pos
                ("invalid context item: " ^ (Twelf.string_of_contextitem c));
              None)
    
    let constant_of_declaration dec =
      match dec with
        | Twelf.Solve(n,t,_) ->
            let vars = Twelf.free_variables t in
            let ty = Lp.arrow_type (List.map Lp.id_type ("Term" :: vars)) Lp.predicate_type in
            Some (Lp.Constant(n, ty))
        | Twelf.Domain(_,_) -> None
end

module OriginalTranslation = Translation(OriginalTranslator)
module SimplifiedTranslation = Translation(SimplifiedTranslator)
module OptimizedTranslation = Translation(OptimizedTranslator)

