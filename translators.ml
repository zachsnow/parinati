(**********************************************************************
*Copyright 2008, 2009 Zach Snow
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
exception TranslationError

module type TRANSLATOR =
sig
  val type_of_contextitem : Twelf.assertion -> Lp.lptype option
  val type_of_declaration : Twelf.declaration -> Lp.lptype option

  val constant_of_contextitem : Twelf.assertion -> Lp.constant option
  val constant_of_declaration : Twelf.declaration -> Lp.constant option
  
  val term_of_contextitem : Twelf.assertion -> Lp.term option
  val term_of_declaration : Twelf.declaration -> Lp.term option
  
  val defaultTypes : Lp.lptype list
  val defaultConstants : Lp.constant list
  val defaultTerms : Lp.term list
end

(*  Name generation.  *)
let counter = ref 0
let generateName () =
  (incr counter;
  "V" ^ (string_of_int (!counter)))

(**********************************************************************
*translateType:
**********************************************************************)
let rec translateType idf tf ty =
  match ty with
      Twelf.IdTerm(x,_) -> idf x
    | Twelf.Type(_) -> tf
    | Twelf.PiTerm(_,l,r,_)
    | Twelf.AlephTerm(_,l,r,_)
    | Twelf.ImplicationTerm(l,r,_) -> Lp.ArrowType(translateType idf tf l, translateType idf tf r)
    | Twelf.ApplicationTerm(l,r,_) -> translateType idf tf l  (*  Only care about simple types? *)
    | Twelf.AbstractionTerm(v, ty, term, pos) ->
        (Errormsg.error pos
          ("invalid type: " ^ (Twelf.string_of_term ty));
        raise TranslationError)

(**********************************************************************
*isType/hasType:
**********************************************************************)
let isType t = Lp.ApplicationTerm(Lp.IdTerm("istype"), [t])
let hasType term ty pos =
  let makeApp term ty =
    let h = "hastype" in
    if !Options.noProofTermsOptimization then
      Lp.ApplicationTerm(Lp.IdTerm(h), [ty])
    else if !Options.indexingOptimization then
      Lp.ApplicationTerm(Lp.IdTerm(h), [ty; term])
    else
      Lp.ApplicationTerm(Lp.IdTerm(h), [term; ty])
  in
  makeApp term ty

(**********************************************************************
*encodeTerm:
* Encodes an LF term as a Lambda Prolog term.
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
        let pos = Twelf.getTermPos t in
        let () = Errormsg.error pos
          ("unable to encode term: " ^ (Twelf.string_of_term t)) in
        raise TranslationError

let term_of_declaration dec translate =
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
            translate assertion,
            Lp.ApplicationTerm(Lp.IdTerm(n), vars'))
        in
        Some(Lp.normalize term)
    | Twelf.Domain(n, pos) ->
        None  (*  Domains don't work! *)

(**********************************************************************
*OriginalTranslation:
* Just the original translation.
**********************************************************************)
module OriginalTranslation = 
struct
  (********************************************************************
  *translatePositive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translatePositive i =
    (*  translateAbstractionType: *)
    let translateAbstractionType x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translatePositive r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translatePositive l in
      Lp.forAll x (Lp.implies l' r')
    in

    match i with
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
          isType (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          hasType (encodeTerm term) (encodeTerm ty) pos
      
  (**********************************************************************
  *translateNegative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translateNegative j =
    let translateAbstraction x a r pos =
      let l' = translatePositive (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      let r' = translateNegative r in
      Lp.forAll x (Lp.implies l' r')
    in

    match j with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            let l = translateNegative (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
            let r = translateAbstraction x a (Twelf.Assertion(p, q, pos)) pos in
            Lp.ConjunctionTerm(l, r)
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let l = translateNegative (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          let r = translateAbstraction x a (Twelf.Assertion(p', q, pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(Twelf.PiTerm(x, a, b, _), ty, pos) ->
          let l = translateNegative (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let r = translateAbstraction x a (Twelf.Assertion(b, ty, pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(a, Twelf.Type(_), pos) ->
          isType (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          hasType (encodeTerm term) (encodeTerm ty) pos
  
  (*  Never introduce new kinds.  *)
  let type_of_contextitem _ = None
  let type_of_declaration _ = None

  let constant_of_contextitem c =
    match c with
      | Twelf.Assertion(Twelf.IdTerm(n,_),ty,_) ->
          (try
            let idf _ = Lp.idType "object" in
            let tf = Lp.idType "type" in

            Some(Lp.Constant(n, translateType idf tf ty))
          with
            TranslationError -> None)
      | Twelf.Assertion(_, _, pos) ->
          (Errormsg.error pos
            ("invalid context item: " ^ (Twelf.string_of_contextitem c));
          None)

  let constant_of_declaration dec =
    match dec with
        Twelf.Solve(n,t,_) ->
          let vars = Twelf.freeVariables t in
          let ty = Lp.arrowType (List.map Lp.idType ("Term" :: vars)) Lp.predicateType in
          Some (Lp.Constant(n, ty))
      | Twelf.Domain(_,_) -> None
    
  let term_of_contextitem t =
    Some(translatePositive t)
  
  let term_of_declaration dec =
    term_of_declaration dec translateNegative
    
  let defaultTypes =
    [
      Lp.Type("object", 0);
      Lp.Type("type", 0)
    ]
    
  let defaultConstants =
    let obj = Lp.IdType("object") in
    let ty = Lp.IdType("type") in
    [
      Lp.Constant("istype", Lp.arrowType [ty] Lp.predicateType);
      Lp.Constant("hastype", Lp.arrowType [obj; ty] Lp.predicateType)
    ]

  let defaultTerms = []
end

(**********************************************************************
*StrangeTranslation:
* The strange translation of System LF.
**********************************************************************)
module StrangeTranslation = 
struct
  (********************************************************************
  *translate:
  * Translates an SLF context item or judgment to an LP term.
  ********************************************************************)
  let rec translate i =
    (*  translateAbstractionType: *)
    let translateAbstractionType x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translate r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translate l in
      Lp.forAll x (Lp.implies l' r')
    in

    match i with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translateAbstractionType x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos)
      | Twelf.Assertion(p, Twelf.AlephTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstractionType x a p' q pos
      
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generateName () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstractionType x a p' q pos
      | Twelf.Assertion(a, Twelf.Type(_), _) ->
          isType (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          hasType (encodeTerm term) (encodeTerm ty) pos

  let term_of_contextitem t = Some(translate t)
  let term_of_declaration dec = term_of_declaration dec translate
  
  let type_of_contextitem t = OriginalTranslation.type_of_contextitem t
  let type_of_declaration t = OriginalTranslation.type_of_declaration t
  
  let constant_of_contextitem t = OriginalTranslation.constant_of_contextitem t
  let constant_of_declaration t = OriginalTranslation.constant_of_declaration t
  
  let defaultTypes = OriginalTranslation.defaultTypes
  let defaultConstants = OriginalTranslation.defaultConstants
  let defaultTerms = OriginalTranslation.defaultTerms
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
  let rec translate i =
    (*  translateAbstractionType: *)
    let translateAbstractionType x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translate r in    
      let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
      let l' = translate l in
      Lp.forAll x (Lp.implies l' r')
    in

    match i with
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
          isType (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          hasType (encodeTerm term) (encodeTerm ty) pos

  let term_of_contextitem t = Some(translate t)
  let term_of_declaration dec = term_of_declaration dec translate
  
  let type_of_contextitem t = OriginalTranslation.type_of_contextitem t
  let type_of_declaration t = OriginalTranslation.type_of_declaration t
  
  let constant_of_contextitem t = OriginalTranslation.constant_of_contextitem t
  let constant_of_declaration t = OriginalTranslation.constant_of_declaration t
  
  let defaultTypes = OriginalTranslation.defaultTypes
  let defaultConstants = OriginalTranslation.defaultConstants
  let defaultTerms = OriginalTranslation.defaultTerms
end

(******************************************************************
*isRigid:
* Given a term and a list of binders, determines whether the
* term acts as rigid.
******************************************************************)
let rec isRigid term binders =
  match term with
    Twelf.IdTerm(s,_) -> not (List.mem s binders)
  | Twelf.ApplicationTerm(l,_,_) -> isRigid l binders
  | _ ->
      let pos = Twelf.getTermPos term in
      (Errormsg.error pos
        ("unable to determine term rigidity: " ^ (Twelf.string_of_term term));
      raise TranslationError)

(******************************************************************
*collectRigidVariables:
* Given a list of binders and a term, returns a list of variables
* that are used rigidly in the term.  This list is a sublist of
* the given binders.
******************************************************************)
let rec collectRigidVariables binders term =
  let rec collect rhs term =
    match term with
      | Twelf.IdTerm(x,_) ->
          if rhs && List.mem x binders then
            [x]
          else
            []
      | Twelf.ApplicationTerm(l,r,_) ->
          let vars = collect false l in
          if isRigid l binders then
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
  *translatePositive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translatePositive binders i =
    (*  translateAbstractionType: *)
    let translateAbstraction x a p q pos =
      let binders' = x :: binders in
      let r = Twelf.Assertion(p, q, pos) in
      let (r', rigidVariables) = translatePositive binders' r in  
      if List.mem x rigidVariables then
        (Lp.forAll x r', rigidVariables)
      else
        let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
        let (l', _) = translatePositive binders' l in
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
          (isType (encodeTerm a), [])
      | Twelf.Assertion(term, ty, pos) ->
          let rigidVariables = collectRigidVariables binders ty in
          (hasType (encodeTerm term) (encodeTerm ty) pos, rigidVariables)

  (**********************************************************************
  *translateNegative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translateNegative binders j =
    let translateAbstraction x a r pos =
      let (l',_) = translatePositive binders (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      let r' = translateNegative (x :: binders) r in
      Lp.forAll x (Lp.implies l' r')
    in

    match j with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            let l = translateNegative binders (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
            let r = translateAbstraction x a (Twelf.Assertion(p, q, pos)) pos in
            Lp.ConjunctionTerm(l, r)
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let l = translateNegative binders (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          let r = translateAbstraction x a (Twelf.Assertion(p', q, pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(Twelf.PiTerm(x, a, b, _), Twelf.Type(_), pos) ->
          let l = translateNegative binders (Twelf.Assertion(a, Twelf.Type(pos), pos)) in
          let r = translateAbstraction x a (Twelf.Assertion(b, Twelf.Type(pos), pos)) pos in
          Lp.ConjunctionTerm(l, r)
      | Twelf.Assertion(a, Twelf.Type(_), pos) ->
          isType (encodeTerm a)
      | Twelf.Assertion(term, ty, pos) ->
          hasType (encodeTerm term) (encodeTerm ty) pos

  let term_of_contextitem t =
    let (t', _) = translatePositive [] t in
    Some(t')
    
  let term_of_declaration dec =
    term_of_declaration dec (translateNegative [])
  
  let type_of_contextitem t = OriginalTranslation.type_of_contextitem t
  let type_of_declaration t = OriginalTranslation.type_of_declaration t
  
  let constant_of_contextitem t = OriginalTranslation.constant_of_contextitem t
  let constant_of_declaration t = OriginalTranslation.constant_of_declaration t
  
  let defaultTypes = OriginalTranslation.defaultTypes
  let defaultConstants = OriginalTranslation.defaultConstants
  let defaultTerms = OriginalTranslation.defaultTerms

end

(**********************************************************************
*ExtendedTranslation:
* The optimized translation extended by type embedding.
**********************************************************************)
module ExtendedTranslation =
struct
  (********************************************************************
  *hasType:
  * A type embedding hasType.
  ********************************************************************)
  let hasType term ty pos =
    let makeApp tyHead tyArgs proofTerm =
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
      makeApp head args term
    else
      (Errormsg.error pos
        ("type has invalid head: " ^ (Lp.string_of_term ty));
      raise TranslationError)
    
  (********************************************************************
  *translatePositive:
  * Translates an LF context item to an LP term.
  ********************************************************************)
  let rec translatePositive binders i =
    (*  translateAbstraction: *)
    let translateAbstraction x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let (r', ruvs) = translatePositive (x :: binders) r in  
      if List.mem x ruvs then
        (Lp.forAll x r', ruvs)
      else
        let l = Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos) in
        let l' = translateNegative l in
        (Lp.forAll x (Lp.implies l' r'), ruvs)
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
      | Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.ImplicationTerm(a', q, _), pos) ->
          if a = a' then
            translateAbstraction x a p q pos
          else
            (Errormsg.error pos "invalid impliciation type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generateName () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstraction x a p' q pos
      | Twelf.Assertion(_, Twelf.Type(_), pos) ->
          (Lp.top, [])
      | Twelf.Assertion(term, ty, pos) ->
          let rigidVariables = collectRigidVariables binders ty in
          let () = Errormsg.log pos "encoding+" in
          let term' = encodeTerm term in
          let () = Errormsg.log pos ("term+ : " ^ (Lp.string_of_term term')) in
          let ty' = encodeTerm ty in
          let () = Errormsg.log pos ("ty+ : " ^ (Lp.string_of_term ty')) in
          (hasType term' ty' pos, rigidVariables)
      

  (**********************************************************************
  *translateNegative:
  * Translates an LF judgment into an LP term.
  **********************************************************************)
  and translateNegative j =
    let translateAbstraction x a p q pos =
      let r = Twelf.Assertion(p, q, pos) in
      let r' = translateNegative r in
      let (l',_) = translatePositive [] (Twelf.Assertion(Twelf.IdTerm(x, pos), a, pos)) in
      Lp.forAll x (Lp.implies l' r')
    in
    match j with
        Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.PiTerm(x', a', q, _), pos) ->
          if x = x' && a = a' then
            translateAbstraction x a p q pos
          else
            (Errormsg.error pos "invalid quantification variable or type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.PiTerm(x, a, q, _), pos) ->
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstraction x a p' q pos
      | Twelf.Assertion(Twelf.AbstractionTerm(x, a, p, _), Twelf.ImplicationTerm(a', q, _), pos) ->
          if a = a' then
            translateAbstraction x a p q pos
          else
            (Errormsg.error pos "invalid implication type";
            raise TranslationError)
      | Twelf.Assertion(p, Twelf.ImplicationTerm(a, q, _), pos) ->
          let x = generateName () in
          let p' = Twelf.ApplicationTerm(p, Twelf.IdTerm(x, pos), pos) in
          translateAbstraction x a p' q pos
      | Twelf.Assertion(_, Twelf.Type(_), pos) ->
          Lp.top
      | Twelf.Assertion(term, ty, pos) ->
          let () = Errormsg.log pos "encoding-" in
          let term' = encodeTerm term in
          let () = Errormsg.log pos ("term- : " ^ (Lp.string_of_term term')) in
          let ty' = encodeTerm ty in
          let () = Errormsg.log pos ("ty- : " ^ (Lp.string_of_term ty')) in
          hasType term' ty' pos

  let type_of_contextitem c =
    match c with
      | Twelf.Assertion(Twelf.IdTerm(n,_),ty,pos) ->
          (try
            let idf s = Lp.idType (s ^ "-type") in
            let tf = Lp.predicateType in
            let lpType = translateType idf tf ty in

            let targetType = Lp.targetType lpType in
            match targetType with
                Lp.IdType(_) when targetType = Lp.predicateType ->
                  Some(Lp.Type(n ^ "-type", 0))
              | _ -> None
          with
            TranslationError -> None)
      | _ -> None

  let type_of_declaration _ = None

  let constant_of_contextitem c =
    match c with
      | Twelf.Assertion(Twelf.IdTerm(n,_),ty,pos) ->
          (try
            let idf s = Lp.idType (s ^ "-type") in
            let tf = Lp.predicateType in
            let lpType = translateType idf tf ty in

            let targetType = Lp.targetType lpType in
            match targetType with
                Lp.IdType(_) when targetType = Lp.predicateType ->
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
          with
            TranslationError -> None)
      | Twelf.Assertion(_, _, pos) ->
          (Errormsg.error pos
            ("invalid context item: " ^ (Twelf.string_of_contextitem c));
          None)

  let constant_of_declaration dec =
    match dec with
        Twelf.Solve(n,t,_) ->
          let vars = Twelf.freeVariables t in
          let ty = Lp.arrowType (List.map Lp.idType ("Term" :: vars)) Lp.predicateType in
          Some (Lp.Constant(n, ty))
      | Twelf.Domain(_,_) -> None
    
  let term_of_contextitem t =
    let (t', _) = translatePositive [] t in
    Some(t')
  
  let term_of_declaration dec =
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
              translateNegative assertion,
              Lp.ApplicationTerm(Lp.IdTerm(n), vars'))
          in
          Some(Lp.normalize term)
      | Twelf.Domain(n, pos) ->
          None
  
  let defaultTypes = [Lp.Type("type", 0)]
  let defaultConstants = []
  let defaultTerms = []
end
