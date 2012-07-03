(****************************************************************************
* Copyright 2008-2012 Zach Snow
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
exception TranslationError

module Translate (Translation : Translators.TRANSLATION) =
struct
  let default_types =
    if !Options.type_embedding_optimization then
      [
        Lp.Type("type", 0)
      ]
    else
      [
        Lp.Type("object", 0);
        Lp.Type("type", 0)
      ]
  
  let default_constants =
    if !Options.type_embedding_optimization then
      []
    else
      let obj = Lp.IdType("object") in
      let ty = Lp.IdType("type") in
      [
        Lp.Constant("istype", Lp.arrow_type [ty] Lp.predicate_type);
        Lp.Constant("hastype", Lp.arrow_type [obj; ty] Lp.predicate_type)
      ]
      
  let default_terms = []
  
  (**********************************************************************
  *translate:
  * Translate LF specifications into a Lambda Prolog signature and module.
  **********************************************************************)
  let rec translate name twelf =
    match twelf with
      Twelf.Specification(c, d) ->
        let context_types = Util.choose Translation.type_of_contextitem c in
        let context_constants = Util.choose Translation.constant_of_contextitem c in
        let context_terms = Util.choose Translation.term_of_contextitem c in
        
        let declaration_types = Util.choose Translation.type_of_declaration d in
        let declaration_constants = Util.choose Translation.constant_of_declaration d in
        let declaration_terms = Util.choose Translation.term_of_declaration d in
       
        let all_types = context_types @ declaration_types @ default_types in
        let all_constants = context_constants @ declaration_constants @ default_constants in
        let all_terms = context_terms @ declaration_terms @ default_terms in
        
        let s = Lp.Signature(all_types, all_constants) in
        let m = Lp.Module(List.map (fun t -> Lp.Term(t)) all_terms) in

        if !Errormsg.any_errors then
          None
        else
          Some (Lp.Program(name, s, m))
end

let translate name twelf =
  match !Options.translation with
    | Options.Original ->
        let module T = Translate(Translators.OriginalTranslation) in
        T.translate name twelf
    | Options.Simplified ->
        let module T = Translate(Translators.SimplifiedTranslation) in
        T.translate name twelf
    | Options.Optimized ->
       let module T = Translate(Translators.OptimizedTranslation) in
        T.translate name twelf
