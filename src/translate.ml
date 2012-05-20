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
exception TranslationError

module Translation (Translator : Translators.TRANSLATOR) =
struct
  (**********************************************************************
  *term_of_contextitem:
  * Wraps up terms so that they can be elided completely if they
  * are equal to top.
  **********************************************************************)
  let term_of_contextitem c =
    match Translator.term_of_contextitem c with
      | Some c' ->  Lp.elide (Lp.normalize c')
      | None -> None

  (**********************************************************************
  *translate:
  * Translate LF specifications into a Lambda Prolog signature and module.
  **********************************************************************)
  let rec translate name twelf =
    match twelf with
      Twelf.Specification(c, d) ->
        let contextTypes = Util.choose Translator.type_of_contextitem c in
        let contextConstants = Util.choose Translator.constant_of_contextitem c in
        let contextTerms = Util.choose term_of_contextitem c in
        
        let declarationTypes = Util.choose Translator.type_of_declaration d in
        let declarationConstants = Util.choose Translator.constant_of_declaration d in
        let declarationTerms = Util.choose Translator.term_of_declaration d in
       
        let allTypes = contextTypes @ declarationTypes @ Translator.defaultTypes in
        let allConstants = contextConstants @ declarationConstants @ Translator.defaultConstants in
        let allTerms = contextTerms @ declarationTerms @ Translator.defaultTerms in
        
        let s = Lp.Signature(allTypes, allConstants) in
        let m = Lp.Module(List.map (fun t -> Lp.Term(t)) allTerms) in

        if !Errormsg.anyErrors then
          None
        else
          Some (Lp.Program(name, s, m))
end

let translate name twelf =
  match !Options.translation with
    | Options.Original ->
        let module T = Translation(Translators.OriginalTranslation) in
        T.translate name twelf
    | Options.Simplified ->
        let module T = Translation(Translators.SimplifiedTranslation) in
        T.translate name twelf
    | Options.Optimized ->
       let module T = Translation(Translators.OptimizedTranslation) in
        T.translate name twelf
    | Options.Extended ->
        let module T = Translation(Translators.ExtendedTranslation) in
        T.translate name twelf
    | Options.Strange ->
        let module T = Translation(Translators.StrangeTranslation) in
        T.translate name twelf
