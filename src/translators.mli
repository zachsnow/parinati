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

module type TRANSLATION =
sig
  val type_of_contextitem : Twelf.assertion -> Lp.lptype option
  val type_of_declaration : Twelf.declaration -> Lp.lptype option

  val constant_of_contextitem : Twelf.assertion -> Lp.constant option
  val constant_of_declaration : Twelf.declaration -> Lp.constant option
  
  val term_of_contextitem : Twelf.assertion -> Lp.term option
  val term_of_declaration : Twelf.declaration -> Lp.term option
end

module OriginalTranslation : TRANSLATION
module SimplifiedTranslation : TRANSLATION
module OptimizedTranslation : TRANSLATION
