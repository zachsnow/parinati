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

type translator : Twelf.assertion -> Lp.term

module type TRANSLATOR =
sig
  val translate_positive : translator
  val translate_negative : translator
end

module OriginalTranslation : TRANSLATOR
module SimplifiedTranslation : TRANSLATOR
module OptimizedTranslation : TRANSLATOR

val type_of_contextitem : translator -> Twelf.assertion -> Lp.lptype
val type_of_declaration : translator -> Twelf.declaration -> Lp.lptype

val constant_of_contextitem : translator -> Twelf.assertion -> Lp.lptype
val constant_of_declaration : translator -> Twelf.declaration -> Lp.lptype

val term_of_contextitem : translator -> TTwelf.assertion -> Lp.term option
val term_of_declaration : translator -> TTwelf.declaration -> Lp.term option
