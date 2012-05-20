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
val empty : 'a list -> bool
val split3 : ('a * 'b * 'c) list -> ('a list * 'b list * 'c list)
val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val mapn : (int -> 'a) -> int -> 'a list
val mapi : ('a -> int -> 'b) -> 'a list -> 'b list
val split_nth : int -> 'a list -> ('a list * 'a list)
val unique : 'a list -> 'a list
val choose : ('a -> 'b option) -> 'a list -> 'b list

val is_some : 'a option -> bool
val is_none : 'a option -> bool
val get : 'a option -> 'a

