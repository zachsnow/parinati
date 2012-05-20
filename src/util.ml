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

(********************************************************************
* Option Utilities:
********************************************************************)
let is_some v =
  match v with
    Some _ -> true
  | None -> false

let is_none v = not (is_some v)

let get v =
  match v with
    Some v' -> v'
  | None -> raise (Failure "Option.get")

(********************************************************************
* List Utilities:
********************************************************************)
(********************************************************************
*empty:
* Check if a list is empty.
********************************************************************)
let empty = function
    [] -> true
  | _ -> false

(********************************************************************
*split3/combine3: split and combine on 3-tuples.
********************************************************************)
let rec split3 l = match l with
    [] -> ([], [], [])
  | (h1,h2,h3)::tl ->
      let (t1,t2,t3) = split3 tl in
      (h1::t1, h2::t2, h3::t3)

let rec combine3 l1 l2 l3 =
  match l1,l2,l3 with
    [],[],[] -> []
  | (h1::t1, h2::t2, h3::t3) -> (h1,h2,h3)::(combine3 t1 t2 t3)
  | _ -> failwith "Listutils.split3: invalid arguments"

(********************************************************************
*mapn: Given a function and an int, apply the function to all ints
* from 0 to the given one.
********************************************************************)
let mapn f n =
  let rec map f i n =
    if i = n then [] else (f i) :: (map f (i + 1) n)
  in
  map f 0 n

(********************************************************************
*mapi:
* Map with an integer index as well.
********************************************************************)
let mapi f l =
  let rec map f l i =
    match l with
      [] -> []
    | h::tl -> (f h i) :: (map f tl (i + 1))
  in
  map f l 0

(********************************************************************
*unique:
* Remove duplicates from a list.  Should maintain the order.
********************************************************************)
let unique l =
  let insert l x = if List.exists ((=) x) l then l else x :: l in
  List.rev (List.fold_left insert [] l)

(********************************************************************
*choose:
********************************************************************)
let choose f l =
  List.map get (List.filter is_some (List.map f l))

(********************************************************************
*split_nth:
* Returns a pair (l, r) where l is the first n elements of the given
* list and r is the rest.
* Named dumb to look like the other list functions.
********************************************************************)
let split_nth i l =
  let rec split' i l r =
    match (i, r) with
        (0, _) -> (List.rev l, r)
      | (i',h::t) -> split' (i' - 1) (h :: l) (t)
      | _ -> raise (Failure "Listutils.split_nth")
  in
  split' i [] l
