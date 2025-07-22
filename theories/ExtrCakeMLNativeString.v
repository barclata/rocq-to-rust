(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* Extraction to CakeML : does everything in ExtrCakeMLChar.v 
   as well as extracting strings to CakeML's string type. *)

Require Coq.extraction.Extraction.
Require Export EasyBakeCakeML.

Require Import Ascii String Coq.Strings.Byte.
Require Export ExtrCakeMLChar.

(* This differs from ExtrCakeMLString.v as we have not made one yet, and frankly dont plan on ever *) 

Extract Inductive string => "string"
[
(* EmptyString *)
"(* If this appears, you're using String internals. Please don't *)
  """"
"
(* String *)
"(* If this appears, you're using String internals. Please don't *)
  (fn (c, s) => ((String.str c) ^ s))
"
]
"(* If this appears, you're using String internals. Please don't *)
 (fn f0 => fn f1 => fn s =>
    let val l = String.size s in
    if l = 0 then f0 () else f1 (String.sub s 0) (String.substring s 1 (l-1))
    end)
".

(* Unfortunately, CakeML does not like non-infix "=" so we have to hack it like this *)
Extract Inlined Constant String.string_dec => "(op=)".
Extract Inlined Constant String.eqb => "(op=)".
Extract Inlined Constant String.append => "String.^".
Extract Inlined Constant String.concat => "String.concat".
Extract Inlined Constant String.prefix =>
  "(fn s1 => fn s2 =>
     let val l1 = String.size s1 
         val l2 = String.size s2 
     in
     (l1 <= l2) andalso (String.substring s2 0 l1 = s1)
     end)".
Extract Inlined Constant String.string_of_list_ascii =>
  "String.implode".
Extract Inlined Constant String.list_ascii_of_string =>
  "String.explode".
Extract Inlined Constant String.string_of_list_byte =>
  "(fun l ->
      String.implode (List.map Char.fromByte l))".
Extract Inlined Constant String.list_byte_of_string =>
  "(fun s -> List.map (Word8.fromInt o Char.ord) (String.explode s))".

(* Other operations in module String (at the time of this writing):
      String.length
      String.get
      String.substring
      String.index
      String.findex
  They all use type "nat".  If we know that "nat" extracts
  to O | S of nat, we can provide CakeML implementations
  for these functions that work directly on CakeML's strings.
  However "nat" could be extracted to other CakeML types...
*)