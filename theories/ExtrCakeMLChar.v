(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* Extraction to CakeML : extract ascii to CakeML's char type *)

Require Coq.extraction.Extraction.
Require Export EasyBakeCakeML.

Require Import Ascii String Coq.Strings.Byte.
Require Export ExtrCakeMLBasic.

Extract Inductive ascii => char
[
"(* If this appears, you're using Ascii internals. Please don't *)
 (fn (b0,b1,b2,b3,b4,b5,b6,b7) =>
  let fun f b i = if b then (Word8.toInt (Word8.<< (Word8.fromInt 1) i)) else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7)
  end)"
]
"(* If this appears, you're using Ascii internals. Please don't *)
 (fn f => fn c =>
  let val n = Char.ord c in
  let fun h i = (Word8.toInt (Word8.andb (Word8.fromInt n) (Word8.<< (Word8.fromInt 1) i))) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7)
  end end)".

Extract Constant zero => "Char.fromByte (Word8.fromInt 0))".
Extract Constant one => "Char.fromByte (Word8.fromInt 1))".

Extract Inlined Constant ascii_dec => "(op=)".
Extract Inlined Constant Ascii.eqb => "(op=)".
Extract Constant Ascii.compare =>
  "fn c1 => fn c2 =>
    let cmp = Char.compare c1 c2 in
    if cmp < 0 then Less else if cmp = 0 then Equal else Greater
    end".

(* Extraction to CakeML : extract byte to CakeML's Word8 type *)

(* python -c 'print(" ".join(r""" "%s" """.strip() % (r"'"'\''"'" if chr(i) == "'"'"'" else repr(""" "" """.strip()) if chr(i) == """ " """.strip() else repr(chr(i))) for i in range(256)))' # " to satisfy Coq's comment parser *)
Extract Inductive byte => "Word8.word"
[ "(Word8.fromInt 0)" "(Word8.fromInt 1)" "(Word8.fromInt 2)"
"(Word8.fromInt 3)" "(Word8.fromInt 4)" "(Word8.fromInt 5)"
"(Word8.fromInt 6)" "(Word8.fromInt 7)" "(Word8.fromInt 8)"
"(Word8.fromInt 9)" "(Word8.fromInt 10)" "(Word8.fromInt 11)"
"(Word8.fromInt 12)" "(Word8.fromInt 13)" "(Word8.fromInt 14)"
"(Word8.fromInt 15)" "(Word8.fromInt 16)" "(Word8.fromInt 17)"
"(Word8.fromInt 18)" "(Word8.fromInt 19)" "(Word8.fromInt 20)"
"(Word8.fromInt 21)" "(Word8.fromInt 22)" "(Word8.fromInt 23)"
"(Word8.fromInt 24)" "(Word8.fromInt 25)" "(Word8.fromInt 26)"
"(Word8.fromInt 27)" "(Word8.fromInt 28)" "(Word8.fromInt 29)"
"(Word8.fromInt 30)" "(Word8.fromInt 31)" "(Word8.fromInt 32)"
"(Word8.fromInt 33)" "(Word8.fromInt 34)" "(Word8.fromInt 35)"
"(Word8.fromInt 36)" "(Word8.fromInt 37)" "(Word8.fromInt 38)"
"(Word8.fromInt 39)" "(Word8.fromInt 40)" "(Word8.fromInt 41)"
"(Word8.fromInt 42)" "(Word8.fromInt 43)" "(Word8.fromInt 44)"
"(Word8.fromInt 45)" "(Word8.fromInt 46)" "(Word8.fromInt 47)"
"(Word8.fromInt 48)" "(Word8.fromInt 49)" "(Word8.fromInt 50)"
"(Word8.fromInt 51)" "(Word8.fromInt 52)" "(Word8.fromInt 53)"
"(Word8.fromInt 54)" "(Word8.fromInt 55)" "(Word8.fromInt 56)"
"(Word8.fromInt 57)" "(Word8.fromInt 58)" "(Word8.fromInt 59)"
"(Word8.fromInt 60)" "(Word8.fromInt 61)" "(Word8.fromInt 62)"
"(Word8.fromInt 63)" "(Word8.fromInt 64)" "(Word8.fromInt 65)"
"(Word8.fromInt 66)" "(Word8.fromInt 67)" "(Word8.fromInt 68)"
"(Word8.fromInt 69)" "(Word8.fromInt 70)" "(Word8.fromInt 71)"
"(Word8.fromInt 72)" "(Word8.fromInt 73)" "(Word8.fromInt 74)"
"(Word8.fromInt 75)" "(Word8.fromInt 76)" "(Word8.fromInt 77)"
"(Word8.fromInt 78)" "(Word8.fromInt 79)" "(Word8.fromInt 80)"
"(Word8.fromInt 81)" "(Word8.fromInt 82)" "(Word8.fromInt 83)"
"(Word8.fromInt 84)" "(Word8.fromInt 85)" "(Word8.fromInt 86)"
"(Word8.fromInt 87)" "(Word8.fromInt 88)" "(Word8.fromInt 89)"
"(Word8.fromInt 90)" "(Word8.fromInt 91)" "(Word8.fromInt 92)"
"(Word8.fromInt 93)" "(Word8.fromInt 94)" "(Word8.fromInt 95)"
"(Word8.fromInt 96)" "(Word8.fromInt 97)" "(Word8.fromInt 98)"
"(Word8.fromInt 99)" "(Word8.fromInt 100)" "(Word8.fromInt 101)"
"(Word8.fromInt 102)" "(Word8.fromInt 103)" "(Word8.fromInt 104)"
"(Word8.fromInt 105)" "(Word8.fromInt 106)" "(Word8.fromInt 107)"
"(Word8.fromInt 108)" "(Word8.fromInt 109)" "(Word8.fromInt 110)"
"(Word8.fromInt 111)" "(Word8.fromInt 112)" "(Word8.fromInt 113)"
"(Word8.fromInt 114)" "(Word8.fromInt 115)" "(Word8.fromInt 116)"
"(Word8.fromInt 117)" "(Word8.fromInt 118)" "(Word8.fromInt 119)"
"(Word8.fromInt 120)" "(Word8.fromInt 121)" "(Word8.fromInt 122)"
"(Word8.fromInt 123)" "(Word8.fromInt 124)" "(Word8.fromInt 125)"
"(Word8.fromInt 126)" "(Word8.fromInt 127)" "(Word8.fromInt 128)"
"(Word8.fromInt 129)" "(Word8.fromInt 130)" "(Word8.fromInt 131)"
"(Word8.fromInt 132)" "(Word8.fromInt 133)" "(Word8.fromInt 134)"
"(Word8.fromInt 135)" "(Word8.fromInt 136)" "(Word8.fromInt 137)"
"(Word8.fromInt 138)" "(Word8.fromInt 139)" "(Word8.fromInt 140)"
"(Word8.fromInt 141)" "(Word8.fromInt 142)" "(Word8.fromInt 143)"
"(Word8.fromInt 144)" "(Word8.fromInt 145)" "(Word8.fromInt 146)"
"(Word8.fromInt 147)" "(Word8.fromInt 148)" "(Word8.fromInt 149)"
"(Word8.fromInt 150)" "(Word8.fromInt 151)" "(Word8.fromInt 152)"
"(Word8.fromInt 153)" "(Word8.fromInt 154)" "(Word8.fromInt 155)"
"(Word8.fromInt 156)" "(Word8.fromInt 157)" "(Word8.fromInt 158)"
"(Word8.fromInt 159)" "(Word8.fromInt 160)" "(Word8.fromInt 161)"
"(Word8.fromInt 162)" "(Word8.fromInt 163)" "(Word8.fromInt 164)"
"(Word8.fromInt 165)" "(Word8.fromInt 166)" "(Word8.fromInt 167)"
"(Word8.fromInt 168)" "(Word8.fromInt 169)" "(Word8.fromInt 170)"
"(Word8.fromInt 171)" "(Word8.fromInt 172)" "(Word8.fromInt 173)"
"(Word8.fromInt 174)" "(Word8.fromInt 175)" "(Word8.fromInt 176)"
"(Word8.fromInt 177)" "(Word8.fromInt 178)" "(Word8.fromInt 179)"
"(Word8.fromInt 180)" "(Word8.fromInt 181)" "(Word8.fromInt 182)"
"(Word8.fromInt 183)" "(Word8.fromInt 184)" "(Word8.fromInt 185)"
"(Word8.fromInt 186)" "(Word8.fromInt 187)" "(Word8.fromInt 188)"
"(Word8.fromInt 189)" "(Word8.fromInt 190)" "(Word8.fromInt 191)"
"(Word8.fromInt 192)" "(Word8.fromInt 193)" "(Word8.fromInt 194)"
"(Word8.fromInt 195)" "(Word8.fromInt 196)" "(Word8.fromInt 197)"
"(Word8.fromInt 198)" "(Word8.fromInt 199)" "(Word8.fromInt 200)"
"(Word8.fromInt 201)" "(Word8.fromInt 202)" "(Word8.fromInt 203)"
"(Word8.fromInt 204)" "(Word8.fromInt 205)" "(Word8.fromInt 206)"
"(Word8.fromInt 207)" "(Word8.fromInt 208)" "(Word8.fromInt 209)"
"(Word8.fromInt 210)" "(Word8.fromInt 211)" "(Word8.fromInt 212)"
"(Word8.fromInt 213)" "(Word8.fromInt 214)" "(Word8.fromInt 215)"
"(Word8.fromInt 216)" "(Word8.fromInt 217)" "(Word8.fromInt 218)"
"(Word8.fromInt 219)" "(Word8.fromInt 220)" "(Word8.fromInt 221)"
"(Word8.fromInt 222)" "(Word8.fromInt 223)" "(Word8.fromInt 224)"
"(Word8.fromInt 225)" "(Word8.fromInt 226)" "(Word8.fromInt 227)"
"(Word8.fromInt 228)" "(Word8.fromInt 229)" "(Word8.fromInt 230)"
"(Word8.fromInt 231)" "(Word8.fromInt 232)" "(Word8.fromInt 233)"
"(Word8.fromInt 234)" "(Word8.fromInt 235)" "(Word8.fromInt 236)"
"(Word8.fromInt 237)" "(Word8.fromInt 238)" "(Word8.fromInt 239)"
"(Word8.fromInt 240)" "(Word8.fromInt 241)" "(Word8.fromInt 242)"
"(Word8.fromInt 243)" "(Word8.fromInt 244)" "(Word8.fromInt 245)"
"(Word8.fromInt 246)" "(Word8.fromInt 247)" "(Word8.fromInt 248)"
"(Word8.fromInt 249)" "(Word8.fromInt 250)" "(Word8.fromInt 251)"
"(Word8.fromInt 252)" "(Word8.fromInt 253)" "(Word8.fromInt 254)"
"(Word8.fromInt 255)" ].

Extract Inlined Constant Byte.eqb => "(op=)".
Extract Inlined Constant Byte.byte_eq_dec => "(op=)".
Extract Inlined Constant Ascii.ascii_of_byte => "(fn x => Char.fromByte x)".
Extract Inlined Constant Ascii.byte_of_ascii => "(fn x => Word8.fromInt (Char.ord x))".