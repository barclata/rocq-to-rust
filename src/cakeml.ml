(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(*s Production of CakeML syntax. *)

open Pp
open CErrors
open Util
open Names
open ModPath
open Table
open Miniml
open Mlutil
open Common


(*s Some utility functions. *)

let pp_tvar id = str ("'" ^ Id.to_string id)

let pp_abst = function
  | [] -> mt ()
  | l  ->
    str "fn " ++ prlist_with_sep (fun () -> str " => fn ") Id.print l ++
    str " =>" ++ spc ()

let pp_parameters l =
  (pp_boxed_tuple pp_tvar l ++ space_if (not (List.is_empty l)))

let pp_string_parameters l =
  (pp_boxed_tuple str l ++ space_if (not (List.is_empty l)))

let pp_letin pat def body =
  let fstline = str "let val " ++ pat ++ str " =" ++ spc () ++ def in
  hv 0 (hv 0 (hov 2 fstline ++ spc () ++ str "in") ++ spc () ++ hov 0 body ++ str " end")

(*s Ocaml renaming issues. *)

let keywords =
  List.fold_right (fun s -> Id.Set.add (Id.of_string s))
    [ "and"; "andalso"; "as"; "case"; "datatype";
      "else"; "end"; "eqtype"; "exception"; "fn"; "fun"; "handle";
      "if"; "in"; "include"; "let"; "local"; "of"; "op"; "open"; "orelse";
      "raise"; "rec"; "sharing"; "sig"; "signature"; "struct"; "structure";
      "then"; "type"; "val"; "where"; "with"; "withtype";
      "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr" ; "unit" ; "_" ; "ml___dummy" ]
    Id.Set.empty

(* Note: do not shorten [str "foo" ++ fnl ()] into [str "foo\n"],
   the '\n' character interacts badly with the Format boxing mechanism *)

let pp_comment s = str "(* " ++ hov 0 s ++ str " *)"

let pp_header_comment = function
  | None -> mt ()
  | Some com -> pp_comment com ++ fnl2 ()

let then_nl pp = if Pp.ismt pp then mt () else pp ++ fnl ()

(* Bad probably*)
let pp_tdummy usf =
  if usf.tdummy || usf.tunknown then str "type ml___dummy = unit" ++ fnl () else mt ()

(* Bad probably*)
let pp_mldummy usf =
  if usf.mldummy then
    str "let ml__dummy = let rec f _ = f in f" ++ fnl ()
  else mt ()

let preamble _ comment used_modules usf =
  pp_header_comment comment ++
  then_nl (pp_tdummy usf ++ pp_mldummy usf)

(*s The pretty-printer for Ocaml syntax*)

(* Beware of the side-effects of [pp_global] and [pp_modname].
   They are used to update table of content for modules. Many [let]
   below should not be altered since they force evaluation order.
*)

let str_global k r =
  if is_inline_custom r then find_custom r else Common.pp_global k r

let pp_global k r = str (str_global k r)

let pp_global_name k r = str (Common.pp_global k r)

let pp_modname mp = str (Common.pp_module mp)

(* grammar from OCaml 4.06 manual, "Prefix and infix symbols" *)

let infix_symbols =
  ['=' ; '<' ; '>' ; '@' ; '^' ; ';' ; '&' ; '+' ; '-' ; '*' ; '/' ; '$' ; '%' ]
let operator_chars =
  [ '!' ; '$' ; '%' ; '&' ; '*' ; '+' ; '-' ; '.' ; '/' ; ':' ; '<' ; '=' ; '>' ; '?' ; '@' ; '^' ; '|' ; '~' ]

(* infix ops in OCaml, but disallowed by preceding grammar *)

let builtin_infixes =
  [ "::" ; "," ]

let substring_all_opchars s start stop =
  let rec check_char i =
    if i >= stop then true
    else
      List.mem s.[i] operator_chars && check_char (i+1)
  in
  check_char start

let is_infix r =
  is_inline_custom r &&
  (let s = find_custom r in
   let len = String.length s in
   len >= 3 &&
   (* parenthesized *)
   (s.[0] == '(' && s.[len-1] == ')' &&
    let inparens = String.trim (String.sub s 1 (len - 2)) in
    let inparens_len = String.length inparens in
    (* either, begins with infix symbol, any remainder is all operator chars *)
    (List.mem inparens.[0] infix_symbols && substring_all_opchars inparens 1 inparens_len) ||
    (* or, starts with #, at least one more char, all are operator chars *)
    (inparens.[0] == '#' && inparens_len >= 2 && substring_all_opchars inparens 1 inparens_len) ||
    (* or, is an OCaml built-in infix *)
    (List.mem inparens builtin_infixes)))

let get_infix r =
  let s = find_custom r in
  String.sub s 1 (String.length s - 2)

(*s Pretty-printing of types. [par] is a boolean indicating whether parentheses
    are needed or not. *)

let pp_type par vl t =
  let rec pp_rec par = function
    | Tmeta _ | Tvar' _ | Taxiom -> assert false
    | Tvar i -> (try pp_tvar (List.nth vl (pred i))
                 with Failure _ -> (str "'a" ++ int i))
    | Tglob (r,[a1;a2]) when is_infix r ->
      pp_par par (pp_rec true a1 ++ str (get_infix r) ++ pp_rec true a2)
    | Tglob (r,[]) -> pp_global Type r
    | Tglob (gr,l)
      when not (keep_singleton ()) && Coqlib.check_ref sig_type_name gr ->
      pp_tuple_light pp_rec l
    | Tglob (r,l) ->
      pp_tuple_light pp_rec l ++ spc () ++ pp_global Type r
    | Tarr (t1,t2) ->
      pp_par par
        (pp_rec true t1 ++ spc () ++ str "->" ++ spc () ++ pp_rec false t2)
    | Tdummy _ -> str "ml___dummy"
    | Tunknown -> str "ml___dummy"
  in
  hov 0 (pp_rec par t)

(* boolean check that a type has variables or not *)
let rec has_type_vars = function
    | Tmeta _ | Tvar' _ | Taxiom -> true
    | Tvar i -> true
    | Tglob (r,[a1;a2]) when is_infix r -> has_type_vars a1 || has_type_vars a2
    | Tglob (r,[]) -> false
    | Tglob (gr,l)
      when not (keep_singleton ()) && Coqlib.check_ref sig_type_name gr ->
      List.fold_left (||) false (List.map has_type_vars l)
    | Tglob (r,l) ->
      List.fold_left (||) false (List.map has_type_vars l)
    | Tarr (t1,t2) ->
      has_type_vars t1 || has_type_vars t2
    | Tdummy _ -> true
    | Tunknown -> true

(*s Pretty-printing of expressions. [par] indicates whether
    parentheses are needed or not. [env] is the list of names for the
    de Bruijn variables. [args] is the list of collected arguments
    (already pretty-printed). *)

let is_bool_patt p s =
  try
    let r = match p with
      | Pusual r -> r
      | Pcons (r,[]) -> r
      | _ -> raise Not_found
    in
    String.equal (find_custom r) s
  with Not_found -> false


let is_ifthenelse = function
  | [|([],p1,_);([],p2,_)|] -> is_bool_patt p1 "true" && is_bool_patt p2 "false"
  | _ -> false

let expr_needs_par = function
  | MLlam _  -> true
  | MLcase (_,_,[|_|]) -> false
  | MLcase (_,_,pv) -> not (is_ifthenelse pv)
  | _        -> false

let rec pp_expr par env args =
  let apply st = pp_apply st par args
  and apply2 st = pp_apply2 st par args in
  function
  | MLrel n ->
    let id = get_db_name n env in
    (* Try to survive to the occurrence of a Dummy rel.
       TODO: we should get rid of this hack (cf. #592) *)
    let id = if Id.equal id dummy_name then Id.of_string "ml___dummy" else id in
    apply (Id.print id)
  | MLapp (f,args') ->
    let stl = List.map (pp_expr true env []) args' in
    pp_expr par env (stl @ args) f
  | MLlam _ as a ->
    let fl,a' = collect_lams a in
    let fl = List.map id_of_mlid fl in
    let fl,env' = push_vars fl env in
    let st = pp_abst (List.rev fl) ++ pp_expr false env' [] a' in
    apply2 st
  | MLletin (id,a1,a2) ->
    let i,env' = push_vars [id_of_mlid id] env in
    let pp_id = Id.print (List.hd i)
    and pp_a1 = pp_expr false env [] a1
    and pp_a2 = pp_expr (not par && expr_needs_par a2) env' [] a2 in
    hv 0 (apply2 (pp_letin pp_id pp_a1 pp_a2))
  | MLglob r -> apply (pp_global Term r)
  | MLfix (i,ids,defs) ->
    let ids',env' = push_vars (List.rev (Array.to_list ids)) env in
    pp_fix par env' i (Array.of_list (List.rev ids'),defs) args
  | MLexn s ->
    (* An [MLexn] may be applied, but I don't really care. *)
    pp_par par (str "Runtime.assert \"false\" false" ++ spc () ++ str ("(* "^s^" *)"))
  | MLdummy k ->
    (* An [MLdummy] may be applied, but I don't really care. *)
    (match msg_of_implicit k with
     | "" -> str "ml___dummy"
     | s -> str "ml___dummy" ++ spc () ++ str ("(* "^s^" *)"))
  | MLmagic a -> pp_expr true env args a
  | MLaxiom s ->
    pp_par par (str "failwith \"AXIOM TO BE REALIZED (" ++ str s ++ str ")\"")
  | MLcons (_,r,a) as c ->
    assert (List.is_empty args);
    begin match a with
      | _ when is_native_char c -> pp_native_char_cakeml c
      | _ when is_native_string c -> pp_native_string c
      | [a1;a2] when is_infix r ->
        let pp = pp_expr true env [] in
        pp_par par (pp a1 ++ str (get_infix r) ++ pp a2)
      | _ when is_coinductive r ->
        let ne = not (List.is_empty a) in
        let tuple = space_if ne ++ pp_tuple (pp_expr true env []) a in
        pp_par par (str "lazy " ++ pp_par ne (pp_global Cons r ++ tuple))
      | [] -> pp_global Cons r
      | _ ->
        (* let tuple = pp_tuple (pp_expr true env []) a in *)
          (* this was the bug *)
        if String.is_empty (str_global Cons r) (* hack Extract Inductive prod *)
        then pp_tuple (pp_expr true env []) a (* was tuple when fast *)
        else let args = prlist_with_sep spc (pp_expr true env []) a in pp_par par (pp_global Cons r ++ spc () ++ args) end (* args should be tuple to be fast *) (* tuple was args during bug *)

  | MLtuple l ->
    assert (List.is_empty args);
    pp_boxed_tuple (pp_expr true env []) l
  | MLcase (_, t, pv) when is_custom_match pv ->
    if not (is_regular_match pv) then
      user_err Pp.(str "Cannot mix yet user-given match and general patterns.");
    let mkfun (ids,_,e) =
      if not (List.is_empty ids) then named_lams (List.rev ids) e
      else dummy_lams (ast_lift 1 e) 1
    in
    let pp_branch tr = pp_expr true env [] (mkfun tr) ++ fnl () in
    let inner =
      str (find_custom_match pv) ++ fnl () ++
      prvect pp_branch pv ++
      pp_expr true env [] t
    in
    apply2 (hov 2 inner)
  | MLcase (typ, t, pv) ->
    let head =
      if not (is_coinductive_type typ) then pp_expr false env [] t
      else (str "Lazy.force" ++ spc () ++ pp_expr true env [] t)
    in
    (* First, can this match be printed as a let-in ? *)
    if Int.equal (Array.length pv) 1 then
      let s1,s2 = pp_one_pat env pv.(0) in
      hv 0 (apply2 (pp_letin s1 head s2))
    else
      (* Second, can this match be printed as [if ... then ... else] ? *)
      (try apply2 (pp_ifthenelse env head pv)
       with Not_found ->
         (* Otherwise, standard match *)
         apply2
           (v 0 (str "case " ++ head ++ str " of" ++ fnl () ++
                 pp_pat env pv)))
  | MLuint i ->
    assert (args=[]);
    str "(" ++ str (Uint63.compile i) ++ str ")"
  | MLfloat f ->
    assert (args=[]);
    str "(" ++ str (Float64.compile f) ++ str ")"
  | MLstring s ->
      assert (args=[]);
      str "(" ++ str (Pstring.compile s) ++ str ")"
  | MLparray(t,def) ->
    assert (args=[]);
    let tuple = pp_array (pp_expr true env []) (Array.to_list t) in
    let def = pp_expr true env [] def in
    str "(ExtrNative.of_array [|" ++ tuple ++ str "|]" ++ spc () ++ def ++ str")"

and pp_cons_pat r ppl =
  if is_infix r && Int.equal (List.length ppl) 2 then
    List.hd ppl ++ str (get_infix r) ++ List.hd (List.tl ppl)
  else
    if String.is_empty (str_global Cons r) then
      pp_boxed_tuple identity ppl (* Hack Extract Inductive prod *)
    else
      pp_global Cons r ++ space_if (not (List.is_empty ppl)) ++ prlist_with_sep spc identity ppl

and pp_gen_pat ids env = function
  | Pcons (r, l) -> pp_cons_pat r (List.map (pp_gen_pat ids env) l)
  | Pusual r -> pp_cons_pat r (List.map Id.print ids)
  | Ptuple l -> pp_boxed_tuple (pp_gen_pat ids env) l
  | Pwild -> str "_"
  | Prel n -> Id.print (get_db_name n env)

and pp_ifthenelse env expr pv = match pv with
  | [|([],tru,the);([],fal,els)|] when
      (is_bool_patt tru "true") && (is_bool_patt fal "false")
    ->
    hv 0 (hov 2 (str "if " ++ expr) ++ spc () ++
          hov 2 (str "then " ++
                 hov 2 (pp_expr (expr_needs_par the) env [] the)) ++ spc () ++
          hov 2 (str "else " ++
                 hov 2 (pp_expr (expr_needs_par els) env [] els)))
  | _ -> raise Not_found

and pp_one_pat env (ids,p,t) =
  let ids',env' = push_vars (List.rev_map id_of_mlid ids) env in
  pp_gen_pat (List.rev ids') env' p,
  pp_expr (expr_needs_par t) env' [] t

and pp_pat env pv =
  prvecti
    (fun i x ->
       let s1,s2 = pp_one_pat env x in
       hv 2 (hov 4 ((if x = pv.(0) then str "  " else str "| ") ++ s1 ++ str " =>") ++ spc () ++ hov 2 s2) ++
       if Int.equal i (Array.length pv - 1) then mt () else fnl ())
    pv

and pp_function env t =
  let bl,t' = collect_lams t in
  let bl,env' = push_vars (List.map id_of_mlid bl) env in
  match t' with
  | MLcase(Tglob(r,_),MLrel 1,pv) when
      not (is_coinductive r) && List.is_empty (get_record_fields r) &&
      not (is_custom_match pv) ->
    pr_binding (List.rev bl) ++
    str " = case " ++ Id.print (List.hd bl) ++ str " of" ++ fnl () ++
    v 0 (pp_pat env' pv)
  | _ ->
    pr_binding (List.rev bl) ++
      str " =" ++ fnl () ++ str "  " ++
    hov 2 (pp_expr false env' [] t')

(*s names of the functions ([ids]) are already pushed in [env],
    and passed here just for convenience. *)

and pp_fix par env i (ids,bl) args =
  pp_par par
    (v 0 (str "fun " ++
          prvect_with_sep
            (fun () -> fnl () ++ str "and ")
            (fun (fi,ti) -> Id.print fi ++ pp_function env ti)
            (Array.map2 (fun id b -> (id,b)) ids bl) ++
          fnl () ++
          hov 2 (str "in " ++ pp_apply (Id.print ids.(i)) false args)))

(* Ad-hoc double-newline in v boxes, with enough negative whitespace
   to avoid indenting the intermediate blank line *)

let cut2 () = brk (0,-100000) ++ brk (0,0)

let pp_val e typ =
  hov 4 (str "(** val " ++ e ++ str " :" ++ spc () ++ pp_type false [] typ ++
         str " **)")  ++ cut2 ()

(*s Pretty-printing of [Dfix] *)

let pp_Dfix (rv,c,t) =
  let names = Array.map
      (fun r -> if is_inline_custom r then mt () else pp_global_name Term r) rv
  in
  let rec pp init i =
    if i >= Array.length rv then mt ()
    else
      let void = is_inline_custom rv.(i) ||
                 (not (is_custom rv.(i)) &&
                  match c.(i) with MLexn "UNUSED" -> true | _ -> false)
      in
      if void then pp init (i+1)
      else
        let def =
          if is_custom rv.(i) then str " = " ++ str (find_custom rv.(i))
          else
            pp_function (empty_env ()) c.(i)
        in
        (if init then mt () else cut2 ()) ++
        pp_val names.(i) t.(i) ++
        str (if init then "fun " else "and ") ++ names.(i) ++ def ++
        pp false (i+1)
  in pp true 0

(*s Pretty-printing of inductive types declaration. *)

let pp_equiv param_list name = function
  | NoEquiv, _ -> mt ()
  | Equiv kn, i ->
    str " = " ++ pp_parameters param_list ++ pp_global Type (GlobRef.IndRef (MutInd.make1 kn,i))
  | RenEquiv ren, _  ->
    str " = " ++ pp_parameters param_list ++ str (ren^".") ++ name


let pp_one_ind prefix ip_equiv pl name cnames ctyps =
  let pl = rename_tvars keywords pl in
  let needs_parens t = match t with
    | Tglob (_,_::_) | Tarr _ -> true
    | _ -> false
  in
  let pp_constructor i typs =
    (if Int.equal i 0 then mt () else fnl ()) ++
    hov 3 ((if Int.equal i 0 then str "  " else str "| ") ++ cnames.(i) ++ str " " ++
           prlist_with_sep spc (fun ty -> pp_par (needs_parens ty) (pp_type true pl ty)) typs)
  in
  pp_parameters pl ++ str prefix ++ name ++
  pp_equiv pl name ip_equiv ++ str " =" ++
  if Int.equal (Array.length ctyps) 0 then str " unit (* empty inductive *)"
  else fnl () ++ v 0 (prvecti pp_constructor ctyps)

let pp_logical_ind packet =
  pp_comment (Id.print packet.ip_typename ++ str " : logical inductive") ++
  fnl () ++
  pp_comment (str "with constructors : " ++
              prvect_with_sep spc Id.print packet.ip_consnames) ++
  fnl ()

let pp_singleton kn packet =
  let name = pp_global_name Type (GlobRef.IndRef (kn,0)) in
  let l = rename_tvars keywords packet.ip_vars in
  hov 2 (str "type " ++ pp_parameters l ++ name ++ str " =" ++ spc () ++
         pp_type false l (List.hd packet.ip_types.(0)) ++ fnl () ++
         pp_comment (str "singleton inductive, whose constructor was " ++
                     Id.print packet.ip_consnames.(0)))

let pp_coind pl name =
  let pl = rename_tvars keywords pl in
  pp_parameters pl ++ name ++ str " = " ++
  pp_parameters pl ++ str "ml___dummy" ++ name ++ str " Lazy.t" ++
  fnl() ++ str "and "

let pp_ind co kn ind =
  let prefix = if co then "ml___dummy" else "" in
  let initkwd = str "datatype " in
  let nextkwd = fnl () ++ str "and " in
  let names =
    Array.mapi (fun i p -> if p.ip_logical then mt () else
                   pp_global_name Type (GlobRef.IndRef (kn,i)))
      ind.ind_packets
  in
  let cnames =
    Array.mapi
      (fun i p -> if p.ip_logical then [||] else
          Array.mapi (fun j _ -> pp_global Cons (GlobRef.ConstructRef ((kn,i),j+1)))
            p.ip_types)
      ind.ind_packets
  in
  let rec pp i kwd =
    if i >= Array.length ind.ind_packets then mt ()
    else
      let ip = (kn,i) in
      let ip_equiv = ind.ind_equiv, i in
      let p = ind.ind_packets.(i) in
      if is_custom (GlobRef.IndRef ip) then pp (i+1) kwd
      else if p.ip_logical then pp_logical_ind p ++ pp (i+1) kwd
      else
        kwd ++ (if co then pp_coind p.ip_vars names.(i) else mt ()) ++
        pp_one_ind prefix ip_equiv p.ip_vars names.(i) cnames.(i) p.ip_types ++
        pp (i+1) nextkwd
  in
  pp 0 initkwd


(*s Pretty-printing of a declaration. *)

let pp_mind kn i =
  match i.ind_kind with
  | Singleton -> pp_singleton kn i.ind_packets.(0)
  | Coinductive -> pp_ind true kn i
  | Record fields -> pp_ind false kn i
  | Standard -> pp_ind false kn i

let pp_decl = function
  | Dtype (r,_,_) when is_inline_custom r -> mt ()
  | Dterm (r,_,_) when is_inline_custom r -> mt ()
  | Dind (kn,i) -> pp_mind kn i
  | Dtype (r, l, t) ->
    let name = pp_global_name Type r in
    let l = rename_tvars keywords l in
    let ids, def =
      try
        let ids,s = find_type_custom r in
        pp_string_parameters ids, str " =" ++ spc () ++ str s
      with Not_found ->
        pp_parameters l,
        if t == Taxiom then str " (* AXIOM TO BE REALIZED *)"
        else str " =" ++ spc () ++ pp_type false l t
    in
    hov 2 (str "type " ++ ids ++ name ++ def)
  | Dterm (r, a, t) ->
    let def =
      if is_custom r then str (" = " ^ find_custom r)
      else pp_function (empty_env ()) a
    in
    let is_val = if nb_lams a > 0 then false else true in
    let fun_or_val = if is_val then str "val " else str "fun " in
    let name = pp_global_name Term r in
    let typ = if is_val && not (has_type_vars t) then str " : " ++ pp_type true [] t else str "" in
    pp_val name t ++ hov 0 (fun_or_val ++ name ++ typ ++ def ++ mt ()) (* HERE *)
  | Dfix (rv,defs,typs) ->
    pp_Dfix (rv,defs,typs)

let rec pp_structure_elem = function
  | (l,SEdecl d) -> pp_decl d
  | (l,SEmodule m) ->
    let def = pp_module_expr [] m.ml_mod_expr in
    let name = pp_modname (MPdot (top_visible_mp (), l)) in
    hov 1
      (str "structure " ++ name ++ str " =" ++ def)
  | (l,SEmodtype m) -> mt ()

and pp_module_expr params = function
  | MEident _ | MEapply _ -> assert false
  | MEfunctor _ -> assert false
  | MEstruct (mp, sel) ->
    push_visible mp params;
    let try_pp_structure_elem l x =
      let px = pp_structure_elem x in
      if Pp.ismt px then l else px::l
    in
    (* We cannot use fold_right here due to side effects in pp_structure_elem *)
    let l = List.fold_left try_pp_structure_elem [] sel in
    let l = List.rev l in
    pop_visible ();
    str "struct" ++ fnl () ++
    (if List.is_empty l then mt ()
     else
       v 1 (str " " ++ prlist_with_sep cut2 identity l) ++ fnl ())
    ++ str "end"

let rec prlist_sep_nonempty sep f = function
  | [] -> mt ()
  | [h] -> f h
  | h::t ->
    let e = f h in
    let r = prlist_sep_nonempty sep f t in
    if Pp.ismt e then r
    else e ++ sep () ++ r

let do_struct f s =
  let ppl (mp,sel) =
    push_visible mp [];
    let p = prlist_sep_nonempty cut2 f sel in
    (* for monolithic cakeml_extraction, we try to simulate the unavailability
       of [MPfile] in names by artificially nesting these [MPfile] *)
    (if modular () then pop_visible ()); p
  in
  let p = prlist_sep_nonempty cut2 ppl s in
  (if not (modular ()) then repeat (List.length s) pop_visible ());
  v 0 p ++ fnl ()

let pp_struct s = do_struct pp_structure_elem s

let cakeml_descr = {
  keywords = keywords;
  file_suffix = ".cml";
  file_naming = file_of_modfile;
  preamble = preamble;
  pp_struct = pp_struct;
  sig_suffix = None;
  sig_preamble = (fun _ _ _ _ -> mt ());
  pp_sig = (fun _ -> mt ());
  pp_decl = pp_decl;
}