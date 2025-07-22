(* Yoichi Hirai, 2015.
   containing copies from scheme.ml, haskell.ml, and ocaml.ml
*)

open Miniml
open Util
open Pp
open Names
open Table
open Common
open Mlutil


(* type boxed_ml_branch = ml_ident list * ml_pattern * boxed_ml_ast *)

(* and boxed_ml_ast = *)
(*   | BMLrel    of int *)
(*   | BMLrelbox of int *)
(*   | BMLapp    of boxed_ml_ast * boxed_ml_ast list *)
(*   | BMLlam    of ml_ident * boxed_ml_ast *)
(*   | BMLletin  of ml_ident * boxed_ml_ast * boxed_ml_ast *)
(*   | BMLglob   of GlobRef.t *)
(*   | BMLcons   of ml_type * GlobRef.t * boxed_ml_ast list *)
(*   | BMLtuple  of boxed_ml_ast list *)
(*   | BMLcase   of ml_type * boxed_ml_ast * boxed_ml_branch array *)
(*   | BMLfix    of int * Id.t array * boxed_ml_ast array *)
(*   | BMLexn    of string *)
(*   | BMLdummy  of kill_reason *)
(*   | BMLaxiom  of string *)
(*   | BMLmagic  of boxed_ml_ast *)
(*   | BMLuint   of Uint63.t *)
(*   | BMLfloat  of Float64.t *)
(*   | BMLstring of Pstring.t *)
(*   | BMLparray of boxed_ml_ast array * boxed_ml_ast *)

(* and boxed_ml_pattern = *)
(*   | Pcons   of GlobRef.t * ml_pattern list *)
(*   | Ptuple  of ml_pattern list *)
(*   | Prel    of int (\** Cf. the idents in the branch. [Prel 1] is the last one. *\) *)
(*   | Pwild *)
(*   | Pusual  of GlobRef.t (\** Shortcut for Pcons (r,[Prel n;...;Prel 1]) **\) *)

(* let rec gather_ids (pat : ml_pattern) : variable list = *)
(*   match pat with *)
(*   | Pcons (gr, pats) -> *)
(*   | Ptuple pats -> *)
(*   | Prel i -> *)
(*   | Pwild -> *)
(*   | Pusual gr -> *)

(* let rec to_boxed (is_boxed : bool list) (ast: ml_ast) : boxed_ml_ast = *)
(*   | MLrel i -> BMLrel LOGIC LOGIC LOGCI *)
(*   | MLapp (ast', asts) -> BMLapp (to_boxed is_boxed ast, List.map (to_boxed is_boxed) asts) *)
(*   | MLlam (id, ast') -> BMLlam(id, to_boxed (false::is_boxed) ast') *)
(*   | MLletin (id, bound_ast, body_ast) -> *)
(*     BMLletin(id, to_boxed is_boxed bound_ast, to_boxed (false::is_boxed) body_ast) *)
(*   | MLglob gr -> BMLglob gr *)
(*   | MLcons (ty, gr, asts) -> *)
(*     BMLcons (ty, gr, List.map (to_boxed is_boxed) asts) *)
(*   | MLtuple asts -> BMLtuple (List.map (to_boxed is_boxed) asts) *)
(*   | MLcase (ty, mat, branches) -> *)
(*     BMLcase (ty, to_boxed is_boxed mat, List.map (to_boxed_branch is_boxed) branches) *)

(* and to_boxed_branch (is_boxed : bool list) (branch : ml_branch) : boxed_ml_branch = *)
(*   let (ids, pat, ast) = branch in (\* not sure what ids are here *\) *)
(*   let box_ast = *)
(*     match pat with *)
(*     | Pcons (gr, pats) -> *)
(*     | Ptuple pats -> *)
(*     | Prel i -> *)
(*     | Pwild -> *)
(*       (\*evil*\) | Pusual gr -> *)




let keywords : Id.Set.t = Id.Set.empty (* TODO: some has to be added *)

let pp_comment s = str "// " ++ s ++ fnl ()

let pp_sig : ml_signature -> Pp.t = function
  | _ -> mt () (* TODO: should be improved *)

let pp_cons c =
  let typ = str (pp_global Type (IndRef (inductive_of_constructor c))) in
  typ ++ str "::" ++ str (Common.pp_global Cons (ConstructRef c))

let pp_global k (r : GlobRef.t) (cons_with_type : bool) =
  match k, r, cons_with_type with
    | Cons, ConstructRef c, true -> pp_cons c
    | _ -> str (Common.pp_global k r)

(*s Pretty-printing of types. [par] is a boolean indicating whether parentheses
    are needed or not. *)

let pp_tvar id =
  let s = Id.to_string (uppercase_id id) in
  str s

let rec pp_type par vl t =
  let rec pp_rec par = function
    | Tmeta _ | Tvar' _ -> assert false
    | Tvar i ->
      (try pp_tvar (List.nth vl (pred i)) with _ -> str "A" ++ int i)
    | Tglob (r,[]) -> pp_global Type r false
    | Tglob (gr,l)
	    when not (keep_singleton ()) && Coqlib.check_ref sig_type_name gr ->
	    pp_type true vl (List.hd l)
    | Tglob (r,l) ->
	      (pp_global Type r false ++ str "<" ++
	       prlist_with_sep (fun () -> str ",") (pp_type true vl) l ++ str ">")
    | Tarr (t1,t2) ->
	    pp_par par
	      (str "&dyn" ++ spc() ++ str "Fn(" ++ pp_rec true t1 ++ str ")" ++ spc () ++ str "->" ++ spc () ++ pp_rec false t2)
    | Tdummy _ -> str "()"
    | Tunknown -> str "()"
    | Taxiom -> str "() -- AXIOM TO BE REALIZED\n"
  in
  hov 0 (pp_rec par t)

let pr_typed_id vl (id, typ) = str (Id.to_string id) ++ str ": " ++ pp_type false vl typ

let pp_box_type par vl t =
  str "Box<" ++ (pp_type par vl t) ++ str ">"

let pp_one_ind ip pl cv =
  let pl : Id.t list = rename_tvars keywords pl in
  let pp_constructor (r,l) =
    (pp_global Cons r false ++
     match l with
       | [] -> (mt ())
       | _  -> (str "(" ++
      	       	prlist_with_sep
		  (fun () -> (str ", ")) (pp_box_type true pl) l)
	        ++ str ")"
               )
  in
  str "enum " ++
  pp_global Type (IndRef ip) false ++ str "<" ++
  (prlist_with_sep  (fun () -> str ", ")
     pp_tvar pl) ++ str ">" ++
  str " {" ++
  fnl () ++ str "  " ++
  v 0 (prvect_with_sep (fun () -> str "," ++ fnl()) pp_constructor
		     (Array.mapi (fun i c -> GlobRef.ConstructRef (ip,i+1),c) cv))
  ++ fnl() ++ str "}" ++ fnl()

let pp_logical_ind packet =
  pp_comment (Id.print packet.ip_typename ++ str " : logical inductive") ++
  pp_comment (str "with constructors : " ++
	      prvect_with_sep spc Id.print packet.ip_consnames)

let pp_singleton kn packet =
  let l = rename_tvars keywords packet.ip_vars in
  let l' = List.rev l in
  hov 2 (str "type " ++ pp_global Type (IndRef (kn,0)) false ++ spc () ++
	 prlist_with_sep spc Id.print l ++
	 (if not (List.is_empty l) then str " " else mt ()) ++ str "=" ++ spc () ++
	 pp_type false l' (List.hd packet.ip_types.(0)) ++ str ";" ++ fnl () ++
	 pp_comment (str "singleton inductive, whose constructor was " ++
		     Id.print packet.ip_consnames.(0)))

let rec pp_ind kn i ind =
  if i >= Array.length ind.ind_packets then
    fnl ()
  else
    let ip = (kn,i) in
    let p = ind.ind_packets.(i) in
    if is_custom (IndRef (kn,i)) then pp_ind kn (i+1) ind
    else
      if p.ip_logical then
	pp_logical_ind p ++ pp_ind kn (i+1) ind
      else
	pp_one_ind ip p.ip_vars p.ip_types ++ fnl () ++
	pp_ind kn (i+1) ind

let pr_binding (lst : (Id.t * ml_type) list) vl : Pp.t =
  match lst with
    | [] -> str "()"
    | l -> pp_par true (prlist_with_sep (fun () -> str ", ") (pr_typed_id vl) l)

let expr_needs_par = function
  | MLlam _  -> true
  | MLcase (_,_,[|_|]) -> false
  | MLcase (_,_,pv) -> true
  | _        -> false

let boxed_of_env env = List.init (List.length (fst env)) (fun _ -> false)

(*s [collect_lams MLlam(id1,...MLlam(idn,t)...)] returns
    [[idn;...;id1]] and the term [t]. *)

let collect_lams =
  let rec collect acc = function
    | (MLlam(id,t), Tarr (a, b)) -> collect ((id, a)::acc) (t,b)
    | (x,y)           -> acc,x,y
  in collect []

let rec rename_vars avoid = function
  | [] ->
      [], avoid
  | (id, t) :: idl when id == dummy_name ->
      (* we don't rename dummy binders *)
      let (idl', avoid') = rename_vars avoid idl in
      ((id, t) :: idl', avoid')
  | (id, t) :: idl ->
      let (idl, avoid) = rename_vars avoid idl in
      let id = rename_id (lowercase_id id) avoid in
      ((id, t) :: idl, Id.Set.add id avoid)

let push_vars ids ((db,avoid) : Common.env) =
  let ids',avoid' = rename_vars avoid ids in
  ids', ((List.map fst ids') @ db, avoid')

let pp_apply st par args = match args with
  | [] -> st
  | _  -> hov 2 (pp_par par (st ++ spc () ++ pp_par true (prlist_with_sep (fun () -> str ",") identity args)))

let pp_tvar_list (vl : Names.Id.t list) (lst : int list) =
  if List.length lst = 0 then mt () else
    str "<" ++
    (prlist_with_sep (fun () -> str ",") (fun i -> pp_type false vl (Tvar i)) lst)
    ++ str ">"

let pp_apply2 st par args =
  let par' = not (List.is_empty args) || par in
  pp_apply (pp_par par' st) par args

let rec pick_tvars (db_env : Names.Id.t list) (typ : ml_type) : int list =
  match typ with
  | Tarr (typ0, typ1) -> pick_tvars db_env typ0 @ pick_tvars db_env typ1
  | Tglob (_, lst) -> List.concat (List.map (pick_tvars db_env) lst)
  | Tvar i -> [i]
  | Tvar' i -> [i]
  | Tmeta _ -> failwith "meta type variable here?"
  | Tdummy _ -> []
  | Tunknown -> []
  | Taxiom -> []

let rec pp_expr par env boxed args =
  let apply st = pp_apply st par args
  and apply2 st = pp_apply2 st par args in
  function
  | MLrel n ->
    let id = get_db_name n env in
    let is_boxed = List.nth boxed (pred n) in
    (if is_boxed then str "*" else mt ()) ++ apply (Id.print id)
  | MLapp (f,args') ->
	  let stl = List.map (pp_expr true env boxed []) args' in
    pp_expr par env boxed (stl @ args) f
  | MLlam _ as _a-> failwith "MLlam not implemented"
  | MLletin (id,a1,a2) ->
    failwith "MLletin not implemented"
  | MLglob r ->
	  apply (pp_global Term r false)
  | MLcons (typ,r,a) as c ->
    assert (List.is_empty args);
    begin match a with
	    | _ when is_native_char c ->
	      let _ = failwith "native_char not implemented" in
	      pp_native_char c
	    | [] ->
	      pp_global Cons r true
	    | [a] ->
	      pp_par par (pp_global Cons r true
		                ++ pp_par true (pp_box_expr env boxed [] a))
	    | _ ->
	      pp_par par (pp_global Cons r true ++
                    pp_par true (prlist_with_sep (fun _ -> str ", ")
                                   (pp_box_expr env boxed []) a))
	  end
  | MLtuple l ->
    failwith "MLtuple not implemented"
  | MLcase (_,t, pv) when is_custom_match pv ->
    failwith "MLcase not implemented"
  | MLcase (typ,t,pv) ->
    apply2
	    (v 0 (str "match " ++ pp_expr false env boxed [] t ++ str " {" ++
		        fnl () ++ pp_pat env pv ++ fnl() ++ str "}"))
  | MLfix (i,ids,defs) ->
    failwith "MLfix not implemented"
  | MLexn s ->
    failwith "MLexn not implemented"
  | MLdummy _ ->
    failwith "MLdummy not implemented"
  | MLmagic a ->
    failwith "MLmagic not implemented"
  | MLaxiom _ -> pp_par par (str "Prelude.error \"AXIOM TO BE REALIZED\"")
  | MLuint _ | MLfloat _ | MLstring _ | MLparray _ ->
    failwith "MLuint/float/string/parray not implemented"

and pp_cons_pat par r ppl =
  (* let ppl = List.map (fun pp -> str "box " ++ pp ) ppl in *)
  pp_par par
    (pp_global Cons r true ++
     if List.is_empty ppl then mt() else
	     pp_par true (prlist_with_sep (fun () -> str ",") identity ppl))

and pp_gen_pat ids env = function
  | Pcons (r, l) -> failwith "pp_gen_pat0" (* pp_cons_pat r (List.map (pp_gen_pat ids env) l) *)
  | Pusual r -> pp_cons_pat false r (List.map Id.print ids)
  | Ptuple l -> failwith "pp_gen_pat2" (* pp_boxed_tuple (pp_gen_pat ids env) l*)
  | Pwild -> str "_"
  | Prel n -> Id.print (get_db_name n env)

and pp_one_pat env boxed (ids,p,t) =
  let ids',env' = Common.push_vars (List.rev_map id_of_mlid ids) env in
  let boxed' = List.init (List.length ids) (fun _ -> true) @ boxed in
  pp_gen_pat (List.rev ids') env' p,
  pp_expr (expr_needs_par t) env' boxed' [] t

and pp_pat env pv =
  prvecti
    (fun i x ->
       let s1,s2 = pp_one_pat env (boxed_of_env env) x in
       str "  " ++ hv 2 (hov 4 (s1 ++ str " =>") ++ spc () ++ hov 2 s2) ++
       if Int.equal i (Array.length pv - 1) then mt () else str "," ++ fnl ())
    pv

and pp_function env boxed f t typ =
  let bl,t',typ = collect_lams (t, typ) (* collect_lambs should work on type as well *) in
  let bl = List.map (fun i -> (id_of_mlid (fst i), snd i)) bl in
  let bl,env' = push_vars bl env in
  let boxed' = List.init (List.length bl) (fun _ -> false) in
  let tvars0 : int list = pick_tvars (fst env') typ in
  let tvars1 : int list = List.concat (List.map (pick_tvars (fst env')) (List.map snd bl)) in
  let tvars = List.sort_uniq compare (tvars0 @ tvars1) in
  match t' with
  | MLcase(Tglob(r,_),MLrel 1,pv) when
	    not (is_coinductive r) && List.is_empty (get_record_fields r) &&
	    not (is_custom_match pv) ->
    pp_tvar_list (fst env') tvars ++
    pr_binding (List.rev bl) (fst env') ++ str " -> " ++ pp_type false (fst env') typ ++
    str " {" ++ fnl () ++ str "  " ++
	  hov 2 (pp_expr false env' boxed' [] t') ++ fnl() ++ str "}"
  | _ ->
    pp_tvar_list (fst env') tvars ++
    (pr_binding (List.rev bl) (fst env')) ++ str " -> " ++ pp_type false (fst env') typ ++
	  str " {" ++ fnl () ++ str "  " ++
	  hov 2 (pp_expr false env' boxed' [] t') ++ fnl() ++ str "}"
and pp_box_expr env boxed args term =
  (str "Box::new") ++ pp_expr true env boxed args term

let pp_decl : ml_decl -> Pp.t = function
  | Dind (kn, ind) when ind.ind_kind = Singleton ->
    pp_singleton kn ind.ind_packets.(0) ++ fnl ()
  | Dind (kn, ind) ->
    hov 0 (pp_ind kn 0 ind)
  | Dtype (_, _, _) -> failwith "Dtype not implemented"
  | Dterm (r, a, t) ->
    if Table.is_inline_custom r then failwith "inline custom term not implemented"
    else
      let e = pp_global Term r in
	    if is_custom r then
	      failwith "custom term printing not implemented"
	    else
	      let name = pp_global Term r false (*XXX: add typevar list *) in
	      hov 0 (str "fn " ++ name ++ pp_function (empty_env ()) [] e a t ++ fnl2 ())
  | Dfix (rv, defs, typs) ->
    let names = Array.map
	      (fun r -> if is_inline_custom r then mt () else pp_global Term r false) rv
    in
    prvecti
	    (fun i r ->
	       let void = is_inline_custom r ||
	                  (not (is_custom r) && match defs.(i) with MLexn "UNUSED" -> true | _ -> false)
	       in
	       if void then mt ()
	       else
	         (if is_custom r then
		          (names.(i) ++ str " = " ++ str (find_custom r))
	          else
		          str "fn " ++ names.(i) ++ (pp_function (empty_env ()) [] names.(i) defs.(i)) (typs.(i)))
	         ++ fnl2 ())
	    rv

let pp_structure_elem = function
  | (l, SEdecl d) -> pp_decl d
  | (l, SEmodule m) -> failwith "SEmodule not implemented"
  | (l, SEmodtype m) -> failwith "SEmodtype not implemented"

let pp_struct (elms : ml_structure) : Pp.t =
  let pp_sel (mp, sel) =
    push_visible mp [];
    let p = prlist_strict pp_structure_elem sel in
    pop_visible (); p
  in
  prlist_strict pp_sel elms

let preamble _ _ _ _ =
  pp_comment (str "main here to allow easy compilation but we probably want to compile as a lib") ++
  str "fn main() {}" ++ fnl () ++ fnl ()

let rust_descr = {
  keywords = keywords;
  file_suffix = ".rs";
  file_naming = file_of_modfile;
  preamble = preamble ;
  pp_struct = pp_struct;
  sig_suffix = None;
  sig_preamble = (fun _ _ _ _ -> Pp.mt ());
  pp_sig = pp_sig;
  pp_decl = pp_decl
}
