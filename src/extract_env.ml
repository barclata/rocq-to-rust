(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Miniml
open Constr
open Declarations
open Names
open ModPath
open Libnames
open Pp
open CErrors
open Util
open Table
open Extraction
open Modutil
open Common

(***************************************)
(*S Part I: computing Coq environment. *)
(***************************************)

let toplevel_env () =
  let mp, struc = Safe_typing.flatten_env (Global.safe_env ()) in
  mp, List.rev struc

let environment_until dir_opt =
  let rec parse = function
    | [] when Option.is_empty dir_opt -> [toplevel_env ()]
    | [] -> []
    | d :: l ->
      let meb =
        Modops.destr_nofunctor (MPfile d) (Global.lookup_module (MPfile d)).mod_type
      in
      match dir_opt with
      | Some d' when DirPath.equal d d' -> [MPfile d, meb]
      | _ -> (MPfile d, meb) :: (parse l)
  in parse (Library.loaded_libraries ())


(*s Visit:
  a structure recording the needed dependencies for the current cakeml_extraction *)

module type VISIT = sig
  (* Reset the dependencies by emptying the visit lists *)
  val reset : unit -> unit

  (* Add the module_path and all its prefixes to the mp visit list.
     We'll keep all fields of these modules. *)
  val add_mp_all : ModPath.t -> unit

  (* Add reference / ... in the visit lists.
     These functions silently add the mp of their arg in the mp list *)
  val add_ref : GlobRef.t -> unit
  val add_kn : KerName.t -> unit
  val add_decl_deps : ml_decl -> unit
  val add_spec_deps : ml_spec -> unit

  (* Test functions:
     is a particular object a needed dependency for the current cakeml_extraction ? *)
  val needed_ind : MutInd.t -> bool
  val needed_cst : Constant.t -> bool
  val needed_mp : ModPath.t -> bool
  val needed_mp_all : ModPath.t -> bool
end

module Visit : VISIT = struct
  type must_visit =
      { mutable kn : KNset.t;
        mutable mp : MPset.t;
        mutable mp_all : MPset.t }
  (* the imperative internal visit lists *)
  let v = { kn = KNset.empty; mp = MPset.empty; mp_all = MPset.empty }
  (* the accessor functions *)
  let reset () =
    v.kn <- KNset.empty;
    v.mp <- MPset.empty;
    v.mp_all <- MPset.empty
  let needed_ind i = KNset.mem (MutInd.user i) v.kn
  let needed_cst c = KNset.mem (Constant.user c) v.kn
  let needed_mp mp = MPset.mem mp v.mp || MPset.mem mp v.mp_all
  let needed_mp_all mp = MPset.mem mp v.mp_all
  let add_mp mp =
    check_loaded_modfile mp; v.mp <- MPset.union (prefixes_mp mp) v.mp
  let add_mp_all mp =
    check_loaded_modfile mp;
    v.mp <- MPset.union (prefixes_mp mp) v.mp;
    v.mp_all <- MPset.add mp v.mp_all
  let add_kn kn = v.kn <- KNset.add kn v.kn; add_mp (KerName.modpath kn)
  let add_ref = let open GlobRef in function
    | ConstRef c -> add_kn (Constant.user c)
    | IndRef (ind,_) | ConstructRef ((ind,_),_) -> add_kn (MutInd.user ind)
    | VarRef _ -> assert false
  let add_decl_deps = decl_iter_references add_ref add_ref add_ref
  let add_spec_deps = spec_iter_references add_ref add_ref add_ref
end

let add_field_label mp = function
  | (lab, (SFBconst _|SFBmind _ | SFBrules _)) -> Visit.add_kn (KerName.make mp lab)
  | (lab, (SFBmodule _|SFBmodtype _)) -> Visit.add_mp_all (MPdot (mp,lab))

let rec add_labels mp = function
  | MoreFunctor (_,_,m) -> add_labels mp m
  | NoFunctor sign -> List.iter (add_field_label mp) sign

exception Impossible

let check_arity env cb =
  let t = cb.const_type in
  if Reduction.is_arity env t then raise Impossible

let get_body lbody =
  EConstr.of_constr lbody

let check_fix env sg cb i =
  match cb.const_body with
    | Def lbody ->
        (match EConstr.kind sg (get_body lbody) with
          | Fix ((_,j),recd) when Int.equal i j -> check_arity env cb; (true,recd)
          | CoFix (j,recd) when Int.equal i j -> check_arity env cb; (false,recd)
          | _ -> raise Impossible)
    | Undef _ | OpaqueDef _ | Primitive _ | Symbol _ -> raise Impossible

let prec_declaration_equal sg (na1, ca1, ta1) (na2, ca2, ta2) =
  Array.equal (Context.eq_annot Name.equal (EConstr.ERelevance.equal sg)) na1 na2 &&
  Array.equal (EConstr.eq_constr sg) ca1 ca2 &&
  Array.equal (EConstr.eq_constr sg) ta1 ta2

let factor_fix env sg l cb msb =
  let _,recd as check = check_fix env sg cb 0 in
  let n = Array.length (let fi,_,_ = recd in fi) in
  if Int.equal n 1 then [|l|], recd, msb
  else begin
    if List.length msb < n-1 then raise Impossible;
    let msb', msb'' = List.chop (n-1) msb in
    let labels = Array.make n l in
    List.iteri
      (fun j ->
         function
           | (l,SFBconst cb') ->
              let check' = check_fix env sg cb' (j+1) in
              if not ((fst check : bool) == (fst check') &&
                        prec_declaration_equal sg (snd check) (snd check'))
               then raise Impossible;
               labels.(j+1) <- l;
           | _ -> raise Impossible) msb';
    labels, recd, msb''
  end

(** Expanding a [module_alg_expr] into a version without abbreviations
    or functor applications. This is done via a detour to entries
    (hack proposed by Elie)
*)

let vm_state =
  (* VM bytecode is not needed here *)
  let vm_handler _ _ _ () = (), None in
  ((), { Mod_typing.vm_handler })

let expand_mexpr env mp me =
  let inl = Some (Flags.get_inline_level()) in
  let state = ((Environ.universes env, Univ.Constraints.empty), Reductionops.inferred_universes) in
  let mb, (_, cst), _ = Mod_typing.translate_module state vm_state env mp inl (MExpr ([], me, None)) in
  mb.mod_type, mb.mod_delta

let expand_modtype env mp me =
  let inl = Some (Flags.get_inline_level()) in
  let state = ((Environ.universes env, Univ.Constraints.empty), Reductionops.inferred_universes) in
  let mtb, _cst, _ = Mod_typing.translate_modtype state vm_state env mp inl ([],me) in
  mtb

let no_delta = Mod_subst.empty_delta_resolver

let flatten_modtype env mp me_alg struc_opt =
  match struc_opt with
  | Some me -> me, no_delta
  | None ->
     let mtb = expand_modtype env mp me_alg in
     mtb.mod_type, mtb.mod_delta

(** Ad-hoc update of environment, inspired by [Mod_typing.check_with_aux_def].
*)

let env_for_mtb_with_def env mp me reso idl =
  let struc = Modops.destr_nofunctor mp me in
  let l = Label.of_id (List.hd idl) in
  let spot = function (l',SFBconst _) -> Label.equal l l' | _ -> false in
  let before = fst (List.split_when spot struc) in
  Modops.add_structure mp before reso env

let make_cst resolver mp l =
  Mod_subst.constant_of_delta_kn resolver (KerName.make mp l)

let make_mind resolver mp l =
  Mod_subst.mind_of_delta_kn resolver (KerName.make mp l)

(* From a [structure_body] (i.e. a list of [structure_field_body])
   to specifications. *)

let rec extract_structure_spec env mp reso = function
  | [] -> []
  | (l,SFBconst cb) :: msig ->
      let c = make_cst reso mp l in
      let s = extract_constant_spec env c cb in
      let specs = extract_structure_spec env mp reso msig in
      if logical_spec s then specs
      else begin Visit.add_spec_deps s; (l,Spec s) :: specs end
  | (l,SFBmind _) :: msig ->
      let mind = make_mind reso mp l in
      let s = Sind (mind, extract_inductive env mind) in
      let specs = extract_structure_spec env mp reso msig in
      if logical_spec s then specs
      else begin Visit.add_spec_deps s; (l,Spec s) :: specs end
  | (l, SFBrules _) :: msig ->
      let specs = extract_structure_spec env mp reso msig in
      specs
  | (l,SFBmodule mb) :: msig ->
      let specs = extract_structure_spec env mp reso msig in
      let spec = extract_mbody_spec env mb.mod_mp mb in
      (l,Smodule spec) :: specs
  | (l,SFBmodtype mtb) :: msig ->
      let specs = extract_structure_spec env mp reso msig in
      let spec = extract_mbody_spec env mtb.mod_mp mtb in
      (l,Smodtype spec) :: specs

(* From [module_expression] to specifications *)

(* Invariant: the [me_alg] given to [extract_mexpr_spec] and
   [extract_mexpression_spec] should come from a [mod_type_alg] field.
   This way, any encountered [MEident] should be a true module type. *)

and extract_mexpr_spec env mp1 (me_struct_o,me_alg) = match me_alg with
  | MEident mp -> Visit.add_mp_all mp; MTident mp
  | MEwith(me',WithDef(idl,(c,ctx)))->
      let me_struct,delta = flatten_modtype env mp1 me' me_struct_o in
      let env' = env_for_mtb_with_def env mp1 me_struct delta idl in
      let mt = extract_mexpr_spec env mp1 (None,me') in
      let sg = Evd.from_env env in
      (match extract_with_type env' sg (EConstr.of_constr c) with
       (* cb may contain some kn *)
         | None -> mt
         | Some (vl,typ) ->
            type_iter_references Visit.add_ref typ;
            MTwith(mt,ML_With_type(idl,vl,typ)))
  | MEwith(me',WithMod(idl,mp))->
      Visit.add_mp_all mp;
      MTwith(extract_mexpr_spec env mp1 (None,me'), ML_With_module(idl,mp))
  | MEapply _ ->
     (* No higher-order module type in OCaml : we use the expanded version *)
     let me_struct,delta = flatten_modtype env mp1 me_alg me_struct_o in
     extract_msignature_spec env mp1 delta me_struct

and extract_mexpression_spec env mp1 (me_struct,me_alg) = match me_alg with
  | MEMoreFunctor me_alg' ->
      let mbid, mtb, me_struct' = match me_struct with
      | MoreFunctor (mbid, mtb, me') -> (mbid, mtb, me')
      | _ -> assert false
      in
      let mp = MPbound mbid in
      let env' = Modops.add_module_type mp mtb env in
      MTfunsig (mbid, extract_mbody_spec env mp mtb,
                extract_mexpression_spec env' mp1 (me_struct',me_alg'))
  | MENoFunctor m -> extract_mexpr_spec env mp1 (Some me_struct,m)

and extract_msignature_spec env mp1 reso = function
  | NoFunctor struc ->
      let env' = Modops.add_structure mp1 struc reso env in
      MTsig (mp1, extract_structure_spec env' mp1 reso struc)
  | MoreFunctor (mbid, mtb, me) ->
      let mp = MPbound mbid in
      let env' = Modops.add_module_type mp mtb env in
      MTfunsig (mbid, extract_mbody_spec env mp mtb,
                extract_msignature_spec env' mp1 reso me)

and extract_mbody_spec : 'a. _ -> _ -> 'a generic_module_body -> _ =
  fun env mp mb -> match mb.mod_type_alg with
  | Some ty -> extract_mexpression_spec env mp (mb.mod_type,ty)
  | None -> extract_msignature_spec env mp mb.mod_delta mb.mod_type

(* From a [structure_body] (i.e. a list of [structure_field_body])
   to implementations.

   NB: when [all=false], the evaluation order of the list is
   important: last to first ensures correct dependencies.
*)

let rec extract_structure access env mp reso ~all = function
  | [] -> []
  | (l,SFBconst cb) :: struc ->
      (try
         let sg = Evd.from_env env in
         let vl,recd,struc = factor_fix env sg l cb struc in
         let vc = Array.map (make_cst reso mp) vl in
         let ms = extract_structure access env mp reso ~all struc in
         let b = Array.exists Visit.needed_cst vc in
         if all || b then
           let d = extract_fixpoint env sg vc recd in
           if (not b) && (logical_decl d) then ms
           else begin Visit.add_decl_deps d; (l,SEdecl d) :: ms end
         else ms
       with Impossible ->
         let ms = extract_structure access env mp reso ~all struc in
         let c = make_cst reso mp l in
         let b = Visit.needed_cst c in
         if all || b then
           let d = extract_constant access env c cb in
           if (not b) && (logical_decl d) then ms
           else begin Visit.add_decl_deps d; (l,SEdecl d) :: ms end
         else ms)
  | (l,SFBmind mib) :: struc ->
      let ms = extract_structure access env mp reso ~all struc in
      let mind = make_mind reso mp l in
      let b = Visit.needed_ind mind in
      if all || b then
        let d = Dind (mind, extract_inductive env mind) in
        if (not b) && (logical_decl d) then ms
        else begin Visit.add_decl_deps d; (l,SEdecl d) :: ms end
      else ms
  | (l, SFBrules rrb) :: struc ->
      let b = List.exists (fun (cst, _) -> Visit.needed_cst cst) rrb.rewrules_rules in
      let ms = extract_structure access env mp reso ~all struc in
      if all || b then begin
        List.iter (fun (cst, _) -> Table.add_symbol_rule (ConstRef cst) l) rrb.rewrules_rules;
        ms
      end else ms
  | (l,SFBmodule mb) :: struc ->
      let ms = extract_structure access env mp reso ~all struc in
      let mp = MPdot (mp,l) in
      let all' = all || Visit.needed_mp_all mp in
      if all' || Visit.needed_mp mp then
        (l,SEmodule (extract_module access env mp ~all:all' mb)) :: ms
      else ms
  | (l,SFBmodtype mtb) :: struc ->
      let ms = extract_structure access env mp reso ~all struc in
      let mp = MPdot (mp,l) in
      if all || Visit.needed_mp mp then
        (l,SEmodtype (extract_mbody_spec env mp mtb)) :: ms
      else ms

(* From [module_expr] and [module_expression] to implementations *)

and extract_mexpr access env mp = function
  | MEwith _ -> assert false (* no 'with' syntax for modules *)
  | me ->
      (* In Haskell/Scheme, we expand everything.
         For now, we also extract everything, dead code will be removed later
         (see [Modutil.optimize_struct]. *)
      let sign, delta = expand_mexpr env mp me in
      extract_msignature access env mp delta ~all:true sign

and extract_mexpression access env mp mty = function
  | MENoFunctor me -> extract_mexpr access env mp me
  | MEMoreFunctor me ->
      let (mbid, mtb, mty) = match mty with
      | MoreFunctor (mbid, mtb, mty) -> (mbid, mtb, mty)
      | NoFunctor _ -> assert false
      in
      let mp1 = MPbound mbid in
      let env' = Modops.add_module_type mp1 mtb	env in
      Miniml.MEfunctor
        (mbid,
         extract_mbody_spec env mp1 mtb,
         extract_mexpression access env' mp mty me)

and extract_msignature access env mp reso ~all = function
  | NoFunctor struc ->
      let env' = Modops.add_structure mp struc reso env in
      Miniml.MEstruct (mp,extract_structure access env' mp reso ~all struc)
  | MoreFunctor (mbid, mtb, me) ->
      let mp1 = MPbound mbid in
      let env' = Modops.add_module_type mp1 mtb	env in
      Miniml.MEfunctor
        (mbid,
         extract_mbody_spec env mp1 mtb,
         extract_msignature access env' mp reso ~all me)

and extract_module access env mp ~all mb =
  (* A module has an empty [mod_expr] when :
     - it is a module variable (for instance X inside a Module F [X:SIG])
     - it is a module assumption (Declare Module).
     Since we look at modules from outside, we shouldn't have variables.
     But a Declare Module at toplevel seems legal (cf #2525). For the
     moment we don't support this situation. *)
  let impl = match mb.mod_expr with
    | Abstract -> error_no_module_expr mp
    | Algebraic me -> extract_mexpression access env mp mb.mod_type me
    | Struct sign ->
      (* This module has a signature, otherwise it would be FullStruct.
         We extract just the elements required by this signature. *)
      let () = add_labels mp mb.mod_type in
      let sign = Modops.annotate_struct_body sign mb.mod_type in
      extract_msignature access env mp mb.mod_delta ~all:false sign
    | FullStruct -> extract_msignature access env mp mb.mod_delta ~all mb.mod_type
  in
  (* Slight optimization: for modules without explicit signatures
     ([FullStruct] case), we build the type out of the extracted
     implementation *)
  let typ = match mb.mod_expr with
    | FullStruct ->
      assert (Option.is_empty mb.mod_type_alg);
      mtyp_of_mexpr impl
    | _ -> extract_mbody_spec env mp mb
  in
  { ml_mod_expr = impl;
    ml_mod_type = typ }

let mono_environment ~opaque_access refs mpl =
  Visit.reset ();
  List.iter Visit.add_ref refs;
  List.iter Visit.add_mp_all mpl;
  let env = Global.env () in
  let l = List.rev (environment_until None) in
  List.rev_map
    (fun (mp,struc) ->
      mp, extract_structure opaque_access env mp no_delta ~all:(Visit.needed_mp_all mp) struc)
    l

(**************************************)
(*S Part II : Input/Output primitives *)
(**************************************)

(* From a filename string "foo.ml" or "foo", builds "foo.ml" and "foo.mli"
   Works similarly for the other languages. *)

let default_id = Id.of_string "Main"

let mono_filename f =
  let d = Rust.rust_descr in
  match f with
    | None -> None, None, default_id
    | Some f ->
        let f =
          if Filename.check_suffix f d.file_suffix then
            Filename.chop_suffix f d.file_suffix
          else f
        in
        let id = default_id in
        let f =
          if Filename.is_relative f then
            Filename.concat (output_directory ()) f
          else f
        in
        Some (f^d.file_suffix), Option.map ((^) f) d.sig_suffix, id

(* Builds a suitable filename from a module id *)

let module_filename mp =
  let f = file_of_modfile mp in
  let id = Id.of_string f in
  let f = Filename.concat (output_directory ()) f in
  let d = Rust.rust_descr in
  let fimpl_base = d.file_naming mp ^ d.file_suffix in
  let fimpl = Filename.concat (output_directory ()) fimpl_base in
  Some fimpl, Option.map ((^) f) d.sig_suffix, id

(*s Extraction of one decl to stdout. *)

let print_one_decl struc mp decl =
  let d = Rust.rust_descr in
  reset_renaming_tables AllButExternal;
  set_phase Pre;
  ignore (d.pp_struct struc);
  set_phase Impl;
  push_visible mp [];
  let ans = d.pp_decl decl in
  pop_visible ();
  v 0 ans

(*s Extraction of a ml struct to a file. *)

(** For Recursive Extraction, writing directly on stdout
    won't work with coqide, we use a buffer instead *)

let buf = Buffer.create 1000

let formatter dry file =
  let ft =
    if dry then Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
    else
      match file with
        | Some f -> Topfmt.with_output_to f
        | None -> Format.formatter_of_buffer buf
  in
  (* XXX: Fixme, this shouldn't depend on Topfmt *)
  (* We never want to see ellipsis ... in extracted code *)
  Format.pp_set_max_boxes ft max_int;
  (* We reuse the width information given via "Set Printing Width" *)
  (match Topfmt.get_margin () with
    | None -> ()
    | Some i ->
      Format.pp_set_margin ft i;
      Format.pp_set_max_indent ft (i-10));
      (* note: max_indent should be < margin above, otherwise it's ignored *)
  ft

let get_comment () =
  let s = file_comment () in
  if String.is_empty s then None
  else
    let split_comment = Str.split (Str.regexp "[ \t\n]+") s in
    Some (prlist_with_sep spc str split_comment)

let print_structure_to_file (fn,si,mo) dry struc =
  Buffer.clear buf;
  let d = Rust.rust_descr in
  reset_renaming_tables AllButExternal;
  let unsafe_needs = {
    mldummy = struct_ast_search Mlutil.isMLdummy struc;
    tdummy = struct_type_search Mlutil.isTdummy struc;
    tunknown = struct_type_search ((==) Tunknown) struc;
    magic = false }
  in
  (* First, a dry run, for computing objects to rename or duplicate *)
  set_phase Pre;
  ignore (d.pp_struct struc);
  let opened = opened_libraries () in
  (* Print the implementation *)
  let cout = if dry then None else Option.map open_out fn in
  let ft = formatter dry cout in
  let comment = get_comment () in
  begin try
    (* The real printing of the implementation *)
    set_phase Impl;
    pp_with ft (d.preamble mo comment opened unsafe_needs);
    pp_with ft (d.pp_struct struc);
    Format.pp_print_flush ft ();
    Option.iter close_out cout;
  with reraise ->
    Format.pp_print_flush ft ();
    Option.iter close_out cout; raise reraise
  end;
  if not dry then Option.iter info_file fn;
  (* Now, let's print the signature *)
  Option.iter
    (fun si ->
       let cout = open_out si in
       let ft = formatter false (Some cout) in
       begin try
         set_phase Intf;
         pp_with ft (d.sig_preamble mo comment opened unsafe_needs);
         pp_with ft (d.pp_sig (signature_of_structure struc));
         Format.pp_print_flush ft ();
         close_out cout;
       with reraise ->
         Format.pp_print_flush ft ();
         close_out cout; raise reraise
       end;
       info_file si)
    (if dry then None else si);
  (* Print the buffer content via Coq standard formatter (ok with coqide). *)
  if not (Int.equal (Buffer.length buf) 0) then begin
    Feedback.msg_notice (str (Buffer.contents buf));
    Buffer.reset buf
  end


(*********************************************)
(*s Part III: the actual cakeml_extraction commands *)
(*********************************************)


let reset () =
  Visit.reset (); reset_tables (); reset_renaming_tables Everything

let init ?(compute=false) ?(inner=false) modular library =
  if not inner then check_inside_section ();
  set_keywords (Rust.rust_descr).keywords;
  set_modular modular;
  set_library library;
  set_extrcompute compute;
  reset ()

let warns () =
  warning_opaques (access_opaque ());
  warning_axioms ()

(* From a list of [reference], let's retrieve whether they correspond
   to modules or [global_reference]. Warn the user if both is possible. *)

let rec locate_ref = function
  | [] -> [],[]
  | qid::l ->
      let mpo = try Some (Nametab.locate_module qid) with Not_found -> None
      and ro =
        try Some (Smartlocate.global_with_alias qid)
        with Nametab.GlobalizationError _ | UserError _ -> None
      in
      match mpo, ro with
        | None, None -> Nametab.error_global_not_found ~info:Exninfo.null qid
        | None, Some r -> let refs,mps = locate_ref l in r::refs,mps
        | Some mp, None -> let refs,mps = locate_ref l in refs,mp::mps
        | Some mp, Some r ->
           warning_ambiguous_name (qid,mp,r);
           let refs,mps = locate_ref l in refs,mp::mps

(*s Recursive cakeml_extraction in the Coq toplevel. The vernacular command is
    \verb!Recursive Extraction! [qualid1] ... [qualidn]. Also used when
    extracting to a file with the command:
    \verb!Extraction "file"! [qualid1] ... [qualidn]. *)

let full_extr opaque_access f (refs,mps) =
  init false false;
  List.iter (fun mp -> if is_modfile mp then error_MPfile_as_mod mp true) mps;
  let struc = optimize_struct (refs,mps) (mono_environment ~opaque_access refs mps) in
  warns ();
  print_structure_to_file (mono_filename f) false struc;
  reset ()

let full_cakeml_extraction ~opaque_access f lr =
  full_extr opaque_access f (locate_ref lr)

(*s Separate cakeml_extraction is similar to recursive cakeml_extraction, with the output
   decomposed in many files, one per Coq .v file *)

let separate_cakeml_extraction ~opaque_access lr =
  init true false;
  let refs,mps = locate_ref lr in
  let struc = optimize_struct (refs,mps) (mono_environment ~opaque_access refs mps) in
  let () = List.iter (function
    | MPfile _, _ -> ()
    | (MPdot _ | MPbound _), _ ->
      user_err (str "Separate Extraction from inside a module is not supported."))
      struc
  in
  warns ();
  let print = function
    | (MPfile dir as mp, sel) as e ->
        print_structure_to_file (module_filename mp) false [e]
    | (MPdot _ | MPbound _), _ -> assert false
  in
  List.iter print struc;
  reset ()

(*s Simple cakeml_extraction in the Coq toplevel. The vernacular command
    is \verb!Extraction! [qualid]. *)

let simple_cakeml_extraction ~opaque_access r =
  match locate_ref [r] with
  | ([], [mp]) as p -> full_extr opaque_access None p
  | [r],[] ->
      init false false;
      let struc = optimize_struct ([r],[]) (mono_environment ~opaque_access [r] []) in
      let d = get_decl_in_structure r struc in
      warns ();
      let flag =
        if is_custom r then str "(** User defined cakeml_extraction *)" ++ fnl()
        else mt ()
      in
      let ans = flag ++ print_one_decl struc (modpath_of_r r) d in
      reset ();
      Feedback.msg_notice ans
  | _ -> assert false


(*s (Recursive) Extraction of a library. The vernacular command is
  \verb!(Recursive) Extraction Library! [M]. *)

let cakeml_extraction_library ~opaque_access is_rec CAst.{loc;v=m} =
  init true true;
  let dir_m =
    let q = qualid_of_ident m in
    try Nametab.full_name_module q with Not_found -> error_unknown_module ?loc q
  in
  Visit.add_mp_all (MPfile dir_m);
  let env = Global.env () in
  let l = List.rev (environment_until (Some dir_m)) in
  let select l (mp,struc) =
    if Visit.needed_mp mp
    then (mp, extract_structure opaque_access env mp no_delta ~all:true struc) :: l
    else l
  in
  let struc = List.fold_left select [] l in
  let struc = optimize_struct ([],[]) struc in
  warns ();
  let print = function
    | (MPfile dir as mp, sel) as e ->
        let dry = not is_rec && not (DirPath.equal dir dir_m) in
        print_structure_to_file (module_filename mp) dry [e]
    | _ -> assert false
  in
  List.iter print struc;
  reset ()

(** For cakeml_extraction compute, we flatten all the module structure,
    getting rid of module types or unapplied functors *)

let flatten_structure struc =
  let rec flatten_elem (lab,elem) = match elem with
    |SEdecl d -> [d]
    |SEmodtype _ -> []
    |SEmodule m -> match m.ml_mod_expr with
      |MEfunctor _ -> []
      |MEident _ | MEapply _ -> assert false (* should be expanded *)
      |MEstruct (_,elems) -> flatten_elems elems
  and flatten_elems l = List.flatten (List.map flatten_elem l)
  in flatten_elems (List.flatten (List.map snd struc))

let structure_for_compute ~opaque_access env sg c =
  init false false ~compute:true;
  let ast, mlt = Extraction.extract_constr env sg c in
  let ast = Mlutil.normalize ast in
  let refs = ref GlobRef.Set.empty in
  let add_ref r = refs := GlobRef.Set.add r !refs in
  let () = ast_iter_references add_ref add_ref add_ref ast in
  let refs = GlobRef.Set.elements !refs in
  let struc = optimize_struct (refs,[]) (mono_environment ~opaque_access refs []) in
  (flatten_structure struc), ast, mlt

(* For the test-suite :
   cakeml_extraction to a temporary file + run ocamlc on it *)

(*

let compile f =
  try
    let args = [ "ocamlc"
               ; "-package";"zarith"
               ; "-I"; Filename.dirname f
               ; "-c"; f^"i"
               ; f
               ] in
    let res = CUnix.sys_command (Envars.ocamlfind ()) args in
    match res with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n | Unix.WSIGNALED n | Unix.WSTOPPED n ->
       CErrors.user_err
         Pp.(str "Compilation of file " ++ str f ++
             str " failed with exit code " ++ int n)
  with Unix.Unix_error (e,_,_) ->
    CErrors.user_err
      Pp.(str "Compilation of file " ++ str f ++
          str " failed with error " ++ str (Unix.error_message e))

let remove f = if Sys.file_exists f then Sys.remove f

let extract_and_compile ~opaque_access l =
  CErrors.user_err (Pp.str "This command only works with OCaml cakeml_extraction");
  let f = Filename.temp_file "testcakeml_extraction" ".ml" in
  let () = full_cakeml_extraction ~opaque_access (Some f) l in
  let () = compile f in
  let () = remove f; remove (f^"i") in
  let base = Filename.chop_suffix f ".ml" in
  let () = remove (base^".cmo"); remove (base^".cmi") in
  Feedback.msg_notice (str "Extracted code successfully compiled")
*)

(* Show the cakeml_extraction of the current ongoing proof *)
let show_cakeml_extraction ~pstate =
  init ~inner:true false false;
  let prf = Declare.Proof.get pstate in
  let sigma, env = Declare.Proof.get_current_context pstate in
  let trms = Proof.partial_proof prf in
  let extr_term t =
    let ast, ty = extract_constr env sigma t in
    let mp = Lib.current_mp () in
    let l = Label.of_id (Declare.Proof.get_name pstate) in
    let fake_ref = GlobRef.ConstRef (Constant.make2 mp l) in
    let decl = Dterm (fake_ref, ast, ty) in
    print_one_decl [] mp decl
  in
  Feedback.msg_notice (Pp.prlist_with_sep Pp.fnl extr_term trms)
