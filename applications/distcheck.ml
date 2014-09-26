(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2012 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

open ExtLib
open Common
open Algo

module Options = struct
  open OptParse
  open OptParser
  let description = "Compute the list broken packages in a repository"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  include Boilerplate.DistcheckOptions
  Boilerplate.DistcheckOptions.add_options options ;;

  include Boilerplate.InputOptions
  Boilerplate.InputOptions.add_options options ;;

  include Boilerplate.DistribOptions;;
  let default = List.remove Boilerplate.DistribOptions.default_options "deb-host-arch" in
  Boilerplate.DistribOptions.add_options ~default options ;;

  let coinst = Boilerplate.vpkglist_option ();;
  add options ~long_name:"coinst" ~help:"Check if these packages are coinstallable" coinst;;

  let realversionfield = StdOpt.str_option ();;
  add options ~long_name:"real-version-field" ~help:"Specify field
  where the original version of a package is stored in the CUDF file"
  realversionfield;;

  let brute_opt = StdOpt.store_true ();;
  add options ~long_name:"brute" brute_opt;;

  let add_missings_opt = StdOpt.store_true ();;
  add options ~long_name:"add_miss" add_missings_opt;;

  let testing_opt = StdOpt.store_true ();;
  add options ~long_name:"testing" testing_opt;;

end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver" 

open Diagnostic

(* implicit prefix of resources derived from name of executable *)
(* (input_format * add_format ?) *)
let guess_format t l =
  match Filename.basename(Sys.argv.(0)) with
  |"debcheck"|"dose-debcheck" -> (`Deb, true)
  |"eclipsecheck"|"dose-eclipsecheck" -> (`Eclipse, true)
  |"rpmcheck"|"dose-rpmcheck" -> (`Synthesis,true)
  |_ when OptParse.Opt.is_set t -> 
      (Url.scheme_of_string (OptParse.Opt.get t),true)
  |_ -> (Input.guess_format [l], false)
;;

let string_of_op = function
  | `Eq -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Leq -> "<="
  | `Lt -> "<"

let string_of_cstr = function
  | None -> "None"
  | Some (r,i) -> Printf.sprintf "%s %d" (string_of_op r) i

let string_of_list str_of l =
  let size = List.length l in
  let b = Buffer.create (size * 8) in
  let rec aux buff = function
    | [el] -> 
      Buffer.add_string buff (str_of el);
      Buffer.add_string buff "]";
      Buffer.contents buff
    | hd::tl -> 
      Buffer.add_string buff (str_of hd);
      Buffer.add_string buff "; ";
      aux buff tl
    | [] -> ""
  in 
  Buffer.add_string b "[";
  aux b l

let string_of_vpkg = function
  | (n, None) -> n
  | (n, Some (op,v)) -> let sop = (string_of_op op) in
			Printf.sprintf "%s (%s %d)" n sop v

let string_of_pkg pkg =
  let n = Cudf.lookup_package_property pkg "package" in
  let v = Cudf.lookup_package_property pkg "version" in
  Printf.sprintf "(%s, %s)" n v

(** Reason printing functions *)

let brute_print_conflict c = 
  match c with
  | (p1,p2,vpkg) -> 
    let p1str = string_of_pkg p1 in
    let p2str = string_of_pkg p2 in
    let vpkgstr = string_of_vpkg vpkg in
    Printf.printf "Conflict : (%s, %s : %s)\n\n" p1str p2str vpkgstr

let brute_print_missing m =
  match m with
  | (pkg, vpkgl) -> let pkgstr = string_of_pkg pkg in
		    let vpkglstr = string_of_list (string_of_vpkg) vpkgl in
		    Printf.printf "Missing : (%s, %s)  \n\n" pkgstr vpkglstr

let brute_print_dependency d =
  match d with
  | (pkg, vpkgl, pkgl) -> let pkgstr = string_of_pkg pkg in
			  let vpkglstr = string_of_list (string_of_vpkg) vpkgl in
			  let pkglstr = string_of_list (string_of_pkg) pkgl in
			  Printf.printf "Dep : (%s, %s, %s)\n\n" pkgstr vpkglstr pkglstr

let brute_print_r = function
  | Diagnostic.Missing m -> brute_print_missing m
  | Diagnostic.Conflict c -> brute_print_conflict c
  | Diagnostic.Dependency d -> brute_print_dependency d
 
let rec brute_print_reasons = function
  | r::rl -> brute_print_r r;
    brute_print_reasons rl
  | [] -> ()

let brute_print add_miss d = 
  match d with 
  |{result = Failure f; request = req} ->
    let diag = f () in
    if add_miss then brute_print_reasons (List.unique (Diagnostic.add_missings diag))
    else brute_print_reasons (List.unique diag)
  | _ -> ()

(** ------------------------------------- *)

(** reducedReason printing functions *)

let brute_print_rconflict c = 
  match c with
  | (n1,n2,vpkg) -> 
    let n1str = string_of_list (string_of_pkg) n1 in
    let n2str = string_of_list (string_of_pkg) n2 in
    let vpkgstr = string_of_vpkg vpkg in
    Printf.printf "Conflict : (%s, %s : %s)\n\n" n1str n2str vpkgstr

let brute_print_rmissing m =
  match m with
  | (node, dep, vpkgl) -> let nodestr = string_of_list (string_of_pkg) node in
		    let vpkglstr = string_of_list (string_of_vpkg) vpkgl in
		    let depstr = string_of_list (string_of_vpkg) dep in
		    Printf.printf "Missing : (%s, %s, %s)  \n\n" nodestr depstr vpkglstr

let brute_print_rdependency d =
  match d with
  | (node, vpkgl, nodel) -> let nodestr = string_of_list (string_of_pkg) node in
			  let vpkglstr = string_of_list (string_of_vpkg) vpkgl in
			  let nodelstr = string_of_list (string_of_list (string_of_pkg)) nodel in
			  Printf.printf "Dep : (%s, %s, %s)\n\n" nodestr vpkglstr nodelstr

let brute_print_rr = function
  | Diagnostic.RMissing m -> brute_print_rmissing m
  | Diagnostic.RConflict c -> brute_print_rconflict c
  | Diagnostic.RDependency d -> brute_print_rdependency d
 
let rec brute_print_rreasons = List.iter brute_print_rr

let brute_print add_miss d = 
  match d with 
  |{result = Failure f; request = req} ->
    let diag = f () in
    if add_miss then brute_print_reasons (List.unique (Diagnostic.add_missings diag))
    else brute_print_reasons (List.unique diag)
  | _ -> ()


let brute_print_deps node rrl =
  let pkg = List.hd node in
  let (deps,confs) = Diagnostic.cnfdeps_and_conflicts node rrl in
  let depstr = string_of_list (string_of_list (string_of_vpkg)) deps in
  let confstr = string_of_list (string_of_pkg) confs in
  Printf.printf "Deps(%s) : %s \n" (string_of_pkg pkg) depstr;
  Printf.printf "Confs(%s) : %s \n" (string_of_pkg pkg) confstr

let brute_print_cone_rules pkg rrl =
  let cr = Diagnostic.cone_rules [pkg] rrl in
  let pkg_str = string_of_pkg pkg in
  Printf.printf "Cone rules %s :\n\n" pkg_str;
  brute_print_rreasons cr;
  Printf.printf " -------\n\n"

let string_of_node = string_of_list string_of_pkg

(** ------------------------------------- *)

(** ------------------------------ *)
(** ------------------------------ *)
(** -------- TO REMOVE ----------- *)

open Defaultgraphs
module EG = ExplanationGraph

let add_dep_edge graph src dst cstr =
  let edge = (src,EG.ExplE.Depends cstr,dst) in
  EG.G.add_edge_e graph edge
;;

let add_dep graph global_id or_id root_node dep partial_missing =
  match dep with
  | RDependency (_,c,nl) ->
    let disjunction = List.length nl > 1 or partial_missing <> None  in
    if disjunction
    then
      (match root_node with
      | EG.ExplV.Pkgs (id,n) ->
	let or_node = EG.ExplV.Or (id,n,or_id) in
	add_dep_edge graph root_node or_node c;
	let (g_id,node_list) =
	  List.fold_left 
	    (fun (id,node_l) n -> 
	      let node = EG.ExplV.Pkgs (id,n) in
	      add_dep_edge graph or_node node c;
	      (id + 1, node::node_l)
	    ) (global_id,[]) nl
	in
	(match partial_missing with
	| None -> ()
	| Some RMissing (_,c1,c2) ->
	  let pm_node = EG.ExplV.Missing c2 in
	  add_dep_edge graph or_node pm_node c1
	| _ -> assert false
	);
	(g_id,or_id + 1,node_list)
      | _ -> assert false)
    else
      let next_node = EG.ExplV.Pkgs (global_id, List.hd nl) in
      add_dep_edge graph root_node next_node c;
      (global_id + 1,or_id,[next_node])
  | _ -> assert false
;;

let cone_table deplist rrl =
  let tmp_tbl = Hashtbl.create (List.length deplist) in
  List.iter
    (fun d ->
      (match d with
      | RDependency (_,_,nl) ->
	let cone =
	  List.fold_left
	    (fun acc' n ->
	      (cone_rules n rrl)@acc'
	    ) [] nl
	in
	Hashtbl.add tmp_tbl d cone
      | _ -> assert false)
    ) deplist;
  tmp_tbl
;;

let conj_context cone_tbl context dep deplist =
  List.unique
    (List.fold_left 
       (fun acc d ->
	 if d <> dep
	 then (Hashtbl.find cone_tbl d)@acc
	 else acc
       ) context deplist)
;;

let rec build_deps graph cn_tbl global_id context root_node rrl =
  match root_node with 
  | EG.ExplV.Pkgs (id,root) ->
    let domain = remove_irrelevant context root (cone_rules root rrl) in
    let root_deps = List.filter (function RDependency (n,_,_) when n = root -> true | _ -> false) domain in
    let root_confs = List.filter (conflict_mem root) domain in
    let debug = 
      Printf.printf "DEBUG 00 : processing %s\n" (EG.string_of_vertex root_node);
      Printf.printf "DEBUG 01 : rrl : \n";
      brute_print_rreasons rrl;
      Printf.printf "DEBUG 02 : domain :\n";
      brute_print_rreasons domain;
      Printf.printf "DEBUG 03 : root_deps :\n";
      brute_print_rreasons root_deps;
      Printf.printf "DEBUG 04 : root_confs :\n";
      brute_print_rreasons root_confs;
    in
    (match List.length root_deps with
    | 0 -> 
      if root_confs <> [] 
      then 
	let debug = 
	  Printf.printf "DEBUG 05 : conflict leaf : %s\n" (EG.string_of_vertex root_node)
	in
	(global_id,[root_node])
      else
	let debug = 
	  Printf.printf "DEBUG 05 : missing leaf\n\n ";
	in
	(match List.hd domain with
	| RMissing (_,c1,c2) ->
	  let miss_node = EG.ExplV.Missing c2 in
	  add_dep_edge graph root_node miss_node c1;
	  (global_id,[])
	| _ -> assert false)
    | dep_nmbr ->
      let next_context,self_cn = if root_confs <> [] then root_confs@context,[root_node] else context,[] in
      let debug =
	Printf.printf "DEBUG 05 : next_context :\n";
	brute_print_rreasons next_context;
	Printf.printf "DEBUG 06 : self_cn : ";
	Printf.printf "%s \n" (EG.string_of_list (EG.string_of_vertex) self_cn)
      in
      let cone_tbl = cone_table root_deps domain in
      let context_tbl = Hashtbl.create (List.length root_deps) in
      List.iter 
	(fun d ->
	  Hashtbl.add context_tbl d (conj_context cone_tbl next_context d root_deps)
	) root_deps;
      let (id,_,cn) =
	List.fold_left
	  (fun (global_id,or_id,cnl) dep ->
	    let simplified = simplify dep domain in
	    let cstr = (match dep with | RDependency (_,c,_) -> c | _ -> assert false) in
	    let pr = List.find (function RDependency (n,c,_) when n = root && c = cstr -> true | _ -> false) simplified in
	    let debug =
	      Printf.printf "DEBUG 07 : simplified : \n";
	      brute_print_rreasons simplified;
	      Printf.printf "DEBUG 08 : pr : ";
	      brute_print_rr pr
	    in
	    let partial_missing =
	      try
		Some (List.find (function RMissing (n,c1,c2) when n = root && c1 <> c2 && c1 = cstr -> true | _ -> false) simplified)
	      with Not_found ->
		None
	    in
	    let (id,or_id',next_roots) = add_dep graph global_id or_id root_node pr partial_missing in
	    let debug =
	      Printf.printf "DEBUG 09 : next_roots :\n";
	      Printf.printf "%s \n" (EG.string_of_list (EG.string_of_vertex) next_roots)
	    in
	    let next_id,cnl' =
	      List.fold_left 
		(fun (id',conflicting_node_list) node ->
		  let cntxt = next_context@(Hashtbl.find context_tbl dep) in 
		  let (id',cnl) = build_deps graph cn_tbl id' cntxt node simplified in
		  let debug =
		    Printf.printf "DEBUG 10 : new conflicting vertex list :\n";
		    Printf.printf "%s \n\n" (EG.string_of_list (EG.string_of_vertex) (cnl@conflicting_node_list))
		  in
		  (id',cnl@conflicting_node_list)
		) (id,[]) next_roots
	    in
	    (next_id,or_id',cnl'@cnl)
	  ) (global_id,0,self_cn) root_deps
      in
      if dep_nmbr > 1 or self_cn <> [] then Hashtbl.add cn_tbl root_node cn;
      (id,cn)
    )
    | _ -> assert false
;; 


let build_expl rl pkg =
  let rrl = reduced_reasons rl in
  let root = [pkg] in
  let expl_graph = EG.G.create () in
  let root_node = EG.ExplV.Pkgs (0,root) in
  EG.G.add_vertex expl_graph root_node;
  let conflicting_nodes_table = Hashtbl.create 10 in 
  let cnl = build_deps expl_graph conflicting_nodes_table 1 [] root_node rrl in
  (expl_graph,conflicting_nodes_table,cnl)
;;

(** -------- TO REMOVE ----------- *)
(** ------------------------------ *)
(** ------------------------------ *)


let test d universe =
  Printf.printf "\n ------ Tests -------\n \n";
  match d with 
  |{result = Failure f; request = req} ->
    let diag = f () in
    let rr = reduced_reasons diag in
    Printf.printf "\n ________ Reduced Reasons list _______ \n \n";
    brute_print_rreasons rr;
    (*Printf.printf "\n ________ Deps _______ \n \n";
    Cudf.iter_packages (fun pkg -> brute_print_deps [pkg] rr) universe;*)
    (*Printf.printf "\n ______ Cone_rules + remove_irlvnt ______ \n \n";
    Cudf.iter_packages (fun pkg -> brute_print_cone_rules pkg rr ) universe;*)
    (* test avec tricky.cudf seulement *)
    (*let p = Cudf.lookup_package universe ("p",1) in
    let b = Cudf.lookup_package universe ("b",1) in
    let a = Cudf.lookup_package universe ("a",1) in
    let cr_p = Diagnostic.cone_rules [p] rr in
    let cr_b = Diagnostic.cone_rules [b] rr in
    let cr_a = Diagnostic.cone_rules [a] rr in
    let rlvnt_p = Diagnostic.remove_irrelevant [] [p] cr_p in
    let rlvnt_b = Diagnostic.remove_irrelevant [] [b] cr_b in
    let rlvnt_a = Diagnostic.remove_irrelevant [] [a] cr_a in
    Printf.printf "Cone p :\n\n";
    brute_print_rreasons cr_p;   
    Printf.printf "Cone a :\n\n";
    brute_print_rreasons cr_a;
    Printf.printf "Cone b :\n\n";
    brute_print_rreasons cr_b;
    Printf.printf "Relevant Cone p :\n\n";
    brute_print_rreasons rlvnt_p;   
    Printf.printf "Relevant Cone a :\n\n";
    brute_print_rreasons rlvnt_a;
    Printf.printf "Relevant Cone b :\n\n";
    brute_print_rreasons rlvnt_b; 
    (* tricky.cudf only : fin *)*)
    (*Printf.printf "\n ________ Simplified _______ \n \n";*)
    match req with
    | Package p ->
      (*let root_deps = 
	List.filter 
	  (function RDependency (n,_,_) when n = [p] -> true | _ -> false
	  ) rr
      in
      (*let smplfd = List.fold_left (fun rr dep -> simplify dep rr) rr root_deps in
      brute_print_rreasons smplfd;*)
      (*List.iter (fun root_dep -> *)
      Printf.printf "\nSimplifying : %s\n\n" (string_of_pkg p);
      (*brute_print_rr root_dep;*)
      let smplfd = List.fold_left (fun acc r -> simplify r acc) rr root_deps in
      brute_print_rreasons smplfd;
      Printf.printf "   --------   \n\n";*)
      let (g,cn_tbl,cnl) = (build_expl diag p) in
      EG.print_egraph g;
      Printf.printf "\nConflicting Nodes Table :\n\n"; 
      Hashtbl.iter 
	(fun v vl ->
	  Printf.printf "%s <-------- \n\n" (EG.string_of_vertex v);
	  Printf.printf "%s \n\n" (EG.string_of_list (EG.string_of_vertex) vl);
	  Printf.printf "\t --------> \n\n"
	) cn_tbl;
      ();
	
    | _ -> ();
    Printf.printf "\n -----------------------\n \n"
  | _ ->
    Printf.printf "\n ________ or_inclusion _______ \n \n";
    let l1 = [("a",None);("b",None);("c", Some(`Lt,8));("c",Some(`Geq,8))] in
    let l1_str = string_of_list (string_of_vpkg) l1 in
    let l2 = [("b",Some(`Neq,8))] in
    let l2_str = string_of_list (string_of_vpkg) l2 in
    let l3 = [("a",None);("c",Some(`Eq,12))] in
    let l3_str = string_of_list (string_of_vpkg) l3 in
    let l4 = [("b",None);("d",None)] in
    let l4_str = string_of_list (string_of_vpkg) l4 in
    Printf.printf " %s includes %s : %B (expected : %B)\n" l1_str l1_str (Diagnostic.or_include l1 l1) true;
    Printf.printf " %s includes %s : %B (expected : %B)\n" l1_str l2_str (Diagnostic.or_include l1 l2) true;
    Printf.printf " %s includes %s : %B (expected : %B)\n" l1_str l3_str (Diagnostic.or_include l1 l3) true;
    Printf.printf " %s includes %s : %B (expected : %B)\n" l1_str l4_str (Diagnostic.or_include l1 l4) false;
    Printf.printf "\n ________ and_inclusion _______ \n \n";
    let l1 = [[("a",None);("b",None)]] in
    let l1_str = string_of_list (string_of_list (string_of_vpkg)) l1 in
    let l2 = [[("c",None);("d",None)];[("a",Some(`Eq,3))]] in
    let l2_str = string_of_list (string_of_list (string_of_vpkg)) l2 in
    Printf.printf " %s includes %s : %B (expected : %B)\n" l1_str l1_str (Diagnostic.and_include l1 l1) true;
    Printf.printf " %s includes %s : %B (expected : %B)\n" l1_str l2_str (Diagnostic.and_include l1 l2) true;
    Printf.printf " %s includes %s : %B (expected : %B)\n" l2_str l1_str (Diagnostic.and_include l2 l1) false;
    Printf.printf "\n -----------------------\n \n"
  

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let inputlist = posargs@(OptParse.Opt.get Options.foreground) in
  let (input_type,implicit) = guess_format Options.inputtype inputlist in

  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let options = Options.set_options input_type in
  let (fg,bg) = Options.parse_cmdline (input_type,implicit) posargs in

  let (preamble,pkgll,_,from_cudf,to_cudf) = Boilerplate.load_list ~options [fg;bg] in
  let (fg_pkglist, bg_pkglist) = match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false in
  let fg_pkglist = 
    if OptParse.Opt.get Options.latest then CudfAdd.latest fg_pkglist
    else fg_pkglist
  in
  let universe = 
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    Cudf.load_universe (CudfAdd.Cudf_set.elements s) 
  in
  let universe_size = Cudf.universe_size universe in

  if OptParse.Opt.is_set Options.checkonly && 
    OptParse.Opt.is_set Options.coinst then
      fatal "--checkonly and --coinst cannot be specified together";

  let checklist = 
    if OptParse.Opt.is_set Options.checkonly then begin
      info "--checkonly specified, consider all packages as background packages";
      List.flatten (
        List.map (fun ((n,a),c) ->
          let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else []
  in

  let coinstlist = 
    if OptParse.Opt.is_set Options.coinst then begin
      info "--coinst specified, consider all packages as background packages";
      List.map (fun ((n,a),c) ->
        let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
        Cudf.lookup_packages ~filter universe name
      ) (OptParse.Opt.get Options.coinst)
    end else []
  in

  let pp =
  (*if OptParse.Opt.is_set Options.realversionfield then
    let rvf = OptParse.Opt.get Options.realversionfield in
    let rvtbl = VersionTable.make_version_table rvf universe universe_size fg in
    let from_cudf_real (n,v) =
      try
	let rv = Hashtbl.find rvtbl (n,v) in
	(n,rv)
      with Not_found -> from_cudf (n,v)
    in 
    CudfAdd.pp from_cudf_real
  else *)
    CudfAdd.pp from_cudf
  in

  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failure in
  let success = OptParse.Opt.get Options.success in
  let explain = OptParse.Opt.get Options.explain in
  let minimal = OptParse.Opt.get Options.minimal in
  let summary = OptParse.Opt.get Options.summary in
  let addmiss = OptParse.Opt.get Options.add_missings_opt in
  let testing = OptParse.Opt.get Options.testing_opt in
  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      let oc = open_out (OptParse.Opt.get Options.outfile) in
      Format.formatter_of_out_channel oc
    else
      Format.std_formatter
  in
  let results = Diagnostic.default_result universe_size in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d =
    if summary then Diagnostic.collect results d ;
    let pp =
      if input_type = `Cudf then 
        fun pkg -> pp ~decode:(fun x -> x) pkg 
      else fun pkg -> pp pkg
    in
    if testing then test d universe;
    if OptParse.Opt.get Options.brute_opt then 
      brute_print addmiss d
    else 
      Diagnostic.fprintf ~pp ~failure ~success ~explain ~minimal ~addmiss fmt d

  in
  Util.Timer.start timer;

  if OptParse.Opt.is_set Options.coinst then begin
    let rl = Depsolver.edos_coinstall_prod universe coinstlist in
    let nbt = List.length (List.filter (fun r -> not (Diagnostic.is_solution r)) rl) in
    let number_checks = List.length rl in 
    ignore(Util.Timer.stop timer ());
    List.iter callback rl;
    if failure || success then Format.fprintf fmt "@]@.";
    Format.fprintf fmt "total-packages: %d@." universe_size;
    Format.fprintf fmt "total-tuples: %d@." number_checks;
    Format.fprintf fmt "broken-tuples: %d@." nbt;
    Boilerplate.exit(nbt)
  end else begin 
    let global_constraints = not(OptParse.Opt.get Options.deb_ignore_essential) in
    let nbp =
      if OptParse.Opt.is_set Options.checkonly then 
        Depsolver.listcheck ~global_constraints ~callback universe checklist
      else
        if bg_pkglist = [] then
          Depsolver.univcheck ~global_constraints ~callback universe 
        else
          Depsolver.listcheck ~global_constraints ~callback universe fg_pkglist
    in
    ignore(Util.Timer.stop timer ());
    
    if failure || success then Format.fprintf fmt "@]@.";
    
    let fn = List.length fg_pkglist in
    let bn = List.length bg_pkglist in
    
    let nb,nf = 
      let cl = List.length checklist in
      if cl != 0 then ((fn + bn) - cl,cl) else (bn,fn)
    in
    
    if nb > 0 then begin
      Format.fprintf fmt "background-packages: %d@." nb;
      Format.fprintf fmt "foreground-packages: %d@." nf
    end;

    Format.fprintf fmt "total-packages: %d@." universe_size;
    (*
    Format.fprintf fmt "broken-percent: %0.2f%%@." 
     ( (float_of_int nbp) /.  (float_of_int universe_size) *. 100. ) ;
    *)
    Format.fprintf fmt "broken-packages: %d@." nbp;
    if summary then 
      Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;
    Boilerplate.exit(nbp)
  end
;;

Boilerplate.if_application
~alternatives:[
  "debcheck";"dose-debcheck"; "dose-distcheck";
  "eclipsecheck";"dose-eclipsecheck";
  "rpmcheck";"dose-rpmcheck"]
__FILE__ main ;;

