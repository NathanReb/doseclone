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

open Defaultgraphs

let test d universe =
  Printf.printf "\n ------ Tests -------\n \n";
  match d with 
  |{result = Failure f; request = req} ->
    let diag = f () in
    let rr = reduced_reasons diag in
    Printf.printf "\n ________ Reduced Reasons list _______ \n \n";
    brute_print_rreasons rr;
    match req with
    | Package p ->
      Printf.printf "DEBUG : before build_expl\n%!";
      let g = (build_expl diag p) in
      Printf.printf "DEBUG : after build_expl\n%!";
      ExplanationGraph.print_egraph g;
      ()	
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
    if testing then 
      test d universe;
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

