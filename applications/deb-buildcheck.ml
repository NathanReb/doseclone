(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Debian
open Common
open Algo
module Src = Debian.Sources
module Deb = Debian.Packages
module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description =
    "Report the broken packages in a debian source list. \
     You must provide a (list of) Debian Packages file(s) and \
     a Debian Sources file in this order"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  include Boilerplate.DistcheckOptions
  Boilerplate.DistcheckOptions.add_options options ;;

  include Boilerplate.InputOptions
  Boilerplate.InputOptions.add_options options ;;

  include Boilerplate.DistribOptions;;
  let default = ["deb-triplettable";"deb-cputable"]@Boilerplate.DistribOptions.default_options in
  Boilerplate.DistribOptions.add_options ~default options ;;

  let dump = StdOpt.str_option ()
  let maforeign = StdOpt.store_true ()
  let noindep = StdOpt.store_true ()
  let includextra = StdOpt.store_true ()
  let triplettable = StdOpt.str_option ()
  let cputable = StdOpt.str_option ()

  open OptParser
  add options ~long_name:"defaultedMAforeign" ~help:"Convert Arch:all packages to Multi-Arch: foreign" maforeign;
  add options ~long_name:"DropBuildIndep" ~help:"Drop Build-Indep dependencies" noindep;
  add options ~long_name:"IncludeExtraSource" ~help:"Include packages with Extra-Source-Only:yes (dropped by default)" includextra;
  add options ~long_name:"deb-triplettable" ~help:"Path to an architecture triplet table like /usr/share/dpkg/triplettable" triplettable;
  add options ~long_name:"deb-cputable" ~help:"Path to a cpu table like /usr/share/dpkg/cputable" cputable;
  add options ~long_name:"dump" ~help:"dump the cudf file" dump;

end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver"

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  Util.Debug.disable "Depsolver_int";
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let fmt = Format.std_formatter in
  if OptParse.Opt.is_set Options.deb_native_arch then
    Format.fprintf fmt "native-architecture: %s@." (OptParse.Opt.get Options.deb_native_arch)
  else
    fatal "You must at least specify the native architecture";

  if OptParse.Opt.is_set Options.deb_foreign_archs then
    Format.fprintf fmt "foreign-architecture: %s@." (String.concat "," (OptParse.Opt.get Options.deb_foreign_archs));

  if OptParse.Opt.is_set Options.deb_host_arch then
    Format.fprintf fmt "host-architecture: %s@." (OptParse.Opt.get Options.deb_host_arch);

  (* we set the Debian.Debcudf options wrt the user provided options *)
  let options = Options.set_deb_options () in
  (* buildarch and native arch must be set to some architecture at this point *)
  let buildarch = Option.get options.Debian.Debcudf.native in
  (* hostarch and noindep can be None *)
  let hostarch = match options.Debian.Debcudf.host with None -> "" | Some s -> s in
  let noindep = OptParse.Opt.get Options.noindep in

  let filter_external_sources par =
    if (OptParse.Opt.get Options.includextra) then true
    else
      try not(Debian.Packages.parse_bool (Debian.Packages.assoc "extra-source-only" par))
      with Not_found -> true
  in

  if (OptParse.Opt.is_set Options.triplettable)
  || OptParse.Opt.is_set Options.cputable then begin
    let ttfile = if OptParse.Opt.is_set Options.triplettable then
        Some (OptParse.Opt.get Options.triplettable)
      else None in
    let ctfile = if OptParse.Opt.is_set Options.cputable then
        Some (OptParse.Opt.get Options.triplettable)
      else None in
    Architecture.read_triplettable ~ttfile ~ctfile ()
  end;

  let pkglist, srclist =
    match posargs with
    |[] | [_] -> fatal 
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l -> 
        begin match List.rev l with
        |h::t ->
          let srclist = Boilerplate.deb_load_source ~filter:filter_external_sources ~noindep buildarch hostarch h in
          let pkglist = Deb.input_raw t in
          (pkglist,srclist)
        |_ -> fatal "An impossible situation occurred ?!#"
        end
  in
  let tables = Debcudf.init_tables (srclist @ pkglist) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,v) = (p,Debian.Debcudf.get_real_version tables (p,v)) in
  let pp = CudfAdd.pp from_cudf in 

  (* XXX here latest could be a bit faster if done at the same time of the cudf
     conversion *)
  let sl = 
    let l = List.map (fun pkg -> Debcudf.tocudf ~options tables pkg) srclist in
    if OptParse.Opt.get Options.latest then
      CudfAdd.latest l
    else
      l
  in
  let bl = 
    List.fold_left (fun acc pkg ->
      let pkg = 
        if OptParse.Opt.get Options.maforeign && pkg.Packages.architecture = "all" then
          { pkg with Packages.multiarch = `Foreign }
        else pkg
      in
      (Debcudf.tocudf ~options tables pkg)::acc
    ) sl pkglist 
  in

  let universe = Cudf.load_universe bl in
  let universe_size = Cudf.universe_size universe in

  let failure = OptParse.Opt.get Options.failure in
  let success = OptParse.Opt.get Options.success in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in

  let checklist =
    if OptParse.Opt.is_set Options.checkonly then begin
      List.flatten (
        List.map (fun ((n,a),c) ->
          let (name,filter) = Debian.Debutil.debvpkg to_cudf (("src:"^n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else sl
  in


  let results = Diagnostic.default_result universe_size in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d = 
    if summary then Diagnostic.collect results d ;
(*
    if success && not explain && not failure then
    else
*)
      Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in

  Util.Timer.start timer;
  let nbp = Depsolver.listcheck ~callback universe checklist in
  ignore(Util.Timer.stop timer ());

  if failure || success then Format.fprintf fmt "@]@.";

  let nb = universe_size in
  let nf = List.length sl in
  Format.fprintf fmt "background-packages: %d@." nb;
  Format.fprintf fmt "foreground-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." nbp;

  if summary then
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    
    Cudf_printer.pp_preamble oc Debcudf.preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe
  end;

  Boilerplate.exit(nbp)
;;

Boilerplate.if_application
  ~alternatives:[
    "deb-buildcheck"; "debbuildcheck";"dose-builddebcheck";
    "deb-crossbuildcheck";"debcrossbuildcheck";
    "dose-debcrossbuildcheck"]
  __FILE__ main
;;
