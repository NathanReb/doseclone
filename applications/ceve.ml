(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  and Jaap Boender <boender@pps.jussieu.fr *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open Cudf
open ExtLib
open Common
IFDEF HASDB THEN
open Db
END

exception Done

module Options =
struct
  type output_types = Dot | CNF | Dimacs

  let confile = ref ""
  let debug = ref 0
  let boolean = ref false
  let info = ref false
  let strong_pred = ref false
  let src = ref ""
  let dst = ref ""
  let cone = ref ""
  let pred_cone = ref ""
  let cone_maxdepth = ref None
  let output_type = ref Dot
  let output_ch = ref stdout

  let set_output_type s =
    if s = "dot" then output_type := Dot
    else if s = "cnf" then output_type := CNF
    else if s = "dimacs" then output_type := Dimacs
    else failwith (Printf.sprintf "Unknown output type: %s" s)
end

let usage = Printf.sprintf "usage: %s [-options] [cudf doc]" (Sys.argv.(0))
let options =
  [
   ("--confile",  Arg.String (fun l -> Options.confile := l ), "Specify a configuration file" );
   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level");
   ("--boolean", Arg.Set Options.boolean, "Output the boolean graph");
   ("--src",  Arg.String (fun l -> Options.src := l ), "Specify a list of packages to analyze" );
   ("--dst",  Arg.String (fun l -> Options.dst := l ), "Specify a pivot package" );
   ("--cone",  Arg.String (fun l -> Options.cone := l ), "Compute the dependency closure" );
   ("--pred-cone",  Arg.String (fun l -> Options.pred_cone := l ), "Compute the dependency closure" );
   ("--cone-maxdepth", Arg.Int (fun i -> Options.cone_maxdepth := Some i ), "Maximum depth of dependency cone");
   ("--info", Arg.Set Options.info, "Print various aggregate information");
   ("--pred", Arg.Set Options.strong_pred, "Print strong predecessor (not direct)");
   ("--output-type", Arg.String Options.set_output_type, "Set the output type (dot, cnf)");
   ("--output-file", Arg.String (fun s -> Options.output_ch := open_out s), "Send output to a file");
  ]

let and_sep_re = Str.regexp "\\s*;\\s*"
let pkg_re = Str.regexp "(\\([a-z][a-z0-9.+-]+\\)\\s*,\\s*\\([a-zA-Z0-9.:-]+\\))"
let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then begin
      (Str.matched_group 1 str, Str.matched_group 2 str)
    end
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s)

(* -------------------------------- *)

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1
;;

(* -------------------------------- *)

let main () =
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  let versions = Hashtbl.create 1023 in
  let load_universe l = 
    let tables = Debian.Debcudf.init_tables l in
    Cudf.load_universe (
      List.map (fun pkg ->
        let cudfpkg = Debian.Debcudf.tocudf tables pkg in
        Hashtbl.add versions 
        (pkg.Debian.Packages.name,pkg.Debian.Packages.version) cudfpkg ;
        cudfpkg
      ) l
    )
  in

  let universe =
    match Input.parse_uri !uri with
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) ->
IFDEF HASDB THEN        
      begin
        Backend.init_database dbtype info (Idbr.parse_query query) ;
        let l = Backend.load_selection (`All) in
        load_universe l
      end
ELSE
      failwith (dbtype ^ " Not supported")
END
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      load_universe l
    end
    |("cudf",(_,_,_,_,file),_) -> 
        let (_,u,_) = parse_cudf file in u
    |_ -> failwith "Not supported"
  in

  let get_cudfpkg (p,v) =
    try Hashtbl.find versions (p,v)
    with Not_found -> assert false
  in

  let module Graph = Defaultgraphs.SyntacticDependencyGraph in

  let pkg_src () = List.map get_cudfpkg (parse_pkg !Options.src) in
  let pkg_dst () =
    (* all packages q in R s.t. q is in the dependency closure of p *)
    let (p,v) = List.hd(parse_pkg !Options.dst) in
    let pid = get_cudfpkg (p,v) in
    List.filter_map (fun pkg ->
      if List.mem pid (Depsolver.dependency_closure universe [pkg]) then
        Some(pkg)
      else None
    ) (Cudf.get_packages universe) 
  in
  let pkg_cone () =
    List.unique (List.fold_left (fun acc (p,v) ->
      let pid = get_cudfpkg (p,v) in
      match !Options.cone_maxdepth with
      | None -> (Depsolver.dependency_closure universe [pid]) @ acc
      | Some d -> (Depsolver.dependency_closure ~maxdepth:d universe [pid]) @ acc
    ) [] (parse_pkg !Options.cone))
  in
  let pkg_pred_cone () =
    List.unique (List.fold_left (fun acc (p,v) ->
      let pid = get_cudfpkg (p,v) in
      match !Options.cone_maxdepth with
      | None -> (Depsolver.reverse_dependency_closure universe [pid]) @ acc
      | Some d -> (Depsolver.reverse_dependency_closure ~maxdepth:d universe [pid]) @ acc
    ) [] (parse_pkg !Options.pred_cone))
  in

  let pkg_src_list = ref [] in
  let pkg_dst_list = ref [] in
  let plist =
    if !Options.src <> "" && !Options.dst <> "" then begin
      let (p,v) = List.hd(parse_pkg !Options.dst) in
      let pid = get_cudfpkg (p,v) in
      pkg_src_list := pkg_src ();
      pkg_dst_list := [pid];
      (pid::!pkg_src_list)
    end
    else if !Options.src <> "" then begin
      pkg_src_list := pkg_src ();
      !pkg_src_list
    end
    else if !Options.dst <> "" then begin
      pkg_dst_list := pkg_dst ();
      !pkg_dst_list
    end
    else if !Options.cone <> "" then 
      pkg_cone ()
    else if !Options.pred_cone <> "" then
      pkg_pred_cone ()
    else Cudf.get_packages universe

  in
  let u = Cudf.load_universe plist in
  match !Options.output_type with
  | Options.Dot -> Graph.D.output_graph !Options.output_ch (Graph.dependency_graph u)
  | Options.CNF -> Depsolver.output_clauses !Options.output_ch u
  | Options.Dimacs -> Depsolver.output_clauses ~dimacs:true !Options.output_ch u
;;

main ();;