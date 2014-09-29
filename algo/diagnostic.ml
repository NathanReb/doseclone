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

module OcamlHash = Hashtbl
open ExtLib
open Common
open Defaultgraphs

include Util.Logging(struct let label = __FILE__ end) ;;

(** One un-installability reason for a package *)
type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list) 
  (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  |Missing of (Cudf.package * Cudf_types.vpkg list) 
  (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  |Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg) 
  (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)

type node = Cudf.package list

type reducedReason =
  | RDependency of (node * Cudf_types.vpkg list * node list)
  | RMissing of (node * Cudf_types.vpkg list * Cudf_types.vpkg list)
  (** Missing (a,dep,vpkglist) means that the [vpkglist] part of
      the dependency [dep] of node [a] cannont be satisfied *)
  | RConflict of (node * node * Cudf_types.vpkg) 


(** The request provided to the solver *)
type request =
  |Package of Cudf.package
  (** Check the installability of one package *)
  |PackageList of Cudf.package list
  (** Check the installability of a list of packages *)

(** The result of an installability query *)
type result =
  |Success of (?all:bool -> unit -> Cudf.package list) 
  (** If successfull returns a function that will
      return the installation set for the given query. Since
      not all packages are tested for installability directly, the
      installation set might be empty. In this case, the solver can
      be called again to provide the real installation set 
      using the parameter [~all:true] *)
  |Failure of (unit -> reason list) 
  (** If unsuccessful returns a function containing the list of reason *)

type diagnosis = { result : result ; request : request }

type pp = (Cudf.package -> string * string * (string * string) list)

module ResultHash = OcamlHash.Make (
  struct
    type t = reason

    let equal v w = match (v,w) with
    |Missing (_,v1),Missing (_,v2) -> v1 = v2
    |Conflict(i1,j1,_),Conflict (i2,j2,_) -> i1 = i2 && j1 = j2
    |_ -> false

    let hash = function
      |Missing (_,vpkgs) -> OcamlHash.hash vpkgs
      |Conflict (i,j,_) -> OcamlHash.hash (i,j)
      |_ -> assert false
  end
)

type summary = {
  mutable missing : int;
  mutable conflict : int;
  mutable unique_missing : int;
  mutable unique_conflict : int;
  summary : (Cudf.package list ref) ResultHash.t 
}

let default_result n = {
  missing = 0;
  conflict = 0;
  unique_missing = 0;
  unique_conflict = 0;
  summary = ResultHash.create n;
}

(** given a list of dependencies, return a list of list containg all
 *  paths in the dependency tree starting from [root] *)
let build_paths deps root =
  let bind m f = List.flatten (List.map f m) in
  let rec aux acc deps root =
    match List.partition (fun (i,_,_) -> CudfAdd.equal i root) deps with
    |([],_) when (List.length acc) = 1 -> [] 
    |(rootlist,_) ->
        bind rootlist (function
          |(i,v,[]) -> [List.rev acc]
          |(i,v,l) -> bind l (fun r -> aux ((i,v)::acc) deps r)
        )
  in
  aux [] deps root
;;

let pp_package ?(source=false) pp fmt pkg =
  let (p,v,fields) = pp pkg in
  Format.fprintf fmt "package: %s@," p;
  Format.fprintf fmt "version: %s" v;
  List.iter (function
    |(("source"|"sourcenumber"),_) -> ()
    |(k,v) -> Format.fprintf fmt "@,%s: %s" k v
  ) fields;
  if source then begin 
    try
      let source = List.assoc "source" fields in
      let sourceversion = 
        try "(= "^(List.assoc "sourcenumber" fields)^")" 
        with Not_found -> ""
      in
      Format.fprintf fmt "@,source: %s %s" source sourceversion
    with Not_found -> ()
  end
;;

let pp_vpkglist pp fmt = 
  (* from libcudf ... again *)
  let pp_list fmt ~pp_item ~sep l =
    let rec aux fmt = function
      | [] -> assert false
      | [last] -> (* last item, no trailing sep *)
          Format.fprintf fmt "@,%a" pp_item last
      | vpkg :: tl -> (* at least one package in tl *)
          Format.fprintf fmt "@,%a%s" pp_item vpkg sep ;
          aux fmt tl
    in
    match l with
    | [] -> ()
    | [sole] -> pp_item fmt sole
    | _ -> Format.fprintf fmt "@[<h>%a@]" aux l
  in
  let string_of_relop = function
      `Eq -> "="
    | `Neq -> "!="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
  in
  let pp_item fmt = function
    |(p,None) -> 
        let (p,_,_) = pp {Cudf.default_package with Cudf.package = p} in
        Format.fprintf fmt "%s" p
    |(p,Some(c,v)) ->
        debug "pp_vpkglist %s %s %i" p (string_of_relop c) v;
        let (p,v,_) = pp {Cudf.default_package with Cudf.package = p ; version = v} in
        Format.fprintf fmt "%s (%s %s)" p (string_of_relop c) v
  in
  pp_list fmt ~pp_item ~sep:" | "

let pp_dependency pp ?(label="depends") fmt (i,vpkgs) =
  Format.fprintf fmt "%a" (pp_package pp) i;
  if vpkgs <> [] then
    Format.fprintf fmt "@,%s: %a" label (pp_vpkglist pp) vpkgs;
;;

let rec pp_list pp fmt = function
  |[h] -> Format.fprintf fmt "@[<v 1>-@,%a@]" pp h
  |h::t ->
      (Format.fprintf fmt "@[<v 1>-@,%a@]@," pp h ;
      pp_list pp fmt t)
  |[] -> ()
;;

let create_pathlist root deps =
  let dl = List.map (function Dependency x -> x |_ -> assert false) deps in
  build_paths (List.unique dl) root
;;

let pp_dependencies pp fmt pathlist =
  let rec aux fmt = function
    |[path] -> Format.fprintf fmt "@[<v 1>-@,@[<v 1>depchain:@,%a@]@]" (pp_list (pp_dependency pp)) path
    |path::pathlist ->
        (Format.fprintf fmt "@[<v 1>-@,@[<v 1>depchain:@,%a@]@]@," (pp_list (pp_dependency pp)) path;
        aux fmt pathlist)
    |[] -> ()
  in
  aux fmt pathlist
;;

let print_error pp root fmt l =
  let (deps,res) = List.partition (function Dependency _ -> true |_ -> false) l in
  let pp_reason fmt = function
    |Conflict (i,j,vpkg) ->
        Format.fprintf fmt "@[<v 1>conflict:@,";
        Format.fprintf fmt "@[<v 1>pkg1:@,%a@," (pp_package ~source:true pp) i;
        Format.fprintf fmt "unsat-conflict: %a@]@," (pp_vpkglist pp) [vpkg];
        Format.fprintf fmt "@[<v 1>pkg2:@,%a@]" (pp_package ~source:true pp) j;
        if deps <> [] then begin
          let pl1 = create_pathlist root (Dependency(i,[],[])::deps) in
          let pl2 = create_pathlist root (Dependency(j,[],[])::deps) in
          if pl1 <> [[]] then
            Format.fprintf fmt "@,@[<v 1>depchain1:@,%a@]" (pp_dependencies pp) pl1;
          if pl2 <> [[]] then
            Format.fprintf fmt "@,@[<v 1>depchain2:@,%a@]" (pp_dependencies pp) pl2;
          Format.fprintf fmt "@]"
        end else
          Format.fprintf fmt "@,@]"
    |Missing (i,vpkgs) ->
        Format.fprintf fmt "@[<v 1>missing:@,";
        Format.fprintf fmt "@[<v 1>pkg:@,%a@]" 
          (pp_dependency ~label:"unsat-dependency" pp) (i,vpkgs);
        let pl = create_pathlist root (Dependency(i,vpkgs,[])::deps) in
        if pl <> [[]] then begin
          Format.fprintf fmt "@,@[<v 1>depchains:@,%a@]" (pp_dependencies pp) pl;
          Format.fprintf fmt "@]"
        end else
          Format.fprintf fmt "@,@]"
    (* only two failures reasons. Dependency describe the 
     * dependency chain to a failure witness *)
    |_ -> assert false 
  in
  pp_list pp_reason fmt res;
;;

(* XXX unplug your imperative brain and rewrite this as a tail recoursive
 * function ! *)
let minimize roots l =
  let module H = Hashtbl in
  let h = H.create (List.length l) in
  List.iter (fun p -> H.add h p.Cudf.package p) l;
  let acc = H.create 1023 in
  let rec visit pkg =
    if not (H.mem acc pkg) then begin
      H.add acc pkg ();
      List.iter (fun vpkgformula ->
        List.iter (fun (name,constr) ->
          try
            let p = H.find h name in
            if Cudf.version_matches p.Cudf.version constr then
              visit p
          with Not_found -> ()
        ) vpkgformula
      ) pkg.Cudf.depends
    end
  in
  begin match roots with 
  |Package r -> visit r
  |PackageList rl -> List.iter visit l end;
  H.fold (fun k _ l -> k::l) acc []
;;

let get_installationset ?(minimal=false) = function
  |{result = Success f ; request = req} -> 
     let s = f ~all:true () in
     if minimal then minimize req s else s
  |{result = Failure _ } -> raise Not_found
;;

let is_solution = function
  |{result = Success _ } -> true
  |{result = Failure _ } -> false
;;

(** [default_pp] default package printer. If the version of the package is
  * a negative number, the version version if printed as "nan" *)
let default_pp pkg =
  let v = 
    if pkg.Cudf.version > 0 then 
      CudfAdd.string_of_version pkg
    else "nan"
  in
  (pkg.Cudf.package,v,[])
;;

let print_error_human ?(prefix="") pp root fmt l =
  let (deps,res) = List.partition (function Dependency _ -> true |_ -> false) l in
  let pp_package pkg =
    let (p,v,fields) = pp pkg in
    Format.sprintf "(%s %s)" p v
  in
  let pp_dependencies fmt pathlist =
    List.iter (fun path ->
      List.iter (fun (i,vpkgs) ->
        if i.Cudf.package <> "dose-dummy-request" then
          Format.fprintf fmt "%s%s@." prefix (pp_package i)
      ) path
    ) pathlist
  in
  let pp_reason fmt = function
    |Conflict (i,j,vpkg) -> begin
      Format.printf "%sThere is a conflict " prefix;
      Format.printf "between package %s and package %s@." (pp_package i) (pp_package j);
      if deps <> [] then begin
          let pl1 = create_pathlist root (Dependency(i,[],[])::deps) in
          let pl2 = create_pathlist root (Dependency(j,[],[])::deps) in
          if pl1 <> [[]] then pp_dependencies fmt pl1;
          if pl2 <> [[]] then pp_dependencies fmt pl2;
        end
    end
    |Missing (i,vpkgs) -> begin
      Format.printf "%sThe dependency %a of package %s cannot be satisfied@." prefix (pp_vpkglist pp) vpkgs (pp_package i);
      let pl = create_pathlist root (Dependency(i,vpkgs,[])::deps) in
      if pl <> [[]] then pp_dependencies fmt pl
    end
    |_ -> assert false 
  in
  if root.Cudf.package = "dose-dummy-request" then
    Format.printf "%sThe request cannot be satisfied@," prefix
  else
    Format.printf "%sThe request for package %s cannot be satisfied@," prefix (pp_package root);
  List.iter (pp_reason fmt) res
;;

let fprintf_human ?(pp=default_pp) ?(prefix="") fmt = function
  |{result = Failure f ; request = Package r } -> 
         print_error_human ~prefix pp r fmt (f ());
  |{result = Failure f ; request = PackageList rl } -> 
      let n = List.length rl in
      List.iteri (fun i r ->
        print_error_human ~prefix pp r fmt (f ());
        if i <> (n-1) then Format.fprintf fmt "@."
      ) rl;
  |_ -> ()
;;

let add_missing rl = function
  | Dependency (pkg,vpkgs,pkglist) ->
    let rec aux acc pkglist = function
      | [] -> acc
      | (name,cstr)::tl ->
	if not (List.exists (fun p -> name = (Cudf.lookup_package_property p "package")) pkglist)
	then aux ((name,cstr)::acc) pkglist tl
	else aux acc pkglist tl
    in 
    let vpkglist = aux [] pkglist vpkgs in
    if List.length vpkglist > 0 then (Missing (pkg,List.rev vpkglist))::rl
    else rl
  | _ -> assert false
;;	  

let add_missings rl =
  let deps = List.filter (function Dependency _ -> true |_ -> false) rl in
  let rec aux acc = function
    | [] -> acc
    | dep::tl -> aux (add_missing acc dep) tl
  in 
  aux rl deps
;;

let fprintf ?(pp=default_pp) ?(failure=false) ?(success=false) ?(explain=false) ?(minimal=false) ?(addmiss=false) fmt d = 
  match d with
  |{result = Success f; request = req } when success ->
       Format.fprintf fmt "@[<v 1>-@,";
       begin match req with
       |Package r -> 
           Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true pp) r
       |PackageList rl -> 
           Format.fprintf fmt "coinst: %s@," 
           (String.concat " , " (List.map CudfAdd.string_of_package rl));
       end;
       Format.fprintf fmt "status: ok@,";
       if explain then begin
         let is = get_installationset ~minimal d in
         if is <> [] then begin
           Format.fprintf fmt "@[<v 1>installationset:@," ;
           Format.fprintf fmt "@[<v>%a@]" (pp_list (pp_package pp)) is;
           Format.fprintf fmt "@]"
         end
       end;
       Format.fprintf fmt "@]@,"
  |{result = Failure f; request = Package r } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "@[<v>%a@]@," (pp_package ~source:true pp) r;
       Format.fprintf fmt "status: broken@,";
       if explain then begin
	 let fl = if addmiss then add_missings (f ()) else (f ()) in
         Format.fprintf fmt "@[<v 1>reasons:@,";
         Format.fprintf fmt "@[<v>%a@]" (print_error pp r) fl;
         Format.fprintf fmt "@]"
       end;
       Format.fprintf fmt "@]@,"
  |{result = Failure f; request = PackageList rl } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "coinst: %s@," (String.concat " , " (List.map CudfAdd.string_of_package rl));
       Format.fprintf fmt "status: broken@,";
       Format.fprintf fmt "@]@,";
       if explain then begin
         Format.fprintf fmt "@[<v 1>reasons:@,";
         List.iter (fun r -> 
           Format.fprintf fmt "@[<v>%a@]@," (print_error pp r) (f ());
         ) rl;
        Format.fprintf fmt "@]@,"
       end;
  |_ -> ()
;;

let printf ?(pp=default_pp) ?(failure=false) ?(success=false) ?(explain=false) d =
  fprintf ~pp ~failure ~success ~explain Format.std_formatter d

let collect results d = 
  let add h k v =
    try let l = ResultHash.find h k in l := v :: !l
    with Not_found -> ResultHash.add h k (ref [v])
  in
  match d with 
  |{result = Failure (f) ; request = Package r } -> 
      List.iter (fun reason ->

        match reason with
        |Conflict (i,j,_) ->
            add results.summary reason r;
            results.conflict <- results.conflict + 1
        |Missing (i,vpkgs) ->
            add results.summary reason r;
            results.missing <- results.missing + 1
        |_ -> ()
      ) (f ())
  |_  -> ()
;;

let pp_summary_row explain pp fmt = function
  |(Conflict (i,j,_),pl) ->
      Format.fprintf fmt "@[<v 1>conflict:@,";
      Format.fprintf fmt "@[<v 1>pkg1:@,%a@]@," (pp_package pp) i;
      Format.fprintf fmt "@[<v 1>pkg2:@,%a@]@," (pp_package pp) j;
      Format.fprintf fmt "@[<v 1>broken-by: %d@]@," (List.length pl);
      if explain then begin
        Format.fprintf fmt "@[<v 1>packages:@," ;
        pp_list (pp_package ~source:true pp) fmt pl;
        Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]"
  |(Missing (i,vpkgs) ,pl) -> 
      Format.fprintf fmt "@[<v 1>missing:@,";
      Format.fprintf fmt "@[<v 1>unsat-dependency: %a@]@," (pp_vpkglist pp) vpkgs;
      Format.fprintf fmt "@[<v 1>broken-by: %d@]@," (List.length pl);
      if explain then begin
        Format.fprintf fmt "@[<v 1>packages:@," ;
        pp_list (pp_package ~source:true pp) fmt pl;
        Format.fprintf fmt "@]"
      end;
      Format.fprintf fmt "@]"
  |_ -> ()
;;

let pp_summary ?(pp=default_pp) ?(explain=false) () fmt result = 
  let l =
    ResultHash.fold (fun k v acc -> 
      let l1 = Util.list_unique !v in
      begin match k with
        |Conflict(_,_,_) -> result.unique_conflict <- result.unique_conflict + 1;
        |Missing(_,_) -> result.unique_missing <- result.unique_missing +1;
        |_ -> ()
      end;
      if List.length l1 > 1 then (k,l1)::acc else acc 
    ) result.summary [] 
  in
  let l = List.sort ~cmp:(fun (_,l1) (_,l2) -> (List.length l2) - (List.length l1)) l in

  Format.fprintf fmt "@[";
  Format.fprintf fmt "missing-packages: %d@." result.missing;
  Format.fprintf fmt "conflict-packages: %d@." result.conflict;
  Format.fprintf fmt "unique-missing-packages: %d@." result.unique_missing;
  Format.fprintf fmt "unique-conflict-packages: %d@." result.unique_conflict;
  Format.fprintf fmt "@]";

  Format.fprintf fmt "@[<v 1>summary:@," ;
  pp_list (pp_summary_row explain pp) fmt l;
  Format.fprintf fmt "@]"
;;

(* ------ Explain.ml ?? -----------*)

let rmissing = function
  | Dependency (pkg,vpkgs,pkglist) ->
    (let rec aux acc pkglist = function
      | [] -> acc
      | (name,cstr)::tl ->
	if not (List.exists (fun p -> name = (Cudf.lookup_package_property p "package")) pkglist)
	then aux ((name,cstr)::acc) pkglist tl
	else aux acc pkglist tl
    in 
    let vpkglist = aux [] pkglist vpkgs in
    if List.length vpkglist > 0 then Some (RMissing ([pkg],vpkgs,List.rev vpkglist))
    else None)
  | _ -> assert false
;;
	  

let rmissings rl =
  let deps = List.filter (function Dependency _ -> true |_ -> false) rl in
  let rec aux acc = function
    | [] -> acc
    | dep::tl -> match (rmissing dep) with
                 | Some m -> aux (m::acc) tl
		 | None -> aux acc tl
  in 
  aux [] deps
;;


let reduced_reason = function
  | Dependency (p,vpkgs,pkgs) ->
    RDependency ([p], vpkgs, List.map (fun x -> [x]) pkgs)
  | Missing (p,vpkgs) ->
    RMissing ([p],vpkgs,vpkgs)
  | Conflict (p1,p2,vpkg) ->
    RConflict ([p1],[p2],vpkg)

let reduced_reasons rl =
  let partial_missings = rmissings rl in
  partial_missings@(List.map reduced_reason (List.unique rl))


let pkg_cmp pkg1 pkg2 =
  let n1 = Cudf.lookup_package_property pkg1 "package" in
  let n2 = Cudf.lookup_package_property pkg2 "package" in
  let name_cmp = Pervasives.compare n1 n2 in
  if name_cmp = 0 
  then 
    let v1 = Cudf.lookup_package_property pkg1 "version" in
    let v2 = Cudf.lookup_package_property pkg2 "version" in
    Pervasives.compare v1 v2
  else name_cmp
;;

(** Returns the Node node dependencies, in rrl, under conjunctive normal form
    and the list of pkg it's in conflict with *)
let cnfdeps_and_conflicts node rrl = 
  let (deps,conflicts) =
  List.fold_left (fun (d,c) rr -> match rr with
  | RDependency (n,vpkgs,_) when n = node -> (vpkgs::d,c)
  | RMissing (n,vpkgs,vpkgspart) when (n = node && vpkgs = vpkgspart) -> (vpkgs::d,c)
  | RConflict (n1,n2,_) -> 
    if n1 = node 
    then (d,n2::c)
    else if n2 = node then (d,n1::c)
    else (d,c)
  | _ -> (d,c)
  ) ([],[]) rrl
  in
  (List.map (List.sort ~cmp:(fun (n1,_) (n2,_) -> Pervasives.compare n1 n2)) deps,
   List.sort ~cmp:pkg_cmp (List.flatten conflicts))
;;


(** Return true if c2 is included in c1, false otherwise *)
let cstr_include c1 c2 = 
  match c1,c2 with
  | None, _ -> true
  | Some (`Geq, 1), _ -> true
  | x,y when x = y -> true
  | Some (`Gt,v), Some (`Gt,w) -> w >= v
  | Some (`Gt,v), Some (`Geq,w) -> w > v
  | Some (`Gt,v), Some (`Eq,w) -> w > v
  | Some (`Gt,v), Some (`Neq,w) -> (v = 1) && (w = 1)
  | Some (`Geq,v), Some (`Gt,w) -> w >= v - 1
  | Some (`Geq,v), Some (`Geq,w) -> w >= v
  | Some (`Geq,v), Some (`Eq,w) -> w >= v
  | Some (`Geq,v), Some (`Neq,w) -> (v = 2) && (w = 1)
  | Some (`Lt,v), Some (`Lt,w) -> w <= v
  | Some (`Lt,v), Some (`Leq,w) -> w < v
  | Some (`Lt,v), Some (`Eq,w) -> w < v
  | Some (`Leq,v), Some (`Lt,w) -> w < v + 1
  | Some (`Leq,v), Some (`Leq,w) -> w <= v
  | Some (`Leq,v), Some (`Eq,w) -> w <= v
  | Some (`Neq,v), Some (`Eq,w) -> w <> v
  | Some (`Neq,v), Some (`Neq,w) -> w = v
  | Some (`Neq,v), Some (`Lt,w) -> w <= v
  | Some (`Neq,v), Some (`Leq,w) -> w < v
  | Some (`Neq,v), Some (`Gt,w) -> w >= v
  | Some (`Neq,v), Some (`Geq,w) -> w > v
  | _ -> false
;;


(** Return true if the constraints disjunction l2 is included in l1, false otherwise
    It assumes that both lists or sorted in alphanumeric order on package name *)
let or_include l1 l2 = 
  let rec aux il1 l1 l2 =
    match l1, l2 with
    | _, [] -> true
    | [], _ -> false
    | (n1,c1)::tl1, (n2,c2)::tl2 -> 
      (match Pervasives.compare n1 n2 with
      | 0 -> if cstr_include c1 c2 then aux il1 il1 tl2
	else aux il1 tl1 l2
      | x when x < 0 -> aux il1 tl1 l2
      | _ -> false)
  in aux l1 l1 l2
;;

(** Return true if the constraints under conjunctive normal form in l2 are included in l1, false otherwise
    It assumes that in both lists, every disjunction are sorted in alphanumeric order on package name *)
let and_include l1 l2 =
  let rec aux il2 l1 l2 =
    match l1,l2 with
    | [],_ -> true
    | _, [] -> false
    | dj1::tl1, dj2::tl2 -> if or_include dj1 dj2 then aux il2 tl1 il2
      else aux il2 l1 tl2
  in aux l2 l1 l2
;;

(** rr_mem pkg rr returns true if the package pkg appears in the reducedReason rr *)
let rr_mem node = function
  | RDependency (n,_,_) -> n = node
  | RConflict (n1,n2,_) -> n1 = node or n2 = node
  | RMissing (n,_,_) -> n = node
;;

let conflict_mem node = function
  | RConflict (n1,n2,_) -> n1 = node or n2 = node
  | _ -> false
;;

let cone_rules node rrl = 
  let initial = List.filter (rr_mem node) rrl in
  let rec aux_node acc node = function
    | [] -> acc
    | rr::tl -> if rr_mem node rr && not (List.mem rr acc)
      then aux_node (rr::acc) node tl
      else aux_node acc node tl
  in 
  let aux_rr acc rr rrl =
    match rr with
    | RDependency (_,_,nl) ->
      List.fold_left (fun acc' n -> aux_node acc' n rrl) acc nl
    | _ -> acc
  in
  let rec aux acc rrl =
    let to_process = List.hd acc in
    let next_step =
      List.fold_left (fun acc rr -> aux_rr acc rr rrl) [] to_process
    in
    if next_step = [] then acc else aux (next_step::acc) rrl
  in
  List.unique (List.flatten (aux [initial] rrl))
;;    
    
(** Return the reducedReason list rrl without the irrelevant reasons
    considering the and_context reducedReason list *)
let remove_irrelevant and_context root rrl =
  let relevant and_context rrl = function
    | RDependency (_,_,nl) -> 
      not (List.exists (fun node -> not (List.exists (rr_mem node) rrl)) nl)
    | RConflict (n1,n2,_) -> 
      List.exists (conflict_mem n1) and_context
      or
      List.exists (conflict_mem n2) and_context
      or
      (
	(List.exists (function RDependency (_,_,nl) -> List.mem n1 nl | _ -> false) rrl or n1 = root)
	&&
	(List.exists (function RDependency (_,_,nl) -> List.mem n2 nl | _ -> false) rrl or n2 = root)
      )
    | RMissing (n,c1,c2) ->
      if c1 <> c2
      then List.exists (function RDependency (n',c',_) -> n' = n && c' = c1 | _ -> false) rrl
      else 
	List.exists (function RDependency (_,_,nl) -> List.mem n nl | _ -> false) rrl
	or
	n = root
  in 
  let rec aux length and_context rrl =
    let filtered = List.filter (relevant and_context rrl) rrl in
    let newlength = List.length filtered in
    if newlength < length then aux newlength and_context filtered
    else filtered
  in
  aux (List.length rrl) and_context rrl
;;

let proper_add elt list =
  if List.mem elt list then list else elt::list
;;

let simplify dep rrl = 
  match dep with
  | RDependency (root,cstr,root_nl) ->
    let size = List.length root_nl in
    let deps_table = Hashtbl.create size in
    List.iter (fun n -> Hashtbl.add deps_table n (cnfdeps_and_conflicts n rrl)) root_nl;
    let rec aux acc not_processed domchng current = function
      | [] -> if domchng
	then aux acc [] false current not_processed
	else
	(match not_processed with
	| [] -> current::acc
	| hd::tl -> aux (current::acc) [] false (hd,[hd]) tl)
      | n::tl -> 
	let (dom,grp) = current in
	(match (Hashtbl.find deps_table dom),(Hashtbl.find deps_table n) with
	| ([],conf1),([],conf2) ->
	  if conf1 = conf2
	  then aux acc not_processed domchng (dom,n::grp) tl
	  else aux acc (n::not_processed) domchng (dom,grp) tl
	| (deps1,[]),(deps2,[]) ->
	  if and_include deps1 deps2
	  then aux acc not_processed domchng (dom,n::grp) tl
	  else if and_include deps2 deps1
	  then aux acc not_processed true (n,n::grp) tl
	  else aux acc (n::not_processed) domchng current tl
	| (deps1,conf1),(deps2,conf2) ->
	  if conf1 = conf2
	  then
	    (if and_include deps1 deps2
	     then aux acc not_processed domchng (dom,n::grp) tl
	     else if and_include deps2 deps1
	     then aux acc not_processed true (n,n::grp) tl
	     else aux acc (n::not_processed) domchng current tl)
	  else aux acc (n::not_processed) domchng current tl
	)
	
    in
    let delete (dom,grp) rrl =
      List.filter
	(fun rr -> match rr with
	| RDependency (n,c,nl) when n <> dom && (List.mem n grp) -> false
	| RConflict (n1,n2,c) when (n1 <> dom && List.mem n1 grp) or (n2 <> dom && List.mem n2 grp) -> false
	| _ -> true
	) rrl
    in 
    let update (dom,grp) rrl =
      List.map
	(fun rr -> match rr with 
	| RDependency (n,c,nl) when n = dom -> 
	  RDependency (List.flatten grp,c,nl)
	| RMissing (n,c1,c2) when List.mem n grp ->
	  let global_cstrnt = List.find (fun cl -> or_include cl c1) (fst (Hashtbl.find deps_table dom)) in
	  RMissing (List.flatten grp,global_cstrnt,c2)
	| RConflict (n1,n2,c) ->
	  if  n1 = dom then RConflict (List.flatten grp,n2,c)
	  else if n2 = dom then RConflict (n1,List.flatten grp,c)
	  else RConflict (n1,n2,c)
	| rr -> rr
	) rrl
    in
    let update_root new_nl rrl = 
      List.map (function RDependency (n,c,_) when n = root && c = cstr -> RDependency (n,c,new_nl) | rr -> rr) rrl
    in
    (match root_nl with
    | [] -> assert false
    | hd::tl -> 
      let cpll = aux [] [] false (hd,[hd]) tl in
      let new_nl = List.fold_left (fun acc (_,grp) -> (List.flatten grp)::acc) [] cpll in
      List.unique (
	List.fold_left (fun acc cpl -> update cpl (delete cpl acc)) (update_root new_nl rrl) cpll)
    )
  | _ -> assert false
;;



(* 
let remove_outdated rrl root =
  let rec aux rrl size =
    let res = 
      List.filter
      (fun rr -> match rr with
      | RDependency (n,_,_) when n <> root -> 
	List.exists (function RDependency (_,_,nl) -> List.mem n nl | _ ->  false) rrl
      | RMissing (n,_,_) -> 
	List.exists (function RDependency (_,_,nl) -> List.mem n nl | _ -> false) rrl
      | RConflict (n1,n2,_) ->
	List.exists (function RDependency (_,_,nl) -> List.mem n1 nl | _ -> false) rrl
	&&
	List.exists (function RDependency (_,_,nl) -> List.mem n2 nl | _ -> false) rrl
      | _ -> true
      ) rrl
    in
    let new_size = List.length res in
    if new_size = size then res else aux res new_size
  in
  aux rrl (List.length rrl)
;;
*)

let add_dep_edge graph src dst cstr =
  let edge = (src,ExplanationGraph.ExplE.Depends cstr,dst) in
  ExplanationGraph.G.add_edge_e graph edge
;;

let add_dep graph global_id or_id root_node dep partial_missing =
  match dep with
  | RDependency (_,c,nl) ->
    let disjunction = List.length nl > 1 or partial_missing <> None  in
    if disjunction
    then
      (match root_node with
      | ExplanationGraph.ExplV.Pkgs (id,n) ->
	let or_node = ExplanationGraph.ExplV.Or (id,n,or_id) in
	add_dep_edge graph root_node or_node c;
	let (g_id,node_list) =
	  List.fold_left 
	    (fun (id,node_l) n -> 
	      let node = ExplanationGraph.ExplV.Pkgs (id,n) in
	      add_dep_edge graph or_node node c;
	      (id + 1, node::node_l)
	    ) (global_id,[]) nl
	in
	(match partial_missing with
	| None -> ()
	| Some RMissing (_,c1,c2) ->
	  let pm_node = ExplanationGraph.ExplV.Missing c2 in
	  add_dep_edge graph or_node pm_node c1
	| _ -> assert false
	);
	(g_id,or_id + 1,node_list)
      | _ -> assert false)
    else
      let next_node = ExplanationGraph.ExplV.Pkgs (global_id, List.hd nl) in
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
  | ExplanationGraph.ExplV.Pkgs (id,root) ->
    let domain = remove_irrelevant context root (cone_rules root rrl) in
    let root_deps = List.filter (function RDependency (n,_,_) when n = root -> true | _ -> false) domain in
    let root_confs = List.filter (conflict_mem root) domain in
    (match List.length root_deps with
    | 0 -> 
      if root_confs <> [] then (global_id,[root_node])
      else
	(match List.hd domain with
	| RMissing (_,c1,c2) ->
	  let miss_node = ExplanationGraph.ExplV.Missing c2 in
	  add_dep_edge graph root_node miss_node c1;
	  (global_id,[])
	| _ -> assert false)
    | dep_nmbr ->
      let next_context,self_cn = if root_confs <> [] then root_confs@context,[root_node] else context,[] in
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
	    let partial_missing =
	      try
		Some (List.find (function RMissing (n,c1,c2) when n = root && c1 <> c2 && c1 = cstr -> true | _ -> false) simplified)
	      with Not_found ->
		None
	    in
	    let (id,or_id',next_roots) = add_dep graph global_id or_id root_node pr partial_missing in
	    let next_id,cnl' =
	      List.fold_left 
		(fun (id',conflicting_node_list) node ->
		  let cntxt = next_context@(Hashtbl.find context_tbl dep) in 
		  let (id',cnl) = build_deps graph cn_tbl id' cntxt node simplified in
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

let add_conflict_edge graph src dst =
  let edge = (src,ExplanationGraph.ExplE.Conflict,dst) in
  if not (ExplanationGraph.G.mem_edge_e graph (dst,ExplanationGraph.ExplE.Conflict,src))
  then ExplanationGraph.G.add_edge_e graph edge
  else ()
;;


let father graph vertex = 
  let el = ExplanationGraph.G.pred_e graph vertex in
  let vl = List.fold_left 
    (fun acc (src,label,_) -> 
      (match label with
      | ExplanationGraph.ExplE.Conflict -> acc
      | _ -> src::acc
      )
    ) [] el 
  in
  List.hd vl
;;

let rec conflict_partners graph cn_tbl vertex = 
  try 
    Hashtbl.find cn_tbl vertex
  with Not_found ->
    conflict_partners graph cn_tbl (father graph vertex)
;;

let rec add_conflict graph src partner = function
  | pkg::tl ->
    (match partner with
    | ExplanationGraph.ExplV.Pkgs (_,pl) ->
      if (List.mem pkg pl) or (List.mem pkg pl)
      then add_conflict_edge graph src partner	
      else add_conflict graph src partner tl
    | _ -> assert false)
  | [] -> ()
  | _ -> assert false
;;

let build_conf graph cn_tbl cnl cl =
  List.iter
    (fun vertex ->
      (match vertex with
      | ExplanationGraph.ExplV.Pkgs (_,pl) ->
	let conflicts = 
	  List.fold_left
	    (fun acc r -> match r with 
	    | Conflict (p1,p2,_) -> 
	      if (List.mem p1 pl)
	      then p2::acc
	      else if (List.mem p2 pl)
	      then p1::acc
	      else acc
	    | _ -> acc
	    ) [] cl
	in 
	let partners = List.filter (fun v -> v <> vertex) (conflict_partners graph cn_tbl vertex) in
	List.iter
	  (fun prtnr ->
	    add_conflict graph vertex prtnr conflicts
	  ) partners
      | _ -> assert false)
    ) cnl
;;
  

let build_expl rl pkg =
  let rrl = reduced_reasons rl in
  let root = [pkg] in
  let expl_graph = ExplanationGraph.G.create () in
  let root_node = ExplanationGraph.ExplV.Pkgs (0,root) in
  ExplanationGraph.G.add_vertex expl_graph root_node;
  let conflicting_nodes_table = Hashtbl.create 10 in 
  let cnl = snd (build_deps expl_graph conflicting_nodes_table 1 [] root_node rrl) in
  let cl = List.filter (function Conflict _ -> true | _ -> false) rl in
  build_conf expl_graph conflicting_nodes_table cnl cl;
  expl_graph
;;


