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

(** Specialized Ocamlgraph modules *)

open ExtLib
open Graph
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

let tr_timer = Util.Timer.create "Defaultgraph.GraphOper.transitive_reduction"
let trbar = Util.Progress.create "Defaultgraph.GraphOper.transitive_reduction"

(** generic operation over imperative graphs *)
module GraphOper (G : Sig.I) = struct

  (** transitive reduction.  Uses the transitive reduction algorithm from The
      Transitive Reduction of a Directed Graph, Aho, Garey and Ullman, 1972 - 
      with the proviso that we know that our graph already is a transitive 
      closure *)
  (* this is a VERY expensive operation on Labelled graphs ... *)
  let transitive_reduction graph =
    Util.Progress.set_total trbar (G.nb_vertex graph);
    Util.Timer.start tr_timer;
    G.iter_vertex (fun v ->
      Util.Progress.progress trbar;
      G.iter_succ (fun w ->
        if not(G.V.equal v w) then
        G.iter_succ (fun z ->
          if not(G.V.equal w z) then
            G.remove_edge graph v z
        ) graph w
      ) graph v;
    ) graph;
    Util.Timer.stop tr_timer ();
    Util.Progress.reset trbar
  ;;

  module O = Oper.I(G) 
  module S = Set.Make(G.V)

  (** extract the subgraph induced by the list l *)
  let subgraph g l =
    let to_set l = List.fold_left (fun s v -> S.add v s) S.empty l in
    let s = to_set l in
    let sg = G.create () in
    S.iter (fun v1 ->
      G.add_vertex sg v1;
      List.iter (fun e ->
        let v2 = G.E.dst e in
        if S.mem v2 s then
          G.add_edge_e sg e
      ) (G.succ_e g v1)
    ) s;
    sg
  ;;

end

(** syntactic dependency graph. Vertex are Cudf packages and
    are indexed considering only the pair name,version .
    Edges are labelled with
    - [OrDepends] : disjuctive dependency
    - [DirDepends] : direct dependecy 
    - [Conflict] : conflict
    *) 
module SyntacticDependencyGraph = struct

  module PkgV = struct
      type t = Pkg of Cudf.package | Or of (Cudf.package * int)
      let compare x y = match (x,y) with
        |Or (p1,i1), Or (p2,i2) when (i1 = i2) && (CudfAdd.equal p1 p2) -> 0
        |Pkg p1, Pkg p2 -> CudfAdd.compare p1 p2
        |_, _ -> Pervasives.compare x y
      let hash = function
        |Pkg p -> Hashtbl.hash (p.Cudf.package,p.Cudf.version)
        |Or (p,i) -> Hashtbl.hash (p.Cudf.package,p.Cudf.version,i)
      let equal x y = match (x,y) with
        |Or (p1,i1), Or (p2,i2) -> (i1 = i2) && (CudfAdd.equal p1 p2)
        |Pkg p1, Pkg p2 -> CudfAdd.equal p1 p2
        |_ -> false
  end

  module PkgE = struct
    type t = OrDepends | DirDepends | Conflict

    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal x y = ((compare x y) = 0)
    let default = DirDepends
  end

  module G = Imperative.Digraph.ConcreteBidirectionalLabeled(PkgV)(PkgE)

  let string_of_vertex vertex =
    match G.V.label vertex with
    |PkgV.Pkg p -> Printf.sprintf "Pkg %s" (CudfAdd.string_of_package p)
    |PkgV.Or (p, _) -> Printf.sprintf "Or %s" (CudfAdd.string_of_package p)

  let string_of_edge edge =
    let label =
      match G.E.label edge with
      |PkgE.DirDepends -> "Direct"
      |PkgE.OrDepends -> "Disjunctive"
      |PkgE.Conflict -> "Conflict"
    in
    let src = G.E.src edge in
    let dst = G.E.dst edge in
    Printf.sprintf "%s %s %s"
      (string_of_vertex src)
      label
      (string_of_vertex dst)

  module DotPrinter = struct
    module Display = struct
        include G
        let vertex_name v =
          match G.V.label v with
          |PkgV.Pkg i -> Printf.sprintf "\"%s\"" (CudfAdd.string_of_package i)
          |PkgV.Or (i,c) -> Printf.sprintf "\"Or%s-%d\"" (CudfAdd.string_of_package i) c

        let graph_attributes = fun _ -> [`Rankdir `LeftToRight]
        let get_subgraph = fun _ -> None

        let default_edge_attributes = fun _ -> []
        let default_vertex_attributes = fun _ -> [`Shape `Box]

        let vertex_attributes v =
          match G.V.label v with
          |PkgV.Or _ -> [`Label "Or" ; `Shape `Diamond]
          |PkgV.Pkg p when p.Cudf.installed -> [ `Color 0x00FF00 ]
          |_ -> []

        let edge_attributes e =
          match G.E.label e with
          |PkgE.DirDepends -> [`Style `Solid]
          |PkgE.OrDepends -> [`Style `Dashed]
          |PkgE.Conflict -> [`Color 0xFF0000; `Style `Solid; `Label "#"]
(*
          |PkgE.DirDepends -> [`Style [`Solid]]
          |PkgE.OrDepends -> [`Style [`Dashed]]
          |PkgE.Conflict -> [`Color 0xFF0000; `Style [`Solid]; `Label "#"]
*)
      end
    include Graph.Graphviz.Dot(Display)
    let print fmt g = fprint_graph fmt g
  end 
  module S = Set.Make(PkgV)

  module GmlPrinter = Gml.Print (G) (
    struct
       let node (v: G.V.label) = []
       let edge (e: G.E.label) = []
    end)

  module GraphmlPrinter = Graphml.Print (G) (
    struct
        let vertex_properties =
            ["package","string",None;
             "version","string",None;
             "architecture","string",None;
             "type","string",None;
             "source","string",None;
             "sourcenumber","string",None;
             "multiarch","string",None;
            ]
       
        let edge_properties = [
          "vpkglist","string",None;
          "binaries","string",None;
          ]

        let map_edge e = []
        let map_vertex = function
          |PkgV.Pkg pkg ->
            let name = ("package",CudfAdd.decode pkg.Cudf.package) in
            let version = ("version",CudfAdd.string_of_version pkg) in
       
            let props =
              List.filter_map (fun (key,_,_) ->
                try let value = Cudf.lookup_package_property pkg key in
                Some(key,value)
                with Not_found -> None
              ) vertex_properties
            in
            name :: version :: props
          |PkgV.Or (pkg, _) -> []

        let edge_uid e = Hashtbl.hash e
        let vertex_uid v = Hashtbl.hash v

    end)

  let depgraphbar = Util.Progress.create "SyntacticDependencyGraph.dependency_graph"

  (** Build the syntactic dependency graph from the given cudf universe *)
  let dependency_graph univ =
    let timer = Util.Timer.create "SyntacticDependencyGraph.dependency_graph" in
    Util.Timer.start timer;
    let conflicts = CudfAdd.init_conflicts univ in
    Util.Progress.set_total depgraphbar (Cudf.universe_size univ);
    let gr = G.create () in
    Cudf.iter_packages (fun pkg ->
      Util.Progress.progress depgraphbar;
      let vpid = G.V.create (PkgV.Pkg pkg) in
      G.add_vertex gr vpid;
      let c = ref 0 in
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps univ vpkgs with 
        |[] -> ()
        |[p] ->
            let vp = G.V.create (PkgV.Pkg p) in
            let edge = G.E.create vpid PkgE.DirDepends vp in
            G.add_edge_e gr edge
        |l ->
            begin
              let vor = G.V.create (PkgV.Or (pkg,!c)) in
              let edgeor = G.E.create vpid PkgE.OrDepends vor in
              G.add_edge_e gr edgeor;
              incr c;
              List.iter (fun p ->
                let vp = G.V.create (PkgV.Pkg p) in
                let oredge = G.E.create vor PkgE.OrDepends vp in
                G.add_edge_e gr oredge
              ) l
            end
      ) pkg.Cudf.depends
      ;
      List.iter (fun p ->
        if not(CudfAdd.equal p pkg) then
          let vp = G.V.create (PkgV.Pkg p) in
          let edge = G.E.create vpid PkgE.Conflict vp in
          G.add_edge_e gr edge
      ) (CudfAdd.who_conflicts conflicts univ pkg)
    ) univ
    ;
    Util.Timer.stop timer gr
  ;;

end

(******************************************************)


module ExplanationGraph = struct

  module ExplV = struct
      type t = Pkgs of (int * Cudf.package list) 
	       | Or of (int * Cudf.package list * int) 
	       | Missing of (Cudf_types.vpkg list)
      let compare x y = 
	match x,y with
	| Pkgs (id1,_), Pkgs (id2,_) -> Pervasives.compare id1 id2
	| Or (id1,_,or_id1), Or(id2,_,or_id2) -> Pervasives.compare (id1,or_id1) (id2,or_id2)
	| Missing c1, Missing c2 -> Pervasives.compare c1 c2 (*TODO*)
	| _ -> Pervasives.compare x y
      let hash = function
	| Pkgs (id,_) -> Hashtbl.hash id
	| Or (id,_,or_id) -> Hashtbl.hash (id,or_id)
	| m -> Hashtbl.hash m (*TODO*)
      let equal x y = 
	match x,y with
	| Pkgs (id1,_), Pkgs (id2,_) -> id1 = id2
	| Or (id1,_,or_id1), Or(id2,_,or_id2) -> id1 = id2 && or_id1 = or_id2
	| Missing c1, Missing c2 -> c1 = c2
	| _ -> false
  end

  module ExplE = struct
    type t = Depends of Cudf_types.vpkg list | Conflict
    let compare = Pervasives.compare (*TODO*)
    let hash = Hashtbl.hash (*TODO*)
    let equal x y = ((compare x y) = 0)
    let default = Conflict
  end

  module G = Imperative.Digraph.ConcreteBidirectionalLabeled(ExplV)(ExplE)
  
  let string_of_op = function
    | `Eq -> "="
    | `Neq -> "!="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"

  let string_of_vpkg = function
    | (n, None) -> n
    | (n, Some (op,v)) -> let sop = (string_of_op op) in
			  Printf.sprintf "%s (%s %d)" n sop v

  let string_of_pkg pkg =
    let n = Cudf.lookup_package_property pkg "package" in
    let v = Cudf.lookup_package_property pkg "version" in
    Printf.sprintf "(%s, %s)" n v

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

  let string_of_node = string_of_list string_of_pkg

  let string_of_vertex v =
    match v with
    | ExplV.Pkgs (i,n) -> Printf.sprintf "Vertex : Pkgs (%d, %s)\n" i (string_of_node n)
    | ExplV.Or (i,n,or_i) ->  Printf.sprintf "Vertex : Or (%d, %s, %d)\n" i (string_of_node n) or_i
    | ExplV.Missing c ->  Printf.sprintf "Vertex : Missing %s\n" (string_of_list (string_of_vpkg) c)

  let string_of_label l = 
    match l with
    | ExplE.Conflict -> "#"
    | ExplE.Depends c -> Printf.sprintf "Depends : %s" (string_of_list (string_of_vpkg) c)

  let string_of_edge e =
    match e with
    | (src,label,dst) -> Printf.sprintf "Edge : \n\tSource : %s\tLabel : %s\n\tDest : %s\n" (string_of_vertex src) (string_of_label label) (string_of_vertex dst)

  let print_egraph g =
    Printf.printf "Graph : \n\n";
    G.iter_vertex 
      (fun v ->
	Printf.printf "%s \n" (string_of_vertex v)
      )	g;
    G.iter_edges_e
      (fun e ->
	Printf.printf "%s \n" (string_of_edge e)
      ) g

end


(** Imperative bidirectional graph for dependecies. *)
(** Imperative unidirectional graph for conflicts. *)
(* Note: ConcreteBidirectionalLabelled graphs are slower and we do not use them
 * here *)
module MakePackageGraph(PkgV : Sig.COMPARABLE with type t = Cudf.package )= struct

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)
  module UG = Imperative.Graph.Concrete(PkgV)
  module O = GraphOper(G)
  module S = Set.Make(PkgV)

 
  module DotPrinter = struct
  module Display = struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (CudfAdd.string_of_package v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes p =
        if p.Cudf.installed then [ `Color 0x00FF00 ] else []

      let edge_attributes e = []
    end
    include Graph.Graphviz.Dot(Display)
    let print fmt g = fprint_graph fmt g
  end 

  module GmlPrinter = Gml.Print (G) (
    struct
       let node (v: G.V.label) = []
       let edge (e: G.E.label) = []
     end
  )

  module GraphmlPrinter = Graphml.Print (G)(
    struct
        let vertex_properties =
            ["package","string",None;
             "version","string",None;
             "architecture","string",None;
             "type","string",None;
             "source","string",None;
             "sourcenumber","string",None;
             "multiarch","string",None;
            ]
       
        let edge_properties = [
          "vpkglist","string",None;
          "binaries","string",None;
          ]

        let map_edge e = []
        let map_vertex pkg =
          let name = ("package",CudfAdd.decode pkg.Cudf.package) in
          let version = ("version",CudfAdd.string_of_version pkg) in
     
          let props =
            List.filter_map (fun (key,_,_) ->
              try let value = Cudf.lookup_package_property pkg key in
              Some(key,value)
              with Not_found -> None
            ) vertex_properties
          in
          name :: version :: props

        let edge_uid e = Hashtbl.hash e
        let vertex_uid v = Hashtbl.hash v

    end)

  (* Maintenance Of Transitive Closures And Transitive Reductions Of Graphs *)
  (* J.A. La Poutre and J. van Leeuwen *)
  let add_edge ?transitive graph i j =
    let rec adapt g k red =
      let new_red = 
        S.fold (fun l acc ->
          if not(G.V.equal k l) then G.add_edge g k l;
          G.fold_succ (fun m acc' ->
            if not (G.mem_edge g k m) then S.add m acc'
            else acc'
          ) g l acc
        ) red S.empty 
      in
      if S.is_empty new_red then ()
      else adapt g k new_red
    in
    let insert g i j =
      adapt g i (S.singleton j);
      G.iter_pred (fun k ->
        if not (G.mem_edge g k j) then
          adapt g k (S.singleton j)
      ) g i
    in
    match transitive with
    |None -> G.add_edge graph i j
    |Some true -> 
      (* add an edge and maintain the transitive clousure of the graph *)
      insert graph i j 
    |Some false ->
      (* TODO : add an edge and maintain the transitive reduction of the graph *)
      G.add_edge graph i j

  (** add to the graph all conjunctive dependencies of package id *)
  let conjdepgraph_int ?(transitive=false) graph univ p =
    G.add_vertex graph p;
    List.iter (fun vpkgs ->
      match CudfAdd.resolve_deps univ vpkgs with
      |[q] when not(CudfAdd.equal q p) -> add_edge ~transitive graph p q
      |_ -> ()
    ) p.Cudf.depends

  (** for all id \in idlist add to the graph all conjunctive dependencies *)
  let conjdepgraph univ idlist =
    let graph = G.create ~size:(List.length idlist) () in
    List.iter (conjdepgraph_int graph univ) idlist;
    graph

  (** given a graph return the conjunctive dependency closure of the package id *)
  let conjdeps graph =
    let h = Hashtbl.create (G.nb_vertex graph) in
    fun id ->
      try Hashtbl.find h id
      with Not_found -> begin
        let module Dfs = Traverse.Dfs(G) in
        let l = ref [] in
        let collect id = l := id :: !l in
        Dfs.prefix_component collect graph id;
        Hashtbl.add h id !l;
        !l
      end

  (** Build the dependency graph from the given cudf universe *)
  let dependency_graph ?(conjunctive=false) universe =
    let gr = G.create () in
    Cudf.iter_packages (fun pkg ->
      G.add_vertex gr pkg;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps universe vpkgs with
        |[p] -> G.add_edge gr pkg p
        |l when not conjunctive -> List.iter (G.add_edge gr pkg) l
        |_ -> ()
      ) pkg.Cudf.depends
    ) universe
    ;
    gr

  (** Build the dependency graph from the given list of packages *)
  let dependency_graph_list ?(conjunctive=false) universe pkglist =
    let gr = G.create () in
    List.iter (fun pkg ->
      G.add_vertex gr pkg;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps universe vpkgs with
        |[p] -> G.add_edge gr pkg p
        |l when not conjunctive -> List.iter (G.add_edge gr pkg) l
        |_ -> ()
      ) pkg.Cudf.depends
    ) pkglist
    ;
    gr

  (** Build the conflict graph from the given cudf universe *)
  let conflict_graph universe =
    let gr = UG.create () in
    Cudf.iter_packages (fun pkg ->
      List.iter (fun (pkgname,constr) ->
        List.iter (UG.add_edge gr pkg)
        (CudfAdd.who_provides universe (pkgname,constr))
      ) pkg.Cudf.conflicts
    ) universe
    ;
    gr

  let undirect graph =
    let gr = UG.create () in
    G.iter_edges (UG.add_edge gr) graph;
    G.iter_vertex (UG.add_vertex gr) graph;
    gr

  (** Return the list of connected component of an undirected graph *)
  let connected_components graph =
    let module C = Graph.Components.Make(UG) in
    C.scc_list graph

  let pred_list graph q =
    G.fold_pred (fun p acc -> p :: acc ) graph q []

  let succ_list graph q =
    G.fold_succ (fun p acc -> p :: acc ) graph q []

  let pred_set graph q =
    if G.mem_vertex graph q then
      G.fold_pred (fun p acc -> S.add p acc) graph q S.empty
    else S.empty

  let succ_set graph q =
    if G.mem_vertex graph q then
      G.fold_succ (fun p acc -> S.add p acc) graph q S.empty
    else S.empty

  let cycle_reduction g =
    let module Hashtbl = CudfAdd.Cudf_hashtbl in
    let module Set = CudfAdd.Cudf_set in
    let visited = Hashtbl.create (G.nb_vertex g) in
    let rec get_cycle res path v =
      match path with
      |[] -> fatal "No cycle in path!"
      |h::t when G.V.equal h v -> (t, res)
      |h::t -> get_cycle (h::res) t v
    in
    let reduce_cycle path v =
      (* Somewhere, v should be in path. This is the cycle. *)
      let (other, c) = get_cycle [] path v in
      let nv = 
        let name = String.concat "/" (List.sort ~cmp:compare (List.map (fun p -> p.Cudf.package) (v::c))) in
        { Cudf.default_package with
          Cudf.package = CudfAdd.encode name;
          Cudf.version = 1;
        }
      in
      G.add_vertex g nv;
      let s = CudfAdd.to_set c in
      List.iter (fun p ->
        if G.mem_vertex g p then begin
          G.iter_pred (fun q -> if not (Set.mem q s) then G.add_edge g q nv) g p;
          G.iter_succ (fun q -> if not (Set.mem q s) then G.add_edge g nv q) g p;
          G.remove_vertex g p;
        end;
        Hashtbl.remove visited p
      ) (v::c);
      (other, nv)
    in
    let rec visit path v =
      if G.mem_vertex g v then begin
        Hashtbl.add visited v true;
        G.iter_succ (fun w ->
          try
            if Hashtbl.find visited w then
              let (other, nv) = reduce_cycle (v::path) w in
              visit other nv
          with Not_found -> visit (v::path) w
        ) g v;
        Hashtbl.replace visited v false
      end
    in
    G.iter_vertex (fun v -> if not (Hashtbl.mem visited v) then visit [] v) g
  ;;

  let out ?(dump=None) ?(dot=None) ?(detrans=false) pkggraph =
    info "Dumping Graph : nodes %d , edges %d"
    (G.nb_vertex pkggraph) (G.nb_edges pkggraph) ;
    
    if detrans then begin
      O.transitive_reduction pkggraph;
      debug "After transitive reduction : nodes %d , edges %d"
      (G.nb_vertex pkggraph) (G.nb_edges pkggraph)
    end ;

    if dump <> None then begin
      let f = Option.get dump in
      debug "Saving marshal graph in %s\n" f ;
      let oc = open_out f in
      Marshal.to_channel oc ((detrans,pkggraph) :> (bool * G.t)) [];
      close_out oc
    end ;

    if dot <> None then begin
      let f = Option.get dot in
      debug "Saving dot graph in %s\n" f ;
      let oc = open_out f in
      DotPrinter.output_graph oc pkggraph;
      close_out oc
    end 

  let load pkglist filename =
    let timer = Util.Timer.create "Defaultgraph.PackageGrah.load" in
    Util.Timer.start timer;
    let ic = open_in filename in
    let (detrans,graph) = ((Marshal.from_channel ic) :> (bool * G.t)) in
    close_in ic ;
    info "Loading Strong Dependencies graph";
    (* we assume the graph is detransitivitized *)
    let sg =
      if detrans then begin 
        info "Computing transitive closure";
        (* O.add_transitive_closure graph *) 
        graph
      end else graph
    in
    Util.Timer.stop timer sg

end

(******************************************************)

module PkgV = struct
    type t = Cudf.package
    let compare = CudfAdd.compare
    let hash = CudfAdd.hash
    let equal = CudfAdd.equal
end

module PkgE = struct
  type t = float
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0.0
end

module PackageGraph = MakePackageGraph(PkgV)

(** Integer Imperative Bidirectional Graph *)
module IntPkgGraph = struct

  module PkgV = struct
      type t = int
      let compare = Pervasives.compare
      let hash i = i
      let equal = (=)
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)
  module S = Set.Make(PkgV)
  module O = GraphOper(G)

  module DotPrinter = struct
    module Display = struct
        include G
        let vertex_name uid = Printf.sprintf "\"%d\"" uid

        let graph_attributes = fun _ -> []
        let get_subgraph = fun _ -> None

        let default_edge_attributes = fun _ -> []
        let default_vertex_attributes = fun _ -> []

        let vertex_attributes v = []

        let edge_attributes e = []
      end
    include Graph.Graphviz.Dot(Display)
    let print fmt g = fprint_graph fmt g
  end 

  module DIn = Dot.Parse (Builder.I(G))(
    struct
      let node (id,_) _ =
        match id with
        |Graph.Dot_ast.String s -> int_of_string s
        |_ -> assert false
      let edge _ = ()
    end
  )

  module GmlPrinter = Gml.Print (G) (
    struct
       let node (v: G.V.label) = []
       let edge (e: G.E.label) = []
     end
  )

  let add_edge transitive graph i j =
    let rec adapt k red =
      let new_red = 
        S.fold (fun l acc ->
          if k <> l then G.add_edge graph k l;
          G.fold_succ (fun m acc' ->
            if not (G.mem_edge graph k m) 
            then S.add m acc'
            else acc'
          ) graph l acc
        ) red S.empty 
      in
      if S.is_empty new_red then ()
      else adapt k new_red
    in
  begin
    debug "Adding edge from %d to %d" i j;
    G.add_edge graph i j;
    if transitive then begin
      adapt i (S.singleton j);
      G.iter_pred (fun k ->
        if not (G.mem_edge graph k j) then
          adapt k (S.singleton j)
      ) graph i
    end
  end

  (** add to the graph all conjunctive dependencies of package id *)
  let conjdepgraph_int ?(transitive=false) graph univ id =
    G.add_vertex graph id;
    let p = CudfAdd.inttovar univ id in
    List.iter (fun vpkgs ->
      match CudfAdd.resolve_vpkgs_int univ vpkgs with
      |[q] when q <> id -> add_edge transitive graph id q
      |_ -> ()
    ) p.Cudf.depends

  (** for all id \in idlist add to the graph all conjunctive dependencies *)
  let conjdepgraph univ idlist =
    let graph = G.create ~size:(List.length idlist) () in
    List.iter (conjdepgraph_int graph univ) idlist;
    graph

  (** given a graph return the conjunctive dependency closure of the package id *)
  let conjdeps graph =
    let h = Hashtbl.create (G.nb_vertex graph) in
    fun id ->
      try Hashtbl.find h id
      with Not_found -> begin
        let module Dfs = Traverse.Dfs(G) in
        let l = ref [] in
        let collect id = l := id :: !l in
        Dfs.prefix_component collect graph id;
        Hashtbl.add h id !l;
        !l
      end

  (** Build the dependency graph from the given index. conjunctive and
      disjunctive dependencies are considered as equal *)
  let dependency_graph ?(conjunctive=false) universe =
    let size = Cudf.universe_size universe in
    let graph = G.create ~size () in
    Cudf.iteri_packages (fun id pkg ->
      G.add_vertex graph id;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_vpkgs_int universe vpkgs with
        |[p] -> G.add_edge graph id p
        |l when not conjunctive -> List.iter (G.add_edge graph id) l
        |_ -> ()
      ) pkg.Cudf.depends
    ) universe;
    graph

  let dependency_graph_list ?(conjunctive=false) universe idlist =
    let queue = Queue.create () in
    let graph = G.create () in
    let visited = Hashtbl.create (2 * (List.length idlist)) in
    List.iter (fun e -> Queue.add e queue) idlist;
    while (Queue.length queue > 0) do
      let id = Queue.take queue in
      let pkg = Cudf.package_by_uid universe id in
      if not(Hashtbl.mem visited id) then begin
        G.add_vertex graph id;
        Hashtbl.add visited id ();
        List.iter (fun vpkgs ->
          match CudfAdd.resolve_vpkgs_int universe vpkgs with
          |[i] when not(Hashtbl.mem visited i) -> begin
              Queue.add i queue;
              G.add_edge graph id i
          end
          |dsj when not conjunctive ->
            List.iter (fun i ->
              if not(Hashtbl.mem visited i) then begin
                Queue.add i queue;
                G.add_edge graph id i
              end
            ) dsj
          |_ -> ()
        ) pkg.Cudf.depends
      end
    done;
    graph
  ;;

  let load pkglist filename =
    let timer = Util.Timer.create "Defaultgraph.StrongDepGraph.load" in
    Util.Timer.start timer;
    let ic = open_in filename in
    let (detrans,graph) = ((Marshal.from_channel ic) :> (bool * G.t)) in
    close_in ic ;
    info "Loading Strong Dependencies graph";
    (* we assume the graph is detransitivitized *)
    let sg =
      if detrans then begin 
        info "Computing transitive closure";
        (* O.add_transitive_closure graph *) 
        graph
      end else graph
    in
    Util.Timer.stop timer sg

end


