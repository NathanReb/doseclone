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

module S = Set.Make(struct type t = int let compare = compare end)
let dependency_closure h root =
  let queue = Queue.create () in
  let visited = ref S.empty in
  Queue.add root queue;
  while (Queue.length queue > 0) do
    let pid = Queue.take queue in
    visited := S.add pid !visited;
    List.iter (fun l ->
      List.iter (fun p ->
        if not (S.mem p !visited) then
          Queue.add p queue;
      ) l
    ) (fst(Hashtbl.find h pid))
  done;
  S.elements !visited
;;

let complete_sd_graph fs fd h root =
  let queue = Queue.create () in
  let visited = ref S.empty in
  Queue.add (root,[root]) queue;
  while (Queue.length queue > 0) do
    let (pid,path) = Queue.take queue in
    visited := S.add pid !visited;
    List.iter (function
      |[p2] ->
            if not (S.mem p2 !visited) then begin
              Queue.add (p2,pid::path) queue ;
              fd pid p2 ;
              List.iter(fun p1 -> fs p1 p2) (List.rev path)
            end
      |_ -> ()
    ) (fst(Hashtbl.find h pid))
  done
;;


