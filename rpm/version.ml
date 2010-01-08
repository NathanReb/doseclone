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

(** compare epoch:version-release strings *)
(* external rpmEVRcmp : string -> string -> int = "rpm_EVRcmp" *)

(** compare only version strings *)
external rpmvercmp : string -> string -> int = "rpm_vercmp"

(********************************************)
(* the parse version function is taken from deb-rpm-check/rpm.ml 
 * Copyright (C) 2005 Jerome Vouillon *)

let pr_version ch (epoch, version, release) =
  begin match epoch with
    None   -> ()
  | Some e -> Format.fprintf ch "%d:" e
  end;
  Format.fprintf ch "%s" version;
  match release with
    Some r -> Format.fprintf ch "-%s" r
  | None   -> ()

let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = is_lower c || is_upper c
let is_alnum c = is_alpha c || is_digit c

(* parseEVR, rpmds.c *)
let version_re_1 =
  Str.regexp
  "^\\(\\([0-9]*\\):\\)?\\(.*\\)-\\([^-]*\\)$"
let version_re_2 =
  Str.regexp
  "^\\(\\([0-9]*\\):\\)?\\(.*\\)\\(-\\)?$"
  (* HACK: last parenthesis never matched *)

let check_version s = s <> "" && not (is_alnum s.[String.length s - 1])

let parse_version s =
  if s = "" then
    None
  else if not (Str.string_match version_re_1 s 0 ||
               Str.string_match version_re_2 s 0) then
    failwith ("Bad version " ^ s)
  else begin
    let epoch =
      try
        let s = Str.matched_group 2 s in
        Some (if s = "" then 0 else int_of_string s)
      with Not_found ->
        None
    in
    let version = Str.matched_group 3 s in
    let release = try Some (Str.matched_group 4 s) with Not_found -> None in
    if
      check_version s ||
      match release with Some r -> check_version r | _ -> false
    then begin
      let b = Buffer.create 80 in
      Format.bprintf b
        "version '%a' not ending with an alphanumeric character@?"
        pr_version (epoch, version, release);
      (* Util.print_warning (Buffer.contents b) *)
    end;
    Some (epoch, version, release)
  end

(********************************************)

(** default compare EVRcmp *)
let compare s1 s2 =
  let get s = if Option.is_none s then "" else ("-"^Option.get s) in
  let epochcmp e1 e2 =
    match (e1,e2) with
    |(None,None)|(None,Some 0)|(Some 0,None) -> 0
    |(None,Some _) -> -1
    |(Some _,None) -> 1
    |(Some x,Some y) -> Pervasives.compare x y
  in
  let parse s = 
    let p = parse_version s in
    if Option.is_none p then (None,"",None)
    else Option.get p
  in
  let (e1,v1,r1) = parse s1 in
  let (e2,v2,r2) = parse s2 in
  match epochcmp e1 e2 with
  |0 ->
      begin match rpmvercmp v1 v2 with
      |0 -> rpmvercmp (get r1) (get r2)
          (* if r1 <> None && r2 <> None
          then rpmvercmp (get r1) (get r2)
          else 0 *)
      |r -> r
      end
  |r -> r

