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

(** Debian Specific Cudf conversion routines *)

(** abstract data type holding the conversion tables 
    for the debcudf translation. *)
type tables

(** initialize the version conversion tables *)
val init_tables : ?compare:(string -> string -> int) -> Packages.package list -> tables

(** return the cudf version associated to a tuple (name,version) *)
val get_cudf_version : tables -> Format822.name * Format822.version -> int

(** return the real version associated to a Cudf package *)
val get_real_version : tables -> Cudf_types.pkgname * Cudf_types.version -> Format822.version

type extramap = (string * (string * Cudf_types.typedecl1)) list

(** [tocudf tbl p] 
    convert the a debian package representation to cudf.
   - Version and package name normalization.
   - Adding self conflicts.
   - Virtual package normalization.
   - Adding priority information if avaiblable.
   - Mapping APT request.
   @param inst : set the {i Installed} cudf field
*)
val tocudf : tables -> ?extras:extramap -> ?inst:bool -> Packages.package -> Cudf.package

(** convert a debian dependency list in a cudf constraints formula *)
val lltocudf : tables -> Format822.vpkg list list -> Cudf_types.vpkgformula

(** convert a debian conflict list in a cudf constraints list *)
val ltocudf  : tables -> Format822.vpkg list -> Cudf_types.vpkglist

(** declare the Cudf preamble used by cudf. Namely, debcudf add :
    - a property named {b Number} of type string containing the original debian version
    - a property named {b Source} of type string
    - a property named {b Sourceversion} of type string
    *)
val preamble : Cudf.preamble

(** create a Cudf universe from a debian package representation list. *)
val load_universe : Packages.package list -> Cudf.universe
