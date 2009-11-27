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

(** Solver output and diagnostic . Low Level API *)

open ExtLib
open Common
open Mdf

type reason =
  |Dependency of (int * int list)
  |EmptyDependency of (int * Cudf_types.vpkg list)
  |Conflict of (int * int)

  |Installed_alternatives of int list
  |To_install of (Cudf_types.vpkg * int list)
  |To_remove of (Cudf_types.vpkg * int)
  |To_upgrade of (Cudf_types.vpkg * int list)
  |To_upgrade_singleton of (Cudf_types.vpkg * int list)

type result =
  |Success of (unit -> int list)
  |Failure of (unit -> reason list)

type request =
  |Sng of int
  |Lst of int list
  |Req of int
