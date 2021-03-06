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

include Util.Logging(struct let label = __FILE__ end) ;;

let gzip_open_file file =
IFDEF HASZIP THEN
  let ch = Gzip.open_in file in
  let input_char ch = try Gzip.input_char ch with End_of_file -> raise IO.No_more_input in
  let read ch = try Gzip.input ch with End_of_file -> raise IO.No_more_input in
  IO.create_in
  ~read:(fun () -> input_char ch)
  ~input:(read ch)
  ~close:(fun () -> Gzip.close_in ch)
ELSE
    fatal "gzip not supported. re-configure with --with-zip"
END
;;

let bzip_open_file file =
IFDEF HASBZ2 THEN
  (* workaround to avoid segfault :
   * http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=602170 *)
  let _ = Bz2.version in 
  let s = " " in
  let ch = Bz2.open_in (open_in file) in
  let input_char ch = 
    try ignore (Bz2.read ch s 0 1) ; s.[0]
    with End_of_file -> raise IO.No_more_input
  in
  let read ch s pos len =
    try Bz2.read ch s pos len 
    with End_of_file -> raise IO.No_more_input
  in
  IO.create_in
  ~read:(fun () -> input_char ch)
  ~input:(read ch)
  ~close:(fun () -> Bz2.close_in ch)
ELSE
    fatal "bzip not supported. re-configure with --with-bz2"
END

;;
let std_open_file file = IO.input_channel (open_in file)
let open_ch ch = IO.input_channel ch
let close_ch ch = IO.close_in ch

let open_file file =
  if (Unix.stat file).Unix.st_size = 0 then fatal "Input file %s is empty" file;
  let openfun = try
      let ch = open_in file in
      let openfun = match input_byte ch with
        (* gzip magic is 0x1f 0x8b *)
        | 0x1f -> (match input_byte ch with
            | 0x8b -> gzip_open_file
            | _ -> std_open_file)
        (* bz2 magic is "BZh" *)
        | 0x42 -> (match input_byte ch with
            | 0x5a -> (match input_byte ch with
                | 0x68 -> bzip_open_file
                | _ -> std_open_file)
            | _ -> std_open_file)
        (* xz magic is 0xfd "7zXZ" *)
        | 0xfd -> (match input_byte ch with
            | 0x37 -> (match input_byte ch with
                | 0x7a -> (match input_byte ch with
                    | 0x58 -> (match input_byte ch with
                        | 0x5a -> fatal "xz not supported."
                        | _ -> std_open_file)
                    | _ -> std_open_file)
                | _ -> std_open_file)
            | _ -> std_open_file)
        | _ -> std_open_file
      in
      close_in ch;
      openfun
    with End_of_file -> std_open_file in
  openfun file
;;

let parse_uri s =
  let url = Url.of_string s in
  let path =  url.Url.path
  in
  (url.Url.scheme,(None,None,None,None,path),None)

let guess_format urilist =
  match List.flatten urilist with
  |uri::l ->
      let (p_default,_,_) = parse_uri uri in
      if List.for_all (fun u -> 
        let (p_list,_,_) = parse_uri u in
        p_default = p_list
      ) l then p_default
      else
        fatal "The input list contains different format prefixes"
  |_ -> fatal "Impossible to guess input format"
