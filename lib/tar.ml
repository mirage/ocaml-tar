(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Process and create tar file headers *)
module Header = struct
  (** Map of field name -> (start offset, length) taken from wikipedia:
      http://en.wikipedia.org/w/index.php?title=Tar_%28file_format%29&oldid=83554041 *)

  let trim_numerical s =
    String.(trim (map (function '\000' -> ' ' | x -> x) s))

  (** Unmarshal an integer field (stored as 0-padded octal) *)
  let unmarshal_int (x: string) : int =
    let tmp = "0o0" ^ (trim_numerical x) in
    try
      int_of_string tmp
    with Failure msg ->
      failwith (Printf.sprintf "%s: failed to parse integer %S" msg tmp)

  (** Unmarshal an int64 field (stored as 0-padded octal) *)
  let unmarshal_int64 (x: string) : int64 =
    let tmp = "0o0" ^ (trim_numerical x) in
    Int64.of_string tmp

  (** Unmarshal a string *)
  let unmarshal_string (x: string) : string =
    try
      let first_0 = String.index x '\000' in
      String.sub x 0 first_0
    with
      Not_found -> x (* TODO should error *)

  (** Marshal an integer field of size 'n' *)
  let marshal_int (x: int) (n: int) =
    let octal = Printf.sprintf "%0*o" (n - 1) x in
    octal ^ "\000" (* space or NULL allowed *)

  (** Marshal an int64 field of size 'n' *)
  let marshal_int64 (x: int64) (n: int) =
    let octal = Printf.sprintf "%0*Lo" (n - 1) x in
    octal ^ "\000" (* space or NULL allowed *)

  (** Marshal an string field of size 'n' *)
  let marshal_string (x: string) (n: int) =
    if String.length x < n then
      let bytes = Bytes.make n '\000' in
      Bytes.blit_string x 0 bytes 0 (String.length x);
      Bytes.unsafe_to_string bytes
    else
      x

  (** Unmarshal a pax Extended Header File time
      It can contain a <period> ( '.' ) for sub-second granularity, that we ignore.
      https://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_05 *)
  let unmarshal_pax_time (x:string ) : int64 =
    match String.split_on_char '.' x with
    | [seconds] -> Int64.of_string seconds
    | [seconds; _subseconds] -> Int64.of_string seconds
    | _ -> raise (Failure "Wrong pax Extended Header File Times format")

  let hdr_file_name_off = 0
  let sizeof_hdr_file_name = 100

  let hdr_file_mode_off = 100
  let sizeof_hdr_file_mode = 8

  let hdr_user_id_off = 108
  let sizeof_hdr_user_id = 8

  let hdr_group_id_off = 116
  let sizeof_hdr_group_id  = 8

  let hdr_file_size_off = 124
  let sizeof_hdr_file_size = 12

  let hdr_mod_time_off = 136
  let sizeof_hdr_mod_time = 12

  let hdr_chksum_off = 148
  let sizeof_hdr_chksum = 8

  let hdr_link_indicator_off = 156

  let hdr_link_name_off = 157
  let sizeof_hdr_link_name = 100

  let hdr_magic_off = 257
  let sizeof_hdr_magic = 6

  let hdr_version_off = 263
  let sizeof_hdr_version = 2

  let hdr_uname_off = 265
  let sizeof_hdr_uname = 32

  let hdr_gname_off = 297
  let sizeof_hdr_gname = 32

  let hdr_devmajor_off = 329
  let sizeof_hdr_devmajor = 8

  let hdr_devminor_off = 337
  let sizeof_hdr_devminor = 8

  let hdr_prefix_off = 345
  let sizeof_hdr_prefix = 155

  let get_hdr_file_name buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_file_name_off ~len:sizeof_hdr_file_name buf)
  let set_hdr_file_name buf v =
    let v = marshal_string v sizeof_hdr_file_name in
    Cstruct.blit_from_string v 0 buf hdr_file_name_off sizeof_hdr_file_name

  let get_hdr_file_mode buf =
    unmarshal_int
      (Cstruct.to_string ~off:hdr_file_mode_off ~len:sizeof_hdr_file_mode buf)
  let set_hdr_file_mode buf v =
    let v = marshal_int v sizeof_hdr_file_mode in
    Cstruct.blit_from_string v 0 buf hdr_file_mode_off sizeof_hdr_file_mode

  let get_hdr_user_id buf =
    unmarshal_int
      (Cstruct.to_string ~off:hdr_user_id_off ~len:sizeof_hdr_user_id buf)
  let set_hdr_user_id buf v =
    let v = marshal_int v sizeof_hdr_user_id in
    Cstruct.blit_from_string v 0 buf hdr_user_id_off sizeof_hdr_user_id

  let get_hdr_group_id buf =
    unmarshal_int
      (Cstruct.to_string ~off:hdr_group_id_off ~len:sizeof_hdr_group_id buf)
  let set_hdr_group_id buf v =
    let v = marshal_int v sizeof_hdr_group_id in
    Cstruct.blit_from_string v 0 buf hdr_group_id_off sizeof_hdr_group_id

  let get_hdr_file_size buf =
    unmarshal_int64
      (Cstruct.to_string ~off:hdr_file_size_off ~len:sizeof_hdr_file_size buf)
  let set_hdr_file_size buf v =
    let v = marshal_int64 v sizeof_hdr_file_size in
    Cstruct.blit_from_string v 0 buf hdr_file_size_off sizeof_hdr_file_size

  let get_hdr_mod_time buf =
    unmarshal_int64
      (Cstruct.to_string ~off:hdr_mod_time_off ~len:sizeof_hdr_mod_time buf)
  let set_hdr_mod_time buf v =
    let v = marshal_int64 v sizeof_hdr_mod_time in
    Cstruct.blit_from_string v 0 buf hdr_mod_time_off sizeof_hdr_mod_time

  let get_hdr_chksum buf =
    unmarshal_int64
      (Cstruct.to_string ~off:hdr_chksum_off ~len:sizeof_hdr_chksum buf)
  let set_hdr_chksum buf v =
    let v = marshal_int64 v sizeof_hdr_chksum in
    Cstruct.blit_from_string v 0 buf hdr_chksum_off sizeof_hdr_chksum

  let get_hdr_link_indicator buf = Cstruct.get_char buf hdr_link_indicator_off
  let set_hdr_link_indicator buf v =
    Cstruct.set_char buf hdr_link_indicator_off v

  let get_hdr_link_name buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_link_name_off ~len:sizeof_hdr_link_name buf)
  let set_hdr_link_name buf v =
    let v = marshal_string v sizeof_hdr_link_name in
    Cstruct.blit_from_string v 0 buf hdr_link_name_off sizeof_hdr_link_name

  let get_hdr_magic buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_magic_off ~len:sizeof_hdr_magic buf)
  let set_hdr_magic buf v =
    let v = marshal_string v sizeof_hdr_magic in
    Cstruct.blit_from_string v 0 buf hdr_magic_off sizeof_hdr_magic

  let _get_hdr_version buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_version_off ~len:sizeof_hdr_version buf)
  let set_hdr_version buf v =
    let v = marshal_string v sizeof_hdr_version in
    Cstruct.blit_from_string v 0 buf hdr_version_off sizeof_hdr_version

  let get_hdr_uname buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_uname_off ~len:sizeof_hdr_uname buf)
  let set_hdr_uname buf v =
    let v = marshal_string v sizeof_hdr_uname in
    Cstruct.blit_from_string v 0 buf hdr_uname_off sizeof_hdr_uname

  let get_hdr_gname buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_gname_off ~len:sizeof_hdr_gname buf)
  let set_hdr_gname buf v =
    let v = marshal_string v sizeof_hdr_gname in
    Cstruct.blit_from_string v 0 buf hdr_gname_off sizeof_hdr_gname

  let get_hdr_devmajor buf =
    unmarshal_int
      (Cstruct.to_string ~off:hdr_devmajor_off ~len:sizeof_hdr_devmajor buf)
  let set_hdr_devmajor buf v =
    let v = marshal_int v sizeof_hdr_devmajor in
    Cstruct.blit_from_string v 0 buf hdr_devmajor_off sizeof_hdr_devmajor

  let get_hdr_devminor buf =
    unmarshal_int
      (Cstruct.to_string ~off:hdr_devminor_off ~len:sizeof_hdr_devminor buf)
  let set_hdr_devminor buf v =
    let v = marshal_int v sizeof_hdr_devminor in
    Cstruct.blit_from_string v 0 buf hdr_devminor_off sizeof_hdr_devminor

  let get_hdr_prefix buf =
    unmarshal_string
      (Cstruct.to_string ~off:hdr_prefix_off ~len:sizeof_hdr_prefix buf)
  let set_hdr_prefix buf v =
    let v = marshal_string v sizeof_hdr_prefix in
    Cstruct.blit_from_string v 0 buf hdr_prefix_off sizeof_hdr_prefix

  type compatibility =
    | OldGNU
    | GNU
    | V7
    | Ustar
    | Posix

  let compatibility_level = ref V7

  let get_level = function
    | None -> !compatibility_level
    | Some level -> level

  module Link = struct
    type t =
      | Normal
      | Hard
      | Symbolic
      | Character
      | Block
      | Directory
      | FIFO
      | GlobalExtendedHeader
      | PerFileExtendedHeader
      | LongLink
      | LongName

    (* Strictly speaking, v7 supports Normal (as \0) and Hard only *)
    let to_char ?level =
      let level = get_level level in function
        | Normal                -> if level = V7 then '\000' else '0'
        | Hard                  -> '1'
        | Symbolic              -> '2'
        | Character             -> '3'
        | Block                 -> '4'
        | Directory             -> '5'
        | FIFO                  -> '6'
        | GlobalExtendedHeader  -> 'g'
        | PerFileExtendedHeader -> 'x'
        | LongLink              -> 'K'
        | LongName              -> 'L'

    let of_char = function
      | '1' -> Hard
      | '2' -> Symbolic
      | 'g' -> GlobalExtendedHeader
      | 'x' -> PerFileExtendedHeader
      | 'K' -> LongLink
      | 'L' -> LongName
      | '3' -> Character
      | '4' -> Block
      | '5' -> Directory
      | '6' -> FIFO
      | _ -> Normal (* if value is malformed, treat as a normal file *)

    let to_string = function
      | Normal -> "Normal"
      | Hard -> "Hard"
      | Symbolic -> "Symbolic"
      | Character -> "Character"
      | Block -> "Block"
      | Directory -> "Directory"
      | FIFO -> "FIFO"
      | GlobalExtendedHeader -> "GlobalExtendedHeader"
      | PerFileExtendedHeader -> "PerFileExtendedHeader"
      | LongLink -> "LongLink"
      | LongName -> "LongName"
  end

  module Extended = struct
    type t = {
      access_time: int64 option;
      charset: string option;
      comment: string option;
      group_id: int option;
      gname: string option;
      header_charset: string option;
      link_path: string option;
      mod_time: int64 option;
      path: string option;
      file_size: int64 option;
      user_id: int option;
      uname: string option;
    }
    (** Represents a "Pax" extended header *)

    let make ?access_time ?charset ?comment ?group_id ?gname ?header_charset
      ?link_path ?mod_time ?path ?file_size ?user_id ?uname () =
      { access_time; charset; comment; group_id; gname; header_charset;
        link_path; mod_time; path; file_size; user_id; uname }

    (** Pretty-print the header record *)
    let to_detailed_string (x: t) =
      let opt f = function None -> "" | Some v -> f v in
      let table = [ "access_time",    opt Int64.to_string x.access_time;
                    "charset",        opt Fun.id x.charset;
                    "comment",        opt Fun.id x.comment;
                    "group_id",       opt string_of_int x.group_id;
                    "gname",          opt Fun.id x.gname;
                    "header_charset", opt Fun.id x.header_charset;
                    "link_path",      opt Fun.id x.link_path;
                    "mod_time",       opt Int64.to_string x.mod_time;
                    "path",           opt Fun.id x.path;
                    "file_size",      opt Int64.to_string x.file_size;
                    "user_id",        opt string_of_int x.user_id;
                    "uname",          opt Fun.id x.uname;
                  ] in
      "{\n\t" ^ (String.concat "\n\t" (List.map (fun (k, v) -> k ^ ": " ^ v) table)) ^ "}"

    let marshal t =
      let pairs =
        (match t.access_time    with None -> [] | Some x -> [ "atime", Int64.to_string x ])
      @ (match t.charset        with None -> [] | Some x -> [ "charset", x ])
      @ (match t.comment        with None -> [] | Some x -> [ "comment", x ])
      @ (match t.group_id       with None -> [] | Some x -> [ "gid", string_of_int x ])
      @ (match t.gname          with None -> [] | Some x -> [ "group_name", x ])
      @ (match t.header_charset with None -> [] | Some x -> [ "hdrcharset", x ])
      @ (match t.link_path      with None -> [] | Some x -> [ "linkpath", x ])
      @ (match t.mod_time       with None -> [] | Some x -> [ "mtime", Int64.to_string x ])
      @ (match t.path           with None -> [] | Some x -> [ "path", x ])
      @ (match t.file_size      with None -> [] | Some x -> [ "size", Int64.to_string x ])
      @ (match t.user_id        with None -> [] | Some x -> [ "uid", string_of_int x ])
      @ (match t.uname          with None -> [] | Some x -> [ "uname", x ]) in
      let txt = String.concat "" (List.map (fun (k, v) ->
        let length = 8 + 1 + (String.length k) + 1 + (String.length v) + 1 in
        Printf.sprintf "%08d %s=%s\n" length k v
      ) pairs) in
      Cstruct.of_string txt

    let merge global extended =
      match global with
      | Some g ->
         let merge g e = match e with None -> g | Some _ ->  e in
         let access_time = merge g.access_time extended.access_time
         and charset = merge g.charset extended.charset
         and comment = merge g.comment extended.comment
         and group_id = merge g.group_id extended.group_id
         and gname = merge g.gname extended.gname
         and header_charset = merge g.header_charset extended.header_charset
         and link_path = merge g.link_path extended.link_path
         and mod_time = merge g.mod_time extended.mod_time
         and path = merge g.path extended.path
         and file_size = merge g.file_size extended.file_size
         and user_id = merge g.user_id extended.user_id
         and uname = merge g.uname extended.uname
         in
         { access_time; charset; comment; group_id; gname;
           header_charset; link_path; mod_time; path; file_size;
           user_id; uname }
      | None -> extended

    let unmarshal ~(global: t option) (c: Cstruct.t) : t =
      (* "%d %s=%s\n", <length>, <keyword>, <value> with constraints that
         - the <keyword> cannot contain an equals sign
         - the <length> is the number of octets of the record, including \n
        *)
      let find buffer char =
        let rec loop i =
          if i = Cstruct.length buffer
          then None
          else if Cstruct.get_char buffer i = char
          then Some i
          else loop (i + 1) in
        loop 0 in
      let rec loop remaining =
        if Cstruct.length remaining = 0
        then []
        else begin
          (* Find the space, then decode the length *)
          match find remaining ' ' with
          | None -> failwith "Failed to decode pax extended header record"
          | Some i ->
            let length = int_of_string @@ Cstruct.to_string @@ Cstruct.sub remaining 0 i in
            let record = Cstruct.sub remaining 0 length in
            let remaining = Cstruct.shift remaining length in
            begin match find record '=' with
            | None -> failwith "Failed to decode pax extended header record"
            | Some j ->
              let keyword = Cstruct.to_string @@ Cstruct.sub record (i + 1) (j - i - 1) in
              let v = Cstruct.to_string @@ Cstruct.sub record (j + 1) (Cstruct.length record - j - 2) in
              (keyword, v) :: (loop remaining)
            end
        end in
      let pairs = loop c in
      let option name f =
        if List.mem_assoc name pairs
        then Some (f (List.assoc name pairs))
        else None in
      (* integers are stored as decimal, not octal here *)
      let access_time    = option "atime" unmarshal_pax_time in
      let charset        = option "charset" unmarshal_string in
      let comment        = option "comment" unmarshal_string in
      let group_id       = option "gid" int_of_string in
      let gname          = option "group_name" unmarshal_string in
      let header_charset = option "hdrcharset" unmarshal_string in
      let link_path      = option "linkpath" unmarshal_string in
      let mod_time       = option "mtime" unmarshal_pax_time in
      let path           = option "path" unmarshal_string in
      let file_size      = option "size" Int64.of_string in
      let user_id        = option "uid" int_of_string in
      let uname          = option "uname" unmarshal_string in
      { access_time; charset; comment; group_id; gname;
        header_charset; link_path; mod_time; path; file_size;
        user_id; uname } |> merge global

  end

  (** Represents a standard (non-USTAR) archive (note checksum not stored) *)
  type t = { file_name: string;
             file_mode: int;
             user_id: int;
             group_id: int;
             file_size: int64;
             mod_time: int64;
             link_indicator: Link.t;
             link_name: string;
             uname: string;
             gname: string;
             devmajor: int;
             devminor: int;
             extended: Extended.t option;
           }

  (** Helper function to make a simple header *)
  let make ?(file_mode=0o400) ?(user_id=0) ?(group_id=0) ?(mod_time=0L) ?(link_indicator=Link.Normal) ?(link_name="") ?(uname="") ?(gname="") ?(devmajor=0) ?(devminor=0) file_name file_size =
    (* If some fields are too big, we must use a pax header *)
    let need_pax_header =
      Int64.unsigned_compare file_size 0o077777777777L > 0
       || user_id  > 0x07777777
       || group_id > 0x07777777 in
    let extended =
      if need_pax_header
      then Some (Extended.make ~file_size ~user_id ~group_id ())
      else None in
    { file_name;
      file_mode;
      user_id;
      group_id;
      file_size;
      mod_time;
      link_indicator;
      link_name;
      uname;
      gname;
      devmajor;
      devminor;
      extended }

  (** Length of a header block *)
  let length = 512

  (** A blank header block (two of these in series mark the end of the tar) *)
  let zero_block = Cstruct.create length

  (** [allzeroes buf] is true if [buf] contains only zero bytes *)
  let allzeroes buf =
    let rec loop i =
      (i >= Cstruct.length buf) || (Cstruct.get_uint8 buf i = 0 && (loop (i + 1))) in
    loop 0

  (** Pretty-print the header record *)
  let to_detailed_string (x: t) =
    let table = [ "file_name",      x.file_name;
                  "file_mode",      string_of_int x.file_mode;
                  "user_id",        string_of_int x.user_id;
                  "group_id",       string_of_int x.group_id;
                  "file_size",      Int64.to_string x.file_size;
                  "mod_time",       Int64.to_string x.mod_time;
                  "link_indicator", Link.to_string x.link_indicator;
                  "link_name",      x.link_name ] in
    "{\n\t" ^ (String.concat "\n\t" (List.map (fun (k, v) -> k ^ ": " ^ v) table)) ^ "}"

  (** Thrown when unmarshalling a header if the checksums don't match *)
  exception Checksum_mismatch

  (** From an already-marshalled block, compute what the checksum should be *)
  let checksum (x: Cstruct.t) : int64 =
    (* Sum of all the byte values of the header with the checksum field taken
       as 8 ' ' (spaces) *)
    let result = ref 0 in
    let in_checksum_range i =
      i >= hdr_chksum_off && i < hdr_chksum_off + sizeof_hdr_chksum
    in
    for i = 0 to Cstruct.length x - 1 do
      let v =
        if in_checksum_range i then
          int_of_char ' '
        else
          Cstruct.get_uint8 x i
      in
      result := !result + v
    done;
    Int64.of_int !result

  (** Unmarshal a header block, returning None if it's all zeroes *)
  let unmarshal ?(extended = Extended.make ()) (c: Cstruct.t) : t option =
    if allzeroes c then None
    else
      let chksum = get_hdr_chksum c in
      if checksum c <> chksum then raise Checksum_mismatch
      else let ustar =
             let magic = get_hdr_magic c in
             (* GNU tar and Posix differ in interpretation of the character following ustar. For Posix, it should be '\0' but GNU tar uses ' ' *)
             String.length magic >= 5 && (String.sub magic 0 5 = "ustar") in
        let prefix = if ustar then get_hdr_prefix c else "" in
        let file_name = match extended.Extended.path with
          | Some path -> path
          | None ->
            let file_name = get_hdr_file_name c in
            if file_name = "" then prefix
            else if prefix = "" then file_name
            else Filename.concat prefix file_name in
        let file_mode = get_hdr_file_mode c in
        let user_id = match extended.Extended.user_id with
          | None -> get_hdr_user_id c
          | Some x -> x in
        let group_id = match extended.Extended.group_id with
          | None -> get_hdr_group_id c
          | Some x -> x in
        let file_size = match extended.Extended.file_size with
          | None -> get_hdr_file_size c
          | Some x -> x in
        let mod_time = match extended.Extended.mod_time with
          | None -> get_hdr_mod_time c
          | Some x -> x in
        let link_indicator = Link.of_char (get_hdr_link_indicator c) in
        let uname = match extended.Extended.uname with
          | None -> if ustar then get_hdr_uname c else ""
          | Some x -> x in
        let gname = match extended.Extended.gname with
          | None -> if ustar then get_hdr_gname c else ""
          | Some x -> x in
        let devmajor  = if ustar then get_hdr_devmajor c else 0 in
        let devminor  = if ustar then get_hdr_devminor c else 0 in

        let link_name = match extended.Extended.link_path with
          | Some link_path -> link_path
          | None -> get_hdr_link_name c in
        Some (make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator
           ~link_name ~uname ~gname ~devmajor ~devminor file_name file_size)

  (** Marshal a header block, computing and inserting the checksum *)
  let marshal ?level c (x: t) =
    let level = get_level level in
    (* The caller (e.g. write_block) is expected to insert the extra ././@LongLink header *)
    if String.length x.file_name > sizeof_hdr_file_name && level <> GNU then
      if level = Ustar then
        if String.length x.file_name > 256 then failwith "file_name too long"
        else let (prefix, file_name) =
               let is_directory = if x.file_name.[String.length x.file_name - 1] = '/' then "/" else "" in
               let rec split prefix file_name =
                 if String.length file_name > sizeof_hdr_file_name then failwith "file_name can't be split"
                 else if String.length prefix > sizeof_hdr_prefix then split (Filename.dirname prefix) (Filename.concat (Filename.basename prefix) file_name ^ is_directory)
                 else (prefix, file_name) in
               split (Filename.dirname x.file_name) (Filename.basename x.file_name ^ is_directory) in
          set_hdr_file_name c file_name;
          set_hdr_prefix c prefix
      else failwith "file_name too long"
    else set_hdr_file_name c x.file_name;
    (* This relies on the fact that the block was initialised to null characters *)
    if level = Ustar || (level = GNU && x.devmajor = 0 && x.devminor = 0) then begin
      if level = Ustar then begin
        set_hdr_magic c "ustar";
        set_hdr_version c "00";
      end else begin
        (* OLD GNU MAGIC: use "ustar " as magic, and another " " in the version *)
        set_hdr_magic c "ustar ";
        set_hdr_version c " ";
      end;
      set_hdr_uname c x.uname;
      set_hdr_gname c x.gname;
      if level = Ustar then begin
        set_hdr_devmajor c x.devmajor;
        set_hdr_devminor c x.devminor;
      end
    end else begin
      if x.devmajor <> 0 then failwith "devmajor not supported in this format";
      if x.devminor <> 0 then failwith "devminor not supported in this format";
      if x.uname <> "" then failwith "uname not supported in this format";
      if x.gname <> "" then failwith "gname not supported in this format";
    end;
    set_hdr_file_mode c x.file_mode;
    set_hdr_user_id c x.user_id;
    set_hdr_group_id c x.group_id;
    set_hdr_file_size c x.file_size;
    set_hdr_mod_time c x.mod_time;
    set_hdr_link_indicator c (Link.to_char ~level x.link_indicator);
    (* The caller (e.g. write_block) is expected to insert the extra ././@LongLink header *)
    if String.length x.link_name > sizeof_hdr_link_name && level <> GNU then failwith "link_name too long";
    set_hdr_link_name c x.link_name;
    (* Finally, compute the checksum *)
    let chksum = checksum c in
    set_hdr_chksum c chksum

  (** Thrown if we detect the end of the tar (at least two zero blocks in sequence) *)
  exception End_of_stream

  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks *)
  let compute_zero_padding_length (x: t) : int =
    (* round up to next whole number of block lengths *)
    let length = Int64.of_int length in
    let lenm1 = Int64.sub length Int64.one in
    let next_block_length = (Int64.mul length (Int64.div (Int64.add x.file_size lenm1) length)) in
    Int64.to_int (Int64.sub next_block_length x.file_size)

  (** Return the required zero-padding as a string *)
  let zero_padding (x: t) =
    let zero_padding_len = compute_zero_padding_length x in
    Cstruct.sub zero_block 0 zero_padding_len

  let to_sectors (x: t) =
    let bytes = Int64.(add x.file_size (of_int (compute_zero_padding_length x))) in
    Int64.div bytes 512L
end

module type ASYNC = sig
  type 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module type READER = sig
  type in_channel
  type 'a io
  val really_read: in_channel -> Cstruct.t -> unit io
  val skip: in_channel -> int -> unit io
end

module type WRITER = sig
  type out_channel
  type 'a io
  val really_write: out_channel -> Cstruct.t -> unit io
end

module type HEADERREADER = sig
  type in_channel
  type 'a io
  val read : global:Header.Extended.t option -> in_channel ->
    (Header.t * Header.Extended.t option, [` Eof ]) result io
end

module type HEADERWRITER = sig
  type out_channel
  type 'a io
  val write : ?level:Header.compatibility -> Header.t -> out_channel -> unit io
  val write_global_extended_header : Header.Extended.t -> out_channel -> unit io
end

let longlink = "././@LongLink"

module HeaderReader(Async: ASYNC)(Reader: READER with type 'a io = 'a Async.t) = struct
  open Async
  open Reader

  type in_channel = Reader.in_channel
  type 'a io = 'a Async.t

  let read ~global (ifd: Reader.in_channel) : (Header.t * Header.Extended.t option, [ `Eof ]) result t =
    (* We might need to read 2 headers at once if we encounter a Pax header *)
    let buffer = Cstruct.create Header.length in
    let real_header_buf = Cstruct.create Header.length in

    let next_block global () =
      really_read ifd buffer
      >>= fun () ->
      match Header.unmarshal ?extended:global buffer with
      | None -> return None
      | Some hdr -> return (Some hdr)
    in

    let rec get_hdr ~next_longname ~next_longlink global () : (Header.t * Header.Extended.t option, [> `Eof ]) result t =
      next_block global ()
      >>= function
      | Some x when x.Header.link_indicator = Header.Link.GlobalExtendedHeader ->
        let extra_header_buf = Cstruct.create (Int64.to_int x.Header.file_size) in
        really_read ifd extra_header_buf
        >>= fun () ->
        skip ifd (Header.compute_zero_padding_length x)
        >>= fun () ->
        (* unmarshal merges the previous global (if any) with the
           discovered global (if any) and returns the new global. *)
        let global = Header.Extended.unmarshal ~global extra_header_buf in
        get_hdr ~next_longname ~next_longlink (Some global) ()
      | Some x when x.Header.link_indicator = Header.Link.PerFileExtendedHeader ->
        let extra_header_buf = Cstruct.create (Int64.to_int x.Header.file_size) in
        really_read ifd extra_header_buf
        >>= fun () ->
        skip ifd (Header.compute_zero_padding_length x)
        >>= fun () ->
        let extended = Header.Extended.unmarshal ~global extra_header_buf in
        really_read ifd real_header_buf
        >>= fun () ->
        begin match Header.unmarshal ~extended real_header_buf with
          | None ->
            (* FIXME: Corrupt pax headers *)
            return (Error `Eof)
          | Some x -> return (Ok (x, global))
        end
      | Some ({ Header.link_indicator = Header.Link.LongLink | Header.Link.LongName; _ } as x) when x.Header.file_name = longlink ->
        let extra_header_buf = Cstruct.create (Int64.to_int x.Header.file_size) in
        really_read ifd extra_header_buf
        >>= fun () ->
        skip ifd (Header.compute_zero_padding_length x)
        >>= fun () ->
        let name = Cstruct.to_string ~len:(Cstruct.length extra_header_buf - 1) extra_header_buf in
        let next_longlink = if x.Header.link_indicator = Header.Link.LongLink then Some name else next_longlink in
        let next_longname = if x.Header.link_indicator = Header.Link.LongName then Some name else next_longname in
        get_hdr ~next_longname ~next_longlink global ()
      | Some x ->
        (* XXX: unclear how/if pax headers should interact with gnu extensions *)
        let x = match next_longname with
          | None -> x
          | Some file_name -> { x with file_name }
        in
        let x = match next_longlink with
          | None -> x
          | Some link_name -> { x with link_name }
        in
        let x = 
          (* For backward compatibility we treat normal files ending in slash
             as directories. Because [Link.of_char] treats unrecognized link
             indicator values as normal files we check directly. This is not
             completely correct as [Header.Link.of_char] turns unknown link
             indicators into [Header.Link.Normal]. Ideally, it should only be
             done for '0' and '\000'. *)
          if String.length x.file_name > 0
          && x.file_name.[String.length x.file_name - 1] = '/'
          && x.link_indicator = Header.Link.Normal then
            { x with link_indicator = Header.Link.Directory }
          else
            x
        in
        return (Ok (x, global))
      | None ->
        begin
          next_block global ()
          >>= function
          | Some x -> return (Ok (x, global))
          | None -> return (Error `Eof)
        end
    in

    get_hdr ~next_longname:None ~next_longlink:None global ()

end

module HeaderWriter(Async: ASYNC)(Writer: WRITER with type 'a io = 'a Async.t) = struct
  open Async
  open Writer

  type out_channel = Writer.out_channel
  type 'a io = 'a Async.t

  let write_unextended ?level header fd =
    let level = Header.get_level level in
    let buffer = Cstruct.create Header.length in
    let blank = {Header.file_name = longlink; file_mode = 0; user_id = 0; group_id = 0; mod_time = 0L; file_size = 0L; link_indicator = Header.Link.LongLink; link_name = ""; uname = "root"; gname = "root"; devmajor = 0; devminor = 0; extended = None} in
    ( if level = Header.GNU then begin
        ( if String.length header.Header.link_name > Header.sizeof_hdr_link_name then begin
            let file_size = String.length header.Header.link_name + 1 in
            let blank = {blank with Header.file_size = Int64.of_int file_size} in
            Header.marshal ~level buffer { blank with link_indicator = Header.Link.LongLink };
            really_write fd buffer
            >>= fun () ->
            let payload = Cstruct.of_string (header.Header.link_name ^ "\000") in
            really_write fd payload
            >>= fun () ->
            really_write fd (Header.zero_padding blank)
          end else return () )
        >>= fun () ->
        ( if String.length header.Header.file_name > Header.sizeof_hdr_file_name then begin
            let file_size = String.length header.Header.file_name + 1 in
            let blank = {blank with Header.file_size = Int64.of_int file_size} in
            Header.marshal ~level buffer { blank with link_indicator = Header.Link.LongName };
            really_write fd buffer
            >>= fun () ->
            let payload = Cstruct.of_string (header.Header.file_name ^ "\000") in
            really_write fd payload
            >>= fun () ->
            really_write fd (Header.zero_padding blank)
          end else return () )
        >>= fun () ->
        Cstruct.memset buffer 0;
        return ()
      end else return () )
    >>= fun () ->
    Header.marshal ~level buffer header;
    really_write fd buffer

  let write_extended ?level ~link_indicator hdr fd =
    let link_indicator_name = match link_indicator with
      | Header.Link.PerFileExtendedHeader -> "paxheader"
      | Header.Link.GlobalExtendedHeader -> "pax_global_header"
      | _ -> assert false
    in
    let pax_payload = Header.Extended.marshal hdr in
    let pax = Header.make ~link_indicator link_indicator_name
                (Int64.of_int @@ Cstruct.length pax_payload) in
    write_unextended ?level pax fd
    >>= fun () ->
    really_write fd pax_payload
    >>= fun () ->
    really_write fd (Header.zero_padding pax)

  let write ?level header fd =
    ( match header.Header.extended with
      | None -> return ()
      | Some e ->
        write_extended ?level ~link_indicator:Header.Link.PerFileExtendedHeader e fd )
    >>= fun () ->
    write_unextended ?level header fd

  let write_global_extended_header global fd =
    write_extended ~link_indicator:Header.Link.GlobalExtendedHeader global fd
end
