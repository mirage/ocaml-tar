(** Read tar.gz files with eio *)

open Eio

type source

val of_source : 'a Flow.source -> source

val get_next_header :
  ?level:Tar.Header.compatibility ->
  global:Tar.Header.Extended.t option ->
  source ->
  Tar.Header.t * Tar.Header.Extended.t option
(** Returns the next header block or fails with {!Tar.Header.End_of_stream} if
    two consecutive zero-filled blocks are discovered. Assumes stream is
    positioned at the possible start of a header block.

    @raise Stdlib.End_of_file if the stream unexpectedly fails. *)

val really_read : source -> Cstruct.t -> unit
(** [really_read fd buf] fills [buf] with data from [fd] or raises
    {!Stdlib.End_of_file}. *)

type filter = [ `Skip | `Header | `Header_and_file ]

val fold :
  ?level:Tar.Header.compatibility ->
  ?filter:(Tar.Header.t -> filter) ->
  (Tar.Header.t -> Flow.source_ty Resource.t -> 'a -> 'a) ->
  source ->
  'a ->
  'a

val skip : source -> int -> unit

type sink

val of_sink :
  ?bits:int ->
  ?q:int ->
  level:int ->
  mtime:int32 ->
  Gz.os ->
  'a Flow.sink ->
  sink

val write_block :
  ?level:Tar.Header.compatibility ->
  ?global:Tar.Header.Extended.t ->
  Tar.Header.t ->
  sink ->
  (unit -> string option) ->
  unit
(** [write_block hdr oc stream] writes [hdr], then {i deflate} the given
    [stream], then zero-pads so the stream is positionned for the next block.

    A simple usage to write a file:
    {[
      let stream_of_fd fd =
        let buf = Bytes.create 0x1000 in
        fun () -> match Unix.read fd buf 0 (Bytes.length buf) with
        | 0 -> None
        | len -> Some (Bytes.sub_string buf 0 len)
        | exception End_of_file -> None

      let add_file oc filename =
        let fd = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
        let hdr = Tar.Header.make ... in
        write_block hdr oc (stream_of_fd fd) ;
        Unix.close fd
    ]} *)

val write_end : sink -> unit
(** [write_end oc] writes a stream terminator to [oc]. *)
