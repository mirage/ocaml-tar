(** {1 Processing tar content with cstruct buffers} *)

type in_channel
type out_channel

val make_in_channel : Cstruct.t -> in_channel
(** [make_in_channel buf] uses [buf] as a source of raw tar content. *)

val make_out_channel : unit -> out_channel
(** [make_out_channel ()] returns a buffer to hold serialized tar content. *)

val to_string : out_channel -> string
(** [to_string oc] returns the contents of [oc] as a string of bytes. *)

val to_cstruct : out_channel -> Cstruct.t
(** [to_cstruct oc] returns the contents of [oc] as a {!Cstruct.t}. *)

val really_read : in_channel -> Cstruct.t -> unit
(** [really_read ic buf] fills [buf] with data from [ic] or raises
    {!Stdlib.End_of_file} *)

val read_zerocopy : in_channel -> int -> Cstruct.t
(** [read_zerocopy ic len] updates the position and returns a sub cstruct of
    [ic] or raises {!Stdlib.End_of_file} *)

val skip : in_channel -> int -> unit

val really_write : out_channel -> Cstruct.t -> unit
(** [really_write oc buf] writes the full contents of [buf] to [oc]
    or raises {!Stdlib.End_of_file}. *)

module HeaderReader : Tar.HEADERREADER with type in_channel = in_channel and type 'a io = 'a
module HeaderWriter : Tar.HEADERWRITER with type out_channel = out_channel and type 'a io = 'a
