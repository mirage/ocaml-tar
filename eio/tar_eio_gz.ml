open Eio

module Monad = struct
  type 'a t = 'a

  let ( >>= ) a f = f a
  let return = Fun.id
end

module Reader = struct
  type in_channel = Buf_read.t
  type 'a t = 'a

  let read f = Flow.single_read (Buf_read.as_flow f)
  let really_read f b = Flow.read_exact (Buf_read.as_flow f) b
  let skip = Buf_read.consume
end

module Writer = struct
  type out_channel = Flow.sink_ty Resource.t
  type 'a t = 'a

  let really_write f b = Flow.write f [ b ]
end

include Tar_gz.Make (Monad) (Writer) (Reader)

type source = in_channel
type sink = out_channel

let of_sink ?bits ?q ~level ~mtime os f =
  of_out_channel ?bits ?q ~level ~mtime os (f :> Flow.sink_ty Resource.t)

let of_source f =
  of_in_channel ~internal:(Cstruct.create 65536)
    (Buf_read.of_flow ~max_size:max_int f)

type filter = [ `Skip | `Header | `Header_and_file ]

let fold ?level ?(filter = fun _ -> `Header) f source init =
  let rec aux global acc =
    match get_next_header ?level ~global source with
    | hdr, global ->
        let size = Int64.to_int hdr.file_size in
        let padding = Tar.Header.compute_zero_padding_length hdr in
        let acc =
          match (filter hdr : filter) with
          | `Skip ->
              skip source (size + padding);
              acc
          | `Header ->
              skip source (size + padding);
              f hdr (Eio.Flow.string_source "") acc
          | `Header_and_file ->
              let buf = Cstruct.create size in
              let src = Eio.Flow.cstruct_source [ buf ] in
              really_read source buf;
              skip source padding;
              f hdr src acc
        in
        aux global acc
    | exception Tar.Header.End_of_stream -> acc
  in
  aux None init
