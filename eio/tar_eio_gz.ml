open Eio

module Monad = struct
  type 'a t = 'a

  let ( >>= ) a f = f a
  let return = Fun.id
end

module Reader = struct
  type in_channel = Flow.source_ty Resource.t
  type 'a t = 'a

  let read = Flow.single_read
  let really_read f b = Flow.read_exact f b
  let buffer_null = Cstruct.create 65536

  let skip f (n : int) =
    let rec loop (n : int) =
      if n <= 0 then ()
      else
        let amount = min n (Cstruct.length buffer_null) in
        let block = Cstruct.sub buffer_null 0 amount in
        really_read f block;
        loop (n - amount)
    in
    loop n
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
    (f :> Flow.source_ty Eio.Resource.t)

let fold ?level f source init =
  let rec aux global acc =
    match get_next_header ?level ~global source with
    | hdr, global ->
        let acc = f hdr acc in
        let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
        skip source to_skip;
        aux global acc
    | exception Tar.Header.End_of_stream -> acc
  in
  aux None init
