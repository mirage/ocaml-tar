open Eio

module Monad = struct
  type 'a t = 'a
  let (>>=) a f = f a
  let return = Fun.id
end

module Reader = struct
  type in_channel = Flow.source_ty Resource.t
  type 'a t = 'a
  let read = Flow.single_read
  let really_read f b = Flow.read_exact f b
  let skip f (n: int) =
    let buffer_size = 32768 in
    let buffer = Cstruct.create buffer_size in
    let rec loop (n: int) =
      if n <= 0 then ()
      else
        let amount = min n buffer_size in
        let block = Cstruct.sub buffer 0 amount in
        really_read f block;
        loop (n - amount) in
    loop n
end

module Writer = struct
  type out_channel = Flow.sink_ty Resource.t
  type 'a t = 'a
  let really_write f b = Flow.write f [ b ]
end

include Tar_gz.Make(Monad)(Writer)(Reader)
