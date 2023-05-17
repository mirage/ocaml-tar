module Direct = struct
  type 'a t = 'a
  let return x = x
  let ( >>= ) m f = f m
end

module Cstruct_io = struct
  (* Input from a single Cstruct.t value *)

  type 'a io = 'a Direct.t

  type in_channel = {
    mutable pos : int;
    data : Cstruct.t;
  }

  let make_in_channel data =
    { pos = 0; data }

  let check_available ch len =
    min (Cstruct.length ch.data - ch.pos) len

  let really_read ic buf =
    let len = Cstruct.length buf in
    if check_available ic len <> len then raise End_of_file;
    Cstruct.blit ic.data ic.pos buf 0 len;
    ic.pos <- ic.pos + len

  let skip ic n =
    if check_available ic n <> n then raise End_of_file;
    ic.pos <- ic.pos + n

  (* Output to a list of Cstruct.t values *)

  type out_channel = {
    mutable data : Cstruct.t list;
  }

  let make_out_channel () = { data = [] }

  let really_write oc buf =
    oc.data <- Cstruct.sub_copy buf 0 (Cstruct.length buf) :: oc.data

  let to_string oc =
    Cstruct.copyv (List.rev oc.data)

  let to_cstruct oc =
    Cstruct.concat (List.rev oc.data)
end

module HeaderReader = Tar.HeaderReader(Direct)(Cstruct_io)
module HeaderWriter = Tar.HeaderWriter(Direct)(Cstruct_io)

include Cstruct_io
