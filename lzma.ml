open Core
open Ctypes
open PosixTypes
open Foreign
open Printf

(* TODO: Upstream this to Ctypes library
 * see https://github.com/ocamllabs/ocaml-ctypes/pull/573 *)

let carray_to_string a =
    let len = CArray.length a in
    let bytes = Bytes.create len in
    for i = 0 to len - 1 do
        Bytes.set bytes i (char_of_int
            (Unsigned.UInt8.to_int (CArray.unsafe_get a i)))
    done;
    Bytes.unsafe_to_string bytes

(* --------------------------------------------------------------------------- *)

type data = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* LZMA types *)

(* ALLOC_FUNC *)
(* typedef void * ALLOC_FUNC(
    void *opaque,
    size_t nmemb,
    size_t size);
 *)
let allocfunc_t = ptr void @-> size_t @-> size_t @-> returning (ptr void)

(* FREE_FUNC *)
(* typedef void FREE_FUNC(
    void *opaque,
    void *ptr);
 *)
let freefunc_t = ptr void @-> ptr void @-> returning void

type lzma_allocator
let lzma_allocator : lzma_allocator structure typ = structure "lzma_allocator"

let allocator_alloc = field lzma_allocator "alloc" (funptr allocfunc_t);;
let allocator_free = field lzma_allocator "free" (funptr freefunc_t);;

let () = seal lzma_allocator;;

(* --------------------------------------------------------------------------- *)
(* lzma_stream *)
type lzma_stream
let lzma_stream : lzma_stream structure typ = structure "lzma_stream"

let stream_next_in = field lzma_stream "next_in" (ptr uint8_t);;
let stream_avail_in = field lzma_stream "avail_in" size_t;;
let stream_total_in = field lzma_stream "total_in" uint64_t;;
let stream_next_out = field lzma_stream "next_out" (ptr uint8_t);;
let stream_avail_out = field lzma_stream "avail_out" size_t;;
let stream_total_out = field lzma_stream "total_out" uint64_t;;
let stream_allocator = field lzma_stream "allocator" (ptr lzma_allocator);;
let stream_internal = field lzma_stream "internal" (ptr void);; (* Internal state *)
let stream_res1 = field lzma_stream "reserved_ptr1" (ptr void);;
let stream_res2 = field lzma_stream "reserved_ptr2" (ptr void);;
let stream_res3 = field lzma_stream "reserved_ptr3" (ptr void);;
let stream_res4 = field lzma_stream "reserved_ptr4" (ptr void);;
let streak_seekpos = field lzma_stream "seek_pos" uint64_t;;
let stream_resint2 = field lzma_stream "reserved_int2" uint64_t;;
let stream_resint3 = field lzma_stream "reserved_int3" size_t;;
let stream_resint4 = field lzma_stream "reserved_int4" size_t;;
let stream_resenum1 = field lzma_stream "reserved_enum1" uint32_t;;
let stream_resenum2 = field lzma_stream "reserved_enum2" uint32_t;;

let () = seal lzma_stream;;

type lzma_stream_caml = {
    data : data;
}

(* lzma_filter *)
type lzma_filter
let lzma_filter : lzma_filter structure typ = structure "lzma_filter"

let filter_id = field lzma_filter "id" uint64_t;; (* lzma_vli *)
let filter_options = field lzma_filter "options" (ptr void);;

let () = seal lzma_filter;;

(* --------------------------------------------------------------------------- *)

(* XZ Utils (lzma.h) API *)

(*
 * int lzma_stream_decoder(lzma_stream *strm, uint64_t memlimit, uint32_t flags)
 *
 *)
let lzma_initialize =
    foreign "lzma_stream_decoder" (ptr lzma_stream @-> uint64_t @-> uint32_t @-> (returning int))

(*
 * void lzma_end(lzma_stream **strm)
 *
 *)
let lzma_finalize =
    foreign "lzma_end" (ptr (ptr lzma_stream) @-> (returning void))

(*
 * lzma_ret lzma_code(lzma_stream *strm, lzma_action action)
 *
 *)
let lzma_code =
    foreign "lzma_code" (ptr lzma_stream @-> int @-> (returning int))

(*
 * lzma_ret lzma_raw_decoder(lzma_stream *strm, const lzma_filter *filters)
 *
 *)
let lzma_raw_decoder =
    foreign "lzma_raw_decoder" (ptr lzma_stream @-> ptr lzma_filter @-> (returning int))

(*
 * lzma_ret lzma_auto_decoder(lzma_stream *strm, uint64_t memlimit, uint32_t flags)
 *
 *)
let lzma_auto_decoder =
    foreign "lzma_auto_decoder" (ptr lzma_stream @-> uint64_t @-> uint32_t @-> (returning int))

(*
 * lzma_ret lzma_alone_decoder(lzma_stream *strm, uint64_t memlimit)
 *
 *)
let lzma_alone_decoder =
    foreign "lzma_alone_decoder" (ptr lzma_stream @-> uint64_t @-> (returning int))

(* --------------------------------------------------------------------------- *)
let lzma_tell_no_check = 0x01
let lzma_tell_unsupported_check = 0x02
let lzma_tell_any_check = 0x04
let lzma_ignore_check = 0x10
let lzma_concatenated = 0x08

type lzma_ret =
    | LZMA_OK
    | LZMA_STREAM_END
    | LZMA_NO_CHECK
    | LZMA_UNSUPPORTED_CHECK
    | LZMA_GET_CHECK
    | LZMA_MEM_ERROR
    | LZMA_MEMLIMIT_ERROR
    | LZMA_FORMAT_ERROR
    | LZMA_OPTIONS_ERROR
    | LZMA_DATA_ERROR
    | LZMA_BUF_ERROR
    | LZMA_PROG_ERROR
    | LZMA_SEEK_NEEDED
    [@@deriving enum]

(* TODO: Add function for textual representation of errors *)

(* TODO: Propagate all errors up *)

(* --------------------------------------------------------------------------- *)

type lzma_action =
    | LZMA_RUN
    | LZMA_SYNC_FLUSH
    | LZMA_FULL_FLUSH
    | LZMA_FINISH
    | LZMA_FULL_BARRIER
    [@@deriving enum]

(* --------------------------------------------------------------------------- *)

let lzma_init () =
    let stream = allocate_n lzma_stream ~count:1 in
    let flags = Unsigned.UInt32.of_int lzma_concatenated in
    let memlimit = Unsigned.UInt64.of_int64 Int64.max_value in
    let res = lzma_ret_of_enum
        (lzma_initialize stream memlimit flags) in
    match res with
    | Some LZMA_OK -> Ok stream
    | _ -> Or_error.error_string "Cannot initialize XZ LZMA stream"


let lzma_init_auto () =
    let stream = allocate_n lzma_stream ~count:1 in
    let flags = Unsigned.UInt32.of_int lzma_concatenated in
    let memlimit = Unsigned.UInt64.of_int64 Int64.max_value in
    let res = lzma_ret_of_enum
        (lzma_auto_decoder stream memlimit flags) in
    match res with
    | Some LZMA_OK -> Ok stream
    | _ -> Or_error.error_string "Cannot initialize LZMA stream (AUTO)"

let lzma_init_raw () =
    let stream = allocate_n lzma_stream ~count:1 in
    let flags = Unsigned.UInt32.of_int lzma_concatenated in
    let memlimit = Unsigned.UInt64.of_int64 Int64.max_value in
    let res = lzma_ret_of_enum
        (lzma_auto_decoder stream memlimit flags) in
    match res with
    | Some LZMA_OK -> Ok stream
    | _ -> Or_error.error_string "Cannot initialize raw LZMA stream"

let lzma_deinit stream =
    lzma_finalize stream

let lzma_decompress_internal stream action dataptr datasz =
    let datasize = Unsigned.Size_t.to_int datasz in
    let outbufsize = datasize * 10 in
    let outbuf = CArray.make uint8_t outbufsize in
    let outbufptr = CArray.start outbuf in
    let outbufsz = Unsigned.Size_t.of_int outbufsize in
    setf (!@ stream) stream_next_in dataptr;
    setf (!@ stream) stream_avail_in datasz;
    setf (!@ stream) stream_next_out outbufptr;
    setf (!@ stream) stream_avail_out outbufsz;
    let res = lzma_ret_of_enum (lzma_code stream (lzma_action_to_enum action)) in
    match res with
    | Some LZMA_OK -> (
            let totalout = getf (!@ stream) stream_total_out in
            let tot64 = (Unsigned.UInt64.to_int64 totalout) in
            Printf.printf "LZMA_DEC: uncompressed 0x%x bytes -> 0x%Lx bytes\n"
                datasize tot64 ;
            (* Save the outstream *)
            if tot64 < (Int64.of_int outbufsize) then
                match (Int64.to_int tot64) with
                | Some total ->
                    let realout = CArray.sub outbuf 0 total in
                    let outstr = carray_to_string realout in
                    Ok outstr
                | None ->
                        Or_error.error_string "Output is too big"
            else
                Or_error.error_string "Decompression: output is too big"
    )
    | Some LZMA_MEM_ERROR ->
            Or_error.error_string "Decompression: memory allocation failed"
    | Some LZMA_FORMAT_ERROR ->
            Or_error.error_string "Decompression: wrong stream format"
    | Some LZMA_OPTIONS_ERROR ->
            Or_error.error_string "Decompression: unsupported compression options"
    | Some LZMA_DATA_ERROR -> (
            (* Here we still can have some output *)
            let totalout = getf (!@ stream) stream_total_out in
            let tot64 = (Unsigned.UInt64.to_int64 totalout) in
            Printf.printf "LZMA_DEC [corrupted]: uncompressed 0x%x bytes -> 0x%Lx bytes\n"
                datasize tot64 ;
            (* Save the outstream *)
            if tot64 < (Int64.of_int outbufsize) then
                match (Int64.to_int tot64) with
                | Some total ->
                    let realout = CArray.sub outbuf 0 total in
                    let outstr = carray_to_string realout in
                    Ok outstr
                | None ->
                    Or_error.error_string "Decompression: data is corrupt and output is too big"
            else
                Or_error.error_string "Decompression: data is corrupt and output is too big"
    )
    | Some LZMA_BUF_ERROR ->
            Or_error.error_string "Decompression: compressed data is truncated"
    | _ -> Or_error.error_string "Decompression: unknown error"

let lzma_decompress_xz_ba (ba : data) =
    let action = LZMA_RUN in
    let maybe_stream = lzma_init () in
    match maybe_stream with
    | Ok stream ->
            let datasize = Bigarray.Array1.dim ba in
            let datasz = Unsigned.Size_t.of_int datasize in
            let datap = Ctypes.bigarray_start array1 ba in
            let dataptr = coerce (ptr char) (ptr uint8_t) datap in
            lzma_decompress_internal stream action dataptr datasz
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA stream"

let lzma_decompress_xz_string buf =
    let action = LZMA_RUN in
    let maybe_stream = lzma_init () in
    match maybe_stream with
    | Ok stream ->
            let datasize = String.length buf in
            let datasz = Unsigned.Size_t.of_int datasize in
            let bufp = CArray.of_string buf in
            let dataptr = coerce (ptr char) (ptr uint8_t) (CArray.start bufp) in
            lzma_decompress_internal stream action dataptr datasz
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA stream"

let lzma_decompress_xz_bytes buf =
    lzma_decompress_xz_string (Bytes.unsafe_to_string buf)

let lzma_decompress_auto_ba (ba : data) =
    let action = LZMA_RUN in
    let maybe_stream = lzma_init_auto () in
    match maybe_stream with
    | Ok stream ->
            let datasize = Bigarray.Array1.dim ba in
            let datasz = Unsigned.Size_t.of_int datasize in
            let datap = Ctypes.bigarray_start array1 ba in
            let dataptr = coerce (ptr char) (ptr uint8_t) datap in
            lzma_decompress_internal stream action dataptr datasz
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA stream"

let lzma_decompress_auto_string buf =
    let action = LZMA_RUN in
    let maybe_stream = lzma_init_auto () in
    match maybe_stream with
    | Ok stream ->
            let datasize = String.length buf in
            let datasz = Unsigned.Size_t.of_int datasize in
            let bufp = CArray.of_string buf in
            let dataptr = coerce (ptr char) (ptr uint8_t) (CArray.start bufp) in
            lzma_decompress_internal stream action dataptr datasz
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA stream"

let lzma_decompress_auto_bytes buf =
    lzma_decompress_auto_string (Bytes.unsafe_to_string buf)

let lzma_decompress_raw_ba (ba : data) =
    let action = LZMA_RUN in
    let maybe_stream = lzma_init_raw () in
    match maybe_stream with
    | Ok stream ->
            let datasize = Bigarray.Array1.dim ba in
            let datasz = Unsigned.Size_t.of_int datasize in
            let datap = Ctypes.bigarray_start array1 ba in
            let dataptr = coerce (ptr char) (ptr uint8_t) datap in
            lzma_decompress_internal stream action dataptr datasz
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA stream"

let lzma_decompress_raw_string buf =
    let action = LZMA_RUN in
    let maybe_stream = lzma_init_raw () in
    match maybe_stream with
    | Ok stream ->
            let datasize = String.length buf in
            let datasz = Unsigned.Size_t.of_int datasize in
            let bufp = CArray.of_string buf in
            let dataptr = coerce (ptr char) (ptr uint8_t) (CArray.start bufp) in
            lzma_decompress_internal stream action dataptr datasz
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA stream"

let lzma_decompress_raw_bytes buf =
    lzma_decompress_raw_string (Bytes.unsafe_to_string buf)
