type lzma_allocator
type lzma_stream

type data = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val lzma_decompress_xz_ba : data -> string Core.Or_error.t
val lzma_decompress_xz_string : string -> string Core.Or_error.t
val lzma_decompress_xz_bytes : bytes -> string Core.Or_error.t
val lzma_decompress_auto_ba : data -> string Core.Or_error.t
val lzma_decompress_auto_string : string -> string Core.Or_error.t
val lzma_decompress_auto_bytes : bytes -> string Core.Or_error.t

