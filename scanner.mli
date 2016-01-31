type t

val of_char_stream : char Stream.t -> t

val next : t -> char

val peek : t -> char option
