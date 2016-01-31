type t = char Stream.t * int ref

let of_char_stream stream = (stream, ref 1)

let next (stream, line) =
  match Stream.next stream with
    | '\n' -> line := !line + 1; '\n'
    | c -> c

let peek (stream, _) = Stream.peek stream

let get_line (_, line) = !line
