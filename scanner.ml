type t = char Stream.t * int ref

exception Peeked_past_end_of_stream

let of_char_stream stream = (stream, ref 1)

let next (stream, line) =
  match Stream.next stream with
    | '\n' -> line := !line + 1; '\n'
    | c -> c

let peek (stream, _) =
  match Stream.peek stream with
  | Some c -> c
  | _ -> raise Peeked_past_end_of_stream

let get_line (_, line) = !line
