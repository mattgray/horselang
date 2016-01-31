type t = (char * int * int) Stream.t

let of_char_stream s = Stream.from
    (fun _ -> try Some (Stream.next s, 0, 0) with Stream.Failure -> None)

let next s = let c, _, _ = Stream.next s in c

let peek s =
  match Stream.peek s with
    | Some (c, _, _) -> Some c
    | None -> None
