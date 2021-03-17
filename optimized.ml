module BytesHash = struct
  include Bytes
  let hash (k : t) = Hashtbl.hash k
end
module BytesHashtbl = Hashtbl.Make (BytesHash)

let lowercase_inplace pad i j =
  for k = i to j - 1 do
    Bytes.unsafe_set pad k @@ Char.lowercase_ascii @@ Bytes.unsafe_get pad k
  done

let sub_and_add_word countwords source i len =
  let word = Bytes.sub source i len in
  match BytesHashtbl.find countwords word with
  | v ->
    BytesHashtbl.replace countwords word (v + 1)
  | exception Not_found ->
    BytesHashtbl.add countwords word 1

let[@inline] is_whitespace c = c = ' ' || c = '\n'

let rec skip_whitespace countwords pad ~lim i =
  if i >= lim then
    (read_more_and_white[@taillcall]) countwords pad i
  else if is_whitespace @@ Bytes.unsafe_get pad i then
    (skip_whitespace[@tailcall]) countwords pad ~lim (i + 1)
  else
    (read_next_word[@tailcall]) countwords pad ~lim i i
and read_next_word countwords pad ~lim i j =
  if j >= lim then
    (read_more_and_next[@taillcall]) countwords pad i j
  else if is_whitespace @@ Bytes.unsafe_get pad j then begin
    sub_and_add_word countwords pad i (j-i);
    (skip_whitespace[@taillcall]) countwords pad ~lim (j+1)
  end else
    (read_next_word[@taillcall]) countwords pad ~lim i (j + 1)
and read_more_and_next countwords pad i j =
  let l = j - i in
  Bytes.blit pad i pad 0 l;
  let read = input stdin pad l (Bytes.length pad - l) in
  let lim = l+read in
  lowercase_inplace pad l lim;
  if lim = 0 then ()
  else (read_next_word[@taillcall]) countwords pad ~lim 0 l
and read_more_and_white countwords pad i =
  let lim = input stdin pad 0 (Bytes.length pad) in
  lowercase_inplace pad 0 lim;
  if lim = 0 then ()
  else (skip_whitespace[@taillcall]) countwords pad ~lim 0

let[@inline] read_start countwords pad =
  (read_more_and_next[@specialize]) countwords pad 0 0

let[@inline] print_slot (w, c) =
  print_bytes w;
  print_char ' ';
  print_int c;
  print_newline ()

let () =
  let countwords = BytesHashtbl.create 33_000 in
  let size = 64*1024 in
  let pad = Bytes.make size ' ' in
  read_start countwords pad ;

  let a = Array.make (BytesHashtbl.length countwords) (Bytes.empty,0) in
  let f k v i = a.(i) <- (k,v); i+1 in
  let _ = BytesHashtbl.fold f countwords 0 in
  Array.sort (fun (_,i) (_,j) -> Int.compare j i) a;
  Array.iter print_slot a
