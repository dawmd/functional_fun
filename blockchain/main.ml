(* person A, person B, the amount of money person A transfers to person B *)
type transaction = string * string * int
type register = transaction list * int

(* new, empty register *)
let empty_register : register = ([], 0)

(* adds a new transaction to a register *)
let add_transaction (reg : register) (new_tran : transaction) : register =
  (new_tran :: (fst reg), snd reg + 1)



(* number of block, nounce, list of transactions, previous block's hash, hash *)
type hashcode = string
type blockdata = {
  num_of_block : int;
  nonce : int;
  reg : register;
  prev_hash : hashcode option
}
type block = blockdata * hashcode

(* just some substitute for the actual function until I figure out
   how to get it in ocaml *)
let sha256 x = "0000000000000000"

(* the number of zeroes at the beginning of a "good" hashcode *)
let num_of_zeros = 4

(* examines if the given hash is "good" *)
let check_hash hash =
  if String.length hash < num_of_zeros then
    false
  else
    let rec aux index =
      if index >= num_of_zeros then
        true
      else if String.get hash index = '0' then
        aux (index + 1)
      else
        false
    in aux 0

(* finds the value of nonce the given block should have *)
let find_nonce (block : block) =
  let data = fst block in
  let rec aux curr_nonce =
    let tmp_data = {data with nonce = curr_nonce} in
    let tmp_hash = sha256 tmp_data in
    if check_hash tmp_hash then
      curr_nonce
    else
      aux (curr_nonce + 1)
  in aux 0

let set_num (block : block) (new_num : int) : block =
  let new_data = {(fst block) with num_of_block = new_num} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

let set_nonce (block : block) (new_nonce : int) : block =
  let new_data = {(fst block) with nonce = new_nonce} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

let set_reg (block : block) (new_reg : register) : block =
  let new_data = {(fst block) with reg = new_reg} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

let set_prev_hash (block : block) (new_prev_hash : hashcode) : block =
  let new_data = {(fst block) with prev_hash = Some new_prev_hash} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

(* default number of a block *)
let default_num = 0
(* default value of the nounce field *)
let default_nonce = 0

(* creates a new block consisting of a given register *)
let create_block (reg_of_block : register) : block =
  let new_data = {
    num_of_block = default_num;
    nonce        = default_nonce;
    reg          = reg_of_block;
    prev_hash    = None
  } in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

let add_transaction_to_block (block : block) (new_tran : transaction) : block =
  let new_reg = add_transaction (fst block).reg new_tran in
  set_reg block new_reg



type blockchain = block list * int

let empty_chain : blockchain = ([], 0)

let push_back (block : block) (chain : blockchain) : blockchain =
  let ls = fst chain in
  match ls with
  | [] -> (block :: [], 1)
  | h :: t ->
    let previous_hash = snd h in
    let new_data = {(fst block) with prev_hash = Some previous_hash} in
    let new_nonce = find_nonce (new_data, snd block) in
    let new_data = {new_data with nonce = new_nonce} in
    ((new_data, sha256 new_data) :: ls, snd chain + 1)

let is_chain_empty (chain : blockchain) = chain = ([], 0)

let is_chain_correct (chain : blockchain) =
  if is_chain_empty chain then true else
  let rec aux ls =
    let head = List.hd ls
    and tail = List.tl ls in
    if not (check_hash (snd head)) then
      false
    else
      let next_head = List.hd tail in
      match (fst head).prev_hash with
      | None -> if tail = [] then true else false
      | Some hash ->
        if hash <> snd next_head then
          false
        else
          aux tail in
  aux (fst chain)

let chain_length (chain : blockchain) : int = snd chain

let get_nth_block (chain : blockchain) (num_of_block : int) : block =
  if chain_length chain < num_of_block || num_of_block < default_num then
    assert false
  else
    let rec aux num ls =
      if num = num_of_block then
        List.hd ls
      else
        aux (num - 1) (List.tl ls) in
    aux (chain_length chain) (fst chain)


(* TO DO: add comments, update the mli file *)
