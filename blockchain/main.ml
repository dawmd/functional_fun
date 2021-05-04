(* person A, person B, the amount of money person A transfers to person B *)
type transaction = string * string * int
type register = transaction list

(* new, empty register *)
let empty_register : register = []

(* adds a new transaction to a register *)
let add_transaction (reg : register) (new_tran : transaction) : register =
  new_tran :: reg

(* number of block, nounce, list of transactions, previous block's hash, hash *)
type hashcode = string
type block = int * int * register * hashcode * hashcode
type blockchain = block list

(* new, empty blockchain *)
let empty_chain : blockchain = []

(* just some substitute for the actual function until I figure out
   how to get it in ocaml *)
let sha256 x = "0000000000000000"

(* returns the number of the given block *)
let get_num (block : block) =
  match block with
  | (num, _, _, _, _) -> num

(* returns the register the given block includes *)
let get_reg (block : block) =
  match block with
  | (_, _, reg, _, _) -> reg

(* returns the hash of the previous block in the blockchain
   the given one is in *)
let get_prev_hash (block : block) =
  match block with
  | (_, _, _, prev_hash, _) -> prev_hash

(* returns the hash of the block *)
let get_hash (block : block) =
  match block with
  | (_, _, _, _, hash) -> hash

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


(* finds the value of nounce the given block should have *)
let find_nounce (block : block) =
  let num_of_block = get_num block
  and reg = get_reg block
  and prev_hash = get_prev_hash block in
  let rec aux curr_nounce =
    let tmp_hash = sha256 (num_of_block, curr_nounce, reg, prev_hash) in
    if check_hash tmp_hash then
      curr_nounce
    else
      aux (curr_nounce + 1)
  in aux 0

(* default hashcode consisting of 16 zeros *)
let default_hash : hashcode = "0000000000000000"

(* sets the number of the given block to a new one *)
let set_num (block : block) (new_num : int) : block =
  match block with
  | (_, nounce, reg, prev_hash, _) ->
    let new_hash = sha256 (new_num, nounce, reg, prev_hash) in
    (new_num, nounce, reg, prev_hash, new_hash)

(* sets the nounce of the given block to a new one *)
let set_nounce (block : block) (new_nounce : int) : block =
  match block with
  | (num, _, reg, prev_hash, _) ->
    let new_hash = sha256 (num, new_nounce, reg, prev_hash) in
    (num, new_nounce, reg, prev_hash, new_hash)

(* sets the register of the given block to a new one *)
let set_reg (block : block) (new_reg : register) : block =
  match block with
  | (num, nounce, _, prev_hash, _) ->
    let new_hash = sha256 (num, nounce, new_reg, prev_hash) in
    (num, nounce, new_reg, prev_hash, new_hash)

(* sets the previous hashcode of the given block to a new one *)
let set_prev_hash (block : block) (new_prev_hash : hashcode) : block =
  match block with
  | (num, nounce, reg, _, _) ->
    let new_hash = sha256 (num, nounce, reg, new_prev_hash) in
    (num, nounce, reg, new_prev_hash, new_hash)

(* default value of the nounce field *)
let default_nounce = 0

(* creates a new block consisting of a number and register *)
let create_block (num_of_block : int) (reg : register) : block =
  let new_hash = sha256 (num_of_block, default_nounce, reg, default_hash) in
  (num_of_block, default_nounce, reg, default_hash, new_hash)

(* adds a new block to a blockchain *)
let push_back (block : block) (chain : blockchain) : blockchain =
  let prev_hash = get_hash (List.hd chain)
  and prev_num = get_num (List.hd chain) in
  let modified_block = set_prev_hash block prev_hash in
  let modified_block = set_num modified_block (prev_num + 1) in
  let new_nounce = find_nounce modified_block in
  let new_block : block = match modified_block with
  | (num, _, reg, _, _) ->
    let new_hash = sha256 (num, new_nounce, reg, prev_hash) in
    (num, new_nounce, reg, prev_hash, new_hash) in
  new_block :: chain

(* adds a new transaction to a block *)
let add_transaction_to_block (block : block) (new_tran : transaction) : block =
  let new_reg = add_transaction (get_reg block) new_tran in
  set_reg block new_reg

(* examines if the blockchain consists of a correct sequence of blocks *)
let rec is_chain_correct (chain : blockchain) =
  match chain with
  | h :: [] -> true
  | h :: t ->
    if not (check_hash (get_hash h)) then
      false
    else
      let next_head = List.hd t in
      if not (get_prev_hash h = get_hash next_head) then
        false
      else
        is_chain_correct t
  | [] -> true

(* returns the n-th newest block in a blockchain *)
let rec get_nth_block (chain : blockchain) (num_of_block : int) : block =
  match chain with
  | [] -> assert false
  | h :: t ->
    if num_of_block = 0 then h else get_nth_block chain (num_of_block - 1)


(*
TO DO:
1) modify the register type to include the number of transactions
2) modify the blockchain type to include the number of blocks in the chain
*)