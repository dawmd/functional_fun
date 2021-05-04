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
type block = int * int * register * hashcode * hashcode
type blockchain = block list * int

(* just some substitute for the actual function until I figure out
   how to get it in ocaml *)
let sha256 x = "0000000000000000"

(* returns the number of the given block *)
let get_num ((num, _, _, _, _) : block) = num

(* returns the register the given block includes *)
let get_reg ((_, _, reg, _, _) : block) = reg

(* returns the hash of the previous block in the blockchain
   the given one is in *)
let get_prev_hash ((_, _, _, prev_hash, _) : block) = prev_hash

(* returns the hash of the block *)
let get_hash ((_, _, _, _, hash) : block) = hash

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
let set_num ((_, nounce, reg, prev_hash, _) : block) (new_num : int) : block =
  let new_hash = sha256 (new_num, nounce, reg, prev_hash) in
  (new_num, nounce, reg, prev_hash, new_hash)

(* sets the nounce of the given block to a new one *)
let set_nounce ((num, _, reg, prev_hash, _) : block) (new_nounce : int) : block =
  let new_hash = sha256 (num, new_nounce, reg, prev_hash) in
  (num, new_nounce, reg, prev_hash, new_hash)

(* sets the register of the given block to a new one *)
let set_reg ((num, nounce, _, prev_hash, _) : block) (new_reg : register) : block =
  let new_hash = sha256 (num, nounce, new_reg, prev_hash) in
  (num, nounce, new_reg, prev_hash, new_hash)

(* sets the previous hashcode of the given block to a new one *)
let set_prev_hash ((num, nounce, reg, _, _) : block) (new_prev_hash : hashcode) : block =
  let new_hash = sha256 (num, nounce, reg, new_prev_hash) in
  (num, nounce, reg, new_prev_hash, new_hash)

(* default value of the nounce field *)
let default_nounce = 0
(* default number of a block *)
let default_num = 0

(* creates a new block consisting of a number and register *)
let create_block (reg : register) : block =
  let new_hash = sha256 (default_num, default_nounce, reg, default_hash) in
  (default_num, default_nounce, reg, default_hash, new_hash)

(* adds a new transaction to a block *)
let add_transaction_to_block (block : block) (new_tran : transaction) : block =
  let new_reg = add_transaction (get_reg block) new_tran in
  set_reg block new_reg

(* new, empty blockchain *)
let empty_chain : blockchain = ([], 0)

(* modifies a block so it can be attached at the end of another one *)
let attach_block (prev_block : block) (new_block : block) : block =
  let prev_num = get_num prev_block
  and prev_hash = get_hash prev_block in
  let modified_block = set_num new_block (prev_num + 1) in
  set_prev_hash modified_block prev_hash

(* adds a new block to a blockchain *)
let push_back (block : block) (chain : blockchain) : blockchain =
  let ls = fst chain in
  match ls with
  | [] -> (block :: [], 1)
  | h :: t ->
    let modified_block = attach_block h block in
    (modified_block :: ls, snd chain + 1)

(* examines if the blockchain consists of a correct sequence of blocks *)
let is_chain_correct (chain : blockchain) =
  let rec aux ls =
    match ls with
    | h :: [] -> true
    | h :: t ->
      if not (check_hash (get_hash h)) then
        false
      else
        let next_head = List.hd t in
        if not (get_prev_hash h = get_hash next_head) then
          false
        else
          aux t
    | [] -> true
  in aux (fst chain)


(* returns the n-th newest block in a blockchain *)
let get_nth_block (chain : blockchain) (num_of_block : int) : block =
  let chain_length = snd chain in
  if chain_length < num_of_block || num_of_block < default_num then
    assert false (* should throw an exception instead *)
  else
    let rec aux num ls =
      if num = num_of_block then
        List.hd ls
      else
        aux (num - 1) (List.tl ls)
    in aux chain_length (fst chain)

let chain_length (chain : blockchain) : int = snd chain