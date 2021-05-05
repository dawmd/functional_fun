(* just some substitute for the actual function until I figure out
   how to get it in ocaml *)
let sha256 x : hashcode = "0000000000000000"

type transaction_data = string * string * int
type transaction = { data : transaction_data; hash : hashcode }
type register = { txs : transaction list; length : int }
type hashcode = string

(* new, empty register *)
let empty_register : register = { txs = []; length = 0}

(* adds a new transaction to a register *)
let add_transaction (reg : register) (new_tran : transaction) : register =
  { txs = new_tran :: reg.txs; length = reg.length + 1 }



(* number of block, nonce, list of transactions, previous block's hash, hash *)
type blockdata = {
  num_of_block : int;
  nonce : int;
  reg : register;
  prev_hash : hashcode option
}
type block = blockdata * hashcode

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



type blockchain = {blocks : block list; length : int}

let empty_chain : blockchain = {blocks = []; length = 0}

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

type blockchain = { blocks : block list; length : int }

let empty_chain : blockchain = { blocks = []; length = 0 }

let is_chain_empty (chain : blockchain) = chain = empty_chain

(* the number of zeroes at the beginning of a "good" hashcode *)
let num_of_zeros = 4

(* examines if the given hash is "good" *)
let check_hash (hash : hashcode) : bool =
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

let check_block_hash (block : block) : bool =
  let blocks_hash = sha256 (fst block) in
  snd block = blocks_hash && check_hash blocks_hash

let append_block (block : block) (chain : blockchain) : blockchain option =
  if is_chain_empty chain then
    if check_block_hash block && (fst block).num_of_block = default_num then
      Some { blocks = block :: []; length = 1 }
    else
      None
  else
    let fst_block = List.hd chain.blocks in
    if check_block_hash block
    && (fst block).num_of_block = (fst fst_block).num_of_block + 1 then
      match (fst block).prev_hash with
      | None -> None
      | Some hash ->
        if hash = snd fst_block then
          Some { blocks = block :: chain.blocks; length = chain.length + 1 }
        else
          None
    else
      None

(* finds the value of nonce the given block should have *)
let find_nonce (block : block) =
  let data = fst block in
  let rec aux curr_nonce =
    let tmp_data = { data with nonce = curr_nonce } in
    let tmp_hash = sha256 tmp_data in
    if check_hash tmp_hash then
      curr_nonce
    else
      aux (curr_nonce + 1)
  in aux 0

let mine (chain : blockchain) (block : block) =
  let last_block = List.hd chain.blocks in
  let last_num = (fst last_block).num_of_block
  and prev_hash = snd last_block in
  let block = set_prev_hash (set_num block (last_num + 1)) prev_hash in
  let new_nonce = find_nonce block in
  let new_block = { (fst block) with nonce = new_nonce } in
  (new_block, sha256 new_block)

let get_nth_block (chain : blockchain) (num_of_block : int) : block option =
  if chain.length + default_num - 1 < num_of_block || num_of_block < default_num then
    None
  else
    let rec aux curr_num ls =
      if num_of_block = curr_num then
        Some (List.hd ls)
      else
        aux (curr_num - 1) (List.tl ls) in
    aux (chain.length + default_num - 1) chain.blocks

let is_chain_correct (chain : blockchain) : bool =
  let blocklist = chain.blocks
  and length = chain.length in
  let rec aux ls curr_num =
    match ls with
    | (data, hash) :: [] ->
      if data.num_of_block <> curr_num then
        false
      else if data.prev_hash <> None then
        false
      else
        check_block_hash (data, hash)
    | (data, hash) :: t ->
      if data.num_of_block <> curr_num then
        false
      else if data.prev_hash = Some (snd (List.hd t)) then
        false
      else if check_block_hash (data, hash) then
        aux t (curr_num - 1)
      else false
    | _ -> assert false in
  aux blocklist (length + default_num - 1)