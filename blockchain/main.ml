type hashcode = string

(* just some substitute for the actual function until I figure out
   how to get it in ocaml *)
let sha256 x : hashcode = "0000000000000000"

(* a record of a transaction between two people *)
type transaction_data = { payer : string; recipient : string; money : float }
type transaction      = { data : transaction_data; hash : hashcode }

(* Creates a new transaction or, if the amount of money is non-positive, returns None *)
let create_tx (payer : string) (recipient : string) (money : float) : transaction option =
  if money >= 0. then
    Some { payer = payer; recipient = recipient; money = money }
  else
    None

(* a register of transactions *)
type register         = { txs : transaction list; length : int }

(* new, empty register *)
let empty_register : register = { txs = []; length = 0 }

(* adds a new transaction to a register *)
let add_transaction (reg : register) (new_tran : transaction) : register =
  { txs = new_tran :: reg.txs; length = reg.length + 1 }



(* data a single block in a blockchain consists of *)
type blockdata = {
  num_of_block : int; (* number of a block *)
  nonce        : int; (* nonce value *)
  reg          : register; (* register of transactions *)
  (* the hash of the previous block in the chain
     or None if a block is the first one in a chain *)
  prev_hash    : hashcode option
}
(* a single block a blockchain consists of *)
type block = blockdata * hashcode

(* sets a block's number *)
let set_num (block : block) (new_num : int) : block =
  let new_data = {(fst block) with num_of_block = new_num} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

(* sets a block's nonce *)
let set_nonce (block : block) (new_nonce : int) : block =
  let new_data = {(fst block) with nonce = new_nonce} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

(* sets a block's register *)
let set_reg (block : block) (new_reg : register) : block =
  let new_data = {(fst block) with reg = new_reg} in
  let new_hash = sha256 new_data in
  (new_data, new_hash)

(* sets the previous hash's value of a block *)
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

(* adds a single transaction to a block *)
let add_transaction_to_block (block : block) (new_tran : transaction) : block =
  let new_reg = add_transaction (fst block).reg new_tran in
  set_reg block new_reg

(* blocks a blockchain consists of; the length of the chain *)
type blockchain = { blocks : block list; length : int }

(* an empty blockchain*)
let empty_chain : blockchain = { blocks = []; length = 0 }

(* examines if a blockchain is empty *)
let is_chain_empty (chain : blockchain) = chain = empty_chain

(* the number of zeroes at the beginning of a "good" hashcode *)
let num_of_zeros = 4

(* examines if the given hash is "good" *)
let check_hash (hash : hashcode) : bool =
  (* the hashcode consists of too few characters *)
  if String.length hash < num_of_zeros then
    false
  else
    (* checks if the first num_of_zeros characters
       in the hashcode are zeros - PoW *)
    let rec aux index =
      if index >= num_of_zeros then
        true
      else if String.get hash index = '0' then
        aux (index + 1)
      else
        false
    in aux 0

(* checks if a block's hash is the same as the one
   generated by the sha256 function
   -- if someone hasn't changed it --
   and if there's proof of work *)
let check_block_hash (block : block) : bool =
  let blocks_hash = sha256 (fst block) in
  snd block = blocks_hash && check_hash blocks_hash

(* checks if a block can be attached to a blockchain;
   if it can be, appends it at the end of it;
   if it cannot be, returns None *)
let append_block (block : block) (chain : blockchain) : blockchain option =
  if is_chain_empty chain then
    (* checks if the number of the block really is the smallest possible
       -- if it's equal to the default number of a block --
       and if the hash is correct *)
    if check_block_hash block && (fst block).num_of_block = default_num then
      Some { blocks = block :: []; length = 1 }
    else
      None
  else
    (* checks if the number of the new block is the concurrent number
       to the last one's in the chain, if its previous hash value
       is the same as the last block's hash and if the hash of the new
       block is correct -- PoW and the same as a new hash generated by sha256 *)
    let fst_block = List.hd chain.blocks in
    if check_block_hash block
    && (fst block).num_of_block = (fst fst_block).num_of_block + 1 then
      match (fst block).prev_hash with
      | None      -> None
      | Some hash ->
        if hash = snd fst_block then
          Some { blocks = block :: chain.blocks; length = chain.length + 1 }
        else
          None
    else
      None

(* finds the value of nonce a given block should have
   to have PoW and returns it *)
let find_nonce (block : block) : int =
  let data = fst block in
  (* checks all the integer numbers until it finds one
     that generates PoW *)
  let rec aux curr_nonce =
    let tmp_data = { data with nonce = curr_nonce } in
    let tmp_hash = sha256 tmp_data in
    if check_hash tmp_hash then
      curr_nonce
    else
      aux (curr_nonce + 1)
  in aux 0

(* mining -- tries to modify a block so that it can be
   attached at the end of a given blockchain *)
let mine (chain : blockchain) (block : block) : block =
  let last_block = List.hd chain.blocks in
  let last_num   = (fst last_block).num_of_block
  and prev_hash  = snd last_block in
  let block      = set_prev_hash (set_num block (last_num + 1)) prev_hash in
  let new_nonce  = find_nonce block in
  let new_block  = set_nonce block new_nonce in
  (new_block, sha256 new_block)

(* returns the n-th block in the blockchain (starting from 1)
   or, if there aren't such a block, returns None *)
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

(* checks if the chain consists of a correct sequence of blocks *)
let is_chain_correct (chain : blockchain) : bool =
  let blocklist = chain.blocks
  and length = chain.length in
  (* an empty blockchain is always correct *)
  if blocklist = [] then true else
  (* examines block by block, checking their hashes and numbers *)
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
    (* this case never happens *)
    | _ -> assert false in
  aux blocklist (length + default_num - 1)

(* returns a blockchain's length *)
let chain_length (chain : blockchain) : int = chain.length
  