type transaction
type register

(* an empty register of transaction *)
val empty_register : register
(* adds a new transaction to a register *)
val add_transaction : register -> transaction -> register

type block

(* creates a new, empty block consisting of a register *)
val create_block : register -> block
(* adds a new transaction to a block *)
val add_transaction_to_block : block -> transaction -> block

type blockchain

(* an empty blockchain *)
val empty_chain : blockchain
(* checks if a blockchain is empty *)
val is_chain_empty : blockchain -> bool
(* attaches a new block at the end of a blockchain *)
val append_block : block -> blockchain -> blockchain
(* modifies a given block so it can be attached to a given blockchain *)
val mine : blockchain -> block -> block
(* returns the n-th block in a blockchain *)
val get_nth_block : blockchain -> int -> block
(* examines if a blockchain consists of correctly attached blocks *)
val is_chain_correct : blockchain -> bool
(* returns a blockchain's length -- the number of blocks
   it consists of *)
val chain_length : blockchain -> int
