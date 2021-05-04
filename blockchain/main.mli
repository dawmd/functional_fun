type transaction
type register

(* an empty register of transaction *)
val empty_register : register
(* adds a new transaction to a register *)
val add_transaction : register -> transaction -> register

type block

(* returns the register a block consists of *)
val get_reg : block -> register
(* replaces the register of a block with a new one *)
val set_reg : block -> register -> block
(* creates a new, empty block consisting of a register *)
val create_block : register -> block
(* adds a new transaction to a block *)
val add_transaction_to_block : block -> transaction -> block

type blockchain

(* an empty blockchain *)
val empty_chain : blockchain
(* attaches a new block to a blockchain *)
val push_back : block -> blockchain -> blockchain
(* examines if a blockchain consists of correctly attached blocks *)
val is_chain_correct : blockchain -> bool
(* returns a blockchain's size -- the number of blocks
   it consists of *)
val chain_length : blockchain -> int
(* returns the n-th block in a blockchain *)
val get_nth_block : blockchain -> int -> block
