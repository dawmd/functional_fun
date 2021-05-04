type transaction
type register

val empty_register : register
val add_transaction : register -> transaction -> register

type hashcode
type block
type blockchain

val get_reg : block -> register
val set_num : block -> int -> block
val set_reg : block -> register -> block
val create_block : register -> block
val add_transaction_to_block : block -> transaction -> block
val empty_chain : blockchain
val push_back : block -> blockchain -> blockchain
val is_chain_correct : blockchain -> bool
val get_nth_block : blockchain -> int -> block
val chain_length : blockchain -> int
