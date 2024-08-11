open Utils.Configuration
open Utils.Common

type setup_conf = {
  me: (node_id * node_info);
  peers: (node_id * node_info) list
}
type peer = (node_id * (node_info))

class type ['a] handler  = object 
  method ready : unit -> bool
  method setup : setup_conf -> bool
  method close : unit -> unit
  method broadcast : 'a -> unit 
  method name : unit -> string
  method read : unit -> 'a option
end;;

module type Protocol
= sig

  (* type message_data = string *)

  val get_instance: unit -> 'message_data handler
end


