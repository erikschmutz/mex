open Common
open Utils.Configuration
open Utils.Common
let maxlen = 1024

let rec select_ready rs ws es t =
  try Unix.select rs ws es t
  with Unix.Unix_error (err, fn, arg) ->
      select_ready rs ws es t 
      
let has_connections = fun (sockets, timeout)-> 
  try let read, _, _ = Unix.select sockets [] [] timeout in Some(read) with Unix.Unix_error (err, fn, arg) -> None

exception ServerError of string

let send socket message =
  let response = Marshal.to_bytes message [] in
  let response_len = (Bytes.length response) in 
  ignore(Unix.send socket response 0 response_len [])

  
let connect server_name server_port =  
  let server_addr =
    try (Unix.gethostbyname server_name).h_addr_list.(0)
    with Not_found ->
      prerr_endline (server_name ^ ": Host not found");
      exit 2 
  in 
  let sock = 
    Unix.socket 
      Unix.PF_INET 
      Unix.SOCK_DGRAM 
      (Unix.getprotobyname "udp").Unix.p_proto 
    in
  Unix.connect sock (Unix.ADDR_INET (server_addr, server_port));
  sock
  
let get_socket = fun (port) ->  
  let sock = 
      Unix.socket 
        Unix.PF_INET 
        Unix.SOCK_DGRAM 
        (Unix.getprotobyname "udp").Unix.p_proto 
      in
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  sock

let remove_duplicates (type a) (l: a list) =
  let module S = Set.Make(struct type t = a let compare = compare end) in
  let rec remove acc seen_set = function
      | [] -> List.rev acc
      | a :: rest when S.mem a seen_set -> remove acc seen_set rest
      | a :: rest -> remove (a::acc) (S.add a seen_set) rest in
  remove [] S.empty l
  
let recieve_message sock buffer connections =
  match Unix.recvfrom sock buffer 0 maxlen [] with
      | len, (Unix.ADDR_INET (addr, port) as sockaddr) ->
          Some(Marshal.from_bytes (Bytes.sub buffer 0 len) 0),
          (Unix.gethostbyaddr addr).Unix.h_name ^":"^ string_of_int port,
          sockaddr,
          remove_duplicates(sockaddr::connections)
      | _ -> assert false

let send_message sock adress message = 
  let reponse_len = String.length message in
  let response = (Bytes.of_string message) in 
  ignore(Unix.sendto sock response 0 reponse_len [] adress)
  
class ['a] udp_server = object(self)  
  val mutable socket: Unix.file_descr option = None;
  val mutable connections: Unix.sockaddr list = [];

  method read(): 'a option = 
    let buffer = Bytes.create maxlen in
    match socket with 
    | Some(s)-> begin 
        match has_connections ([s], 0.1) with 
        | Some([])-> None
        | Some(head:_)-> 
          let payload, _, _,c = recieve_message s buffer connections in 
          connections <- c;
          payload
        | None -> None
      end
    | None -> None;



  method start (port:int) = 
    socket <- Some(get_socket port);

end

class ['a] udp_client = object(self)
  val mutable peer_connections: Unix.file_descr list = [];

  method start = 
    Printf.printf "Starting client...\n";

  method connect_to (port) = 
    let connection = connect "localhost" port in  
    peer_connections <- connection::peer_connections
    
  method connected() =
    peer_connections
  
  method broadcast (message: 'a) =
    ignore(List.iter (
        fun c -> begin
          send c message;
        end
    ) peer_connections);
end

class ['a] udp_handler: ['a] handler = object(self)
  val server = new udp_server;
  val client = new udp_client;

  val mutable id = -1;
  val mutable peers: peer list = []

  val broadcaster = fun (instance) ->
    ignore()
  
  method close () = 
    ignore()
     
  method setup (c: setup_conf) = 
    let node_id, node_info = c.me in 
    server#start(node_info.port);
    client#start;

    id <- node_id;
    peers <- c.peers;

    List.iter (fun (peer: peer) -> 
      let i, (node_info) = peer in 
      if id != i then 
        client#connect_to(node_info.port)
    ) peers; 

    true;

  method ready() = 
    true

  method broadcast (msg: 'a) = 
    client#broadcast(msg)

  method read () = 
    server#read()

  method name () = "UDP"
end

module UDP: Protocol = struct 
  let get_instance = fun () -> new udp_handler;;
end