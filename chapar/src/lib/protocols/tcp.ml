open Common
open Utils.Configuration
open Utils.Common

let has_connections = fun (sockets, timeout)-> 
  try let read, _, _ = Unix.select sockets [] [] timeout in Some(read) with Unix.Unix_error (err, fn, arg) -> None

exception ServerError of string

class ['a] tcp_server = object(self)  
  val max_connections = 10;

  val mutable status = "ACCEPTING"
  val mutable wanted_status = "ACCEPTING"

  val mutable port: int = -1
  val mutable is_created = false
  val mutable in_channels: in_channel list = [];
  val mutable sockets: Unix.file_descr list = [];
  val mutable out_channels: out_channel list = [];
  val mutable id = -1;
  val mutable peers: peer list = []
  val mutable connected_peers: node_id list = []
  val mutable socket: Unix.file_descr option = None;
  val mutable stopped = false;
  val mutable thread: Thread.t option = None;

  val incomming: 'a Queue.t =  Queue.create();
  val mutable accepting = true;

  method get_messages () = 
    incomming

  method listen () = 


    if stopped then ignore()
    else begin
    let result = has_connections(sockets, 0.01) in   
        match result  with 
          | Some(s) -> 
            List.iter(fun sock -> 
                Printf.printf "New message\n";
                flush_all();

                let payload = Marshal.from_channel (Unix.in_channel_of_descr sock) in 
                Queue.add payload incomming
              ) s;

          | None -> ignore()
        
      end

  method get_thread() = 
    match thread with 
    | Some(t) -> t 
    | None -> raise (ServerError("No thread"))

  method accept() =
    flush_all();

    match socket with 
    | Some(socket) -> begin
        match has_connections([socket], 0.01) with 
        | Some([]) -> ignore()
        | Some(c) -> begin
          let (s, addr) = Unix.accept socket in
          flush_all();

          let in_channel = Unix.in_channel_of_descr s in 
          let out_channel = Unix.out_channel_of_descr s in
    
          sockets <- s :: sockets;
          in_channels <-  in_channel :: in_channels; 
          out_channels <-  out_channel :: out_channels;
        
          ignore();
        end
    
        | None -> ignore()
        end
    | None -> ignore()
  

  method set_status (s) =
    wanted_status <- s;

  method clean_up () = 
      List.iter(fun sock -> 
        Unix.close sock;
      ) sockets; 

      match socket with
        | Some(s) -> Unix.close s;
        | None -> ();
     
  method step () = 

    if status == "ACCEPTING" && (max_connections <= List.length sockets) then begin
      wanted_status <- "LISTENING";
    end;

    if wanted_status != status then begin
        status <- wanted_status;
    end;

    match status with 
      | "ACCEPTING" -> 
        ignore(self#accept());
        ignore(self#step());
      | "LISTENING" -> 
        ignore(self#listen());
        ignore(self#step());
      | "STOPPED" -> self#clean_up()
      | _ -> raise (ServerError("Unknown status"))

     
        
  method start (p) = 
    port <- p;
    Printf.printf "Starting server on port %n...\n" p;
    let sock = 
      Unix.socket 
        Unix.PF_INET 
        Unix.SOCK_STREAM 
        0
      in
    socket <- Some(sock);
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Printf.eprintf "Bound socket server...\n";
    Unix.listen sock 4096;
    
    thread <- Some(Thread.create(fun () -> self#step())());
    ignore()

end

class ['a] tcp_client = object(self)
  val mutable peer_connections: Unix.file_descr list = [];

  method start = 
    Printf.printf "Starting client...\n";

  method connect_to (port) = 
    let server_addr = Unix.inet_addr_of_string "0.0.0.0" in
    let sock = Unix.socket PF_INET SOCK_STREAM 0 in 
    let server_port = port in
    let server_sockaddr = Unix.ADDR_INET (server_addr, server_port) in 
    
    try let _ = Unix.connect sock server_sockaddr in  
      peer_connections <- sock :: peer_connections;
    with Unix.Unix_error (err, fn, arg) -> self#connect_to(port);
    
  
  method connected() =
    peer_connections
  
  method broadcast (message: 'a) =
    ignore(List.iter (
        fun c -> begin
          let chan = Unix.out_channel_of_descr c in
          Marshal.to_channel chan message [];
          flush chan;
        end
    ) peer_connections);
end

class ['a] tcp_handler: ['a] handler = object(self)
  val server = new tcp_server;
  val client = new tcp_client;

  val mutable id = -1;
  val mutable peers: peer list = []


  val broadcaster = fun (instance) ->
    ignore()
  
  method close () = 
    server#set_status("STOPPED");
    Thread.join (server#get_thread());
     
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

    let connected = client#connected() in 
    let is_ready = (List.length peers - 1) == (List.length connected) in 
    (* Printf.printf "Checking if handler is ready %n/%n..\n" (List.length connected) (List.length peers - 1); *)

    if is_ready then begin 
      (* Printf.printf "TCP(%n) has found %n/%n peers, is ready. \n" id (List.length connected) (List.length peers - 1); *)
      server#set_status("LISTENING");
      flush_all()
    end;
    is_ready

  method broadcast (msg: 'a) = 
    client#broadcast(msg)

  method read () = 
    let message_queue = server#get_messages() in
    (* Printf.printf "Messages in queue(%n) %n \n" id (Queue.length (message_queue)); *)
    (* flush_all(); *)
    if (Queue.length (message_queue)) > 0 then 
      Some(Queue.pop (message_queue))
    else None

  method name () = "TCP"
end

module TCP: Protocol = struct 
  let get_instance = fun () -> new tcp_handler;;
end