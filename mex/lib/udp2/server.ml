


let maxlen = 1024

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
          Marshal.from_bytes (Bytes.sub buffer 0 len) 0,
          (Unix.gethostbyaddr addr).Unix.h_name ^":"^ string_of_int port,
          sockaddr,
          remove_duplicates(sockaddr::connections)
      | _ -> assert false

let send_message sock adress message = 
  let reponse_len = String.length message in
  let response = (Bytes.of_string message) in 
  ignore(Unix.sendto sock response 0 reponse_len [] adress)

let rec broadcast sock adresses message = 
  Printf.printf "Broadcast %d" (List.length adresses);
  match adresses with 
  | adress::tail -> 
    send_message sock adress message;
    broadcast sock tail message
    | [] -> ignore()

  (* let reponse_len = String.length message in
  let response = (Bytes.of_string message) in 
  Unix.sendto sock response 0 reponse_len [] adress *)

let starts_with prefix str =
    let prefix_len = String.length prefix in
    let str_len = String.length str in
    if str_len < prefix_len then
      false
    else
      let prefix_sub = String.sub str 0 prefix_len in
      prefix_sub = prefix

let rec loop sock buffer connections : unit = 
  let message, hishost, _sockaddr, connections = recieve_message sock buffer connections in 
  Printf.printf "total connections %d\n" (List.length connections);
  Printf.printf "Client (%s) said: %s \n%!" hishost message;
  let response_str = 
    if message = "\n" 
      then "Why did you not write anything?\n" 
  else "CMD OK: " ^ message 
  in 
    (* let b = starts_with message "ALL" in  *)
    broadcast sock connections response_str;
    (* if b then  *)
    (* else ignore(send_message sock sockaddr response_str); *)
    loop sock buffer connections

type 'a server = {
  read: unit -> 'a
}
let start = fun (port: int) -> 
  let sock = get_socket port in
  let buffer = Bytes.create maxlen in
  let connections = [] in  
  {
    read =  fun () -> recieve_message sock buffer connections
  };;
  
  (* loop sock buffer []; *)
