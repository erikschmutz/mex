let maxlen = 1024;;
type 'a payload = ('a * string * Unix.sockaddr);;

type 'a client =  {
  sock: Unix.file_descr;
  write: 'a -> unit;
  read: unit -> 'a payload;
};;

let recieve_message sock : 'a payload = 
  let buffer = Bytes.create maxlen in 
  match Unix.recvfrom sock buffer 0 maxlen [] with
      | len, (Unix.ADDR_INET (addr, _) as sockaddr) ->
          Marshal.from_bytes (Bytes.sub buffer 0 len) len,
          (Unix.gethostbyaddr addr).Unix.h_name,
          sockaddr
      | _ -> assert false

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

let start = fun port ->
  let connection = connect "localhost" port in 
  let s = send connection in 
  let r = fun () -> recieve_message connection in
  {
    read = r;
    write = s;
    sock = connection
  }

