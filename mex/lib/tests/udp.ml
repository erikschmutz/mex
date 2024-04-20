module Simple = struct


  type message = (int * string)
  
  let start_server = fun _ -> 
    print_endline "~~Server started~~\n";
    let server = Lib.Udp.start_server 8082 in 
    let result : (message * _ * _) = server.read() in
    let (msg, payload), _, _ = result in
    Printf.printf "%i %s" msg payload
   
    let start_client = fun _ -> 
      print_endline "~~Client started~~\n";

      let client = Lib.Udp.start_client 8082 in  
      let message = Lib.Std.read_line() in 
      client.write (1, message)
      
end

module KeyValue(Alg: Lib.Utils) = struct
  
  type message = (int * string)
  
  let start_server = fun _ -> 
    print_endline "~~Server started~~\n";
    let server = Lib.Udp.start_server 8082 in 
    let result : (message * _ * _) = server.read() in
    let (msg, payload), _, _ = result in
    Printf.printf "%i %s" msg payload
   
  let start_client = fun _ -> 
    let client = Lib.Udp.start_client 8082 in  
    let message = Lib.Std.read_line() in 
    client.write (1, message)
      
end