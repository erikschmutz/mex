

let () = 
    if Array.length Sys.argv != 2 then 
      raise (Invalid_argument ("An argument needs to be made to start either server or client"));;
    
     let arg = Sys.argv.(1) in 
      if arg = "server" then Tests.Udp.Simple.start_server()
      else if arg = "client" then Tests.Udp.Simple.start_client()
      else raise (Invalid_argument ("First argument needs to be either server or client found: \"" ^ arg ^"\""))
