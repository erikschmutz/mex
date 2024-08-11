
module UDPStartAlg1 = struct 
  open Runtime.Runtime(Alg1.Main.Algorithm1)(Protocols.Udp.UDP)
  let start = start
end

module UDPStartAlg2 = struct 
  open Runtime.Runtime(Alg2.Main.Algorithm2)(Protocols.Udp.UDP)
  let start = start
end

module UDPStartAlg3 = struct 
  open Runtime.Runtime(Alg3.Main.Algorithm3)(Protocols.Udp.UDP)
  let start = start
end


module TCPStartAlg1 = struct 
  open Runtime.Runtime(Alg1.Main.Algorithm1)(Protocols.Tcp.TCP)
  let start = start
  let start_all = start_all
end

module TCPStartAlg2 = struct 
  open Runtime.Runtime(Alg2.Main.Algorithm2)(Protocols.Tcp.TCP)
  let start = start
end


module TCPStartAlg3 = struct 
  open Runtime.Runtime(Alg3.Main.Algorithm3)(Protocols.Tcp.TCP)
  let start = start
end



let () = 
    let protocol = Sys.argv.(1) in  
    let algorthim = Sys.argv.(2) in  
    let nid = int_of_string Sys.argv.(3) in   
    let benchfile = Sys.argv.(4) in   
    let port = 8060 + nid in 
    let bench =  Utils.Benchprog.prog_of_bench benchfile in 
    let config = Utils.ReadConfig.readConfiguration "config.txt" in  
    let nodes = List.map (fun (id, conf) -> (
      id, conf
    )) config in 
    let program =  if nid == -1 then Utils.Benchprog.prog_of_bench "bench.txt" else Skip in 
     match (protocol, algorthim) with
        | ("tcp", "alg1") -> TCPStartAlg1.start nodes nid program;
        | ("tcp", "alg2") -> TCPStartAlg2.start nodes nid program;
        | ("tcp", "alg3") -> TCPStartAlg3.start nodes nid program;   
        | ("udp", "alg1") -> UDPStartAlg1.start nodes nid program;
        | ("udp", "alg2") -> UDPStartAlg2.start nodes nid program;
        | ("udp", "alg3") -> UDPStartAlg3.start nodes nid program;       
        | _ -> raise (Invalid_argument ("Bad format args needs to be: \n chapar <protocol> <algorthim> <nid>"))
