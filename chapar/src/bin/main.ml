
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



(* open Utils.Common *)
(* open Lib.Core.Runtime(Alg2)(Lib.) *)
let () = 
    (* if Array.length Sys.argv != 4 then 
      raise (Invalid_argument ("Should be done in the following algorithm"));; *)

    
    let protocol = Sys.argv.(1) in  
    let algorthim = Sys.argv.(2) in  
    let nid = int_of_string Sys.argv.(3) in   
    let benchfile = Sys.argv.(4) in   
    let port = 8060 + nid in 
    let bench =  Utils.Benchprog.prog_of_bench benchfile in 
    let config = Utils.ReadConfig.readConfiguration "config.txt" in  
    (* TCPStartAlg1.start config 1 Skip *)
    TCPStartAlg1.start_all (List.map (fun (id, conf) -> (
      id, conf, if id != -1 then Utils.Common.Skip else bench
    )) config)
    
    (* match (protocol, algorthim) with *)


    
     (* let protocol = Sys.argv.(1) in 
     let algorthim = Sys.argv.(2) in  
     match (protocol, algorthim) with
        | ("tcp", "alg1") -> TCPStartAlg1.start(1, ({ port = 9000; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"));
           
         | ("tcp", "alg2") -> TCPStartAlg2.start([
          (1, ({ port = 9000; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"));
          (2, ({ port = 9001; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"))
        ])
        | ("tcp", "alg3") -> TCPStartAlg3.start([
          (1, ({ port = 9000; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"));
          (2, ({ port = 9001; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"))
        ])
        | ("udp", "alg1") -> UDPStartAlg1.start([
          (1, ({ port = 9000; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"));
          (2, ({ port = 9001; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"))
        ])
        | ("udp", "alg2") -> UDPStartAlg2.start([
          (1, ({ port = 9000; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"));
          (2, ({ port = 9001; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"))
        ])
        | ("udp", "alg3") -> UDPStartAlg3.start([
          (1, ({ port = 9000; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"));
          (2, ({ port = 9001; ip = "0.0.0.0" }, Utils.Benchprog.prog_of_bench "bench.txt"))
        ])
       
  
      else raise (Invalid_argument ("First argument needs to be either server or client found: \"" ^ arg ^"\""))  *)
