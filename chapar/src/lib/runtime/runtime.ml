open Utils.Configuration
open Utils.Common


module Worker(Alg: Utils.Algorithm.Algorithm) (Prot: Protocols.Common.Protocol) = struct
  open Alg
  open Prot

  type message_data = node_id * key * valu * update_data
  type a = message_data
  
  type env = {   
    nid: node_id;
    handler: message_data Protocols.Common.handler;
    mutable state: state;
    mutable prog: program;
    mutable queue: mqueue;
    mutable worker: bool;
    mutable index: int;
 }  


  let prog_step env : bool =
    let st = env.state in
    env.index <- env.index + 1;
    match env.prog with
      | Put (k, v, p) ->
          Printf.eprintf "Executing PUT (%i) %i = %i \n" env.nid k v;
          let (st', u) = put_method env.nid st k v in         
          env.state  <- st';
          env.prog <- p;
          let u' = to_data u in
          let start = Unix.time() in 
          env.handler#broadcast(env.nid, k, v, u');
          Printf.printf "Broadcast %f\n" (Unix.time() -. start);
          false
  
        | Get (k, pf) ->
            Printf.eprintf "Executing GET (%i) \n" env.nid;
            (* print_string "Getting"; *)
            flush_all();   
            let (v, st') = get_method env.nid st k in
            env.state <- st';
            env.prog <- pf v;
            false
  
        | Skip ->
            true
  
        | Fault ->
            true

  let deliver_step (a: env * message_data) =
    let env, m = a in 
    let (n, k, v, u) = m in
    let u' = from_data u in
    let u = enqueue_message env.nid env.queue (n, k, v, u') in 
    (* Printf.printf "Sending step done %i \n" env.nid; *)
    u
    
  let rec prog_step_rec env n =
    if n = 0 then
        false
    else
        let b = prog_step env in
        if b then
          true
        else
          prog_step_rec env (n - 1)

  let exec_step env : bool =
    let state' = check_messages env.nid env.state env.queue in
    env.state <- state';
    prog_step_rec env 1000
    
  let rec step_loop env: unit = 
    let result = env.handler#read() in 
    let fin = match result with 
      | Some(msg) -> 
        deliver_step(env, msg);
        false;
      | None -> 
        exec_step env
    in if not fin or env.worker then
        step_loop env
    else if fin then begin
      Printf.printf "Done with loop for id %i" env.nid;
      flush_all()
    end
      

    


  


 
  let rec wait_until_ready (instance: message_data Protocols.Common.handler) : unit =
    if instance#ready() then ignore()
    else wait_until_ready instance

  let start (config: (node_id * node_info) list) (nid: int) program = 
    Printf.eprintf "Starting worker \n";
    let my_config = List.assoc nid config in 
    let instance = get_instance() in 
    let name =   instance#name() in 
      Printf.eprintf "Setting up %s worker\n" name;
      ignore(instance#setup { 
        me=(nid, my_config); 
        peers=config
      });

    wait_until_ready instance;
    let port = my_config.port in 
    let state = init_method nid in 
    let queue = init_queue in 
    let env = {
      nid = nid;
      handler = instance;
      state = state;
      prog = program;
      queue = queue;
      worker = nid != -1;
      index = nid
    } in 
      Printf.printf "~~ Worker started with id %i on port listening port %i~~\n" nid port;
      flush stdout;
      ignore(step_loop env)   
end


module Runtime
  (Alg: Utils.Algorithm.Algorithm)
  (Prot: Protocols.Common.Protocol)
= struct
  open Worker(Alg)(Prot)



  let start (config: (node_id * (node_info)) list) (id: int) program = 
    print_endline "Running in cluster mode...\n";
    Printf.printf "~~ Cluster started with %i nodes~~\n" (List.length config); 
    start config id program

  let start_all (config: (node_id * node_info * program) list) = 
    print_endline "Running in cluster mode...\n";
    Printf.printf "~~ Cluster started with %i nodes~~\n" (List.length config); 
    let c = List.map (fun (id, info, program) -> (id, info)) config in 
    let threads = List.map (fun ((id, _, program)) -> Thread.create (start c id) program) config in 
    List.iter (fun t ->  Thread.join t) threads
end
