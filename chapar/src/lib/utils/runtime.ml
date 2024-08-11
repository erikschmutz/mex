module Worker(Alg: Algorithm.Algorithm) (Prot: Protocols.Common.Protocol) = struct
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
    mutable index: int;
 }  


  let prog_step env : bool =
    let st = env.state in
    env.index <- env.index + 1;
    match env.prog with
      | Put (k, v, p) ->
          (* Printf.eprintf "Executing PUT (%i) %i = %i \n" env.nid k v; *)
          let (st', u) = put_method env.nid st k v in   
          print_string "Putting";
          flush_all();      
          env.state  <- st';
          env.prog <- p;
          let u' = to_data u in
          env.handler#broadcast(env.nid, k, v, u');
          false
  
        | Get (k, pf) ->
            (* Printf.eprintf "Executing GET (%i) \n" env.nid; *)
            let (v, st') = get_method env.nid st k in
            print_string "Getting";
            flush_all();
            env.state <- st';
            env.prog <- pf v;
            false
  
        | Skip ->
            true
  
        | Fault ->
            true

  let deliver_step (a: env * message_data) =
    Printf.printf "Sending step.\n";
    let env, m = a in 
    let (n, k, v, u) = m in
    let u' = from_data u in
      enqueue_message env.nid env.queue (n, k, v, u')
             
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
    prog_step_rec env 350
    
  let rec step_loop env: bool = 
    let fin = match env.handler#read() with 
      | Some(msg) -> 
        deliver_step(env, msg);
        false;
      | None -> 
        exec_step env
    in if not fin then
        step_loop env
    else false


  


 
  let rec wait_until_ready (instance: message_data Protocols.Common.handler) : unit =
    if instance#ready() then ignore()
    else wait_until_ready instance

  let start (nid: int) (config: (node_id * (node_info*program)) list) = 
    Printf.eprintf "Starting worker \n";
    let my_config, program = List.assoc nid config in 
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
      index = 0
    } in 
      Printf.printf "~~ Worker started with id %i on port listening port %i~~\n" nid port;
      flush stdout;
      ignore(step_loop env)   
end


module Runtime
  (Alg: Algorithm.Algorithm)
  (Prot: Protocols.Common.Protocol)
= struct
  open Worker(Alg)(Prot)

  let start (config: (node_id * (node_info*program)) list) = 
    print_endline "Running in cluster mode...\n";
    Printf.printf "~~ Cluster started with %i nodes~~\n" (List.length config); 
    let threads = List.map (fun (i, _) -> Thread.create (start i) config) config in 
    List.iter (fun t ->  Thread.join t) threads
end
