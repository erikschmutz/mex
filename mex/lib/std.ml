
let read_line = fun () -> 
  match In_channel.input_line stdin with 
  | Some line ->  line
| None -> failwith "This shouldn't happen"