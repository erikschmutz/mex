(* open List *)
open Configuration
(* open String *)

let readConfiguration filename =
let net_info = ref [] in
(* let mysub st = String.sub st 7 (String.length st - 7) in *)
let mysub ms = (
      let i = (String.index ms '@') in
      String.sub ms (i + 1) (String.length ms - i - 1)) in
let chan = open_in filename in
(* try *)
  ignore(input_line chan);
  ignore(input_line chan);
  ignore(input_line chan);
  ignore(input_line chan);
  ignore(input_line chan);
  ignore(input_line chan);
  ignore(input_line chan);
  net_info := (-1, {ip = mysub (input_line chan); port = 3000}) :: !net_info;
  ignore(input_line chan);
  net_info := (0, {ip = mysub (input_line chan); port = 8060}) :: !net_info;
  net_info := (1, {ip = mysub (input_line chan); port = 8061}) :: !net_info;
  net_info := (2, {ip = mysub (input_line chan); port = 8062}) :: !net_info;
  net_info := (3, {ip = mysub (input_line chan); port = 8063}) :: !net_info;
(* with End_of_file -> *)
  close_in chan;
  List.rev !net_info
  