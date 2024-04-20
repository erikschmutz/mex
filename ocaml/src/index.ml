(* let key = 1;;
let value = 123;;
let id = 1;;

let state = Test.Algorithm1.init_method 1;;
let db, _ = Test.Algorithm1.put_method id state key value;;
let result, _ = Test.Algorithm1.get_method id db key;; *)

let state = Test.Algorithm1.init_method 1;;
let state, _ = Test.Algorithm1.put_method 1 state 1 2;;
let state, _ = Test.Algorithm1.put_method 1 state 2 4;;

let a, _ = Test.Algorithm1.get_method 1 state 1;; 
let b, _ = Test.Algorithm1.get_method 1 state 2;; 

let () =
  Printf.printf "result = %d, %d \n" a b