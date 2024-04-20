
type 'a sig2 = 'a
  (* singleton inductive, whose constructor was exist2 *)



(** val add : int -> int -> int **)

let add = (+)

module Nat =
 struct
 end

module SysPredefs =
 struct
  (** val coq_MaxNId : int **)

  let coq_MaxNId = 4

  type coq_NId = int

  (** val bnats : int -> int list sig2 **)

  let rec bnats n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun n' -> n' :: (bnats n'))
      n

  (** val nids : coq_NId list **)

  let nids =
    bnats coq_MaxNId

  type coq_Key = int

  (** val override : (int -> 'a1) -> int -> 'a1 -> int -> 'a1 **)

  let override m k v k' =
    if (=) k' k then v else m k'
 end

module KVSAlg1 =
 struct
  type coq_Clock = int

  type 'val0 coq_StateRec = { store : (SysPredefs.coq_Key -> 'val0);
                              clock : (SysPredefs.coq_NId -> coq_Clock) }

  (** val store : 'a1 coq_StateRec -> SysPredefs.coq_Key -> 'a1 **)

  let store s =
    s.store

  (** val clock : 'a1 coq_StateRec -> SysPredefs.coq_NId -> coq_Clock **)

  let clock s =
    s.clock

  (** val state :
      (SysPredefs.coq_Key -> 'a1) -> (SysPredefs.coq_NId -> coq_Clock) -> 'a1
      coq_StateRec **)

  let state store0 x =
    { store = store0; clock = x }

  type 'val0 coq_State = 'val0 coq_StateRec

  type 'val0 coq_UpdateRec = { sender_node : SysPredefs.coq_NId;
                               sender_clock : (SysPredefs.coq_NId ->
                                              coq_Clock) }

  (** val sender_node : 'a1 coq_UpdateRec -> SysPredefs.coq_NId **)

  let sender_node u =
    u.sender_node

  (** val sender_clock :
      'a1 coq_UpdateRec -> SysPredefs.coq_NId -> coq_Clock **)

  let sender_clock u =
    u.sender_clock

  (** val update :
      SysPredefs.coq_NId -> (SysPredefs.coq_NId -> coq_Clock) -> 'a1
      coq_UpdateRec **)

  let update sender_node0 sender_clock0 =
    { sender_node = sender_node0; sender_clock = sender_clock0 }

  type 'val0 coq_Update = 'val0 coq_UpdateRec

  (** val dummy_update : 'a1 coq_UpdateRec **)

  let dummy_update =
    update 0 (fun _ -> 0)

  (** val init_method : 'a1 -> 'a1 coq_State **)

  let init_method init_val =
    state (fun _ -> init_val) (fun _ -> 0)

  (** val get_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 * 'a1
      coq_State **)

  let get_method _ this k =
    let s = this.store in let c = this.clock in ((s k), (state s c))

  (** val put_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_State * 'a1 coq_Update **)

  let put_method n this k v =
    let s = this.store in
    let c = this.clock in
    let c' = SysPredefs.override c n (add (c n) (Stdlib.succ 0)) in
    let s' = SysPredefs.override s k v in ((state s' c'), (update n c'))

  (** val guard_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_Update -> bool **)

  let guard_method _ this _ _ u =
    let c = this.clock in
    let n' = u.sender_node in
    let c' = u.sender_clock in
    (&&) ((=) (c' n') (add (c n') (Stdlib.succ 0)))
      ((fun a b c -> List.fold_left a c b) (fun b n ->
        (&&) b ((||) ((=) n n') ((<=) (c' n) (c n)))) SysPredefs.nids true)

  (** val update_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_Update -> 'a1 coq_State **)

  let update_method _ this k v u =
    let s = this.store in
    let c = this.clock in
    let n' = u.sender_node in
    let c' = u.sender_clock in
    let c'' = SysPredefs.override c n' (c' n') in
    let s'' = SysPredefs.override s k v in state s'' c''
 end
