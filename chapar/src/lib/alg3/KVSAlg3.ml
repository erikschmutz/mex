
type 'a sig2 = 'a
  (* singleton inductive, whose constructor was exist2 *)



(** val add : int -> int -> int **)

let rec add = (+)



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

module KVSAlg3 =
 struct
  type coq_Clock = int

  type 'val0 coq_Entry = { entry_val : 'val0;
                           entry_node : SysPredefs.coq_NId;
                           entry_clock : coq_Clock }

  (** val entry_val : 'a1 coq_Entry -> 'a1 **)

  let entry_val e =
    e.entry_val

  (** val entry_node : 'a1 coq_Entry -> SysPredefs.coq_NId **)

  let entry_node e =
    e.entry_node

  (** val entry_clock : 'a1 coq_Entry -> coq_Clock **)

  let entry_clock e =
    e.entry_clock

  (** val entry : 'a1 -> SysPredefs.coq_NId -> coq_Clock -> 'a1 coq_Entry **)

  let entry entry_val0 x x0 =
    { entry_val = entry_val0; entry_node = x; entry_clock = x0 }

  type 'val0 coq_StateRec = { store : (SysPredefs.coq_Key -> 'val0 coq_Entry);
                              coq_rec : (SysPredefs.coq_NId -> coq_Clock);
                              dep : (SysPredefs.coq_NId -> coq_Clock) }

  (** val store : 'a1 coq_StateRec -> SysPredefs.coq_Key -> 'a1 coq_Entry **)

  let store s =
    s.store

  (** val coq_rec : 'a1 coq_StateRec -> SysPredefs.coq_NId -> coq_Clock **)

  let coq_rec s =
    s.coq_rec

  (** val dep : 'a1 coq_StateRec -> SysPredefs.coq_NId -> coq_Clock **)

  let dep s =
    s.dep

  (** val state :
      (SysPredefs.coq_Key -> 'a1 coq_Entry) -> (SysPredefs.coq_NId ->
      coq_Clock) -> (SysPredefs.coq_NId -> coq_Clock) -> 'a1 coq_StateRec **)

  let state store0 x x0 =
    { store = store0; coq_rec = x; dep = x0 }

  type 'val0 coq_State = 'val0 coq_StateRec

  type 'val0 coq_UpdateRec = { sender_node : SysPredefs.coq_NId;
                               sender_dep : (SysPredefs.coq_NId -> coq_Clock) }

  (** val sender_node : 'a1 coq_UpdateRec -> SysPredefs.coq_NId **)

  let sender_node u =
    u.sender_node

  (** val sender_dep :
      'a1 coq_UpdateRec -> SysPredefs.coq_NId -> coq_Clock **)

  let sender_dep u =
    u.sender_dep

  (** val update :
      SysPredefs.coq_NId -> (SysPredefs.coq_NId -> coq_Clock) -> 'a1
      coq_UpdateRec **)

  let update sender_node0 x =
    { sender_node = sender_node0; sender_dep = x }

  type 'val0 coq_Update = 'val0 coq_UpdateRec

  (** val dummy_update : 'a1 coq_UpdateRec **)

  let dummy_update =
    update 0 (fun _ -> 0)

  (** val init_method : 'a1 -> 'a1 coq_State **)

  let init_method init_val =
    state (fun _ -> entry init_val 0 0) (fun _ -> 0) (fun _ -> 0)

  (** val get_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 * 'a1
      coq_State **)

  let get_method _ this k =
    let s = this.store in
    let r = this.coq_rec in
    let d = this.dep in
    let e = s k in
    let v = e.entry_val in
    let n' = e.entry_node in
    let c' = e.entry_clock in
    let d' = SysPredefs.override d n' (Pervasives.max (d n') c') in
    (v, (state s r d'))

  (** val put_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_State * 'a1 coq_Update **)

  let put_method n this k v =
    let s = this.store in
    let r = this.coq_rec in
    let d = this.dep in
    let d' = SysPredefs.override d n (add (r n) (Pervasives.succ 0)) in
    let r' = SysPredefs.override r n (add (r n) (Pervasives.succ 0)) in
    let s' = SysPredefs.override s k (entry v n (d' n)) in
    ((state s' r' d'), (update n d'))

  (** val guard_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_Update -> bool **)

  let guard_method _ this _ _ u =
    let r = this.coq_rec in
    let n' = u.sender_node in
    let d' = u.sender_dep in
    (&&)
      ((fun a b c -> List.fold_left a c b) (fun b n ->
        (&&) b ((<=) (d' n) (r n))) SysPredefs.nids true)
      ((=) (d' n') (add (r n') (Pervasives.succ 0)))

  (** val update_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_Update -> 'a1 coq_State **)

  let update_method _ this k v u =
    let s = this.store in
    let r = this.coq_rec in
    let d = this.dep in
    let n' = u.sender_node in
    let d' = u.sender_dep in
    let r' = SysPredefs.override r n' (d' n') in
    let d'' = fun n -> Pervasives.max (d n) (d' n) in
    let s' = SysPredefs.override s k (entry v n' (d' n')) in state s' r' d''
 end
