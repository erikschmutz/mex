
(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y



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

  (** val init_nid : int **)

  let init_nid =
    coq_MaxNId

  type coq_Key = int

  (** val override : (int -> 'a1) -> int -> 'a1 -> int -> 'a1 **)

  let override m k v k' =
    if (=) k' k then v else m k'
 end

module KVSAlg2 =
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
                              received : (SysPredefs.coq_NId -> coq_Clock);
                              clock : coq_Clock;
                              dep : (SysPredefs.coq_NId * coq_Clock) list }

  (** val store : 'a1 coq_StateRec -> SysPredefs.coq_Key -> 'a1 coq_Entry **)

  let store s =
    s.store

  (** val received : 'a1 coq_StateRec -> SysPredefs.coq_NId -> coq_Clock **)

  let received s =
    s.received

  (** val clock : 'a1 coq_StateRec -> coq_Clock **)

  let clock s =
    s.clock

  (** val dep : 'a1 coq_StateRec -> (SysPredefs.coq_NId * coq_Clock) list **)

  let dep s =
    s.dep

  (** val state :
      (SysPredefs.coq_Key -> 'a1 coq_Entry) -> (SysPredefs.coq_NId ->
      coq_Clock) -> coq_Clock -> (SysPredefs.coq_NId * coq_Clock) list -> 'a1
      coq_StateRec **)

  let state store0 x x0 x1 =
    { store = store0; received = x; clock = x0; dep = x1 }

  type 'val0 coq_State = 'val0 coq_StateRec

  type 'val0 coq_UpdateRec = { sender_node : SysPredefs.coq_NId;
                               sender_clock : coq_Clock;
                               sender_dep : (SysPredefs.coq_NId * coq_Clock)
                                            list }

  (** val sender_node : 'a1 coq_UpdateRec -> SysPredefs.coq_NId **)

  let sender_node u =
    u.sender_node

  (** val sender_clock : 'a1 coq_UpdateRec -> coq_Clock **)

  let sender_clock u =
    u.sender_clock

  (** val sender_dep :
      'a1 coq_UpdateRec -> (SysPredefs.coq_NId * coq_Clock) list **)

  let sender_dep u =
    u.sender_dep

  (** val update :
      SysPredefs.coq_NId -> coq_Clock -> (SysPredefs.coq_NId * coq_Clock)
      list -> 'a1 coq_UpdateRec **)

  let update sender_node0 x x0 =
    { sender_node = sender_node0; sender_clock = x; sender_dep = x0 }

  type 'val0 coq_Update = 'val0 coq_UpdateRec

  (** val dummy_update : 'a1 coq_UpdateRec **)

  let dummy_update =
    update 0 0 []

  (** val init_method : 'a1 -> 'a1 coq_State **)

  let init_method init_val =
    state (fun _ -> entry init_val SysPredefs.init_nid 0) (fun _ -> 0) 0 []

  (** val get_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 * 'a1
      coq_State **)

  let get_method _ this k =
    let s = this.store in
    let r = this.received in
    let c = this.clock in
    let d = this.dep in
    let e = s k in
    let v = e.entry_val in
    let n' = e.entry_node in
    let c' = e.entry_clock in
    let d' = if not ((=) n' SysPredefs.init_nid) then (n', c') :: d else d in
    (v, (state s r c d'))

  (** val put_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_State * 'a1 coq_Update **)

  let put_method n this k v =
    let s = this.store in
    let r = this.received in
    let c = this.clock in
    let d = this.dep in
    let c' = add c (Pervasives.succ 0) in
    let s' = SysPredefs.override s k (entry v n c') in
    let d' = (n, c') :: [] in
    let r' = SysPredefs.override r n c' in
    ((state s' r' c' d'), (update n c' d))

  (** val guard_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_Update -> bool **)

  let guard_method _ this _ _ u =
    let r = this.received in
    let d' = u.sender_dep in
    (fun a b c -> List.fold_left a c b) (fun b p ->
      let n' = fst p in let c' = snd p in (&&) b ((<=) c' (r n'))) d' true

  (** val update_method :
      SysPredefs.coq_NId -> 'a1 coq_State -> SysPredefs.coq_Key -> 'a1 -> 'a1
      coq_Update -> 'a1 coq_State **)

  let update_method _ this k v u =
    let s = this.store in
    let r = this.received in
    let c = this.clock in
    let d = this.dep in
    let n' = u.sender_node in
    let c' = u.sender_clock in
    let s' = SysPredefs.override s k (entry v n' c') in
    let r' = SysPredefs.override r n' c' in state s' r' c d
 end
