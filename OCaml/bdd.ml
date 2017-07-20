type variable = int (* 1..max_var *)

let max_var : int ref = ref 0
let get_max_var () = !max_var
let max_nodes = 10000



type node_info = {
  uid : int;
  var : int;
  true_paths : int;
  false_paths : int;
}

type bdd = { uid: int; mutable node : view }
and view = True | False | Node of variable * bdd (*low*) * bdd (*high*)



let view b = b.node


let equal x y = match x, y with
  | Node (v1, l1, h1), Node (v2, l2, h2) -> 
      v1 == v2 && l1 == l2 && h1 == h2
  | _ ->
      x == y
let compare x y = match view x, view y with
  | Node (v1, l1, h1), Node (v2, l2, h2) -> if v1 == v2 && l1.uid == l2.uid && h1.uid == h2.uid then 0 else 1
  | _ -> if x == y then 0 else 1

let hash_node v l h = abs (v + 1000 * l.uid + 1000000 * h.uid)

let hash_bdd = function
  | True -> 1
  | False -> 0
  | Node (v, l, h) -> hash_node v l h

let gentag = let r = ref (-1) in fun () -> incr r; !r

 
(* zero and one allocated once and for all *)
let zero = { uid = gentag (); node = False }
let one = { uid = gentag (); node = True }
  
let var b = match b.node with
  | False | True -> !max_var + 1
  | Node (v, _, _) -> v

let low b = match b.node with
  | Node (_, l, _) -> l
  | _ -> b

let high b = match b.node with
  | Node (_, _, h) -> h
  | _ -> b


module H = struct
  type t = bdd
 (* let equal {uid = u;node = n} {uid = u2;node = n2} = (equal n n2) *)
  let hash {uid = u;node = n} = hash_bdd n 
  let compare x y = compare x y
end

module W = Set.Make(H)

let random_terminal () = if Random.bool () then one else zero

let create_empty_bdd n = 
    let t = ref (W.empty) in 
    t := W.add zero !t;
    t := W.add one !t; 
    !t

let add_node set node = let bdd = {uid = gentag (); node=node} in  
  if var bdd > !max_var then max_var := var bdd; 
  if (low bdd == high bdd) then low bdd else 
  try 
  W.find bdd !set
with _ -> 
  set := W.add bdd !set; 
  bdd

let rec create_random_bdd depth = 
  let bdd = ref (create_empty_bdd (depth*depth)) in 
  begin
  let node_array = Array.make_matrix depth depth zero in
  for i = 0 to depth - 2  do
    for j = 0 to depth - 1 do  
        if i == 0 then 
          node_array.(i).(j) <- add_node bdd (Node(depth-i, random_terminal (), random_terminal ()))
        else 
          node_array.(i).(j) <- add_node bdd  (Node(depth-i, node_array.(i-1).(Random.int depth), node_array.(i-1).(Random.int depth)))
    done;
  done;
  let root = add_node bdd  (Node(1, node_array.(depth-2).(Random.int depth), node_array.(depth-2).(Random.int depth))) in 
  let bdd = !bdd in 
  (bdd, root)

end


let is_terminal bdd = bdd == zero || bdd == one

(* let iterd bdd = 
  let s = Stack.create () in 
  let rec inner_iter bdd acc =
    match view bdd with
    | True | False -> if Stack.is_empty s then List.rev acc else inner_iter (Stack.pop s) acc 
    | Node(v, l, h) -> 
    begin
    Stack.push h s;
    inner_iter l ((bdd.uid)::acc)
  end
in inner_iter bdd []
 *)
let iterd bdd = 
  let s = Stack.create () in 
  let rec inner_iter bdd set =
  match view bdd with
  | True | False -> if Stack.is_empty s then set else inner_iter (Stack.pop s) set 
  | Node(v, l, h) -> 
    begin
    Stack.push h s;
    let set = ref set in 
    try
    let _ = W.find bdd !set in 
    if Stack.is_empty s then !set else inner_iter (Stack.pop s) !set 
    with Not_found -> 
    set := W.add bdd !set;
    inner_iter l !set
  end
in inner_iter bdd W.empty

