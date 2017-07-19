type variable = int (* 1..max_var *)

let max_var : int ref = ref 0
let get_max_var () = !max_var
let max_nodes = 10000



type node_info = {
  uid : int;
  var : int;
  edges : int list;
}

type bdd = { uid: int; mutable node : view }
and view = True | False | Node of variable * bdd (*low*) * bdd (*high*)



let view b = b.node


let equal x y = match x, y with
  | Node (v1, l1, h1), Node (v2, l2, h2) -> 
      v1 == v2 && l1 == l2 && h1 == h2
  | _ ->
      x == y

let hash_node v l h = abs (v + 1000 * l.uid + 1000000 * h.uid)

let hash = function
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
  let equal {uid = u;node = n} {uid = u2;node = n2} = (equal n n2)
  let hash {uid = u;node = n} = hash n 
end

module W = Weak.Make(H)

let random_terminal () = if Random.bool () then one else zero

let create_empty_bdd n = let t = W.create n in 
    let _ = W.merge t zero in
    let _ = W.merge t one in 
    t

let add_node table node = let bdd = {uid = gentag (); node=node} in  
  if var bdd > !max_var then max_var := var bdd; 
  if (low bdd == high bdd) then low bdd else 
  let b2 = W.merge table bdd in b2


let rec create_random_bdd depth = 
  let bdd = create_empty_bdd (depth*depth) in 
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
  (bdd, root)

end

let node_info bdd = 
  {
  uid = bdd.uid;
  var = var bdd;
  edges = [(low bdd).uid; (high bdd).uid]
  }

let is_terminal bdd = bdd == zero || bdd == one

let iterd bdd = 
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

let remove_uid table uid = 
  let node_list = ref [] in 
  W.iter (fun x -> if x.uid == uid then node_list := x::!node_list) table;
  List.iter (fun x -> W.remove table x) !node_list;
  Gc.full_major()

let rec set_node table node_old node_new = 
  W.iter (fun x -> 
    match view x with
    Node(v, l, h) -> 
    if l == node_old then  x.node <- Node(var x, node_new, h);
    if h == node_old then x.node <- Node(var x, l, node_new);
    Gc.full_major ()
    | True | False -> ()) table
  
let rec check_duplicates table =
  let node_list = ref [] in 
  W.iter (fun x -> if low x == high x then node_list := x::!node_list; ) table;
  List.iter (fun x -> set_node table x (low x)) !node_list; 
  List.iter (fun x -> remove_uid table (x.uid)) !node_list;
  if List.length !node_list > 0 then check_duplicates table
  
let round (bdd, root) depth = 
  Gc.full_major ();
  let limit = get_max_var () - depth in 
  let node_list = ref [] in 
  W.iter (fun x -> if var x >= limit then node_list := x :: !node_list) bdd;
  List.iter (fun x -> if low  x == zero ||  high x == zero then set_node bdd x one) !node_list;
  check_duplicates bdd
