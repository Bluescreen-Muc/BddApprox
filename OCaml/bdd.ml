exception LineNumberNotFound of string
exception BddInputError of string
exception ExpectationVsRealityError of string
exception ExpectedUnaryError 
exception NoTerminalAllowed of string

module Int = struct 
   type t = int 
   (* use Pervasives compare *)
   let compare = compare
 end
module IntSet = Set.Make(Int)

type node_info = {mutable paths: int; mutable true_paths:int; mutable false_paths: int; mutable parent_count: int; mutable dominate_count: int; mutable score: float}
let empty_node_info () = {paths=0; true_paths=0; false_paths=0; parent_count=0; dominate_count=0; score=0.0}

type funny = AND | OR | NOT | IMP | EQ

let functions = [|AND; OR; IMP; NOT|]

let random_funct ?(opnot = true) ()= 
    if opnot == true then let n = Random.int (Array.length functions) in Array.get functions n
	else let n = Random.int ((Array.length functions)-1) in Array.get functions (n)


type variable = int (* 1..max_var *)
type bdd = { mutable uid: int; mutable node : node }
and node = True | False | Node of {mutable var: int; mutable low: bdd; mutable high: bdd; mutable info: node_info} 
type bdd_info = {mutable depth: int; mutable truth_rate: float; mutable true_paths: int; mutable false_paths: int ; mutable node_count: int; mutable root: bdd; fillrate: int}

let copy_bdd_info info = {depth=info.depth;truth_rate=info.truth_rate; true_paths=info.true_paths; false_paths=info.false_paths ; node_count=info.node_count; root=info.root; fillrate=info.fillrate} 
let cur_uid = ref (2)
let genuid = fun () -> incr cur_uid

let zero = { uid = 0; node = False }
let one = { uid = 1; node = True }

let empty_bdd_info () = {depth=0;truth_rate=0.0; true_paths=0; false_paths=0 ; node_count=0; root=zero; fillrate=0}

let set_dom_count bdd x = match bdd.node with 
	| Node n -> n.info.dominate_count <- x
	| _ -> ()

let get_false_paths bdd = match bdd.node with 
	| Node n -> n.info.false_paths
	| True | False -> raise (ExpectationVsRealityError "get_false_paths")

let get_true_paths bdd = match bdd.node with 
	| Node n -> n.info.true_paths
	| True | False -> raise (ExpectationVsRealityError "get_false_paths")

module Helper = struct

	let rec pow a = function
  		| 0 -> 1
  		| 1 -> a
  		| n -> let b = pow a (n / 2) in b * b * (if n mod 2 = 0 then 1 else a)

  	let first (a, _, _) = a

	let second (_, b, _) = b
	
	let third (_, _, c) = c

	let get_some x = 
		match x with
 		| Some x -> x
 		| None -> raise (ExpectationVsRealityError "expected something")

 	let trim_parenthesis str = let output = ref "" in String.iter (
 		fun x -> if not (x == '"') then output :=  !output^(String.make 1 x)) str;
 		!output

 	let create_random_rumbers n max = let s = ref IntSet.empty in 
		while IntSet.cardinal !s  < n do
			s := IntSet.add ((Random.int max) + 1) !s;
		done;
		!s 

	let max_nodes depth = ((pow 2 ((depth/2) + 1) -1) + pow 2 ((depth + 1)/2))
	
end

let max_var : int ref = ref 0
let get_max_var () = !max_var
let reset_max_var () = max_var := 0

let max_nodes = 200000

let equal x y = match x, y with
  | Node n1, Node n2 -> 
      n1.var == n2.var && n1.low == n2.low && n1.high == n2.high
  | _ -> x == y

let hash = function
  | True -> 1
  | False -> 0
  | Node n -> abs (n.var + max_nodes * n.low.uid + max_nodes * max_nodes * n.high.uid)

let hash_bdd bdd = 
	hash(bdd.node)

let var bdd = match bdd.node with
  | False | True -> !max_var + 1
  | Node n -> n.var

let low bdd = match bdd.node with
  | Node n -> n.low
  | _ -> bdd

let high bdd = match bdd.node with
  | Node n -> n.high
  | _ -> bdd

let uid bdd = bdd.uid

let get_score bdd = match bdd.node with
  | Node n -> n.info.score
  | _ ->  (-1.0)
let random_terminal () = if Random.bool () then one else zero

let print_node x = match x.node with
	| True -> print_string "TRUE"
	| False -> print_string "FALSE"
	| _ -> Printf.printf "uid: %d var: %d low: %d high: %d \n" (uid x) (var x) (uid (low x)) (uid (high x))

let true_paths bdd = 
	match bdd.node with 
	| Node x -> x.info.true_paths
	| True | False -> raise (NoTerminalAllowed (Printf.sprintf "true_paths %d" bdd.uid))

let paths bdd = 
	match bdd.node with 
	| Node x -> x.info.paths
	| True | False -> raise (NoTerminalAllowed (Printf.sprintf "paths %d" bdd.uid))

let print_table table = 
	Hashtbl.iter  (fun x y  -> print_node y)  table 

let create_empty_bdd () = let t = Hashtbl.create 10 in 
    let _ = Hashtbl.add t  0 zero in
    let _ = Hashtbl.add t 1 one in 
    t

let add_node table node = 
	match node with 
	|Node n -> begin
  	if (n.low == n.high) then n.low else 
  	let node_hash = (hash node) in  
  	try
  		Hashtbl.find table (node_hash)
  	with _ -> let bdd = {uid = !cur_uid; node=node} in genuid (); Hashtbl.add table (node_hash) bdd; bdd
	end
  	| _ -> raise (ExpectationVsRealityError "add_node")

let create_single_bdd var = 
	let t = Hashtbl.create 3 in 
    ignore(Hashtbl.add t  0 zero);
    ignore(Hashtbl.add t 1 one);
    add_node t (Node{var=var; low=zero; high=one;info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}})

let connected_to_terminal node = if node.uid == 1 || node.uid == 0 then 0 else let output = ref 0 in 
	begin
		if (low node).node == True || (high node).node == True then output := !output + 2;
		if (low node).node == False || (high node).node == False then output := !output + 1;
	end;
	!output


let rec add_bdd table node = match node with 
	| Node n1 -> (add_node table (Node{var=n1.var;low=(add_bdd table n1.low.node);high=(add_bdd table n1.high.node);info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}}))
	| t -> if t == True then one else zero

let rec apply funct bdd1 bdd2 table =
	match funct, bdd1.node, bdd2.node with
	| _, Node n1, Node n2 -> 
		if n1.var < n2.var then add_node table (Node{var=n1.var; low=(apply funct n1.low bdd2 table); high=(apply funct n1.high bdd2 table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}}) 
		else if n1.var > n2.var then add_node table (Node{var=n2.var; low=(apply funct bdd1 n2.low table); high=(apply funct bdd1 n2.high table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}}) 
		else add_node table (Node{var=n1.var; low=(apply funct n1.low n2.low table); high=(apply funct n1.high n2.high table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}}) 
	| AND, _ , False | AND, False , _ ->  zero  
	| AND, Node n1 , True -> add_bdd table bdd1.node
	| AND, True, Node n1 -> add_bdd table bdd2.node
	| OR, _ , True | OR, True , _ ->  one
	| OR, Node n1 , False -> add_bdd table bdd1.node
	| OR, False, Node n1 -> add_bdd table bdd2.node
	| IMP, False, _ | IMP, _, True ->  one
	| _, Node n1, t -> add_node table (Node{var=n1.var; low=(apply funct n1.low bdd2 table); high=(apply funct n1.high bdd2 table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}}) 
	| _, t, Node n1 -> add_node table (Node{var=n1.var; low=(apply funct bdd1 n1.low table); high=(apply funct bdd1 n1.high table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}}) 
	| _, t1, t2 -> match funct with 
	| AND -> if t1 == True && t2 == True then one else zero
	| OR -> if t1 == True || t2 == True then one else zero 
	| IMP -> begin
			match t1, t2 with
			| False, _ -> one
			| _, True -> one
			| _, _ -> zero	
			end
	| EQ -> begin
			match t1, t2 with
			| False, True -> zero
			| True, False -> zero
			| _, _ -> one	
			end
	| _ -> raise (ExpectedUnaryError)


let is_terminal bdd =
	bdd.uid == 1 || bdd.uid == 0

let switch_terminal terminal = if (terminal.uid == 0) then one else if (terminal.uid == 1) then zero else raise (ExpectationVsRealityError "switch terminal" )

let switch_terminals bdd = match bdd.node with
	| Node n -> 
		begin
			if (is_terminal n.low) then n.low <- (switch_terminal n.low); 
			if (is_terminal n.high) then n.high <- (switch_terminal n.high);
		end;
	| _ -> ()

let opnot table bdd = let to_do = ref [] in 
	Hashtbl.iter (fun x y -> 
		match y.node with 
		|Node n -> begin
			match n.low.node, n.high.node with
			| Node c1, Node c2 -> ()
			| _, _ -> to_do := (x,y)::!to_do
		
		end
		| _ -> () 
	) table;
	if (List.length !to_do) == 0 then Hashtbl.iter (fun x y -> print_node y; print_newline();) table;
	print_newline();
	List.iter (fun (x,y) -> Hashtbl.remove table x) !to_do;
	List.iter (fun (x,y) -> match y.node with 
		|Node n -> begin
			if n.low.uid == 1 then n.low <- zero else if n.low.uid == 0 then n.low <- one;
			if n.high.uid == 1 then n.high <- zero else if n.high.uid == 0 then n.high <- one;
			Hashtbl.add table (hash y.node) y
		end;
		| _ -> ();
		) !to_do;
	bdd
(* let opnot table bdd = let to_do = ref [] in 
	Hashtbl.iter (fun x y -> 
		if (connected_to_terminal y > 0) then 
		begin
			to_do := (x,y)::!to_do;
		end) table;
	print_int (List.length !to_do);
	print_newline();
	List.iter (fun (x,y) -> Hashtbl.remove table x) !to_do;
	List.iter (fun (x,y) -> switch_terminals y; Hashtbl.add table (hash y.node) y) !to_do;
	bdd *)
let create_random_bdd ?(n = 5) depth =
	let table = create_empty_bdd () in 
	let vars = Helper.create_random_rumbers n depth in 
	let bdds = ref [] in 
	IntSet.iter (fun x -> bdds := (create_single_bdd x)::!bdds) vars;
	List.fold_left (fun acc x -> let funct = ref (random_funct()) in 
		if !funct == NOT then  begin
			match x.node with 
		| Node n -> 
			n.low <- one;
			n.high <- zero; 
			funct :=  random_funct ~opnot:false ();
		| _ -> raise (ExpectationVsRealityError "create_random_bdd");
	end;
		apply !funct acc x table) (List.hd !bdds) (List.tl !bdds)

let node_count table = Hashtbl.length table

let get_bdd_id str = let reg = Str.regexp "\\(^[0-9]+\\), " in 
	if not (Str.string_match reg str 0) then raise (LineNumberNotFound (Str.first_chars str (min (String.length str) 20))) else 
	let output = Str.matched_group 1 str in 
	output

let create_var_map table = let var_table = Hashtbl.create 10 in 
	Hashtbl.iter(fun x y -> match y.node with 
		| Node n -> Hashtbl.add var_table (n.var) y
		| _ -> ()) table;
	var_table


let sort_table_keys ?(reverse=false) table = let output = ref [] in 
	Hashtbl.iter (fun x y -> if not (List.mem x !output) then output := x :: !output) table;
	List.sort compare !output

module NodeLabel = struct
	
	let path_label bdd = match bdd.node with 
		| Node n ->Printf.sprintf "\"%d %d %d %.2f\"" (n.info.paths) (n.info.false_paths) (n.info.true_paths) (float_of_int(n.info.true_paths) /. float_of_int(n.info.false_paths + n.info.true_paths))
		| True -> "T"
		| False -> "F"
	
	let var_label bdd = match bdd.node with 
		| Node n -> Printf.sprintf "\"V: %d\"" (var bdd) 
		| True -> "T"
		| False -> "F"

	let parent_label bdd = match bdd.node with 
		| Node n -> Printf.sprintf "\"PC: %d\"" n.info.parent_count 
		| True -> "T"
		| False -> "F"

	let dominate_label bdd = match bdd.node with 
		| Node n -> Printf.sprintf "\"DC: %d\"" n.info.dominate_count 
		| True -> "T"
		| False -> "F"

	let score_label bdd = match bdd.node with 
		| Node n -> Printf.sprintf "\"DomScore: %.4f\"" n.info.score
		| True -> "T"
		| False -> "F"

	let all_info_label bdd = 
		let text = Helper.trim_parenthesis (Printf.sprintf "%s\n%s\n%s\n%s" (path_label bdd) (parent_label bdd) (dominate_label bdd) (score_label bdd)) in 
		match bdd.node with
		| Node n -> Printf.sprintf "\"%s\"" text
		| True -> "T"
		| False -> "F"

end


module BddData = struct

	type bdd_dataset = {mutable bdd: (int, bdd) Hashtbl.t; mutable info: bdd_info}

	let get_table data = data.bdd
	let set_table data table = data.bdd <- table
	
	let get_depth data = data.info.depth
	let set_depth data x = data.info.depth <- x

	let get_root data = data.info.root
	let set_root data root = data.info.root <- root

	let get_info data = data.info
	let set_info data info = data.info <- info 

	let get_truth_rate data = data.info.truth_rate
	let set_truth_rate data x = data.info.truth_rate <- x

	let empty () = { bdd = create_empty_bdd (); info = empty_bdd_info ()}

	let create x y = {bdd = x; info = y}
	
  	let print_nodes data = let table = ref data.bdd in 
  		Hashtbl.iter (fun x y -> print_node y) !table

	let create_single_bdd var = let data = ref (empty ()) in let table = ref !data.bdd in 
		ignore(Hashtbl.add !table 0 zero);
    	ignore(Hashtbl.add !table 1 one);
    	let root = add_node !data.bdd (Node{var=var; low=zero; high=one;info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0; score=0.0}})
		in { bdd = !table; info = {depth=var; truth_rate=0.0; node_count= 0; root=root; true_paths=0; false_paths=0; fillrate=0}}
    
	let count_nodes x = x.info.node_count <- Hashtbl.length x.bdd; x.info.node_count

	let count_truth_rate x = let table = x.bdd in let info = x.info in
		let true_hits = ref 0 in let false_hits = ref 0 in 
		Hashtbl.iter (fun x y -> 
		match y.node with 
		|Node n -> begin
			let multiplier = int_of_float(2.0** (float_of_int(info.depth - n.var))) in
			match n.low.node, n.high.node with
			| Node c1, Node c2 -> ();
			| True, Node _ | Node _, True -> true_hits := !true_hits + multiplier;
			| False, Node _ | Node _, False -> false_hits := !false_hits + multiplier;
			| _ -> begin
				true_hits := !true_hits + multiplier;
				 false_hits := !false_hits + multiplier;
				end
		end
		| _ -> ()  
		) table;
		info.truth_rate <- (float_of_int !true_hits) /. ((float_of_int !true_hits) +. (float_of_int !false_hits));
		info.truth_rate

	let count_paths data = 
		let info = ref data.info 
		in let root = get_root data in let depth = !info.depth in	let var_map = create_var_map data.bdd in
		 match root.node with
		| Node n -> begin 
		for i = depth downto 1 do
			try
  				let values = Hashtbl.find_all var_map i in 
				n.info.paths <- Helper.pow 2 depth;
				List.iter (fun x -> match x.node with 
				| Node n -> 
				begin
				match n.low.node, n.high.node with 
				|Node lc, Node hc -> begin
				lc.info.paths <- lc.info.paths + n.info.paths / 2;
				hc.info.paths <- hc.info.paths + n.info.paths / 2;
				end;
				| Node lc, _ -> lc.info.paths <-  lc.info.paths + n.info.paths / 2
				| _,  Node hc-> hc.info.paths <-   hc.info.paths + n.info.paths / 2
				| _ -> ()
				end
				| _ -> ())values;
			with Not_found -> ()
		done;
		end
			| _ -> ()

	let count_false_true data =
		let table = data.bdd in let info = data.info in let var_map = create_var_map table in
		let depth = info.depth  in
		let reset_paths table= 
			Hashtbl.iter (fun x y -> 
			match y.node with
			| Node n -> 
			begin
				n.info.true_paths <- 0;
				n.info.false_paths <- 0;
				n.info.paths <- 0;
			end
			| True | False -> () ) table
		in reset_paths table;

		count_paths data;
		for i = depth downto 1 do
			try
  				let values = Hashtbl.find_all var_map i in 
  				List.iter (fun x -> match x.node with
  				| Node n -> begin
  						if n.low.uid == 0 || n.high.uid == 0 then n.info.false_paths <- n.info.paths / 2;
						if n.low.uid == 1 || n.high.uid == 1 then n.info.true_paths <- n.info.paths / 2;

						if  (n.low.uid <> 0) && (n.low.uid <> 1) then 
						begin
								let sum = float_of_int((true_paths (low x)) + get_false_paths (low x) )in 
								n.info.true_paths <- int_of_float(float_of_int (n.info.true_paths) +. float_of_int(n.info.paths) *. (float_of_int (true_paths (low x))) /. 2.0 /. sum);
								n.info.false_paths <- int_of_float(float_of_int (n.info.false_paths) +. float_of_int(n.info.paths) *. (float_of_int (get_false_paths (low x))) /. 2.0 /. sum);
						end;
						
						if  n.high.uid <> 0 && n.high.uid <> 1 then 
						begin
								let sum = float_of_int((true_paths (high x)) + get_false_paths (high x) )in 
								n.info.true_paths <- int_of_float(float_of_int (n.info.true_paths) +. float_of_int(n.info.paths) *. (float_of_int (true_paths (high x))) /. 2.0 /. sum);
								n.info.false_paths <- int_of_float(float_of_int (n.info.false_paths) +. float_of_int(n.info.paths) *. (float_of_int (get_false_paths (high x))) /. 2.0 /. sum);
  						end;
  					end
  				| True | False -> raise  (NoTerminalAllowed "count_false_true")
  				) values
  			with Not_found -> ()
		done;
		match (get_root data).node with
		|Node n -> begin
			info.true_paths <- (Helper.pow 2 (n.var - 1)) * n.info.true_paths;
			info.false_paths <- (Helper.pow 2 (n.var - 1)) * n.info.false_paths;
			end;
		| False -> info.false_paths <- Helper.pow 2 (get_depth data);
		| True -> info.true_paths <- Helper.pow 2 (get_depth data);
		info.truth_rate <- float_of_int(info.true_paths) /. float_of_int(info.true_paths + info.false_paths)

	
	let fill_parents data = 
		let reset_parents table= 
			Hashtbl.iter (fun x y -> 
			match y.node with
			| Node n -> n.info.parent_count <- 0
			| True | False -> () ) table in 

		reset_parents data.bdd;
		Hashtbl.iter (fun x y -> match y.node with
		| Node n -> begin
		begin
			match n.low.node with 
			| Node nlow -> nlow.info.parent_count <- nlow.info.parent_count + 1;
			| _ -> ();
			end;
			begin
			match n.high.node with 
			| Node nhigh -> nhigh.info.parent_count <- nhigh.info.parent_count + 1
			| _ -> ()
			end
		end
		| _ -> ()) data.bdd


	let rec inner_fill_dominate bdd acc score= 
			match bdd.node with  
			| Node n -> 
			begin
			begin
			match n.low.node with 
			| Node nlow -> let pc = nlow.info.parent_count in 
			if pc == 1 then begin
				score := !score + 1; 
				inner_fill_dominate n.low acc score;
			end
			else acc := n.low.uid::!acc;
			if List.length (List.find_all (fun x -> x == n.low.uid) !acc) >= pc then 
			begin
				score := !score + 1; 
				inner_fill_dominate n.low acc score;
			end
			| _ -> ();
			end;
			match n.high.node with 
			| Node nhigh -> let pc = nhigh.info.parent_count in 
			if pc == 1 then begin
				score := !score + 1; 
				inner_fill_dominate n.high acc score;
			end
			else acc := n.high.uid::!acc;
			if List.length (List.find_all (fun x -> x == n.high.uid) !acc) >= pc then 
			begin
				score := !score + 1;  
				inner_fill_dominate n.high acc score;
			end
			| _ -> ()
			end
			| _ -> ()

	(*REMINDER STARTS AT 1*)
	let fill_dominate data = let root_uid = (get_root data).uid in Hashtbl.iter (fun x y -> if not (y.uid == root_uid) then 
		begin
		match y.node with 
			| Node n -> let count = ref 0 in let l = ref [] in inner_fill_dominate y l count; n.info.dominate_count <- !count + 1;
			| _ -> ();
		end
		else set_dom_count y ((count_nodes data) - 3)) data.bdd

	let calc_dom_score ?(side=1) data = let sum_paths = data.info.true_paths + data.info.false_paths in 
		Hashtbl.iter(fun x y -> match y.node with
			| Node n -> n.info.score <- float_of_int (n.info.dominate_count * sum_paths) /. float_of_int(n.info.false_paths) 
			| _ -> ()) data.bdd

	let fill_info data = 
		data.info.node_count <- count_nodes data;
		count_false_true data;
		fill_parents data;
		fill_dominate data

	let replace_bdd data old_bdd new_bdd =  let bdd = ref (Hashtbl.find data.bdd (hash_bdd (old_bdd))) in  !bdd.uid <- new_bdd.uid; !bdd.node <- new_bdd.node

	let create_random_function ?(compress=true) depth n =

		let var_realloc table bdd = let s = ref (IntSet.empty) in 
			Hashtbl.iter (fun x y -> 
			match y.node with
			| Node n -> s := IntSet.add n.var !s;
			| _ -> ()
			)table;
			let l = IntSet.elements !s in 
			let len = (List.length l) in 
			let output = ref (Hashtbl.create len) in 
			print_newline ();
			for i = 1 to len do
				Hashtbl.add !output (List.nth l (i-1)) i;
			done;
			Hashtbl.iter (fun x y -> match y.node with
			| Node n -> n.var <- Hashtbl.find !output n.var
			| _ -> ()) table;
			let output_table = ref (create_empty_bdd ()) in 
			let output_bdd = ref (apply AND bdd one !output_table) in 
			(!output_table, !output_bdd, len)
			in

		reset_max_var ();
		let result = ref (create_random_bdd depth) in
		let table = ref (create_empty_bdd()) in 
		for i = 1 to n do
		let funct = random_funct () in 
			if funct == NOT then 
				begin
					result := opnot !table !result 
				end
			else 
				begin
					table := (create_empty_bdd());
 					let tmp = create_random_bdd depth in 
 					result := apply funct !result tmp (!table);
 				end;
 			print_int (Hashtbl.length !table);
 			print_newline ();
		done;
		if compress ==true  then let new_data = var_realloc !table !result in create (Helper.first new_data) {depth=(Helper.third new_data); truth_rate=0.0; node_count=0; root=(Helper.second new_data); true_paths=0; false_paths=0;fillrate=0}
		else create !table {depth=depth; truth_rate=0.0; node_count=0; root=(!result); true_paths=0; false_paths=0; fillrate=0}

	let to_dot_file ?(lf = (fun x -> string_of_int (var x))) file data = let table = get_table data in let oc = open_out file in 
		let create_rank_string rank values label_function = let output = ref "{\ngraph [rank=same];\n" in 
			List.iter (fun x -> let str = Printf.sprintf "%d	[fillcolor=%d, label=%s, rank=%d];\n" x.uid ((var x) mod 11 + 1) (label_function x) rank in 
			output := !output ^ str) values;
			output := !output ^ "}\n";
			!output

		in let rank = ref 0 in 
  		Printf.fprintf oc "digraph \"\" {
		node [colorscheme=set312,
		label=\"\\N\",
		shape=circle,
		style=filled
		];\n";
		let var_map = create_var_map table in let keys = sort_table_keys var_map in 
		List.iter (fun x -> rank := !rank + 1; 
		let values = (Hashtbl.find_all var_map x) in let str = (create_rank_string !rank values lf) in Printf.fprintf oc "%s" str) keys;
		Printf.fprintf oc "0	 [fillcolor=White, label=F, rank=None, shape=doublecircle];\n1	 [fillcolor=White, label=T, rank=None, shape=doublecircle];\n";
		Hashtbl.iter (fun x y -> if y.uid > 1 then let uid = y.uid in let low_uid = (low y).uid in let high_uid = (high y).uid in 
		let str = Printf.sprintf "%d -> %d 		[style=dotted];\n%d -> %d\n" uid low_uid uid high_uid in Printf.fprintf oc "%s" str) table;
		Printf.fprintf oc "}"; 
		close_out oc

	let save_bdd file data = let table = get_table data in 
		
		let print_info_string data = Printf.sprintf "Node_count: %d; Truth_rate: %f; Root: %d; Depth: %d" (count_nodes data) (count_truth_rate data) (get_root data).uid (get_depth data) in 

		let hash_entry_to_string x y = Printf.sprintf "UID:%d VAR:%d LOW_UID:%d HIGH_UID:%d" y.uid (var y) (low y).uid (high y).uid in

		let bdd_to_string table =  let output = ref "" in 
			Hashtbl.iter (fun x y -> output := (!output ^ (hash_entry_to_string x y)) ^ ";") table;
			Str.string_before !output ((String.length !output) -1) in

		let bdd_id = if Sys.file_exists file then
		let chan = open_in file in
		let last_line = ref "" in
		try
  			while true do
    			last_line := input_line chan;
  			done; 1
		with | End_of_file ->
			close_in chan;
  			(int_of_string (get_bdd_id !last_line)) + 1
   		else 1 in let  oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 file in
		Printf.fprintf oc "%d, [%s], %s\n" bdd_id (bdd_to_string table) (print_info_string data);   (* write something *)   
  		close_out oc

  	let rebuild data = let depth = (get_depth data) in let table = ref (create_empty_bdd()) in let result = ref (apply AND (get_root data) one !table) in 
		(* create !table {depth=depth; truth_rate=0.0; node_count=0; root=(!result)} *)
		set_table data !table;
		set_info data {depth=depth; truth_rate=0.0; node_count=0; root=(!result); true_paths=0; false_paths=0; fillrate=0} 

	let rounding_up_remap from_level data = 

		let remap_lowest bdd = match bdd.node with
			|Node n -> 
				begin
				match n.low.node, n.high.node with 
				| True, _ | _, True -> ()
				| False ,  n1 -> n.high <- one
				| Node n1, False -> n.low <- one
				| Node n1, Node n2 -> if n1.info.true_paths > n2.info.true_paths then n.low <- one else n.high <- one
				end
			|_ -> () in 

		let table = data.bdd in let depth = (get_depth data) in let var_map = create_var_map table in 
		count_false_true data;
		for i = from_level to depth do
			try
				let values = Hashtbl.find_all var_map i in 
				List.iter (fun x -> remap_lowest x)values;
			with _ -> ()
		done;
		rebuild data;
		fill_info data

	let rounding_up_replace from_level data = 

		let replace_lowest bdd table = match bdd.node with
			|Node n -> 
				begin
				match n.low.node, n.high.node with 
				| True, _ | _, True -> ()
				| False ,  Node n1 -> let test = ref (Hashtbl.find table (hash (n.high.node))) in !test.uid <- 1; !test.node <- True;
				| Node n1, False -> let test = ref (Hashtbl.find table (hash (n.low.node))) in !test.uid <- 1; !test.node <- True;
				| Node n1, Node n2 -> if n1.info.true_paths > n2.info.true_paths then let test = ref (Hashtbl.find table (hash (n.low.node))) in !test.uid <- 1; !test.node <- True
					else let test = ref (Hashtbl.find table (hash (n.high.node))) in !test.uid <- 1; !test.node <- True;
				| _ -> ()
				end
			|_ -> () in 

		let table = ref (data.bdd) in let depth = (get_depth data) in let var_map = create_var_map !table in 
		count_false_true data;
		for i = from_level to depth do
			try
				let values = Hashtbl.find_all var_map i in 
				List.iter (fun x -> replace_lowest x !table)values;
			with _ -> ()
		done;
		rebuild data;
		fill_info data


	let rounding_remap from_level data = 

		let remap bdd = match bdd.node with
			|Node n -> 
				begin
				match n.low.node with 
				| True | False  -> ();
				| Node n1 -> if n1.info.true_paths > n1.info.false_paths then n.low <- one else n.low <- zero;
				match n.high.node with 
				| True | False  -> ()
				| Node n1 -> if n1.info.true_paths > n1.info.false_paths then n.high <- one else n.high <- zero
				
				end
			|_ -> () in 

		let table = data.bdd in let depth = (get_depth data) in let var_map = create_var_map table in 
		count_false_true data;
		for i = from_level to depth do
			try
				let values = Hashtbl.find_all var_map i in 
				List.iter (fun x -> remap x)values;
			with _ -> ()
		done;
		rebuild data;
		fill_info data

	let rounding_replace from_level data = 

		let replace_lowest bdd table = match bdd.node with
			|Node n -> 
			begin
				match n.low.node with 
				| True | False -> ()
				| Node n1 -> if n1.info.true_paths > n1.info.false_paths then let test = ref (Hashtbl.find table (hash (n.low.node))) in !test.uid <- 1; !test.node <- True
					else  let test = ref (Hashtbl.find table (hash (n.low.node))) in  !test.uid <- 0; !test.node <- False;
				
				match n.high.node with 
				| True | False -> ()
				| Node n1 -> if n1.info.true_paths > n1.info.false_paths then let test = ref (Hashtbl.find table (hash (n.high.node))) in !test.uid <- 1; !test.node <- True
					else  let test = ref (Hashtbl.find table (hash (n.high.node))) in  !test.uid <- 0; !test.node <- False;
			end
			|_ -> () in 

		let table = ref (data.bdd) in let depth = (get_depth data) in let var_map = create_var_map !table in 
		count_false_true data;
		for i = from_level to depth do
			try
				let values = Hashtbl.find_all var_map i in 
				List.iter (fun x -> replace_lowest x !table) values;
			with _ -> ()
		done;
		rebuild data;
		fill_info data

	let approx_dom ?(side =1) ?(relative=false) ?(effort = 0) max_error data = 
		
		let neg_compare_score (bdd1, _) (bdd2, _) = 
			match bdd1.node, bdd2.node with 
			| Node n1, Node n2 -> -(compare n1.info.score n2.info.score)
			|Node n1, True | Node n1, False -> raise (NoTerminalAllowed "neg_compare_score")
			| True, Node n1 | False, Node n1 -> raise (NoTerminalAllowed "neg_compare_score")
			| _ -> raise (NoTerminalAllowed "neg_compare_score")

		in
		count_false_true data;
		fill_parents data;
		let sum_paths = data.info.true_paths + data.info.false_paths in
		let max_false = if not relative then int_of_float (max_error *. float_of_int sum_paths) else int_of_float (max_error *. float_of_int data.info.true_paths)  in 
		let bdd_list = ref [] in 
		if side == 1 then 
		begin
			Hashtbl.iter (fun x y -> 
			match y.node with
			| Node n -> if n.info.false_paths <= max_false then bdd_list := (y, hash(y.node))::(!bdd_list)
			| _ -> ()) data.bdd;
		end
	    else begin
	    	Hashtbl.iter (fun x y -> 
			match y.node with
			| Node n -> if (n.info.false_paths <= max_false) || (n.info.true_paths <= max_false) then 
			begin
			bdd_list := if n.info.false_paths < n.info.true_paths then  (y, hash(y.node))::(!bdd_list) else (y, -hash(y.node))::(!bdd_list);
			end
			| _ -> ()) data.bdd;
	    end;
	    if side == 1 then 
		begin
		List.iter (fun (x,y) -> let count = ref 0 in let l = ref [] in 
		match x.node with 
		| Node n -> inner_fill_dominate x l count; n.info.dominate_count <- 1 + !count; n.info.score <- float_of_int (n.info.dominate_count * sum_paths) /. float_of_int(n.info.false_paths);
		| _ -> ()) !bdd_list;
		bdd_list := List.sort neg_compare_score !bdd_list;
		end
		else begin
		List.iter (fun (x,y) -> let count = ref 0 in let l = ref [] in 
		match x.node with 
		| Node n -> let paths = if y > 0 then float_of_int(n.info.false_paths) else float_of_int(n.info.true_paths) in inner_fill_dominate x l count; 
					n.info.dominate_count <- 1 + !count; n.info.score <- float_of_int (n.info.dominate_count * sum_paths) /. paths;
		| _ -> ()) !bdd_list;
		bdd_list := List.sort neg_compare_score !bdd_list;
	    end;
	    if side = 1 then 
	    begin
		let cur_false = ref 0 in
		List.iter (fun (x,y) ->  let bdd = ref (Hashtbl.find data.bdd y) in   (* if not (List.mem y !removed_node_hashes) then  *)
			match x.node with 
			| Node n ->  
				if (get_false_paths !bdd) > 0 then 
				begin
			cur_false := !cur_false + (get_false_paths !bdd);
			if !cur_false < max_false then 
			begin
				!bdd.uid <- 1; 
				!bdd.node <- True; 
				count_false_true data;
			end
		end
			| _ -> ()) !bdd_list;
		end
		else  begin
		let cur_false = ref 0 in
		List.iter (fun (x,y) ->  let bdd = ref (Hashtbl.find data.bdd (abs y)) in   (* if not (List.mem y !removed_node_hashes) then  *)
			match x.node with 
			| Node n ->  
				if (get_false_paths !bdd) > 0 then 
				begin
			let error_paths = if y > 0 then (get_false_paths !bdd) else (get_true_paths !bdd) in 
			cur_false := !cur_false + error_paths;
			if !cur_false < max_false then 
			begin
				!bdd.uid <- if y > 0 then 1 else 0; 
				!bdd.node <- if y > 0 then True else False; 
				count_false_true data;
			end
		end
			| _ -> ()) !bdd_list;
		end;
		rebuild data;
		fill_info data
		
		

	let print_info data = Printf.printf "Depth: %d Truth_rate: %f Nodes: %d Root: %d" (get_depth data) (get_truth_rate data) data.info.node_count data.info.root.uid

	let replace data new_table new_info = 
		set_info data new_info;
		set_table data new_table


	let approx_one_sided data approx_funct =

		let evaluate_one_sided info_old info_new = 
			let old_true_paths = info_old.true_paths in let old_false_paths = info_old.false_paths in let new_true_paths = info_new.true_paths in 
			let node_reduction = 1.0 -. (float_of_int (info_new.node_count) /. float_of_int(info_old.node_count))  in
			let error_rate = float_of_int(new_true_paths - old_true_paths) /. float_of_int (old_false_paths + old_true_paths) in 
			let relative_error =  float_of_int(new_true_paths - old_true_paths) /. float_of_int(new_true_paths) in
			Printf.printf "Node_reduction: %f	Error_rate: %f Relative_error: %f"  node_reduction error_rate relative_error
		in
		fill_info data;
		let info_old = copy_bdd_info data.info in 
		approx_funct data;
		evaluate_one_sided info_old data.info

	let approx_two_sided data approx_funct =

		let evaluate_two_sided old_data new_data = 
			let info_new = new_data.info in let info_old = old_data.info in 
			let node_reduction = 1.0 -. (float_of_int (info_new.node_count) /. float_of_int(info_old.node_count))  in
			let table = ref(create_empty_bdd ()) in let result = ref(apply EQ (get_root old_data) (get_root new_data) !table) in
			let comparison = create !table {depth=info_old.depth; truth_rate=0.0; node_count=0; root=(!result); true_paths=0; false_paths=0;fillrate=0} in
			count_false_true comparison;
			let error_rate = float_of_int(comparison.info.false_paths)  /. float_of_int(info_old.true_paths + info_old.false_paths) in 
			Printf.printf "Node_reduction: %f	Error_rate: %f \n"  node_reduction error_rate in 
		
		fill_info data;
		let table = ref(create_empty_bdd ()) in let result = ref(apply AND (get_root data) one !table) in
		let old_data = create !table {depth=data.info.depth; truth_rate=data.info.truth_rate; node_count=data.info.node_count; root=(!result); true_paths=data.info.true_paths;
		false_paths=data.info.false_paths;fillrate=data.info.fillrate} in 
		approx_funct data;
		evaluate_two_sided old_data data

	let tmp_uid = ref 2	
	let incr_uid = incr tmp_uid; !tmp_uid
	let create2 i j = 
		 let matrix =  Array.make_matrix i (j-1) zero in 
		 for y = 0 to (j-2) do
		 	for x = 0 to (i-1) do
		 	matrix.(x).(y) <- {uid=incr_uid; node=Node{var=0;low=zero;high=zero;info=empty_node_info()}};
		 done;
		done;

		 for y = (j-3) downto 0 do
		 	for x = (i-1) downto 0 do
		 		match matrix.(x).(y).node with 
		 		| Node n -> n.low <-matrix.(Random.int i).(y+1); n.high <-matrix.(Random.int i).(y+1); n.var <- y+2
		 		| _ -> raise (ExpectationVsRealityError "create2");
		 	done;
		done;
		for x = 0 to (i-1) do
			match matrix.(x).(j-2).node with
			Node n -> n.low <- random_terminal (); n.high <- random_terminal ()
			| _ -> raise (ExpectationVsRealityError "creat2")
		done;
		let table = ref (create_empty_bdd()) in let root = {uid=(-1); node=Node{var=1;low=matrix.(Random.int i).(0);high=matrix.(Random.int i).(0);info=empty_node_info()}} in
		let result = ref (apply AND (root) one !table) in
		print_int (count_nodes (create !table {depth=j; truth_rate=0.0; node_count=0; root=(!result); true_paths=0; false_paths=0; fillrate=0}));
		print_newline ()
end

module Approx = struct


end

let get_lowest_child x = 
	if (low x).uid == 1 || (high x).uid == 1 then (-1)
	else if (low x).uid == 0 then 1 else if (high x).uid == 0 then 0 
	else if true_paths (low x) > true_paths (high x) then 1 else 0

module Formula = struct 

	type formula = 
  	| Ffalse 
  	| Ftrue 
  	| Fvar of variable 
  	| Fand of formula * formula
  	| For  of formula * formula
  	| Fnot of formula

	let rec build = let table = ref (create_empty_bdd ()) in function
  	| Ffalse -> (zero, !table)
  	| Ftrue -> (one, !table)
  	(* | Fvar var -> create_single_bdd var *)
  	| Fand (f1, f2) -> (apply AND (fst(build f1)) (fst(build f2)) (!table), !table)
  	| For (f1, f2) -> (apply OR (fst(build f1)) (fst(build f2)) (!table), !table)
  	(* | Fnot f -> opnot (build f) *)
  	| _ -> (zero, !table) 

end

module FileIO = struct




end

let speedtest () =

	Random.init 12345;
	let data = BddData.create_random_function 63 20 in 
	let table = BddData.get_table data in 
	let t = Sys.time () in 
	for i = 1 to 1000 do
		BddData.count_paths data;
		

	done;
	Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
	BddData.to_dot_file ~lf:NodeLabel.path_label "test2.dot" data
let speed_test () =
	let t = Sys.time () in 
	
	let data = ref (BddData.empty ()) in 
	Random.init 12345;
	for i = 1 to 400 do
	
		 data := BddData.create_random_function 100 29;
		 BddData.approx_two_sided !data (BddData.approx_dom ~side:2 0.01);

	done;
	Printf.printf "Execution time: %fs\n" (Sys.time() -. t)


let test3 x = 
	Random.init 1234;
	let data = BddData.create_random_function 60 x in 
	BddData.count_false_true data;
	BddData.to_dot_file  "test.dot" data; 
	BddData.approx_one_sided data (BddData.rounding_up_remap 10);
	BddData.save_bdd "test.txt" data; 
	(* BddData.approx_one_sided data (BddData.rounding 50 get_low);  *)
	BddData.to_dot_file  "test2.dot" data

let test4 x = 
	Random.init 1234;
	let data = BddData.create_random_function 60 x in 
	BddData.to_dot_file ~lf:NodeLabel.path_label "test1.dot" data;
	BddData.approx_one_sided data (BddData.approx_dom 0.01);
	Random.init 1234;
	let data = BddData.create_random_function 60 x in 
	BddData.approx_two_sided data (BddData.approx_dom ~side:2 0.1);
	BddData.to_dot_file ~lf:NodeLabel.path_label "test2.dot" data;
	BddData.print_info data
