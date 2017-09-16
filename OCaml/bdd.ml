exception LineNumberNotFound of string
exception BddInputError of string
exception ExpectationVsRealityError of string
exception ExpectedUnaryError 
exception NoTerminalAllowed of string

type node_info = {mutable paths: int; mutable true_paths:int; mutable false_paths: int; mutable parent_count: int; mutable dominate_count: int}
type funny = AND | OR | NOT
type variable = int (* 1..max_var *)
type bdd = { mutable uid: int; mutable node : node }
and node = True | False | Node of {mutable var: int; mutable low: bdd; mutable high: bdd; mutable info: node_info}
type bdd_info = {mutable depth: int; mutable truth_rate: float; mutable true_paths: int; mutable false_paths: int ; mutable node_count: int; mutable root: bdd; fillrate: int}

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c
let functions = [|AND; OR; NOT|]

let empty_node_info () = {paths=0; true_paths=0; false_paths=0; parent_count=0; dominate_count=0}

let random_funct ?(opnot = true) ()= 
    if opnot == true then let n = Random.int (Array.length functions) in Array.get functions n
	else let n = Random.int ((Array.length functions)-1) in Array.get functions (n)

let max_var : int ref = ref 0
let get_max_var () = !max_var
let reset_max_var () = max_var := 0

let max_nodes = 200000

let node b = b.node

let equal x y = match x, y with
  | Node n1, Node n2 -> 
      n1.var == n2.var && n1.low == n2.low && n1.high == n2.high
  | _ -> x == y

let hash = function
  | True -> 1
  | False -> 0
  | Node n -> abs (n.var + max_nodes * n.low.uid + max_nodes * max_nodes * n.high.uid)

let cur_uid = ref (2)
let genuid = fun () -> incr cur_uid

let zero = { uid = 0; node = False }
let one = { uid = 1; node = True }

let empty_bdd_info () = {depth=0;truth_rate=0.0; true_paths=0; false_paths=0 ; node_count=0; root=zero; fillrate=0}

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

module H = struct
  type t = bdd
  let equal {uid = u;node = n} {uid = u2;node = n2} = equal n n2
  let hash {uid = u;node = n} = hash n 
end

let random_terminal () = if Random.bool () then one else zero

let print_node x = match x.node with
	| True -> print_string "TRUE"
	| False -> print_string "FALSE"
	| _ -> Printf.printf "uid: %d var: %d low: %d high: %d \n" (uid x) (var x) (uid (low x)) (uid (high x))

let true_paths bdd = 
	match bdd.node with 
	| Node x -> x.info.true_paths
	| True | False -> raise (NoTerminalAllowed (Printf.sprintf "true_paths %d" bdd.uid))

let false_paths bdd = 
	match bdd.node with 
	| Node x -> x.info.false_paths
	| True | False -> raise (NoTerminalAllowed (Printf.sprintf "false_paths %d" bdd.uid))

let print_table table = 
	Hashtbl.iter  (fun x y  -> print_node y)  table 

let create_empty_bdd () = let t = Hashtbl.create 10 in 
    let _ = Hashtbl.add t  0 zero in
    let _ = Hashtbl.add t 1 one in 
    t

let add_node table node = let bdd = {uid = !cur_uid; node=node} in  
  if var bdd > !max_var then max_var := var bdd; 
  if (low bdd == high bdd) then low bdd else 
  (* let b2 = if Ht.mem table node then W.merge table bdd in b2 *)
  try 
  Hashtbl.find table (hash bdd.node)
  with _ -> genuid (); Hashtbl.add table (hash bdd.node) bdd; bdd

let create_single_bdd var = 
	let t = ref (Hashtbl.create 10) in 
    ignore(Hashtbl.add !t  0 zero);
    ignore(Hashtbl.add !t 1 one);
    (add_node !t (Node{var=var; low=zero; high=one;info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}}), !t)

module Int = struct 
   type t = int 
   (* use Pervasives compare *)
   let compare = compare
 end

module Ints = Set.Make(Int)

let create_random_rumbers n max = let s = ref Ints.empty in 
	while Ints.cardinal !s  < n do
		s := Ints.add ((Random.int max) + 1) !s;
	done;
	!s 

let to_bool terminal =
	match terminal with 
	| False -> false
	| True -> true
	| _ -> false 

let op funct op1 op2 = 
	match funct with 
	| AND -> if (to_bool op1) && (to_bool op2) then one else zero
	| OR -> if (to_bool op1) || (to_bool op2) then one else zero 
	| _ -> raise (ExpectedUnaryError)

let connected_to_terminal node = if node.uid == 1 || node.uid == 0 then 0 else let output = ref 0 in 
	begin
		if (low node).node == True || (high node).node == True then output := !output + 2;
		if (low node).node == False || (high node).node == False then output := !output + 1;
	end;
	!output

let is_terminal bdd =
	 match bdd.node with 
	| Node x -> false
	| True | False -> true

let switch_terminal terminal = if terminal == zero then one else if terminal == one then zero else raise (BddInputError "switch_terminals")

let switch_terminals bdd = match bdd.node with
	| Node n -> 
		begin
			if (is_terminal n.low) then n.low <- (switch_terminal n.low); 
			if (is_terminal n.high) then n.high <- (switch_terminal n.high);
		end;
	| _ -> ()

let rec apply funct bdd1 bdd2 table =
	match bdd1.node, bdd2.node with
	| Node n1, Node n2 -> 
		if n1.var < n2.var then add_node table (Node{var=n1.var; low=(apply funct n1.low bdd2 table); high=(apply funct n1.high bdd2 table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}}) 
		else if n1.var > n2.var then add_node table (Node{var=n2.var; low=(apply funct bdd1 n2.low table); high=(apply funct bdd1 n2.high table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}}) 
		else add_node table (Node{var=n1.var; low=(apply funct n1.low n2.low table); high=(apply funct n1.high n2.high table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}}) 
	| Node n1, t -> add_node table (Node{var=n1.var; low=(apply funct n1.low bdd2 table ); high=(apply funct n1.high bdd2 table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}}) 
	| t, Node n1 -> add_node table (Node{var=n1.var; low=(apply funct n1.low bdd1 table); high=(apply funct n1.high bdd1 table); info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}}) 
	| t1, t2 -> op funct t1 t2

let opnot table bdd = let to_do = ref [] in 
	Hashtbl.iter (fun x y -> 
		if (connected_to_terminal y > 0) then 
		begin
			to_do := (x,y)::!to_do;
		end) table;
	List.iter (fun (x,y) -> Hashtbl.remove table x) !to_do;
	List.iter (fun (x,y) -> switch_terminals y; Hashtbl.add table (hash y.node) y) !to_do;
	bdd

let create_random_bdd ?(n = 5) depth =
	let table = ref (create_empty_bdd ()) in 
	let vars = create_random_rumbers n depth in 
	let bdds = ref [] in 
	Ints.iter (fun x -> bdds := [create_single_bdd x] @ !bdds) vars;
	List.fold_left (fun acc x -> let funct = random_funct() in 
		if funct == NOT then apply (random_funct ~opnot: false ()) (opnot !table acc) (fst x) !table
		else apply funct acc (fst x) !table) (fst (List.hd !bdds)) (List.tl !bdds)

let node_count table = Hashtbl.length table

let get_bdd_id str = let reg = Str.regexp "\\(^[0-9]+\\), " in 
	if not (Str.string_match reg str 0) then raise (LineNumberNotFound (Str.first_chars str (min (String.length str) 20))) else 
	let output = Str.matched_group 1 str in 
	output

let get_some x =
 	match x with
 	| Some x -> x
 	| None -> raise (ExpectationVsRealityError "expected something")

let create_var_map table = let var_table = ref (Hashtbl.create 10) in 
	Hashtbl.iter(fun x y -> if not (is_terminal y) then  Hashtbl.add !var_table (var y) (y)) table;
	!var_table


let sort_table_keys ?(reverse=false) table = let output = ref [] in 
	Hashtbl.iter (fun x y -> if not (List.mem x !output) then output := x :: !output) table;
	List.sort compare !output

let paths_labeling bdd = 
	Printf.sprintf "\"%d %d\"" (false_paths bdd) (true_paths bdd) 

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

	let add_node data node = let table = ref data.bdd in let bdd = {uid = !cur_uid; node=node} in  
  		if var bdd > (get_depth data) then set_depth data (var bdd); 
  		if (low bdd == high bdd) then low bdd else 
 		 try 
  			Hashtbl.find !table (hash bdd.node)
  		 with _ -> genuid (); Hashtbl.add !table (hash bdd.node) bdd; bdd
	
  	let print_nodes data = let table = ref data.bdd in 
  		Hashtbl.iter (fun x y -> print_node y) !table

	let create_single_bdd var = let data = ref (empty ()) in let table = ref !data.bdd in 
		ignore(Hashtbl.add !table 0 zero);
    	ignore(Hashtbl.add !table 1 one);
    	let root = add_node !data (Node{var=var; low=zero; high=one;info={paths=0;true_paths=0;false_paths=0; parent_count=0; dominate_count=0}})
		in { bdd = !table; info = {depth=var; truth_rate=0.0; node_count= 0; root=root; true_paths=0; false_paths=0; fillrate=0}}
    
	let count_nodes x = x.info.node_count <- Hashtbl.length x.bdd; x.info.node_count

	let count_truth_rate x = let table = x.bdd in let info = x.info in
		let true_hits = ref 0 in let false_hits = ref 0 in 
		Hashtbl.iter (fun x y -> let check =(connected_to_terminal y) in 
			if (check > 0) then let multiplier = int_of_float(2.0** (float_of_int(info.depth - var y))) in
			begin
				true_hits := !true_hits + ((check / 2) * multiplier);
				false_hits := !false_hits + ((check mod 2) * multiplier);
			end
		) table;
		info.truth_rate <- (float_of_int !true_hits) /. ((float_of_int !true_hits) +. (float_of_int !false_hits));
		info.truth_rate

	let count_paths data = 
		let rec inner_count_paths bdd paths = 
			match bdd.node with
			| True | False -> ()
			| Node n -> n.info.paths <- n.info.paths + paths; inner_count_paths n.low (paths / 2); inner_count_paths n.high (paths/2)
			in let info = ref data.info 
			in let root = ref (get_root data) in let depth = !info.depth 
			in inner_count_paths !root (pow 2 depth);
			match !root.node with 
			| Node n1 -> n1.info.paths
			| _ -> 0

	let count_false_true data =

		let table = ref data.bdd in let info = ref data.info in let var_map = create_var_map !table in
		let depth = !info.depth  in

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
		in reset_paths !table;

		for i = depth downto 1 do
			try
  				let values = Hashtbl.find_all var_map i in 
  				List.iter (fun x -> match x.node with
  				| Node n -> begin
  						if low x == zero || high x == zero then n.info.false_paths <- n.info.false_paths + (pow 2 (depth-(var x)));
						if low x == one || high x == one then n.info.true_paths <- n.info.true_paths + (pow 2 (depth-(var x)));
						if not (is_terminal (low x)) then 
						begin
							let var_diff = (var n.low) - (var x) in 
								n.info.true_paths <- n.info.true_paths + (pow 2 (var_diff - 1)) * (true_paths (low x));
								n.info.false_paths <- n.info.false_paths + (pow 2 (var_diff - 1)) * (false_paths (low x));
						end;
						if not (is_terminal (high x)) then 
						begin
							let var_diff = (var n.high) - (var x) in 
								n.info.true_paths <- n.info.true_paths + (pow 2 (var_diff - 1)) * (true_paths (high x));
								n.info.false_paths <- n.info.false_paths + (pow 2 (var_diff - 1)) * (false_paths (high x));
  						end;
  						n.info.paths <- n.info.true_paths + n.info.false_paths; 
  					end
  				| True | False -> raise  (NoTerminalAllowed "count_false_true")
  				) values
  			with Not_found -> ()
		done;
		match (get_root data).node with
		|Node n -> begin
			!info.true_paths <- (pow 2 (n.var - 1)) * n.info.true_paths;
			!info.false_paths <- (pow 2 (n.var - 1)) * n.info.false_paths;
			end;
		| False -> !info.false_paths <- pow 2 (get_depth data);
		| True -> !info.true_paths <- pow 2 (get_depth data);
		!info.truth_rate <- float_of_int(!info.true_paths) /. float_of_int(!info.true_paths + !info.false_paths)

	

	let fill_info data = 
		data.info.node_count <- count_nodes data;
		count_false_true data
	
	let fill_parents data = Hashtbl.iter (fun x y -> match y.node with
		| Node n -> begin
			match n.low.node with 
			| Node nlow -> nlow.info.parent_count <- nlow.info.parent_count + 1;
			| _ -> ();
			match n.high.node with 
			| Node nhigh -> nhigh.info.parent_count <- nhigh.info.parent_count + 1
			| _ -> ()
		end
		| _ -> ()) data.bdd


	let fill_dominate bdd = 
		let rec inner_fill_dominate bdd acc score= 
			match bdd.node with  
			| Node n -> begin
			
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
	in let score = ref 0 in let l = ref [] in inner_fill_dominate bdd l score;
	string_of_int !score

	let create_random_function ?(compress=true) depth n =

		let var_realloc table bdd = let s = ref (Ints.empty) in 
			Hashtbl.iter (fun x y -> 
			match y.node with
			| Node n -> s := Ints.add n.var !s;
			| _ -> ()
			)table;
			let l = Ints.elements !s in 
			let len = (List.length l) in 
			let output = ref (Hashtbl.create len) in 
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
 					print_int (Hashtbl.length !table);
 					print_newline()
 				end
		done;
		if compress ==true  then let new_data = var_realloc !table !result in create (first new_data) {depth=(third new_data); truth_rate=0.0; node_count=0; root=(second new_data); true_paths=0; false_paths=0;fillrate=0}
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
		let info_old = data.info in 
		approx_funct data;
		let info_new = data.info in 
		evaluate_one_sided info_old info_new

	let approx_two_sided data approx_funct =

		let evaluate_two_sided info_old info_new bdd_old bdd_new table_new table_old = 
			let node_reduction = 1.0 -. (float_of_int (info_new.node_count) /. float_of_int(info_old.node_count))  in
			let table = ref(create_empty_bdd ()) in let result = ref(apply AND bdd_old bdd_new !table) in
			let correct_true_data = create !table {depth=info_old.depth; truth_rate=0.0; node_count=0; root=(!result); true_paths=0; false_paths=0;fillrate=0} in
			let table = ref(create_empty_bdd ()) in ignore(opnot table_old bdd_old); ignore(opnot table_new bdd_new); let result =  ref(apply AND bdd_old bdd_new !table)in
			let correct_false_data = create !table {depth=info_old.depth; truth_rate=0.0; node_count=0; root=(!result); true_paths=0; false_paths=0; fillrate=0} in
			count_false_true correct_true_data;
			count_false_true correct_false_data;
			let error_rate = float_of_int(abs(info_old.true_paths - correct_true_data.info.true_paths) + abs(info_old.false_paths - correct_false_data.info.true_paths)) /. float_of_int(info_old.true_paths + info_old.false_paths) in 
			Printf.printf "Node_reduction: %f	Error_rate: %f ctdf: %d cfdf: %d"  node_reduction error_rate correct_true_data.info.false_paths correct_false_data.info.false_paths in 
		
		fill_info data;
		let info_old = data.info in 
		let table_old = Hashtbl.copy data.bdd in 
		let bdd_old = get_root data in 
		approx_funct data;
		let info_new = data.info in 
		let table_new = data.bdd in
		let bdd_new = get_root data in 
		evaluate_two_sided info_old info_new bdd_old bdd_new table_new table_old


end

let get_lowest_child x = 
	if (low x) == one || (high x) == one then (-1)
	else if (low x) == zero then 1 else if (high x) == zero then 0 
	else if true_paths (low x) > true_paths (high x) then 1 else 0



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
  | Fvar var -> create_single_bdd var
  | Fand (f1, f2) -> (apply AND (fst(build f1)) (fst(build f2)) (!table), !table)
  | For (f1, f2) -> (apply OR (fst(build f1)) (fst(build f2)) (!table), !table)
  (* | Fnot f -> opnot (build f) *)
  | _ -> (zero, !table) 
  (* | For (f1, f2) -> mk_or (build f1) (build f2)
  | Fimp (f1, f2) -> mk_imp (build f1) (build f2)
  | Fiff (f1, f2) -> mk_iff (build f1) (build f2)
  | Fnot f -> mk_not (build f)

 *)

let speed_test () =
	let t = Sys.time () in 
	let n = ref 0 in 
	for i = 1 to 100000000 do
		 n := 1
	done;
	Printf.printf "Execution time: %fs\n" (Sys.time() -. t)

let speed_test2 () =
	Random.init 12345;
	let data = BddData.create_random_function 60 18 in
	let t = Sys.time () in 
	for i = 1 to 20000 do
		 BddData.fill_parents data;
	done;
	Printf.printf "Execution time: %fs\n" (Sys.time() -. t)


let test x = 
	Random.init 1234;
	let data = BddData.create_random_function 60 x in 
	BddData.count_false_true data;
	BddData.fill_parents data;	
	BddData.to_dot_file  ~lf: BddData.fill_dominate "test.dot" data; 
	BddData.approx_one_sided data (BddData.rounding_up_remap 10);
	BddData.save_bdd "test.txt" data; 
	(* BddData.approx_one_sided data (BddData.rounding 50 get_low);  *)
	BddData.to_dot_file  "test2.dot" data

let test3 x = 
	Random.init 1234;
	let data = BddData.create_random_function 60 x in 
	BddData.count_false_true data;
	BddData.to_dot_file  "test.dot" data; 
	BddData.approx_one_sided data (BddData.rounding_up_replace 10);
	BddData.save_bdd "test.txt" data; 
	(* BddData.approx_one_sided data (BddData.rounding 50 get_low);  *)
	BddData.to_dot_file  "test2.dot" data

let test4 x = 
	(* Random.init 1234; *)
	let data = BddData.create_random_function 60 x in 
	BddData.count_false_true data;
	BddData.to_dot_file  "test.dot" data; 
	BddData.approx_two_sided data (BddData.rounding_remap 10);
	BddData.save_bdd "test.txt" data; 
	(* BddData.approx_one_sided data (BddData.rounding 50 get_low);  *)
	BddData.to_dot_file  "test2.dot" data

