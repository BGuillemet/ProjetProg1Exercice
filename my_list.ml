type 'a my_list =
| Nil
| Cons of 'a * 'a my_list

let string_of_list str_fun l =
	let rec string_content l = match l with
		|Nil -> ""
		|Cons (a, Nil) -> str_fun a
		|Cons (a, q) -> (str_fun a) ^ ", " ^ (string_content q)
	in "[" ^ (string_content l) ^ "]" 


let rec hd l = match l with
	|Nil -> failwith "liste vide"
	|Cons (a, q) -> a


let rec tl l = match l with
	|Nil -> failwith "liste vide"
	|Cons (a, q) -> q


let rec length l = match l with
	|Nil -> 0
	|Cons (a, q) -> 1 + (length q)


let rec map f l = match l with
	|Nil -> Nil
	|Cons (a, q) -> let reste = (map f q) in Cons ((f a), reste)
