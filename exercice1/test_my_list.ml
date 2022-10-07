open My_list

let string_of_nat_list = string_of_list string_of_int;;

let string_of_string_list = string_of_list (fun x -> x);;

let empty = Nil;;

let one = Cons ("a", Nil);;

let lst = Cons(1, Cons(3, Cons(6, Cons(10, Nil))));;

let test_hd () =
  Printf.printf "Tête de %s : %s.\n" (string_of_string_list one) (hd one);
  Printf.printf "Tête de %s : %d.\n\n" (string_of_nat_list lst) (hd lst);;

let test_tl () =
  Printf.printf "Queue de %s : %s.\n" (string_of_string_list one) (string_of_string_list (tl one));
  Printf.printf "Queue de %s : %s.\n\n" (string_of_nat_list lst) (string_of_nat_list (tl lst));;

let test_length () =
  Printf.printf "Taille de %s : %d.\n" (string_of_string_list one) (length one);
  Printf.printf "Taille de %s : %d.\n" (string_of_nat_list lst) (length lst);
  Printf.printf "Taille de %s : %d.\n\n" (string_of_string_list empty) (length empty);;

let test_map ()=
  Printf.printf "Map de (x -> xx) sur %s : %s.\n" (string_of_string_list one) (string_of_string_list (map (fun s -> s ^ s) one));
  Printf.printf "Map de (x -> 2x) sur %s : %s.\n" (string_of_nat_list lst) (string_of_nat_list (map (fun n -> 2 * n) lst));
  Printf.printf "Map de (x -> 2x) sur %s : %s.\n\n" (string_of_nat_list empty) (string_of_nat_list (map (fun n -> 2 * n) empty));;


test_hd (); test_tl (); test_length (); test_map ();;
