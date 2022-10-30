open Asyntax

type type_exp = ExpInt |ExpFloat |NotDef

exception Type_error
exception Var_error

let rec find_type_exp ?(variables = (Array.make 26 NotDef)) exp =
	match exp with 
          |Aff (c, e, suite) -> let type_var = find_type_exp ~variables:variables e in
	  			(* Using ASCII code to find the index of the variable easily *)
                                if variables.((Char.code c) - 97) = NotDef
                                then variables.((Char.code c) - 97) <- type_var
                                else raise Var_error; (* You cannot define a variable twice *)
                                find_type_exp ~variables:variables suite
          |Int x -> ExpInt
          |Float x -> ExpFloat
          |Var c -> let type_var = variables.((Char.code c) - 97) in
                    if type_var = NotDef then raise Var_error (* The variable used is not defined *)
                    else type_var
          |FloatPlus (e1, e2) -> (* Two floats *) 
	  			let t1 = find_type_exp ~variables:variables e1 in
                                if t1 = ExpFloat then
                                let t2 = find_type_exp ~variables:variables e2 in
                                if t2 = ExpFloat then ExpFloat
                                else raise Type_error
                                else raise Type_error
          |Plus (e1, e2) -> (* Two integers *)
	  		    let t1 = find_type_exp ~variables:variables e1 in
                            if t1 = ExpInt then
                            let t2 = find_type_exp ~variables:variables e2 in
                            if t2 = ExpInt then ExpInt
                            else raise Type_error
                            else raise Type_error
          |FloatMinus (e1, e2) -> (* Two floats *)
	  			  let t1 = find_type_exp ~variables:variables e1 in
                                  if t1 = ExpFloat then
                                  let t2 = find_type_exp ~variables:variables e2 in
                                  if t2 = ExpFloat then ExpFloat
                                  else raise Type_error
                                  else raise Type_error
          |Minus (e1, e2) -> (* Two integers *) 
	  		    let t1 = find_type_exp ~variables:variables e1 in
                            if t1 = ExpInt then
                            let t2 = find_type_exp ~variables:variables e2 in
                            if t2 = ExpInt then ExpInt
                            else raise Type_error
                            else raise Type_error
          |FloatTimes (e1, e2) -> (* Two floats *)
	  			  let t1 = find_type_exp ~variables:variables e1 in
                                  if t1 = ExpFloat then
                                  let t2 = find_type_exp ~variables:variables e2 in
                                  if t2 = ExpFloat then ExpFloat
                                  else raise Type_error
                                  else raise Type_error
          |Times (e1, e2) -> (* Two integers *)
	  		    let t1 = find_type_exp ~variables:variables e1 in
                            if t1 = ExpInt then
                            let t2 = find_type_exp ~variables:variables e2 in
                            if t2 = ExpInt then ExpInt
                            else raise Type_error
                            else raise Type_error
          |FloatDiv (e1, e2) -> (* Two floats *)
	  		        let t1 = find_type_exp ~variables:variables e1 in
                                if t1 = ExpFloat then
                                  let t2 = find_type_exp ~variables:variables e2 in
                                  if t2 = ExpFloat then ExpFloat
                                  else raise Type_error
                                else raise Type_error
          |Div (e1, e2) -> (* Two integers *)
	  		  let t1 = find_type_exp ~variables:variables e1 in
                          if t1 = ExpInt then
                          let t2 = find_type_exp ~variables:variables e2 in
                          if t2 = ExpInt then ExpInt
                          else raise Type_error
                          else raise Type_error
          |Mod (e1, e2) -> (* Two integers *)
	  		  let t1 = find_type_exp ~variables:variables e1 in
                          if t1 = ExpInt then
                          let t2 = find_type_exp ~variables:variables e2 in
                          if t2 = ExpInt then ExpInt
                          else raise Type_error
                          else raise Type_error
          |Pow (e1, e2) -> (* Two integers *) 
	  		  let t1 = find_type_exp ~variables:variables e1 in
                          if t1 = ExpInt then
                          let t2 = find_type_exp ~variables:variables e2 in
                          if t2 = ExpInt then ExpInt
                          else raise Type_error
                          else raise Type_error
          |Fact e -> (* Integer *)
	  	    let t = find_type_exp ~variables:variables e in
                    if t = ExpInt then ExpInt else raise Type_error
          |Pos e -> (* Same type as the argument *) find_type_exp ~variables:variables e
          |Neg e -> (* Same type as the argument *) find_type_exp ~variables:variables e
          |OpInt e -> (* Float *) if find_type_exp ~variables:variables e = ExpFloat then ExpInt else raise Type_error
          |OpFloat e -> (* Integer *) if find_type_exp ~variables:variables e = ExpInt then ExpFloat else raise Type_error
