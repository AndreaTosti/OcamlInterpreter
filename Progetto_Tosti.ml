(* ANDREA TOSTI - CORSO B - Mat n. 518111 *)
(* Secondo progetto v.1, 15 dicembre 2016 *)

type ide = string ;;
type 'v env = (string * 'v) list ;;

type exp =
	| CstInt of int
	| CstTrue
	| CstFalse
	| Den of ide (* nome funzione *)
	| DenArg of ide * tuple (* nome funzione, argomento *)
	| Times of exp * exp (* prodotto *)
	| Sum of exp * exp (* somma *)
	| Sub of exp * exp (* sottrazione *)
	| Eq of exp * exp (* uguaglianza *)
	| Iszero of exp (* uguale a zero *)
	| Mod of exp * exp (* modulo *)
	| Or of exp * exp (* or *)
	| And of exp * exp (* and *)
	| Not of exp (* not *)
	| Ifthenelse of exp * exp * exp (* if-then-else *)
	| Apply of exp * exp (* applicazione di funzione *)
	| Let of ide * exp * exp
	| Letrec of ide * ide * exp * exp
	| Etup of tuple (* le tuple sono anche espressioni *)
	| Pipe of tuple (* concatenazione di funzioni *)
	| ManyTimes of int * exp (* esecuzione iterata di funzione *)
and
      tuple = Nil (* tupla vuota *)
    | IntTup of int 
    | BoolTup of bool
	| Seq of exp  * tuple (* tupla di espressioni *)
;;

(* Tipo esprimibile evT *)
type evT = 
	| Int of int
	| Bool of bool
	| Closure of ide * exp * evT env (* Struttura dati : chiusura *)
	| RecClosure of ide * ide * exp * evT env (* Struttura dati : chiusura ricorsiva *)	   
	| TupleVal of tuple
	| Unbound (* variabile non associata ad un valore *)
;;

let emptyEnv = [("", Unbound)] ;;

let bind (s : 'v env) (i : string) (x : evT) = (i, x) :: s ;;

let rec lookup (s : 'v env) (i : string) = match s with
	| [] -> Unbound
	| (j, v)::sl when j = i -> v
	| _::sl -> lookup sl i
;;

let typecheck (x, y) = match x with
	| "int" -> (match y with
					| Int(u) -> true 
					| _ -> false
				)
	| "bool" -> (match y with
					| Bool(u) -> true
					| _ -> false
				)
	| _ -> failwith ("not a valid type (typecheck)")
;;

(* is_zero controlla che x sia un intero e che sia uguale a 0 *)
let is_zero x = match (typecheck("int", x), x) with
	| (true, Int(y)) -> Bool (y = 0)
	| (_, _) -> failwith ("run-time error (is_zero typecheck)")
;;

(* int_mod controlla che x, y siano interi e fa x mod y *)
let int_mod(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with
	| (true, true, Int(v), Int(w)) -> Int(v mod w)
	| (_, _, _, _) -> failwith("run-time error (int_mod typecheck)")
;;

(* int_eq controlla che x, y siano interi e che x = y *)
let int_eq(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with
	| (true, true, Int(v), Int(w)) -> Bool(v = w)
	| (_, _, _, _) -> failwith("run-time error (int_eq typecheck)")
;;

(* int_plus controlla che x, y siano interi e fa x + y *)
let int_plus(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with
	| (true,true, Int(v), Int(w)) -> Int(v + w)
	| (_, _, _, _) -> failwith("run-time error (int_plus typecheck")
;;

(* int_times controlla che x, y siano interi e fa x * y *)
let int_times(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with
	| (true,true, Int(v), Int(w)) -> Int(v * w)
	| (_, _, _, _) -> failwith("run-time error (int_times typecheck)")
;;

(* int_sub controlla che x, y siano interi e fa x - y *)
let int_sub(x, y) = match(typecheck("int", x), typecheck("int", y), x, y) with
	| (true, true, Int(v), Int(w)) -> Int(v - w)
	| (_, _, _, _) -> failwith("run-time error (int_sub typecheck")
;;

(* bool_and con strategia di valutazione "short-circuited" *)
let bool_and(x, y) = match (typecheck("bool", x), x) with
	| (true, Bool(false)) -> Bool(false)
	| (true, Bool(true)) -> 
		(
			match (typecheck("bool", y), y) with
				| (true, Bool(true)) -> Bool(true)
				| (true, Bool(false)) -> Bool(false)
				| (_, _) -> failwith("run-time error (bool_and typecheck)")
		)
	| (_, _) -> failwith("run-time error (bool_and typecheck)")
;;

(* bool_or con strategia di valutazione "short-circuited" *)
let bool_or(x, y) = match(typecheck("bool", x), x) with
	| (true, Bool(v)) -> if v then Bool(true) else 
		(
			match (typecheck("bool", y), y) with
				| (true, Bool(w)) -> if w then Bool(true) else Bool(false)
				| (_, _) -> failwith("run-time error (bool_or typecheck)")
		)
	| (_,_) -> failwith("run-time error (bool_or typecheck)")
;;

(* bool_not controlla che x sia booleano e fa not(x) *)
let bool_not(x) = match (typecheck("bool", x), x) with
	| (true, Bool(v)) -> Bool(not(v))
	| (_, _) -> failwith("run-time error (bool_not typecheck)")
;;

(* Trasformazione di una sequenza di Seq in una concatenazione di sole Apply : 
	Apply(Den(f), Apply(Den(g), ..., Apply(Den(h), CstTrue/CstFalse/CstInt(n)) *)
let rec seqToApply(tupla) = match tupla with
	| Seq(Den(f), BoolTup(b)) -> if b then Apply(Den(f), CstTrue) else Apply(Den(f), CstFalse)
	| Seq(Den(f), IntTup(n)) -> Apply(Den(f), CstInt (n)) 
	| Seq(Den(f), a) -> Apply(Den(f), seqToApply(a))
	| (Seq(_, _) | Nil | IntTup _ | BoolTup _) -> failwith("Error: Illegal argument (seqToApply)")
;;

(* Trasformazione di una tupla in una sequenza di sole Seq : 
	Seq(Den(f), Seq(Den(g), ..., Seq(Den(h), IntTup(n)/BoolTup(b)) *)
let rec tupleToSeq(tupla) = match tupla with
	| IntTup(n) ->  IntTup(n)
	| BoolTup(b) -> BoolTup(b)
	| Seq(ManyTimes(n, Den(f)), a) -> 
		if n < 1 then failwith("Error: Illegal argument for ManyTimes (num < 1) (tupleToSeq)")
			else if n > 1 then Seq(Den(f), (tupleToSeq(Seq(ManyTimes((n-1),Den(f)), a))))
				else Seq(Den(f), tupleToSeq(a))
	| Seq(Den(f), a) -> Seq(Den(f), tupleToSeq(a))
	(* ignoro il parametro b nei due seguenti casi e do' priorita' all'argomento contenuto in Pipe/Etup *)
	| Seq(Pipe(a), b) -> tupleToSeq(tupleToSeq(a))
	| Seq(Etup(a), b) -> tupleToSeq(tupleToSeq(a))
	| Nil -> Nil
	| Seq(_, _) -> failwith("Error : Illegal argument (tupleToSeq)")
;;

let rec eval (e:exp) (s: evT env) = match e with
	| CstInt(n) -> Int(n)
	| CstTrue -> Bool(true)
	| CstFalse -> Bool(false)
	| Eq(e1, e2) -> int_eq((eval e1 s), (eval e2 s))
	| Times(e1, e2) -> int_times((eval e1 s), (eval e2 s))
	| Sum(e1, e2) -> int_plus((eval e1 s), (eval e2 s))
	| Sub(e1, e2) -> int_sub((eval e1 s), (eval e2 s))
	| Iszero(e1) -> is_zero(eval e1 s)
	| Mod(e1, e2) -> int_mod((eval e1 s), (eval e2 s))
	| Or(e1, e2) -> bool_or((eval e1 s), (eval e2 s)) 
	| And(e1, e2) -> bool_and((eval e1 s), (eval e2 s)) 
	| Not(e1) -> bool_not(eval e1 s)
	| Ifthenelse(e1, e2, e3) -> 
		let g = eval e1 s in
			(
				match (typecheck("bool", g), g) with
					| (true, Bool(true)) -> eval e2 s
					| (true, Bool(false)) -> eval e3 s
					| (_, _) -> failwith("Error : non boolean guard")
			)
	| Den(i) -> lookup s i
	| DenArg(f, arg) -> Unbound
	| Let(i, e, ebody) -> eval ebody (bind s i (eval e s)) 
	| Letrec(f, arg, fbody, letbody) -> 
		let env1 = bind s f (RecClosure(f, arg, fbody, s)) in 
			eval fbody env1
	| Apply(Den(f), eArg) -> let fclosure = lookup s f in 
		(
			match fclosure with 
				| Closure(arg, fbody, fDecEnv) -> 
					let aVal = eval eArg s in 
						let aenv = bind fDecEnv arg aVal in 
							eval fbody aenv 
				| RecClosure(f, arg, fbody, fDecEnv) -> 
					let aVal = eval eArg s in 
						let rEnv = bind fDecEnv f fclosure in 
							let aenv = bind rEnv arg aVal in 	
								eval fbody aenv 
				| _ -> failwith("Error : non functional value")
		)
	(* espansione di una tupla Etup *)							 	
	| Etup(Nil) -> TupleVal(Nil)
	| Etup(Seq(a, b)) -> TupleVal(tupleToSeq(Seq(a, b)))
	| Etup(_) -> failwith("Error : Illegal argument for Etup")

	(* espansione e valutazione di una Pipe *)
	| Pipe(Nil) -> TupleVal(Nil)
	| Pipe(Seq(a, b)) -> eval (seqToApply(tupleToSeq(Seq(a, b)))) s
	| Pipe(_) -> failwith("Error : Illegal argument for Pipe")

	(* Esecuzione di una funzione f con parametro arg, num volte consecutive*)
	| ManyTimes(num, DenArg(f, arg)) -> 
		if num > 0 then eval (seqToApply(tupleToSeq(Seq(ManyTimes(num, Den(f)), arg)))) s
						else failwith("Error: Illegal argument for ManyTimes (num < 1)")
	| ManyTimes(_, _) -> failwith("Error: Illegal argument for ManyTimes")
	| Apply(_,_) -> failwith("Error : Not first order function")
;;
