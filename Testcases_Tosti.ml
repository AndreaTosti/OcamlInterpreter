(* ANDREA TOSTI - CORSO B - Mat n. 518111 *)
(* Secondo progetto v.1, 15 dicembre 2016 *)

(* Lista di dichiarazioni NECESSARIE allo svolgimento dei testcase *)
(* --------------------------------------------------------------------------------------------------------------------- *)

let bodyFact = Ifthenelse(Eq(Den("n"),CstInt(0)), CstInt(1), Times(Den("n"),Apply(Den("fact"),Sub(Den("n"),CstInt(1))))) ;;
let bodyDouble = Sum(Den("x"), Den("x")) ;;
let bodyTriple = Sum(Sum(Den("x"), Den("x")), Den("x")) ;;
let bodyTimesOfDiffVar = Times(Den("z"), Den("n")) ;;
let bodyPari = Ifthenelse(Eq(Mod(Den("n"), CstInt(2)), CstInt(0)), CstTrue, CstFalse) ;; 
let bodyMultiploDiCinque = Ifthenelse(Eq(Mod(Den("n"), CstInt(5)),CstInt(0)), CstTrue, CstFalse) ;;

(* IntTup(-65535) non verra' mai utilizzato perche' viene ignorato da Pipe e ManyTimes *)
let sequenza3  = Seq(Den("double"),Seq(ManyTimes(2, Den("double")), 
					Seq(Den("triple"), Seq(Etup(Seq(ManyTimes(3, Den("double")), 
						Seq(Pipe(Seq(ManyTimes(2, Den("triple")),IntTup(10))), IntTup(-65535)))), IntTup(-65535))))) ;;

let environment = 
	[
		("double", Closure("x", bodyDouble, emptyEnv)) ;
		("triple", Closure("x", bodyTriple, emptyEnv)) ;
		("fact", RecClosure("fact", "n", bodyFact, emptyEnv)) ;
		("times_of_diff_var", Closure("n", bodyTimesOfDiffVar, [("z", Int(10))] )) ;
		("sePari", Closure("n", bodyPari, emptyEnv)) ;
		("seMultiploDiCinque",Closure("n", bodyMultiploDiCinque, emptyEnv)) ;
	] ;;

(* --------------------------------------------------------------------------------------------------------------------- *)

(* ESEMPI *)

(* Esempio 1 :  Pipe(double(fact(6))) *)
eval (Pipe(Seq(Den("double"),Seq(Den("fact"),IntTup(6) )))) environment ;;

(* Esempio 2 : La funzione times_of_diff_var fa il prodotto tra il parametro passatogli 
				e la variabile che ha nell'ambiente al momento della dichiarazione di funzione *)
eval (Pipe(Seq(Den("times_of_diff_var"),Seq(Den("double"), IntTup(6)))) ) environment ;;

(* Esempio 3 : Pipe e Etup sulla stessa tupla *)

eval (Pipe(sequenza3)) environment ;;
eval (Etup(sequenza3)) environment ;;

(* Esempio 4 : sePari restituisce un bool *)

eval (Pipe(Seq(Den("sePari"), IntTup 9))) environment ;;
eval (Pipe(Seq(Den("sePari"), IntTup 10))) environment ;;

(* Esempio 5 : seMultiploDiCinque(sePari(10)) -> 
				errore di tipo: seMultiploDiCinque non puo' essere
				 applicato a un booleano *)

eval (Pipe(Seq(Den("seMultiploDiCinque"),Seq(Den("sePari"), IntTup(10))))) environment ;;

(* Esempio 6 : ManyTimes(10, double(5)) *)

eval (ManyTimes(2, DenArg("double", IntTup 5))) environment ;; 

(* Esempio 7 : ManyTimes(0, double(5)) -> errore *)

eval (Etup(Seq(ManyTimes(0, Den("double")), IntTup 5) )) environment ;;

(* Esempio 8 : Etup(..Pipe(Nil)) *)

eval (Etup(Seq(Pipe(Nil), IntTup(-65535)))) environment ;;
