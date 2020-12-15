type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop;;

let opval = function
  Not -> not;;

let biopval = function
    Or -> (||)
  | And -> (&&)
  | If -> fun p q -> (not p) || q
  | Iff -> (=);;

let rec peval ctx = function
    C b -> b
  | V s -> List.assoc s ctx
  | Op (op, p) -> (opval op) (peval ctx p)
  | BiOp (biop, p1, p2) -> (biopval biop) (peval ctx p1) (peval ctx p2);;

let rec vars = function
    C _ -> []
  | V s -> [s]
  | Op (_, p) -> vars p
  | BiOp (_, p1, p2) -> vars p1 @ vars p2;;

let rec remove_dups = function
  [] -> []
  | h::t ->
    if List.mem h t
      then remove_dups t
      else h :: (remove_dups t);;

let pvars p =
  remove_dups (vars p);;

let rec ctxs = function
    [] -> [[]]
  | h::t ->
      let cs = ctxs t in
      (List.map (function c -> (h,true)::c) cs) @
      (List.map (function c -> (h,false)::c) cs);;

let is_tau p =
  let cs = ctxs (pvars p) in
  List.for_all (function c -> peval c p) cs;;

(* Ejemplos *)
(*   (p -> q) <=> (not p or q)   es tautologia   *)
let p1 = BiOp (Iff, BiOp (If, V "p", V "q"), BiOp (Or, Op (Not, V "p"), V "q"));;
(*   ((p -> q) and (q -> r)) -> (p -> r)   es tautologia   *)
let p2 = BiOp (If, BiOp (And, BiOp (If, V "p", V "q"), BiOp (If, V "q", V "r")), BiOp (If, V "p", V "r"));;
(*   ((p -> q) and (not q)) -> (not p)   es el modus tollens y es tautologia   *)
let p3 = BiOp (If, BiOp (And, BiOp (If, V "p", V "q"), Op (Not, V "q")), Op (Not, V "p"));;
(*   (p or q) -> p   no es tautologia   *)
let p4 = BiOp (If, BiOp (Or, V "p", V "q"), V "p");;
(*   (((p or q) -> not c) and ((not n) -> (not p)) and (not q) and (not n)) -> c   no es tautologia *)
(*   ver https://es.wikipedia.org/wiki/%C3%81rbol_sem%C3%A1ntico   *)
let p5 = BiOp (If, BiOp (And, BiOp (If, BiOp (Or, V "p", V "q"), Op (Not, V "c")), BiOp (And, BiOp (If, Op (Not, V "n"), Op (Not, V "p")), BiOp (And, Op (Not, V "q"), Op (Not, V "n")))), V "c");;
(*   not (p and q) <=> not p or not q   es una ley de De Morgan y es tautologia *)
let p6 = BiOp (Iff, Op (Not, BiOp (And, V "p", V "q")), BiOp (Or, Op (Not, V "p"), Op (Not, V "q")));;
(*   not (p or q) <=> not p and not q   es una ley de De Morgan y es tautologia *)
let p7 = BiOp (Iff, Op (Not, BiOp (Or, V "p", V "q")), BiOp (And, Op (Not, V "p"), Op (Not, V "q")));;
