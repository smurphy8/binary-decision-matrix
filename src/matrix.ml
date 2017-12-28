(**



This module is designed to allow a set of items to be ranked by pairwise comparisons where a winner is selected.
At the end of the process each item in the set is scored.  The system is designed so you don't have to double select a choice.

Transitive ordering is assumed... 
if choose (A|B) = B and choose (B|C) = C then choose (A|C) = C

EX:

Given a set of items {A,B,C,D,E}

First Iteration asks user to choose A vs B,C,D,E ...

choose (A|B) => B; 
choose (A|C) => C;
choose (A|D) => A;
choose (A|E) => E;

Giving the results.
A < (B & C & E) 
A > D

Second Iteration asks user to choose B vs C  and E 
(A and D are excluded because they both must be < B)

choose (B|C) => B
choose (B|E) => E

B < E 
B > (A & C & D)

C < (B & E)

C > ( A & C & D) 

D < (A & B & C & D)

So the ranking is:

Order:   E > B > C > A > D
Score:   4   3   2   1   0


Only 2 items were compared to specify the rankings.

 *)



type symbol   = Symbol   of string


type variable = Variable of string

let varX : variable = Variable "x"                          

                          

type 'a relation = {name:string;
                    args:'a list;
                     sort:'a  }





                        
type expression = Symbol of symbol
                | Relation of expression relation 
                | FreeVariable of variable
                | BoundVariable of variable * expression                 








(** Language Example *)

let symbolA : symbol = Symbol "A"
let symbolB : symbol = Symbol "B"
let symbolC : symbol = Symbol "B"
let symbolD : symbol = Symbol "D"





let expA : expression = Symbol symbolA
let expB : expression = Symbol symbolB
let expC : expression = Symbol symbolC
let expD : expression = Symbol symbolD



(*
If relPreferredAtoB t2 t1
   relPreferredAtoB t3 t2
Then
   relPreferredAtoB t3 t1

 *)                      

let relPreferredAtoB a b = Relation  { name = "relPreferredAtoB"; 
                           args = [a;
                                   b];
                           sort =  a }

let assign str t = BoundVariable ((Variable str), t) 


(* Set of Facts *) 
let facts             = [ assign "x1" expA;
                          assign "x2" expB;
                          assign "x3" expC;
                          assign "x4" expD;]
            
  
  
            
type rankable = { name : string ; score: int} 


                  
