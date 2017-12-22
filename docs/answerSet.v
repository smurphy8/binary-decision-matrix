(* 

A description of the Algorithm in Coq

 *)

Require Import Coq.Strings.String.
Require Import Coq.Lists.List.


Inductive name : Set :=
  | Name : string -> name.

Inductive term : Set :=
  | termA  : term
  | termB : term
  | termC : term
  | termD : term.


Inductive expr : Set :=
 | primE : nat -> expr
 | varE : name -> expr.


Inductive tristate : Set :=
| triTrue : tristate
| triFalse : tristate
| triUndefined : tristate.

Definition relation := term -> term -> tristate.

Definition isDefined : tristate -> bool :=
    fix isDefined t := 
    match t  with
    | triTrue => true
    | triFalse => true
    | triUndefined => false
    end.

(* a term is complete when given a list of terms a relation is defined pairwise for 
it and every other element in the list *)
  
Definition complete : relation ->  term -> list term -> bool :=
  fix complete r t lt :=
  
  match  lt with
  | nil => false
  | l :: nil  => isDefined (r t l)
  | l :: ls   => if isDefined (r t l)
                 then complete r t ls
                 else false
  end.
      


Definition checkComplete : relation -> list term -> bool :=
  fix checkComplete r l :=
    match l with
    | nil => false
    | l :: nil => false
    | a :: b :: nil => complete r a (b :: nil)
    | a :: cs  => if complete r a cs
                  then  checkComplete r cs
                  else  false
                                      
    end.

(* 
The algorithm should generate an "answer set" 
that is a set of terms that  have a match under the given relation.

*)
Record answers := mkAnswers { relationA : relation ;
                              terms    : list term;
                              answers_set_complete : true = checkComplete relationA terms 

                              }.


        








