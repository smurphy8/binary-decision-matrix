(*
Author: Scott Murphy
Module: Reduced Ordrered Binary Decision Diagrams
Description: 

A module defining first, ordered binary decision diagrams and second reduced diagrams.
Also defining operations on them.


 *)



(* Data type for BDD *)

type id = Id of int
type label = Label of string

(* The bdd is more complex because it carries extra tags to make it easier to reduce a binary function *)                    

type bdd = { id   : id;
             label: label;
             value: bool;
             leftChild: bdd;
             rightChild: bdd}
             
