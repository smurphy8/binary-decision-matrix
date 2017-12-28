(*
Author: Scott Murphy
Module: Reduced Ordrered Binary Decision Diagrams
Description: 

A module defining first, ordered binary decision diagrams and second reduced diagrams.
Also defining operations on them.


 *)

module Robdd =
  struct




    (* store a list of names with repitition handled by giving each one its own number
       ("x",i) for instance where
       i.1, i.2, i.3 ... Only the last one is stored, this only provides fresh names  

     *)
    
    type names = (string,int) Hashtbl.t               
    type name  = (string * int)


    let emptyNames : names = Hashtbl.create 0
                           


    let addName names (name,i)  = match Hashtbl.find_all names name with
        [] -> Hashtbl.add names name i
       |x::_ -> Hashtbl.replace names name (x+1)




    let showNames names = Hashtbl.fold (fun a b c -> a ^ "." ^ (string_of_int b) ^ " " ^ c ) names ""





    (* produce a fresh name *)               
    let generate oldNames name = match Hashtbl.find_all oldNames name with
        [] -> Hashtbl.add oldNames name 1
       |x::_ -> Hashtbl.replace oldNames name (x+1)





    (* Data type for BDD *)

    type id = Id of int
    type label = Label of string






    (* map over some data *) 
    let mapSome f b  = match b with
      | None  -> b
      | Some a -> Some (f a)

    type bddData = { id : id;
                     label: label;
                     value: bool;}
                 
    type bdd = { node: bddData;
                 zeroChild: (bdd option);
                 oneChild: bdd option  }

             
    (* standard map over data *)
    let rec map f b = { node = f b.node;
                        zeroChild = mapSome (map f) b.zeroChild;
                        oneChild = mapSome (map f) b.oneChild;
                      }


    (* fold with right most bias*)                    
    let rec fold f b bdd : (bool -> 'a -> 'a) -> 'a -> bdd -> 'a =
      let rsltV = f bdd.node.value b
      in match bdd.zeroChild with
           None -> rsltV 
         | Some bddZ -> let rsltZ = (fold f rsltV bddZ)
                        in match bdd.oneChild with
                             None -> rsltZ
                           | Some bddO -> fold f rsltZ bddO

                                        
                                        


    (* 
       The below types and functions are to deal with the business of naming and labeling parts of the BDD
     *)                                        
                                        
                                        
    (*  nbdd are bdd's with the concept of a name space 
        this allows simple naming for the boolean operations that require labeling *)
    type nbdd = { name:string;
                  bdd:bdd}
              
              

  end
