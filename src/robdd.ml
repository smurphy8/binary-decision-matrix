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

    (* BDD Stuff *)

    (* Data type for BDD *)
              
    type id = Id of name    
    type label = Label of name
                           



    type bddData = { label: label}
                 
    type idBddData = {idlabel:label;
                      idZeroChild: id;
                      idOneChild: id;
                      
                     }
                   


    (* 
       The bdd type holds the structure of the graph
       id's represent the automated naming achieved
       by running 'labelIt' against a given graph
     *)
    type bdd = { node: bddData;
                 id: (id option);
                 zeroChild: (bdd option);
                 oneChild: bdd option  }
    (* This map holds bound names for Ids*)
    module IdMap   = Map.Make (
                         struct
                           type t = idBddData
                           let compare = Pervasives.compare
                         end
                       )



                   
    (* structure to hold a list of labels with id and the latest id used *)                  
    type bddIdRecord = { idMap: int IdMap.t;
                         idInt: int}


               

    let showName ((str, i): string*int):string = str ^ "_" ^ (string_of_int i)





    (* join for 'Some' Monad *)
                                               
    let joinSome (ooa : ((id option) option)) : (id option) = match ooa with
      | None  -> None
      |Some None -> None
      |Some oa -> oa

    (* map over some data *) 
    let mapSome (f:'a -> 'b) (b:'a option) : 'b option = (match b with
                                                            | None -> None
                                                            | Some a -> Some (f a))



                      

    
                   
    let mk name z o = {node={label=(Label name)};
                       id = None;
                       zeroChild = z;
                       oneChild = o}

                    
                    
                    
    let gvBddNode n =
      let (Label l) = n.label
      in (showName l) ^ " [label=\"" ^ (showName l) ^ "\"" ^ "];"



    let gvBddNodeEdgeLabel n =
      let (Label i) = n.label
      in (showName i) 

    let gvBddNodeEdge style n1 n2 = (gvBddNodeEdgeLabel n1) ^ " -> " ^ (gvBddNodeEdgeLabel n2) ^ " [style="^style^"];"





    (*
 If the label id(lo(n)) is the same as id(hi(n)), then we set id(n) to be that label.
That is because the boolean function represented at n is the same function as the
one represented at lo(n) and hi(n). In other words, node n performs a redundant
test and can be eliminated by reduction C2.
r If there is another node m such that n and m have the same variable x , and
i
id(lo(n)) = id(lo(m)) and id(hi(n)) = id(hi(m)), then we set id(n) to be id(m).
This is because the nodes n and m compute the same boolean function (compare
with reduction C3).
r Otherwise, we set id(n) to the next unused integer label.
     *)              


    (* Id for the bottom "0" and "1" leaves respectively *)
    let zeroId = (Id ("0",0))
    let oneId = (Id ("1",1))

    (* a bdd with just zero node *)                       
    let zeroBdd = { node = {label = Label ("0",0)};
                    id = Some zeroId;
                    zeroChild = None;
                    oneChild = None}     


    let oneBdd = { node = {label = Label ("1",1)};
                   id = Some oneId;
                   zeroChild = None;
                   oneChild = None}     



    (* Transform the given bdd to one with the id provided *) 
    let reindex (bdd : bdd)  newindex = {node      = bdd.node;
                                         id        = Some newindex;
                                         zeroChild = bdd.zeroChild;
                                         oneChild  = bdd.oneChild}
                                      
    (* Transform the given bdd to one with new children *) 
    let updateChildren (bdd : bdd)  c0 c1 = {bdd with zeroChild = c0;
                                                      oneChild  = c1;}
      
    let updateMap (bddIdRecord : bddIdRecord) (m ) = {bddIdRecord with idMap = m}
                                                                                  
    (* construct a idBddData entry from a given bdd and labels *)
    let makeIdBddData (bdd:bdd) (lbl0:id)  (lbl1:id) : idBddData = {idlabel= bdd.node.label;
                                                                    idZeroChild = lbl0;
                                                                    idOneChild  = lbl1; }

    let fromOptional d ma = match ma with
      |(Some a) -> a
      | None -> d

    (* This function returns the value that should be next assigned to a labeling scheme.
       If the value is already found in the map, then return this value.
       Otherwise get a fresh integer, update the records and return  *)
      
    let getIndexToAssign (idRecord:bddIdRecord) (idData:idBddData) : bddIdRecord * (string * int) = if (IdMap.mem idData idRecord.idMap )
                                                                                                    then (idRecord, ("#",(IdMap.find  idData idRecord.idMap)))
                                                                                                    else let updatedInteger = idRecord.idInt + 1
                                                                                                         in  let updatedMap = IdMap.add idData
                                                                                                                                updatedInteger
                                                                                                                                idRecord.idMap                                 
                                                                                                             in ( {idMap = updatedMap; idInt= updatedInteger}
                                                                                                                , ("#", updatedInteger))

    (* Transform a bdd node into an idbdd node for comparison. *)
    (* idBddData *)   
    (* bddIdRecord *)
       
                                        
                                        
    let labelIt (bdd:bdd) : bdd =  
      let rec label' (indexTracker : bddIdRecord ref) (bdd':bdd) : bdd  =
        ( let zeroChild = bdd'.zeroChild;
          and oneChild  = bdd'.oneChild;
          in let optOneChildIndex = (mapSome (fun bddChild -> bddChild.id) oneChild)
             and optZeroChildIndex = (mapSome (fun bddChild -> bddChild.id) zeroChild)
             and jid (a : ((id option) option)) : (id option) = (joinSome a)                                                 
             in ( match (jid optOneChildIndex, joinSome optZeroChildIndex) with
                    (Some idx1, Some idx0) when (idx1 == idx0) -> let _ = print_string "reindex_line"
                                                                  in fromOptional bdd oneChild
                  | (Some idx1, Some idx0) ->
                     let nodeIdData = makeIdBddData bdd' idx0 idx1
                     and _ = print_string "idx1 idx0 line"
                     in let (newTracker, newIdVal) = getIndexToAssign !indexTracker nodeIdData                      
                        in let _ = indexTracker := newTracker
                           in (reindex bdd' (Id newIdVal))
                  | (None, Some _lbl0) ->
                     let indexedOneChild  =  mapSome (label' indexTracker) oneChild
                     and _ = print_string "None idx0 line"
                     in  label' indexTracker (updateChildren bdd' zeroChild indexedOneChild)
                  | (Some _idx1, None) ->
                     let indexedZeroChild =  mapSome (label' indexTracker) zeroChild
                     and _ = print_string "idx1 None line"
                     in  label' indexTracker (updateChildren bdd' indexedZeroChild oneChild) 
                  | (None,None) ->
                     let indexedZeroChild =  mapSome (label' indexTracker) zeroChild
                     and indexedOneChild  =  mapSome (label' indexTracker) oneChild
                     and _ = print_string "None None line"
                     in label' indexTracker (updateChildren bdd' indexedZeroChild indexedOneChild) )) 
      in let _ = print_string "start label 2"
         in  label' (ref { idMap=(IdMap.empty) ; idInt=1 }) bdd 
          

    (* 

The reductions C1–C3 are at the core of any serious use of OBDDs, for
whenever we construct a BDD we will want to convert it to its reduced form.
In this section, we describe an algorithm reduce which does this efficiently
for ordered BDDs.
If the ordering of B is [x 1 , x 2 , . . . , x l ], then B has at most l + 1 layers. The
algorithm reduce now traverses B layer by layer in a bottom-up fashion,
beginning with the terminal nodes. In traversing B, it assigns an integer
label id(n) to each node n of B, in such a way that the subOBDDs with
root nodes n and m denote the same boolean function if, and only if, id(n)
equals id(m).
Since reduce starts with the layer of terminal nodes, it assigns the first
label (say #0) to the first 0-node it encounters. All other terminal 0-nodes
denote the same function as the first 0-node and therefore get the same label
(compare with reduction C1). Similarly, the 1-nodes all get the next label,
say #1.
Now let us inductively assume that reduce has already assigned integer
labels to all nodes of a layer > i (i.e. all terminal nodes and x j -nodes with
j > i). We describe how nodes of layer i (i.e. x i -nodes) are being handled.

     *)



                       
                       
    (* apply a binary function in two arguments to a bdd *) 
    (* From Logic in Computer Science
Given two diagrams B_f and B_g considering nodes r_f and r_g
(0|1) (0|1) case
1. If both r f and r g are terminal nodes with labels l f and l g , respectively (recall
   that terminal labels are either 0 or 1), then we compute the value l f op l g and
   let the resulting OBDD be B 0 if that value is 0 and B 1 otherwise.

X_i X_i case
2. In the remaining cases, at least one of the root nodes is a non-terminal. Suppose
   that both root nodes are x i -nodes. Then we create an x i -node n with a dashed
   line to apply (op, lo(r f ), lo(r g )) and a solid line to apply (op, hi(r f ), hi(r g )), i.e.
   we call apply recursively on the basis of (6.2).

X_i (X_j : j>i | 0 | 1) case
3. If r f is an x i -node, but r g is a terminal node or an x j -node with j > i,
   then we know that there is no x i -node in B g because the two OBDDs have
   a compatible ordering of boolean variables. Thus, g is independent of x i
   (g ≡ g[0/x i ] ≡ g[1/x i ]). Therefore, we create an x i -node n with a dashed line
   to apply (op, lo(r f ), r g ) and a solid line to apply (op, hi(r f ), r g ).

X_j (X_i : i>j | 0 | 1) case
4. The case in which r g is a non-terminal, but r f is a terminal or an x j -node with
   j > i, is handled symmetrically to case 3.

     *)





                         
    (* Unlike apply above, this is for terminal application only 
       So the behavior in the case that it is applied to a non-terminal node is 
       arbitrary *) 
    (* let applyArgument noptBddA noptBddB op =
     *   (match (noptBddA,noptBddB) with
     *      (Zero, Zero) -> boolAsOptional (op false false)
     *    | (Zero, One)  -> boolAsOptional (op false true)
     *    | (One , Zero) -> boolAsOptional (op true false)
     *    | (One ,One)   -> boolAsOptional (op true true)                               
     *    | (a,_) -> a )
     *   
     * 
     * 
     *   
     * let rec apply (noptBddA : bdd option)  (noptBddB : bdd option) op  =
     *   (match (noptBddA,noptBddB) with
     *      (Zero,Zero) -> applyArgument Zero Zero op
     *    | (Zero,One)  -> applyArgument Zero One  op
     *    | (One ,Zero) -> applyArgument One  Zero op                      
     *    | (One ,One ) -> applyArgument One  One  op
     *                   
     *    | (One ,Some rg) -> Some ( {node = rg.node;
     *                                  id = rg.id;
     *                                  zeroChild = apply One rg.zeroChild op;
     *                                  oneChild  = apply One rg.oneChild op })
     *                       
     *    | (Zero ,Some rg) -> Some ( {node = rg.node;
     *                                   id=rg.id;
     *                                   zeroChild = apply Zero rg.zeroChild op;
     *                                   oneChild  = apply Zero rg.oneChild op })
     *                        
     *    | (Some rf, Some rg) when (rf.node == rg.node) ->
     *       (Some ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild rg.zeroChild op);
     *                oneChild  = (apply rf.oneChild  rg.oneChild op)}))
     * 
     *    | (Some rf, Some rg) when (rf.node != rg.node) ->
     *       (Some ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild rg.zeroChild op);
     *                oneChild  = (apply rf.oneChild  rg.oneChild op)}))
     * 
     *      
     *    | (Some rf, Zero) ->
     *       (Some ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild Zero op);
     *                oneChild  = (apply rf.oneChild  Zero op)}))
     *    | (Some rf, One) ->
     *       (Some ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild One op);
     *                oneChild  = (apply rf.oneChild  One op)}))
     *    | (Some _, Some _) -> failwith "guarded condition should have caught apply"
     *   ) *)



      
      
    (* 
       The below types and functions are to deal with the business of naming and labeling parts of the BDD
     *)                                        
      
      
    (*  nbdd are bdd's with the concept of a name space 
        this allows simple naming for the boolean operations that require labeling *)
              
              
    (* Example graphs for testing *)            

    (* let exBf = let    x4 = mk ("x",4) Zero One
     *            in let x3 = mk ("x",3) (Some x4) One
     *               in let x2 = mk ("x",2) (Some x4) (Some x3)
     *                  in mk ("x",1) (Some x2) (Some x3) *)

    (* let exBg = let    x4 = mk ( "x",4) Zero One
     *            in let x3 = mk ( "x",3) (Some x4) One
     *               in  mk ( "x",1) (Some x4) (Some x3)
     * 
     * 
     * 
     *                 
     * let exBgBf = apply (Some exBf) (Some exBg) (||) *)

    (* let exOneNode = mk ("x",1) Zero One *)
                  
    (* let prop_reflexive_apply : bdd option = (apply One (Some exOneNode) (||) )
     * let show_prop = match prop_reflexive_apply  with
     *     One -> "one"
     *   | Zero -> "Zero"
     *   | (Some rg) -> showBdd rg
     * 
     *)


           
    let exSimple : bdd =  let x3 = mk ("x",3) (Some zeroBdd) (Some zeroBdd)
                    and x2 = mk ("x",2) (Some zeroBdd) (Some oneBdd)
                    in  mk ("x",4) (Some x2) (Some x3)
                   



(* 
  # labelIt exSimple;;
start label 2idx1 idx0 lineidx1 idx0 lineNone None lineidx1 idx0 line- : bdd =
{node = {label = Label ("x", 4)}; id = Some (Id ("#", 4));
 zeroChild =
  Some
   {node = {label = Label ("x", 2)}; id = Some (Id ("#", 2));
    zeroChild =
     Some
      {node = {label = Label ("0", 0)}; id = Some (Id ("0", 0));
       zeroChild = None; oneChild = None};
    oneChild =
     Some
      {node = {label = Label ("1", 1)}; id = Some (Id ("1", 1));
       zeroChild = None; oneChild = None}};
 oneChild =
  Some
   {node = {label = Label ("x", 3)}; id = Some (Id ("#", 3));
    zeroChild =
     Some
      {node = {label = Label ("0", 0)}; id = Some (Id ("0", 0));
       zeroChild = None; oneChild = None};
    oneChild =
     Some
      {node = {label = Label ("1", 1)}; id = Some (Id ("1", 1));
       zeroChild = None; oneChild = None}}}

*)

  end
