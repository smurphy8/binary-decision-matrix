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
                           
                           

    let addName (names:names) (name ,i ) :_    =
      (match Hashtbl.find_all names name with
         [] -> (Hashtbl.add names name i)
        |x::_ -> Hashtbl.replace names name (x + 1))




    let showNames (names:names): string = Hashtbl.fold (fun a b c -> a ^ "." ^ (string_of_int b) ^ " " ^ c ) names ""

    let showName ((str, i): string*int):string = str ^ "_" ^ (string_of_int i)



    (* produce a fresh name *)               
    let generate oldNames name = match Hashtbl.find_all oldNames name with
        [] -> Hashtbl.add oldNames name 1
       |x::_ -> Hashtbl.replace oldNames name (x+1)



    (* BDD Stuff *)

    (* Data type for BDD *)
              
    type id = Id of name    
    type label = Label of name
    type 'a noption = NSome of 'a | Zero | One


    (* map over some data *) 
    let mapNSome (f:'a -> 'b) (b:'a noption) : 'b noption = (match b with
                                                            | Zero  -> b
                                                            | One   -> b
                                                            | NSome a -> NSome (f a))

    let mapSome (f:'a -> 'b) (b:'a option) : 'b option = (match b with
                                                            | None -> None
                                                            | Some a -> Some (f a))

    

    type bddData = { label: label}
                 
    type idBddData = {idlabel:label;
                      idZeroChild: id;
                      idOneChild: id;
                      
                     }
                   
                      
    let zeroNode = {label = Label ("zero",0)}
    let oneNode = {label = Label ("one",1)}
                

    let compareBddData bddA bddB =  let (Label nameLabelA) = bddA.label
                                    and (Label nameLabelB) = bddB.label
                                    in (nameLabelA == nameLabelB) 
                                     


    type bdd = { node: bddData;
                 id: (id option);
                 zeroChild: (bdd noption);
                 oneChild: bdd noption  }

    module IdMap   = Map.Make (
                         struct
                           type t = idBddData
                           let compare = Pervasives.compare
                         end
                       )

    module NodeSet = Set.Make(
                         struct
                           let compare = Pervasives.compare
                           type t = bddData
                         end)

    module EdgeSet = Set.Make(
                         struct
                           let compare = Pervasives.compare
                           type t = bddData*bddData
                         end )
                   
                   
    (* structure to hold a list of labels with id and the latest id used *)                  
    type bddIdRecord = { idMap: int IdMap.t;
                         idInt: int}
                   
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

    let mkBddZeroEdge bdd = (bdd.node, zeroNode)                            
    let gvBddZeroEdge bdd = (gvBddNodeEdgeLabel bdd) ^ " -> " ^ "0 [style=dotted];"

    let mkBddOneEdge bdd = (bdd.node, oneNode)
    let gvBddOneEdge bdd = (gvBddNodeEdgeLabel bdd) ^ " -> " ^ "1 [style=solid];"


                         
    let showBdd bddP =
      let rec showBdd' nodes edgesZero edgesOne bddC = 
        let allNodes = NodeSet.add bddC.node nodes             
        in match bddC.zeroChild with
             Zero   -> (let zel = mkBddZeroEdge bddC
                        in match bddC.oneChild with
                             One -> (let oel = mkBddOneEdge bddC
                                     and _ = print_string "One"
                                     in (allNodes, EdgeSet.add zel edgesZero , EdgeSet.add oel edgesOne ))
                            |Zero -> (let oel = mkBddZeroEdge bddC
                                      and _ = print_string "Zero"
                                      in (allNodes, EdgeSet.add zel edgesZero , EdgeSet.add oel edgesOne ))       
                            |NSome childBdd ->
                              (allNodes,EdgeSet.add zel edgesZero, EdgeSet.add (bddC.node , childBdd.node) edgesOne) )
            |One   -> (let zel = mkBddOneEdge bddC
                       in match bddC.oneChild with
                            One -> (let oel = mkBddOneEdge bddC
                                    and _ = print_string "One"
                                    in (allNodes, EdgeSet.add zel edgesZero , EdgeSet.add oel edgesOne ))
                           |Zero -> (let oel = mkBddZeroEdge bddC
                                     and _ = print_string "Zero"
                                     in (allNodes, EdgeSet.add zel edgesZero , EdgeSet.add oel edgesOne ))      
                           |NSome childBdd ->
                             (allNodes,EdgeSet.add zel edgesZero, EdgeSet.add (bddC.node , childBdd.node) edgesOne) )         
            |NSome bddZ ->
              let (zNodes,zEdges,oEdges) = showBdd' allNodes (EdgeSet.add (bddC.node, bddZ.node) edgesZero) edgesOne bddZ
              in match bddC.oneChild with
                   One -> (let oel = mkBddOneEdge bddC 
                           in (zNodes, zEdges, EdgeSet.add oel oEdges ))
                 | Zero -> (let oel = mkBddOneEdge bddC 
                            in (zNodes, zEdges, EdgeSet.add oel oEdges ))
                 |NSome bddO -> showBdd' zNodes zEdges (EdgeSet.add (bddC.node , bddO.node) oEdges) bddO
                              
                              
      in let (nodes,zEdges,oEdges) = showBdd' (NodeSet.empty) (EdgeSet.empty) (EdgeSet.empty) bddP
         in let nodeDefinitions = (NodeSet.fold (fun n str -> gvBddNode n ^ str)  nodes "")
            and zeroDefinitions = (EdgeSet.fold (fun (ezA,ezB) str -> (gvBddNodeEdge "dotted" ezA ezB) ^ str )  zEdges "")
            and oneDefinitions  = (EdgeSet.fold (fun (ezA,ezB) str -> (gvBddNodeEdge "solid" ezA ezB) ^ str)  oEdges "")
            and preamble = "digraph G { size = \"4,4\"; "
            and graphStart = "subgraph Live { "                         
            in  (preamble ^ nodeDefinitions ^ graphStart ^ zeroDefinitions ^ oneDefinitions ^ "}}")




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

    let is a = match a with
        None -> false
      | Some _ -> true

    (*    Generate an ID for One and Zero nodes *) 
    let getOptId (bddOpt : bdd noption)  : id option = match bddOpt with
        One -> Some (Id ("#",1))
      | Zero -> Some (Id ("#",0))
      | NSome bdd -> bdd.id 

    (* Transform the given bdd to one with the id provided *) 
    let reindex (bdd : bdd)  newindex = {node      = bdd.node;
                                         id        = Some newindex;
                                         zeroChild = bdd.zeroChild;
                                         oneChild  = bdd.oneChild}
                                      
    (* Transform the given bdd to one with the id provided *) 
    let updateChildren (bdd : bdd)  c0 c1 = {node      = bdd.node;
                                             id        = bdd.id;
                                             zeroChild = c0;
                                             oneChild  = c1}
      

    (* construct a idBddData entry from a given bdd and labels *)
    let makeIdBddData (bdd:bdd) (lbl0:id)  (lbl1:id) : idBddData = {idlabel= bdd.node.label;
                                       idZeroChild = lbl0;
                                       idOneChild  = lbl1; }



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
   
                                        
                                        
    let label bdd =
      let rec label' indexTracker bdd' = ( let zeroChild = bdd'.zeroChild;
                                           and oneChild  = bdd'.oneChild;
                                           in let optOneChildLabel = (getOptId zeroChild)
                                              and optZeroChildLabel = (getOptId oneChild)
                                              in ( match (optOneChildLabel, optZeroChildLabel) with
                                                     (Some lbl1, Some lbl0) when (lbl1 == lbl0) -> reindex bdd' lbl1
                                                   | (Some lbl1, Some lbl0) -> bdd'
                                                   | (None, Some _lbl0) ->
                                                      let indexedOneChild  =  mapNSome (label' indexTracker) oneChild
                                                      in  label' indexTracker (updateChildren bdd' zeroChild indexedOneChild)
                                                   | (Some _lbl1, None) ->
                                                      let indexedZeroChild =  mapNSome (label' indexTracker) zeroChild
                                                      in  label' indexTracker (updateChildren bdd' indexedZeroChild oneChild) 
                                                   | (None,None) ->
                                                      let indexedZeroChild =  mapNSome (label' indexTracker) zeroChild
                                                      and indexedOneChild  =  mapNSome (label' indexTracker) oneChild
                                                      in  label' indexTracker (updateChildren bdd' indexedZeroChild indexedOneChild) )) 
      in label' { idMap=(IdMap.empty) ; idInt=1 } bdd
          

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
     * let rec apply (noptBddA : bdd noption)  (noptBddB : bdd noption) op  =
     *   (match (noptBddA,noptBddB) with
     *      (Zero,Zero) -> applyArgument Zero Zero op
     *    | (Zero,One)  -> applyArgument Zero One  op
     *    | (One ,Zero) -> applyArgument One  Zero op                      
     *    | (One ,One ) -> applyArgument One  One  op
     *                   
     *    | (One ,NSome rg) -> NSome ( {node = rg.node;
     *                                  id = rg.id;
     *                                  zeroChild = apply One rg.zeroChild op;
     *                                  oneChild  = apply One rg.oneChild op })
     *                       
     *    | (Zero ,NSome rg) -> NSome ( {node = rg.node;
     *                                   id=rg.id;
     *                                   zeroChild = apply Zero rg.zeroChild op;
     *                                   oneChild  = apply Zero rg.oneChild op })
     *                        
     *    | (NSome rf, NSome rg) when (rf.node == rg.node) ->
     *       (NSome ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild rg.zeroChild op);
     *                oneChild  = (apply rf.oneChild  rg.oneChild op)}))
     * 
     *    | (NSome rf, NSome rg) when (rf.node != rg.node) ->
     *       (NSome ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild rg.zeroChild op);
     *                oneChild  = (apply rf.oneChild  rg.oneChild op)}))
     * 
     *      
     *    | (NSome rf, Zero) ->
     *       (NSome ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild Zero op);
     *                oneChild  = (apply rf.oneChild  Zero op)}))
     *    | (NSome rf, One) ->
     *       (NSome ({node = rf.node;
     *                id = rf.id;
     *                zeroChild = (apply rf.zeroChild One op);
     *                oneChild  = (apply rf.oneChild  One op)}))
     *    | (NSome _, NSome _) -> failwith "guarded condition should have caught apply"
     *   ) *)



      
      
    (* 
       The below types and functions are to deal with the business of naming and labeling parts of the BDD
     *)                                        
      
      
    (*  nbdd are bdd's with the concept of a name space 
        this allows simple naming for the boolean operations that require labeling *)
              
              
    (* Example graphs for testing *)            

    (* let exBf = let    x4 = mk ("x",4) Zero One
     *            in let x3 = mk ("x",3) (NSome x4) One
     *               in let x2 = mk ("x",2) (NSome x4) (NSome x3)
     *                  in mk ("x",1) (NSome x2) (NSome x3) *)

    (* let exBg = let    x4 = mk ( "x",4) Zero One
     *            in let x3 = mk ( "x",3) (NSome x4) One
     *               in  mk ( "x",1) (NSome x4) (NSome x3)
     * 
     * 
     * 
     *                 
     * let exBgBf = apply (NSome exBf) (NSome exBg) (||) *)

    (* let exOneNode = mk ("x",1) Zero One *)
                  
    (* let prop_reflexive_apply : bdd noption = (apply One (NSome exOneNode) (||) )
     * let show_prop = match prop_reflexive_apply  with
     *     One -> "one"
     *   | Zero -> "Zero"
     *   | (NSome rg) -> showBdd rg
     * 
     *                 
     * let exSimple = let x3 = mk ("x",3) Zero One
     *                in mk ("x",1) (NSome x3) (NSome x3) *)

  end
