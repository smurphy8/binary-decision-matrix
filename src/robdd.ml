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

    let showName (str, i)  = str ^ "_" ^ (string_of_int i)



    (* produce a fresh name *)               
    let generate oldNames name = match Hashtbl.find_all oldNames name with
        [] -> Hashtbl.add oldNames name 1
       |x::_ -> Hashtbl.replace oldNames name (x+1)




    (* BDD Stuff *)

    (* Data type for BDD *)
              
    type id = Id of name
    type label = Label of name



    (* map over some data *) 
    let mapSome f b  = match b with
      | None  -> b
      | Some a -> Some (f a)

                

    type bddData = { id : id;
                    label: label}
    
    let zeroNode = {id= Id ("zero",0) ; label = Label ("zero",0)}
    let oneNode = {id= Id ("one",1) ; label = Label ("one",1)}
                 

    let compareBddData bddA bddB =  let (Id nameIdA) = bddA.id
                                    and (Label nameLabelA) = bddA.label
                                    and (Id nameIdB) = bddB.id
                                    and (Label nameLabelB) = bddB.label
                                    in (nameIdA == nameIdB) &&
                                       (nameLabelA == nameLabelB) 
                                                                


    type bdd = { node: bddData;
                 zeroChild: (bdd option);
                 oneChild: bdd option  }



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
           
    
           
    (* standard map over data *)
    let rec map f b = { node = f b.node;
                        zeroChild = mapSome (map f) b.zeroChild;
                        oneChild = mapSome (map f) b.oneChild;
                      }


    (* fold with right most bias*)                    
    let rec fold f b bdd  =
      let rsltV = f bdd.node b
      in match bdd.zeroChild with
           None -> rsltV 
         | Some bddZ -> let rsltZ = (fold f rsltV bddZ)
                        in match bdd.oneChild with
                             None -> rsltZ
                           | Some bddO -> fold f rsltZ bddO

                                        
    let mk name z o = {node={id=(Id name);
                             label=(Label name)};
                       zeroChild = z;
                       oneChild = o}

   

                      
    let gvBddNode n =
      let (Label l) = n.label
         in let (Id i) = n.id
            in (showName i) ^ " [label=\"" ^ (showName l) ^ "\"" ^ "];"



    let gvBddNodeEdgeLabel n =
      let (Id i) = n.id
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
             None   -> (let zel = mkBddZeroEdge bddC
                        in match bddC.oneChild with
                             None -> let oel = mkBddOneEdge bddC
                                     and _ = print_string "None"
                                     in (allNodes, EdgeSet.add zel edgesZero , EdgeSet.add oel edgesOne )
                            |Some childBdd ->
                               (allNodes,EdgeSet.add zel edgesZero, EdgeSet.add (bddC.node , childBdd.node) edgesOne) )
            |Some bddZ ->
               let (zNodes,zEdges,oEdges) = showBdd' allNodes (EdgeSet.add (bddC.node, bddZ.node) edgesZero) edgesOne bddZ
               in match bddC.oneChild with
                    None -> let oel = mkBddOneEdge bddC 
                            in (zNodes, zEdges, EdgeSet.add oel oEdges )
                   |Some bddO -> showBdd' zNodes zEdges (EdgeSet.add (bddC.node , bddO.node) oEdges) bddO
             
                           
      in let (nodes,zEdges,oEdges) = showBdd' (NodeSet.empty) (EdgeSet.empty) (EdgeSet.empty) bddP
         in let nodeDefinitions = (NodeSet.fold (fun n str -> gvBddNode n ^ str)  nodes "")
            and zeroDefinitions = (EdgeSet.fold (fun (ezA,ezB) str -> (gvBddNodeEdge "dotted" ezA ezB) ^ str )  zEdges "")
            and oneDefinitions  = (EdgeSet.fold (fun (ezA,ezB) str -> (gvBddNodeEdge "solid" ezA ezB) ^ str)  oEdges "")
            and preamble = "digraph G { size = \"4,4\"; "
            and graphStart = "subgraph Live { "                         
            in  (preamble ^ nodeDefinitions ^ graphStart ^ zeroDefinitions ^ oneDefinitions ^ "}}")


       
    (* 
       The below types and functions are to deal with the business of naming and labeling parts of the BDD
     *)                                        
       
       
    (*  nbdd are bdd's with the concept of a name space 
        this allows simple naming for the boolean operations that require labeling *)
    type nbdd = { name:name;
                  bdd:bdd}

          

    let exBf = let    x4 = mk ("x",4) None None
               in let x3 = mk ("x",3) (Some x4) None
                  in let x2 = mk ("x",2) (Some x4) (Some x3)
                     in mk ("x",1) (Some x2) (Some x3)


    let exSimple = let x3 = mk ("x",3) None None
                   in mk ("x",1) (Some x3) (Some x3)

  end
