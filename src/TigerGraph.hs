module TigerGraph where

import           Assem
import qualified Data.List as L
import           Data.Set  as S
import           Prelude   hiding (pred, succ)

data Node a =
  Node Int a

type Edge a = (Node a, Node a)

instance Show a => Show (Node a) where
  show (Node i a) = "Node " ++ show i

instance Eq (Node a) where
  (Node i _) == (Node j _) = i == j

instance Ord (Node a) where
  (Node i _) <= (Node j _) = i <= j

data Graph a =
  Graph
    { nodes :: (Set (Node a))
    , edges :: (Set (Edge a))
    }
  deriving (Show)

succ :: Node a -> Graph a -> Set (Node a)
succ vertex graph =
  S.foldr
    (\(src, dst) accum ->
       if src == vertex
         then insert dst accum
         else accum)
    empty
    (edges graph)

pred :: Node a -> Graph a -> Set (Node a)
pred vertex graph =
  S.foldr
    (\(src, dst) accum ->
       if dst == vertex
         then insert src accum
         else accum)
    empty
    (edges graph)

adj :: Node a -> Graph a -> Set (Node a)
adj vertex graph = union (pred vertex graph) (succ vertex graph)

-- Agrega nodos al grafo. Devuelve el grafo actualizado y una lista con
-- los nodos agregados (en orden)
addNodes :: [a] -> (Graph a, [Node a]) -> (Graph a, [Node a])
addNodes [] (graph, insertedNodes) = (graph, reverse insertedNodes)
addNodes (inst:instrs) (graph, insertedNodes) =
  addNodes instrs (newGraph, newNode : insertedNodes)
  where
    newNode = mkNode inst graph
    newGraph = addNode inst graph

addEdges :: [(Node a, Node a)] -> Graph a -> Graph a
addEdges [] graph = graph
addEdges ((a, b):newEdges) graph =
  mkEdge a b (addEdges newEdges graph)

addNode :: a -> Graph a -> Graph a
addNode item graph =
  graph {nodes = insert (Node (index + 1) item) (nodes graph)}
  where
    index = S.foldr (\(Node i _) accum -> max i accum) 0 (nodes graph)

mkNode :: a -> Graph a -> Node a
mkNode item graph = Node (index + 1) item
  where
    index = S.foldr (\(Node i _) accum -> max i accum) 0 (nodes graph)

mkEdge :: Node a -> Node a -> Graph a -> Graph a
mkEdge src dst graph = graph {edges = insert (src, dst) (edges graph)}

rmEdge :: Node a -> Node a -> Graph a -> Graph a
rmEdge src dst graph = graph {edges = delete (src, dst) (edges graph)}

emptyGraph :: Graph a
emptyGraph = Graph empty empty

findNodeByContent :: Eq a => a -> Graph a -> Node a
findNodeByContent searchedElem graph =
  let allNodes = elems $ nodes graph

      searchContent :: Eq a => a -> [Node a] -> Node a
      searchContent _ [] = error "[findNodeByContent] "
      searchContent search (foundNode@(Node _ content):rest) =
        if search == content
          then foundNode
          else searchContent search rest

   in searchContent searchedElem allNodes
