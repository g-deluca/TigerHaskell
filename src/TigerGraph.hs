module TigerGraph where

import Data.Set as S
import Prelude hiding (pred, succ)

data Node a = Node Int a
type Edge a = (Node a, Node a)

instance Eq (Node a) where
  (Node i _) == (Node j _) = i == j

instance Ord (Node a) where
  (Node i _) <= (Node j _) = i <= j

data Graph a = Graph {
  nodes :: (Set (Node a)),
  edges :: (Set (Edge a))
}

succ :: Node a -> Graph a -> Set (Node a)
succ vertex graph =
  S.foldr
    (\(src, dst) accum -> if src == vertex then insert dst accum else accum)
    empty
    (edges graph)

pred :: Node a -> Graph a ->  Set (Node a)
pred vertex graph =
  S.foldr
  (\(src, dst) accum -> if dst == vertex then insert src accum else accum)
  empty
  (edges graph)

adj :: Node a -> Graph a -> Set (Node a)
adj vertex graph =
  union (pred vertex graph) (succ vertex graph)

mkNode :: a -> Graph a -> Graph a
mkNode item graph = graph { nodes = insert (Node (index+1) item) (nodes graph) }
  where index = S.foldr (\(Node i _) accum -> max i accum) 0 (nodes graph)

mkEdge :: Node a -> Node a -> Graph a -> Graph a
mkEdge src dst graph = graph { edges = insert (src, dst) (edges graph)}

rmEdge :: Node a -> Node a -> Graph a -> Graph a
rmEdge src dst graph = graph { edges = delete (src, dst)  (edges graph)}

emptyGraph :: Graph a
emptyGraph = Graph empty empty