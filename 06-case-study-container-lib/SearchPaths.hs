module Search where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- graph represented as a function from nodes to neighbours
type Graph a = a -> Set a

-- breadth-first search: the set in position i consists of
-- the elements reachable from s in i steps and no fewer.
bfs :: Ord a => Graph a -> a -> [Set a]
bfs f s =
    takeWhile (not . Set.null) $
    map snd $
    iterate (expand2 f) $
    (Set.empty, Set.singleton s)

-- Given a pair of (nodes visited earlier, nodes just visited),
-- expand to nodes reachable from those.
expand2 :: Ord a => Graph a ->
    (Set a, Set a) -> (Set a, Set a)
expand2 f (old, new) =
    (seen, Set.difference (unionAll f new) seen)
  where
    seen = Set.union old new

-- the set of nodes reachable in one step from a set
unionAll :: Ord a => (a -> Set a) -> Set a -> Set a
unionAll f s = Set.unions (map f (Set.elems s))

-- breadth-first search: the map in position i maps nodes to minimal
-- paths of length i.
bfsPaths :: Ord a => Graph a -> a -> [Map a [a]]
bfsPaths f s =
    map (Map.map reverse) $
    takeWhile (not . Map.null) $
    map snd $
    iterate (expandMap f) $
    (Set.empty, Map.singleton s [])

-- Given a pair of (nodes visited earlier, paths to nodes just visited),
-- expand to nodes reachable from those.
expandMap :: Ord a => Graph a ->
    (Set a, Map a [a]) -> (Set a, Map a [a])
expandMap f (old, new) =
    (seen, Map.withoutKeys (unionPaths f new) seen)
  where
    seen = Set.union old (Map.keysSet new)

-- extend paths by one step
unionPaths :: Ord a => Graph a -> Map a [a] -> Map a [a]
unionPaths f s =
    Map.unions [Map.fromSet (:path) (f n) |
        (n, path) <- Map.assocs s]
