module Maze where

import Geometry
import Search
import Data.Set (Set)
import qualified Data.Set as Set

-- a maze has a set of open locations, a start point and a goal
type Maze = (Set Point, Point, Point)

-- interpret a multiline string as a maze
readMaze :: String -> Maze
readMaze s =
    (Set.fromList (map fst pcs),
     head [p | (p, c) <- pcs, c == '@'],
     head [p | (p, c) <- pcs, c == '*'])
  where
    pcs = [(p, c) | (p, c) <- readGrid s, c /= '#']

-- points reached in each step from the start to the goal
solve :: Maze -> [Set Point]
solve (open, start, finish) =
    takeWhile (\ s -> not (Set.member finish s)) $
    bfs (moves open) start

-- possible moves in the set from the point
moves :: Set Point -> Graph Point
moves open p = Set.intersection open (neighbours p)

-- neighbours of a point in each compass direction
neighbours :: Point -> Set Point
neighbours p =
    Set.fromList [plusPoint p (oneStep d) |
        d <- [North, East, South, West]]

-- some test mazes

smallTest :: String
smallTest = "\
    \#########\n\
    \#.#...*.#\n\
    \#.#.#####\n\
    \#.....#.#\n\
    \#.###.#.#\n\
    \#...@...#\n\
    \#########\n"

largeTest :: String
largeTest = "\
    \#############################################################\n\
    \#@....#.#.#.#.....#.#.#.#.......#...............#...........#\n\
    \#.#####.#.#.###.#.#.#.#.#####.###.#.#####################.###\n\
    \#.#...#.......#.#.#...#.....#...#.#.....#.....#.....#.......#\n\
    \#.###.###.#.#.###.###.###.###.#########.###.###.###.#.#######\n\
    \#.......#.#.#...#...#.......#.....#...........#.#.#...#.....#\n\
    \###.###.#.#.###.#.#######.#.###.###.#######.#.###.#.#######.#\n\
    \#.....#.#.#.#...........#.#...........#.#...#.#.......#.#...#\n\
    \#.#.#######.#####.###.#.#####.#.#####.#.#.###.###.###.#.#.#.#\n\
    \#.#...#...#.#...#...#.#.......#.....#.#.#.#...#.#...#.....#.#\n\
    \#.#.###.#.#.###.#.###.###.#####.#####.#.###.###.#.###.#######\n\
    \#.#.....#...#.......#.#.......#.#...#...#...........#.......#\n\
    \###.#.#.###.#######.#####.#####.#.#####.#####.#####.###.#####\n\
    \#.#.#.#.#.#.#.....#...#.#...#.#.....#...#...#...#...#.#.#...#\n\
    \#.###.###.#.###.###.###.#####.#.#########.#.###.#.#.#.#.###.#\n\
    \#...#.....#...#...#.#.#.#...#.#...#...#...#.....#.#.#.......#\n\
    \###.#.###.###.#.###.#.#.#.###.#.###.#######.#.###############\n\
    \#.....#...#.....#.......#...........#.......#...#.........#.#\n\
    \#.###.#.#############.#######.#.###.#####.#####.#.#.#.###.#.#\n\
    \#.#...#.#...................#.#...#.....#...#.....#.#...#..*#\n\
    \#############################################################\n"

hamptonCourtMaze :: String
hamptonCourtMaze = "\
    \#########################################\n\
    \#.............#.....#.......#...........#\n\
    \#.###########.#.#####.#####.#.#########.#\n\
    \#.#...........#.#.........#.#.........#.#\n\
    \#.#.###########.#.#######.#.#########.#.#\n\
    \#.#.#...........#...#.....#.........#.#.#\n\
    \#.#.#.#############.#.#######.#####.#.#.#\n\
    \#.#.#.#...#.........#.......#.#...#.#.#.#\n\
    \#.#.#.#.#.#.###############.#.#.#.#.#.#.#\n\
    \#.#...#.#.#.#.............#.#.#.#.#...#.#\n\
    \#.###.#.#.#.#.............#.#.#.#.#.###.#\n\
    \#...#.#.#.#.#.............#.#.#.#.#.#...#\n\
    \###.#.#.#.#.#.......*.....#.#.#.#.#.#.###\n\
    \#...#.#.#.#.#.............#.#.#.#.#.#...#\n\
    \#.###.#.#.#.#.............#.#.#.#.#.#.#.#\n\
    \#.#...#.#.#.#.............#.#.#.#.#.#.#.#\n\
    \#.#.#.#.#.#.########.######.#.#.#.#.#.#.#\n\
    \#.#.#.#.#.#........#.#......#.#.#...#.#.#\n\
    \#.#.#.#.#.########.#.#.######.#.#####.#.#\n\
    \#.#.#...#........#.#.#..........#.....#.#\n\
    \#.#.##############.#.##################.#\n\
    \#.#................#....................#\n\
    \#.#######################################\n\
    \#.......................................#\n\
    \####################@####################\n"
