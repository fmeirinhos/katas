module ShortestKnightPath.Kata (knight) where

knight :: String -> String -> Int
knight si sf = length (takeWhile (parsePosition sf `notElem`) nodes) where 
  nodes = [parsePosition si] : children
  children = map (concatMap knightMoves) nodes
  
  knightMoves (x1,y1) = filter validBoard [(x1+x2, y1+y2) | (x2,y2) <- jumps] where
    jumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
    validBoard (x,y) = x >= 0 && x <= 7 && y >= 0 && y <=7

  parsePosition s = (fromEnum (head s) - fromEnum 'a', fromEnum (last s) - fromEnum '1')