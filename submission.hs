data LeafColour = Black | White deriving(Eq,Show)
data QuadTree = Leaf LeafColour | Node QuadTree QuadTree QuadTree QuadTree deriving(Eq, Show)

allBlack :: Int->QuadTree
allBlack 1 = Leaf Black
allBlack n = Node (allBlack (n `div` 2)) (allBlack (n `div` 2)) (allBlack (n `div` 2)) (allBlack (n `div` 2))

allWhite :: Int -> QuadTree
allWhite 1 = Leaf White
allWhite n = Node (allWhite (n `div` 2)) (allWhite (n `div` 2)) (allWhite (n `div` 2)) (allWhite (n `div` 2))

clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
clockwise a b c d = Node a b c d

anticlockwise ::  QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise a b c d = Node a d c b

height :: QuadTree->Int
height (Leaf _) = 1
height (Node tl tr bl br) =
  1+ (max(max(height tl)(height tr))
        (max(height bl)(height br)))

twoSwitch :: QuadTree->QuadTree
twoSwitch (Node a b c d)
  | (Node  a b c d) == allBlack 2 = allWhite 2
  | (Node  a b c d) == allWhite 2 = allWhite 2
  | otherwise = allBlack 2


ndiff :: QuadTree  -> QuadTree
ndiff a
  | treeHeight == 1 = Leaf White
  | treeHeight == 2 = twoSwitch a
  where
    treeHeight = height a
