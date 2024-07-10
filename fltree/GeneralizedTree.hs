module GeneralizedTree 
  ( Tree (Branch, Leaf)
  , grow
  , clone
  , graft
  ) where

import Data.List ((\\))

-- | An n-ary tree with values at every furcation.
data Tree a = Leaf a
            | Branch a [Tree a]
            deriving (Eq, Ord, Read, Show)

instance Functor Tree where
  fmap f (Leaf a)      = Leaf (f a)
  fmap f (Branch a as) = Branch (f a) (fmap f <$> as)

-- TREE FUNCTIONS.
--
-- | Given an n-ary tree of strings, 'grow' will produce a visualization for the
-- the tree.  The width spans vertically and the depth spans horizontally.  The
-- visualization uses UTF-8 box drawing characters.
--
-- >>> putStrLn $ grow (Branch "Asia" [Branch "China" [Leaf "Beijing",
--       Leaf "Shanghai"], Branch "India" [Leaf "New Delhi", Leaf "Bombay"]]
-- Asia
-- ├── China
-- │   ├── Beijing
-- │   ╰── Shanghai
-- ╰── India
--     ├── New Delhi
--     ╰── Bombay
grow :: Tree String -> String
grow = draw "" True
  where
    draw :: String -> Bool -> Tree String -> String
    draw pad isLast (Branch s []) = paddingBranch pad isLast ++ s
    draw pad isLast (Branch s ts) = paddingBranch pad isLast ++ s ++
                                      concatMap ('\n':)
                                        (furcate (paddingStem pad isLast) ts)
    draw pad isLast (Leaf s) = paddingLeaf pad isLast ++ s
    
    furcate :: String -> [Tree String] -> [String]
    furcate pad (t:[]) = draw pad True  t : []
    furcate pad (t:ts) = draw pad False t : furcate pad ts
  
    paddingStem ""  _      = " "
    paddingStem pad isLast = take (length pad - 3) pad ++
                             if isLast then "    xxx" else "│   xxx"
  
    paddingBranch ""  _      = ""
    paddingBranch pad isLast = take (length pad - 3) pad ++
                               if isLast then "╰── " else "├── "
    paddingLeaf = paddingBranch

-- | 'clone' expects a string of certain properties.  The string represents a
-- tree and has the necessary information to recreate it (hence the name
-- ‘clone’). The string is such that children are indented (uniformly for
-- consistent cloning and readability) greater than the parent.  The indentation
-- simply has to greater than that of the parent.
--
--         Asia
--           China
--          India
--           Pakistan
--
-- The above would create a tree:
--         Branch "Asia" [Leaf "China", Branch "India" [Leaf "Pakistan]].
-- Both China and India are children of Asia, however Pakistan becomes a child
-- of India.  Trees of other types can be produced by mapping 'show' over the
-- tree.
clone :: String -> Tree String
clone = forage . map (span (== ' ')) . lines
  where
    forage :: (Eq a, Ord a) => [(a, a)] -> Tree a
    forage (x:[]) = Leaf (snd x)
    forage (x1:x2:xs)
      | fst x1 < fst x2 = Branch (snd x1) [] =<>- Branch undefined
                                                    (forage <$> cut (x2:xs))
      | otherwise = Leaf (snd x1)
  
    cut :: (Eq a, Ord a) => [(a, a)] -> [[(a, a)]]
    cut []     = []
    cut (x:xs) = let p = takeWhile (\(s, _) -> fst x < s) xs
                 in  (x:p) : cut (xs \\ p)
  
-- | Given tree A and B, 'graft' grafts the children of B into the vertex of A.
--
-- >>> Branch 1 [Leaf 0] =<>- Branch 2 [Leaf 3, Leaf 4]
-- Branch 1 [Leaf 0, Leaf 3, Leaf 4]
--
-- The vertex of B is ignored.
graft, (=<>-) :: Tree a -> Tree a -> Tree a
graft (Branch a as) (Branch _ bs) = Branch a (as ++ bs)
(=<>-) = graft
