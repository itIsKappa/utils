module GeneralizedTree 
  ( Tree (Branch, Leaf)
  , Wood (ASCII, Unicode, Round)
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


-- | This type indicats types of characters to use for drawing the tree.  ASCII
-- uses only characters in the ASCII character set.  This is the best in terms
-- of compatibility; from an aesthetic viewpoint, it is not very good.
-- 'Unicode' uses UTF-8 characters, so does Round.  However 'Unicode' denotes
-- sharp petioles and 'Round' denotes, well, round petioles.  These are simply
-- handles for the character sets, no more.  To obtain the character set, use
-- 'part' with the corresponding 'Wood'.
data Wood = ASCII | Unicode | Round
            deriving (Eq, Read, Show)

-- | The parts of a tree (as regards our drawing of a tree) are the trunk, the
-- ramus, and the petioles.  These are terms borrowed from botany.  'trunk'
-- is the string for the line without any branches.  'ramus' and
-- 'petiole' here indicates two types of branches.  Ramus is the node occurs
-- at the beginning or in-between.  Petiole is the node occurs at the end; this
-- is the node where 'isLast' is true in 'draw' internal for 'grow'.
--
--            Asia
--   Ramus -->├── China
--            │   ├── Beijing
--   Trunk -->│   ╰── Shanghai
-- Petiole -->╰── India
--                ├── New Delhi
--     Petiole -->╰── Bombay
--
-- ^The term ‘petiole’ is used to denote the last child; it might not make much
-- sense to think of it as having a botanical counterpart.  Nevertheless, we
-- need a term to refer to it.  Calling it the last child can become cumbersome,
-- and can protract any description of what is being done.  A petiole is a
-- special case, that is all.
data Part = Part
       { trunk   :: String
       , ramus   :: String
       , petiole :: String
       }

-- | This provides the corresponding 'Part' for the type of 'Wood'.
-- The string for the parts of a tree must not be longer than 4 characters.  The
-- last character must be a space.
part :: Wood -> Part
part ASCII   = Part "|   " "+-- " "`-- "
part Unicode = Part "│   " "├── " "└── "
part Round   = Part "│   " "├── " "╰── "


-- | Given an n-ary tree of strings, 'grow' will produce a visualization for the
-- the tree.  The width spans vertically and the depth spans horizontally.  The
-- visualization uses drawing characters as specified by Wood type.
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
grow :: Wood -> Tree String -> String
grow w = draw "" True
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
  
    {- The value of 'paddingStem' must be a space to avoid a call to this
     - pattern infinitely.  The "xxx" in 'paddingStem' is to be replaced by
     - 'paddingBranch'.  Why should that be so?  When either a ramus or a
     - petiole has been concatenated, what follows might be a trunk.  This must
     - be handled by 'paddingStem'.  'paddingStem' cannot know whether it was a
     - trunk, a ramus, or a petiole that was concatenated.  When 'paddingStem'
     - is called consecutively, the "xxx" must exist lest the actual tree be
     - erased.  Now coming to 'paddingBranch', it must replace "xxx" before it
     - concatenates a ramus or a petiole. -}
    paddingStem ""  _      = " "
    paddingStem pad isLast = take (length pad - 3) pad ++
                               if isLast then
                                 replicate 4 ' ' ++ "xxx"
                               else
                                 trunk (part w)  ++ "xxx"
  
    paddingBranch ""  _      = ""
    paddingBranch pad isLast = take (length pad - 3) pad ++
                                 if isLast then
                                   petiole (part w)
                                 else
                                   ramus (part w)
    paddingLeaf = paddingBranch
    {- There is no reason for 'paddingLeaf' to exist.  Just for my peace of
     - mind. -}

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
-- of India.  Trees of other types can be produced by mapping 'read' over the
-- tree.
clone :: String -> Tree String
clone = forage . map (span (== ' ')) . lines

forage :: (Eq a, Ord a) => [(a, a)] -> Tree a
forage (x1:[]) = Leaf (snd x1)
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
