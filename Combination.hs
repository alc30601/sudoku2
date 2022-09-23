-- Combination.hs
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Combination
( makeLists, permutation, Node ) where





-------------------------------------------------------------------------
-- 木構造データ型の定義
data Node a = Node a [Node a] deriving (Show, Eq)



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


-------------------------------------------------------------------------
-- 与えられたリストの順列を生成する。出力は生成されたここの順列リストを要素として持つリスト
permutation :: (Eq a) => [a] -> [[a]]
permutation xs = makeLists (length xs) excludeFirst xs

-------------------------------------------------------------------------
-- リスト先頭要素と一致していればFalse一致しなければTrue
excludeFirst :: (Eq a) => [a] -> a -> Bool
excludeFirst [] _     = True
excludeFirst (x:xs) y = x /= y


-------------------------------------------------------------------------
-- 順列生成(permutation)の一般化
-- 順列(permutation)は重複する要素がないことが"条件"となるが、この"条件"を関数で与える。
-- また、生成するリスト長も指定可能とする。
makeLists :: (Eq a) => Int ->  ([a] -> a -> Bool) -> [a] -> [[a]]
makeLists n f xs = extractNodes $ makeNodes n f xs


-------------------------------------------------------------------------
-- ノードのリストからリストのリストを生成する。
-- [Ex.] extractNodes $ makeNodes 3 (/=) [1..3] --> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
extractNodes :: [Node a] -> [[a]]
extractNodes = concatMap extractNode


-------------------------------------------------------------------------
-- １つのノードからリストのリストを生成する。
-- [Ex.] extractNode $ makeNode 2 (/=) (0,[1..3]) --> [[0,1,2,3],[0,1,3,2],[0,2,1,3],[0,2,3,1],[0,3,1,2],[0,3,2,1]]
extractNode :: Node a -> [[a]]
extractNode (Node x []) = [[x]]
extractNode (Node x children) = map (x:) childrenValue
    where   childrenValue = concatMap extractNode children


-------------------------------------------------------------------------
-- リストを入力とし、各要素をノードのルート要素とするノードのリストを生成する。
-- [Ex.] makeNodes 3 fff [1..3] --> 
-- [Node 1 [Node 2 [Node 3 []],
--          Node 3 [Node 2 []]],
--  Node 2 [Node 1 [Node 3 []],
--          Node 3 [Node 1 []]],
--  Node 3 [Node 1 [Node 2 []], 
--          Node 2 [Node 1 []]]]
makeNodes :: (Eq a) => Int -> ([a] -> a -> Bool) -> [a] -> [Node a]
makeNodes n f xs = map (makeNode n f) (fMakeTuples f [] xs)

-------------------------------------------------------------------------
-- 木の生成(親リストは空でないことを前提とする)
-- [Ex.] makeNode 3 fff ([0],[1..3]) -->
-- Node 0 [ Node 1 [Node 2 [], Node 3 []],
--          Node 2 [Node 1 [], Node 3 []],
--          Node 3 [Node 1 [], Node 2 []]]
makeNode :: (Eq a) => Int -> ([a] -> a -> Bool) -> ([a], [a]) -> Node a
makeNode _ _ (x:xs, [])  = Node x []
makeNode 1 _ (x:xs, _)   = Node x []
makeNode n f (all@(x:xs), ys) = Node x children
    where   childTuples = fMakeTuples f all ys
            children = map (makeNode (n-1) f) childTuples



-------------------------------------------------------------------------
-- リストを２つ受け取り(第1リスト、第2リストと称する)、第２リストの各要素を第一リストに結合し
-- その結合リストと第２リストをfExcludeTuple'にかけて生成されるタプルを書く要素として持つ
-- リストを生成する。
-- [Ex.] fExcludeTuples fff [2,3] [1,2,3] --> [([1,2,3],[2,3]),([2,2,3],[1,3]),([3,2,3],[1,2])]
fMakeTuples :: ([a] -> a -> Bool) -> [a] -> [a] -> [([a], [a])]
fMakeTuples _ _ [] = []
fMakeTuples f xs ys = map (makeTuple' xs ys) ys
    where   makeTuple' rs cs c = fMakeTuple f (c:rs) cs

-------------------------------------------------------------------------
-- リストを２つ受け取り(第1リスト、第2リストと称する)、第1リストと第2リストの各要素を関数にかけ、
-- Trueとなる第2りすとの要素のみ残し、第1リスト、第2リストを持つタプルを生成する。
-- [Ex.] fExcludeTuple fff [2,3,4] [1..5] --> ([2,3,4],[1,3,4,5])
fMakeTuple :: ([a] -> a -> Bool) -> [a] -> [a] -> ([a], [a])
fMakeTuple _ xs [] = (xs, [])
fMakeTuple f xs ys = (xs, [y | y <- ys, f xs y])




-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::













-- -------------------------------------------------------------------------
-- -- 与えられたリストの要素の順列を生成する。
-- -- [Ex.] permutation 3 (/=) [1..3] --> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- -- [Ex.] permutation 2 (/=) [1..3] --> [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
-- permutation :: Eq a => Int -> (a -> a -> Bool) -> [a] -> [[a]]
-- permutation n f xs = extractNodes $ makeNodes n f xs

-- -------------------------------------------------------------------------
-- -- リストを入力とし、各要素をノードのルート要素とするノードのリストを生成する。
-- -- [Ex.] makeNodes 3 (/=) [1..3] --> 
-- --      [Node 1 [Node 2 [Node 3 []],
-- --               Node 3 [Node 2 []]],
-- --       Node 2 [Node 1 [Node 3 []],
-- --               Node 3 [Node 1 []]],
-- --       Node 3 [Node 1 [Node 2 []],
-- --               Node 2 [Node 1 []]]]
-- makeNodes :: Eq a => Int -> (a -> a -> Bool) -> [a] -> [Node a]
-- makeNodes n f xs = map (makeNode n f) (fExcludeTuples f xs)


-- -------------------------------------------------------------------------
-- -- 木の生成
-- -- 与えられたリストの要素を含むNode型のデータ構造を生成する。
-- -- 指定された数までの枝の深さ、指定された関数で子ノードをフィルタする。
-- -- [Ex.] makeNode 2 (/=) (0,[1..3]) --> 
-- --              Node 0 [Node 1 [Node 2 [],Node 3 []],
-- --                      Node 2 [Node 1 [],Node 3 []],
-- --                      Node 3 [Node 1 [],Node 2 []]]
-- makeNode :: (Eq a) => Int -> (a -> a -> Bool) -> (a, [a]) -> Node a
-- makeNode _ _ (x, [])  = Node x []
-- makeNode 1 _ (x, _)  = Node x []
-- makeNode n f (x, xs)  = Node x children
--     where childTuples = fExcludeTuples f xs
--           children = map (makeNode (n-1) f) childTuples


-- -------------------------------------------------------------------------
-- -- リストから関数fを満たす要素のみを持つリストのタプルからなるリストを返す。
-- -- [Ex.] fExcludeTuples (/=) [1..3] --> [(1,[2,3]),(2,[1,3]),(3,[1,2])]
-- fExcludeTuples :: (a -> a -> Bool) -> [a] -> [(a, [a])]
-- fExcludeTuples _ [] = []
-- fExcludeTuples f xs = map (fExcludeTuple f xs) xs


-- -------------------------------------------------------------------------
-- -- xsから関数fを満たす要素のみを持つリストを作り、xとそのリストのタプルを返す。
-- -- [Ex.] fExcludeTuple (/=) [1..5] 2 --> (2,[1,3,4,5])
-- fExcludeTuple :: (a -> a -> Bool) -> [a] -> a -> (a, [a])
-- fExcludeTuple _ [] x = (x, [])
-- fExcludeTuple f xs x = (x, [y | y <- xs, f y x])

