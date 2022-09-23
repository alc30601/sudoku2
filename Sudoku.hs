-- Sudoku.hs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
import Distribution.Simple.Utils (xargs)
import Combination (makeLists, permutation, Node)


{-
      順列数    nxnの組合せ数                                           縦横重複除去
1x1 : 1         1                                                       1
2x2 : 2         4                                                       2
3x3 : 6         216                                                     12
4x4 : 24        331776                                                  576
5x5 : 120       24883200000                                             161280
6x6 : 720       139314069504000000                                      
7x7 : 5040      82606411253903523840000000                              
8x8 : 40320     6984964247141514123629140377600000000                   
9x9 : 362880    109110688415571316480344899355894085582848000000000     

    順列数          : length $ permutation [1..n]
    nxnの組合せ数   : (length $ permutation [1..n])^n
    縦横重複除去    : length $ matrix n
-}


-- NGパターン
ex1 :: [[Int]]
ex1 = [ [1,2,3,4,5,6,7,8,9],
        [2,1,4,3,6,5,8,9,7],
        [3,4,1,2,7,8,9,5,6],
        [4,3,2,1,8,9,6,7,5],
        [5,6,7,8,9,1,2,3,4],
        [6,5,8,9,1,7,3,4,2],
        [7,8,9,5,2,3,4,6,1],
        [8,9,6,7,4,2,5,1,3],
        [9,7,5,6,3,4,1,2,8]]

-- 数独解例
ex2 :: [[Int]]
ex2 = [ [7,6,4,3,2,8,5,9,1],
        [1,9,8,4,5,7,3,6,2],
        [2,3,5,6,1,9,8,7,4],
        [6,7,9,8,4,1,2,5,3],
        [5,1,3,2,9,6,4,8,7],
        [4,8,2,7,3,5,6,1,9],
        [3,5,7,1,8,2,9,4,6],
        [9,4,6,5,7,3,1,2,8],
        [8,2,1,9,6,4,7,3,5]]



-------------------------------------------------------------------------
-- 4x4の小数独
sudoku4 :: [[[Int]]]
sudoku4 = [x | x <- matrix 4, hasDuplicateInBlocks4 x]

hasDuplicateInBlocks4 :: (Eq a) => [[a]] -> Bool
hasDuplicateInBlocks4 xs =   (hasDuplicateIn3Rows4 $ take 2 xs) && 
                            (hasDuplicateIn3Rows4 $ drop 2 xs)

hasDuplicateIn3Rows4 :: Eq a => [[a]] -> Bool
hasDuplicateIn3Rows4 xs =    (hasDuplicateInBlock $ take 2 xst) && 
                            (hasDuplicateInBlock $ drop 2 xst)
        where xst = transpose xs




-------------------------------------------------------------------------
-- n個の整数の順列のn個の組合せ。同一インデックスの要素は同じ値とならない。
matrix :: Int -> [[[Int]]]
matrix n = makeLists n rowFilter (permutation [1..n])




-------------------------------------------------------------------------
-- 数独ルールに則った可能な全配置
sudoku :: [[[Int]]]
sudoku = makeLists 9 sudokuFilter (permutation [1..9])

-------------------------------------------------------------------------
-- 列方向の重複チェック及び3x3の小ブロックの重複チェック
sudokuFilter :: Eq a => [[a]] -> [a] -> Bool
sudokuFilter sx x = blockFilter sx x && rowFilter sx x



-------------------------------------------------------------------------
-- 小ブロックの重複チェックを行う。
-- 何れとも一致しない場合True、１つでも重複するものがあればFalse
-- 最新の１行、２行、３行を組み合わせる。
-- 1行 → 最新の1行
-- 2行 → 最新の2行
-- 3行 → 最新の3行
-- 4行 → 最新の1行
-- 5行 → 最新の2行
-- 6行 → 最新の3行
-- 7行 → 最新の1行
-- 8行 → 最新の2行
-- 9行 → 最新の3行
blockFilter :: (Eq a) => [[a]] -> [a] -> Bool
blockFilter sx x = hasDuplicateIn3Rows (take num (x:sx))
    where   len = length sx + 1
            remainder = mod len 3
            num = if remainder == 0
                    then 3
                    else remainder
{-
    -- | mod len 3 == 0 = hasDuplicateIn3Rows (take 3 xsx)
    -- | otherwise      = True
-}

-------------------------------------------------------------------------
-- 3x3の小ブロック内に重複があるかチェックする。入力は9x9の行列を前提とする。
-- 重複がなければTrue、あればFalse
-- 9x9の行列を先ず、３行ごとに分け、3x9の行列とする。
-- 更に3x9の行列を転置し9x3の行列とし、３行ごとに分離し、3x3の行列とする。
-- その3x3毎に重複する要素があるか調べる。
hasDuplicateInBlocks :: (Eq a) => [[a]] -> Bool
hasDuplicateInBlocks xs =   (hasDuplicateIn3Rows $ take 3 xs) && 
                            (hasDuplicateIn3Rows $ take 3 $ drop 3 xs) &&
                            (hasDuplicateIn3Rows $ drop 6 xs)

-------------------------------------------------------------------------
-- 3x9の行列を転置し9x3とし、３行毎に分割し、それぞれの小ブロック(3x3)内に重複があるか調べる。
hasDuplicateIn3Rows :: Eq a => [[a]] -> Bool
hasDuplicateIn3Rows xs =    (hasDuplicateInBlock $ take 3 xst) && 
                            (hasDuplicateInBlock $ take 3 $ drop 3 xst) &&
                            (hasDuplicateInBlock $ drop 6 xst)
        where xst = transpose xs

-------------------------------------------------------------------------
-- 3x3の行列に重複があるかを調べる。
hasDuplicateInBlock :: (Eq a, Foldable t) => t [a] -> Bool
hasDuplicateInBlock xs = checkDuplicate (concat xs)


-- -------------------------------------------------------------------------
-- -- 転置行列
-- -- Data.List.transposeに既に転置関数はあるが、ネットを参考に自作。
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)


-- -------------------------------------------------------------------------
-- -- リスト内に重複する要素があればFalseなければTrue
checkDuplicate :: (Eq a) => [a] -> Bool
checkDuplicate [] = True
checkDuplicate (x:xs) = (not $ elem x xs) && checkDuplicate xs




-------------------------------------------------------------------------
-- 第２引数のリストの個々の要素が第一引数のリストの同一インデックスの個々の要素の
-- 何れとも一致しない場合True１つでも一致するものがあればFalse
rowFilter :: (Eq a) => [[a]] -> [a] -> Bool
rowFilter sx x = not $ duplicateColumn sx x


-------------------------------------------------------------------------
-- 与えられた行が既に存在する行群内に重複する列要素があるか調べる。
-- １つでも重複する列要素があればTrue、１つも重複する列要素がなければFalse
-- [Ex.] duplicateColumn [[1,2,3],[2,3,4],[3,4,5]] [1,3,5] -> True
-- [Ex.] duplicateColumn [[1,2,3],[2,3,4],[3,4,5]] [4,5,1] -> False
duplicateColumn :: (Eq a) => [[a]] -> [a] -> Bool
duplicateColumn xs x = any (hasSameColumn x) xs

-------------------------------------------------------------------------
-- ２つのリストのそれぞれの要素が同じか否か判定する。
-- １つでも同じ要素があればTrue、１つも一致する要素がなければFalse
-- [Ex.] hasSameColumn  [1,2,3] [2,3,4] -> False
-- [Ex.] hasSameColumn  [1,2,3] [2,3,3] -> True
hasSameColumn :: (Eq a) => [a] -> [a] -> Bool
hasSameColumn xs ys = or $ zipWith (==) xs ys









-- -------------------------------------------------------------------------
-- -- 複数の行に対し重複しない行を抽出する。
-- -- src : 追加候補の行データ
-- -- xs  : 既に存在する行群
-- -- ret : ｓsrcの中からxsに追加可能な行データ一覧
-- -- [Ex.] availableRow [[2,2,2],[9,9,9]] [[1,2,3],[2,3,4],[3,4,5]] -> [[9,9,9]]
-- availableRow :: (Eq a) => [[a]] -> [[a]] -> [[a]]
-- availableRow src xs = [x | x <- src, not $ duplicateColumn xs x]


-- -------------------------------------------------------------------------
-- -- 与えられた行列のリストから列方向の重複を省いたものだけ抽出したリストを返す。
-- columnFilter :: (Eq a) => [[[a]]] -> [[[a]]]
-- columnFilter xs = [x | x <- xs, checkColumn x]


-- -------------------------------------------------------------------------
-- -- 行列の列方向に同じ値の要素がないかチェックする。１つでも重複あればFalse
-- checkColumn :: (Eq a) => [[a]] -> Bool
-- checkColumn xs = foldr ((&&) . checkDuplicate) True $ transpose xs








-------------------------------------------------------------------------
main :: IO ()
main = do
    print "-- Sudoku --"
    -- print sudoku
