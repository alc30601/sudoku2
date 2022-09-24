-- Solver.hs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.Maybe
import GHC.Read (choose)


-------------------------------------------------------------------------
-- 数独ソルバー

-- Usage:
--  ghci> sudokuSolver yomi0
--  ghci> sudokuDifficulty yomi0

-- 問題として虫食いの9x9行列が与えられる。
-- この虫食いを埋める解を全て計算する。
-- １つの問題に対して複数の回答を出力する。

-- 虫食いは特定の値で表現する。数独の場合は0とする。
-- 9x9の行列で0のところがまだ値が確定していない場所とする。
-- １つの問題には１個以上の虫食いが存在する。

-- 虫食いを頭から順番に埋めていく。
-- １つの穴を埋めることの出来る可能性のある値が０個以上存在する。
-- 即ち、ある穴に対してどの値も入れることができない場合
-- １個以上の値が入りうる場合
-- １個以上の値が入りうる場合は、可能な値を全て入れた解(途中解)を生成する。
-- ある穴に対して何がしかの値を入れた途中解は入れる前に比べ虫食いの数が１つ減っている。
-- この連鎖を繰り返し虫食いを１つづつ入れていき、虫食い穴が無くなるまで繰り返す。
-- 虫食い穴が１つも無い解が完全解の１つとなる。
-- このように穴を１つ埋めるたびに分岐が増える木構造での解探索となる。

-------------------------------------------------------------------------
-- ある穴に対して可能な値が１つもない場合はその分岐は解になりえないので捨てる。
-- このようにして穴に値を設定して木を下っていき、穴が無くなるところまで到達した解を収集する。
-- それが求める解の集合となる。

-------------------------------------------------------------------------
-- 分岐の数をなるべく減らすために可能な値が最も少ないものを次の解とする。
-- 実際は頭から可能な数を木にせずに分岐していったほうが実行速度は速い。


-------------------------------------------------------------------------
-- 数独ソルバ
-- 問題を入力すれば、可能な答えのリストを返す。1
sudokuSolver :: (Eq a, Num a, Enum a) => Mat a -> [Mat a]
sudokuSolver m = leafPick 0 $ makeTree 0 [1..9] m

-------------------------------------------------------------------------
-- 数独難易度判定
-- 問題を入力すれば、解に至るまでに生成される木のノード数(Branch, Leaf)を返す。
-- この数値が大きいほうが可能なパターン数が多く難しいと考えられる。
sudokuDifficulty :: (Eq a, Num a, Enum a) => Mat a -> (Int, Int, Int)
sudokuDifficulty m = countTree 0 (0,0, 0) $ makeTree 0 [1..9] m


-------------------------------------------------------------------------
-- 木構造データ型の定義
data Tree a = Branch [Tree a] | Leaf a deriving (Show, Eq)

-------------------------------------------------------------------------
-- 行列を表す型シノニム
type Mat a = [[a]]

-- 行列上の位置を表す型シノニム
type Pos = (Int, Int)




-------------------------------------------------------------------------
-- 木から葉を摘む。
-- 但し、未完成の葉は摘まない。<- 第一引数の値を含むもの
leafPick :: Eq a => a -> Tree (Mat a) -> [Mat a]
leafPick v (Leaf m)
        | hasTheValue m v = []
        | otherwise       = [m]
leafPick v (Branch []) = []
leafPick v (Branch (x:xs)) = leafPick v x ++ leafPick v (Branch xs)




-------------------------------------------------------------------------
-- 木から枝の数、葉の数、葉の中で穴のない歯の数を数える。
-- v : 穴を表す値
-- (b, l, s) : b=分岐数, l=葉数, s=穴のない葉数
countTree :: Eq a => a -> (Int, Int, Int) -> Tree (Mat a) -> (Int, Int, Int)
countTree v (b, l, s) (Leaf m) = (b, l+1, sol)
        where sol = if hasTheValue m v
                        then 0
                        else 1
countTree v (b, l, s) (Branch []) = (b+1, l, s)
countTree v (b, l, s) (Branch children) =  
        let childCountList = map (countTree v (0,0,0)) children
        in foldl (\(a1, a2, a3) (b1, b2, b3) -> (a1+b1, a2+b2, a3+b3)) (b+1, l, s) childCountList

-------------------------------------------------------------------------
-- 行列の中にある値が存在するかどうか調べる。
-- 存在すればTrue、１つもなければFalse
hasTheValue :: Eq a => Mat a -> a -> Bool
hasTheValue m x = foldr (||) False $ map (elem x) m




-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- 木を作る。１つの行列を土台として、その行列の中の(可能であれば全ての)穴に
-- 入れることの出来る値を入れた行列を木の末端として持つ木を構成する。
-- makeTree v vs m
-- v    : 穴を示す値
-- vs   : 穴に入る可能性のある値のリスト
-- m    : 行列
-- ret  : 末端の葉にこれ以上入れられない行列を持つ木を返す。
makeTree :: Eq a => a -> [a] -> Mat a -> Tree (Mat a)
makeTree v vs m = if null children
                        then Leaf m
                        else Branch (map (makeTree v vs) children)
        where   children = solve v vs m


-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- １つの穴に入る可能性のある値を全て入れた結果の行列(複数)を返す。
-- 入れる穴は最も入る値の可能性の小さいもの
-- 穴が一つもない場合、または１つも値が入れられない穴がある場合は空リスト[]を返す。
-- 複数の値が入るうる場合は複数の値を入れた複数の行列のリストが返される。
-- solve v vs m
-- v    : 穴を示す値
-- vs   : 穴に入る可能性のある値のリスト
-- m    : 行列
-- ret  : ある穴に入れられる値を入れた行列のリスト
solve :: Eq a => a -> [a] -> Mat a -> [Mat a]
solve v vs m
        | null avails = []              -- 穴が無い場合
        | null (snd next) = []          -- 穴はあるが値の入れられない穴がある場合
        | otherwise =   let next = head avails
                        in map (fillInAHole (fst next) m) (snd next)
        where   avails = minPossibilities $ availables v vs m
                next = head avails





-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- 全ての穴及びそれぞれの穴に入る値の一覧のうち、一番入り得る値の可能性の少ないものを抽出する。
minPossibilities :: [(Pos, [a])] -> [(Pos, [a])]
minPossibilities [] = []
minPossibilities (x:xs) = [extractMin x xs]
        where   extractMin :: (Pos, [a]) -> [(Pos, [a])] -> (Pos, [a])
                extractMin x [] = x
                extractMin x (y:ys) = if isSmaller x y
                                then extractMin x ys
                                else extractMin y ys
                ---------------------------------------------------------
                -- ２つの(Pos, [a])に対し[a]の個数の大小を比較する。
                -- 第一引数の[a]の個数のほうが少なければTrue、多ければFalse
                isSmaller :: (Pos, [a]) -> (Pos, [a]) -> Bool
                isSmaller (_, x1) (_, x2) = length x1 <= length x2



-------------------------------------------------------------------------
-- 行列の全ての穴に対して、入れることの出来る値のリストを生成し、
-- 穴の場所、その穴に入れられる値のリスト　のペア　を穴毎に作りリスト化する。
-- v    : 穴を表す値
-- vs   : 穴に入れる値の候補
-- m    : 穴を含む行列
-- ret  : 穴の場所とその穴に入れられる値のリストのペアのリスト
availables :: Eq a => a -> [a] -> Mat a -> [(Pos, [a])]
availables v vs m = map (\p -> (p, [x | x <- vs, checkTheHole p m x])) ps
        where   ps = findInMatrix v m





-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- 行列の特定箇所に値を設定する。
fillInAHole :: Pos -> Mat a -> a -> Mat a
fillInAHole p m x = replaceInList r newRow m
        where   r = fst p
                c = snd p
                newRow = replaceInList c x (m !! r)

-------------------------------------------------------------------------
-- リストの指定された場所の要素を入れ替えたリストを生成する。
replaceInList :: Int -> a -> [a] -> [a]
replaceInList _ _ []     = []
replaceInList 0 y (x:xs) = y:xs
replaceInList n y (x:xs) = x:replaceInList (n-1) y xs





-------------------------------------------------------------------------
-------------------------------------------------------------------------
--  １つの穴に対してある値が入るかどうかを調べる。
-- 入れることができればOK(True)、できなければNG(False)
-- 横、縦、3x3のブロックそれぞれで重複する要素が何れにもなければOKとする。
checkTheHole :: Eq a => Pos -> Mat a -> a -> Bool
checkTheHole p m x = checkRow p m x && checkColumn p m x && checkBlock p m x

-------------------------------------------------------------------------
-- Posで示される行に指定した値と重複する要素がないか調べる。
-- 重複があればFalse、なければTrue
checkRow :: Eq a => Pos -> Mat a -> a -> Bool
checkRow p m x = notElem x theRow
        where   r = fst p
                theRow = m !! r

-------------------------------------------------------------------------
-- Posで示される列に指定した値と重複する要素がないか調べる。
-- 重複があればFalse、なければTrue
checkColumn :: Eq a => Pos -> Mat a -> a -> Bool
checkColumn p m x = notElem x theCol
        where   c = snd p
                theCol = transpose m !! c

-------------------------------------------------------------------------
-- Posで示される位置の属する3x3のブロック内に指定した値と重複する要素がないか調べる。
-- 重複があればFalse、なければTrue
checkBlock :: Eq a => Pos -> Mat a -> a -> Bool
checkBlock p m x = notElem x $ concat $ cutBlock p m

-------------------------------------------------------------------------
-- 指定された位置の属する3x3のブロックを切り出す。
cutBlock :: Pos -> Mat a -> Mat a
cutBlock p m = transpose $ take 3 $ drop c $ transpose rows
        where   r = div (fst p) 3 * 3
                rows = take 3 $ drop r m
                c = div (snd p) 3 * 3


---------------------------------------------------------------------------
-- -- 転置行列
-- -- Data.List.transposeに既に転置関数はあるが、ネットを参考に自作。
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)





-- -- -------------------------------------------------------------------------
-- -- -------------------------------------------------------------------------
-- -- -- 指定された要素の行列内の全ての存在箇所をリストにして返す。
findInMatrix :: Eq a => a -> Mat a -> [Pos]
findInMatrix x xs = findInMatrix' x xs 0
        where   findInMatrix' :: Eq a => a -> Mat a -> Int -> [Pos]
                findInMatrix' _ [] _ = []
                findInMatrix' v (x:xs) r = if ts /= []
                                                then ts ++ findInMatrix' v xs (r+1)
                                                else findInMatrix' v xs (r+1)
                                        where   ts = map (\c -> (r,c)) (findPos v x)


-- -- -------------------------------------------------------------------------
-- -- -- 指定された要素のリスト内の存在箇所をリストにして返す。
findPos :: Eq a => a -> [a] -> [Int]
findPos v xs = findPos' v xs 0
        where   findPos' :: Eq a => a -> [a] -> Int -> [Int]
                findPos' _ [] _  = []
                findPos' v (x:xs) index  = if v == x
                                        then index:findPos' v xs (index + 1)
                                        else findPos' v xs (index + 1)




-------------------------------------------------------------------------
main :: IO ()
main = do
    print "-- Solver --"
--     T.B.D.


