-- ×できること論 -> チューリング完全なら同じなので不毛
-- ×効率論 -> 全部アセンブリが一番速いので不毛
-- ○表現力論 
-- ○操作性論

import Prelude hiding( Left, Right )
import Data.List(foldl')

data Heading = North
             | East
             | West
             | South
             -- [POINT] 文字列表現の自動導出
             deriving( Show, Read )

type Status = ( (Int, Int), Heading )

data Direction = Forward
               | Backward
               | Left
               | Right
               -- [POINT] 文字列表現の自動導出
               deriving( Enum, Show, Read )


-- [POINT] 明確なパターン分け
advance:: Direction -> Status -> Status
advance Forward  ((x, y), North) = (( x, y+1 ), North)
advance Backward ((x, y), North) = (( x, y-1 ), South)
advance Left     ((x, y), North) = (( x-1, y ), West )
advance Right    ((x, y), North) = (( x+1, y ), East )

advance Forward  ((x, y), East) = (( x+1, y ), East ) 
advance Backward ((x, y), East) = (( x-1, y ), West )
advance Left     ((x, y), East) = (( x, y+1 ), North)
advance Right    ((x, y), East) = (( x, y-1 ), South)

advance Forward  ((x, y), West) = (( x-1, y ), West )
advance Backward ((x, y), West) = (( x+1, y ), East )
advance Left     ((x, y), West) = (( x, y-1 ), South)
advance Right    ((x, y), West) = (( x, y+1 ), North)

advance Forward  ((x, y), South) = (( x, y-1 ), South)
advance Backward ((x, y), South) = (( x, y+1 ), North)
advance Left     ((x, y), South) = (( x+1, y ), East )
advance Right    ((x, y), South) = (( x-1, y ), West )

f1 = advance Right (　advance Forward (　advance Left ( advance Forward ( advance Forward ((0,0),North) ) ) ) )


-- [POINT] 2引数の関数に引数を部分適応して1引数の関数
forward :: Status -> Status
forward  = advance Forward
backward = advance Backward
left = advance Left
right = advance Right

f2 = right ( forward ( left ( forward ( forward ((0,0),North) ) ) ) )


-- [POINT] 演算時を使った関数適応(右から結合)
f3 = right $ forward $ left $ forward $ forward ((0,0),North)


-- [POINT] 引数の順序を入れ替えて演算子として使う(左から結合)
advance':: Status -> Direction -> Status
advance' = flip advance

f4 = ((0,0),North) `advance'` Forward `advance'` Forward `advance'` Left `advance'` Forward `advance'` Right


-- [POINT] 独自演算子の定義
(==>):: Status -> Direction -> Status
(==>) = advance'

f5 = ((0,0),North) ==> Forward ==> Forward ==> Left ==> Forward ==> Right


-- あるいは
($>) = flip ($)
f6 = ((0,0),North) $> forward $> forward $> left $> forward $> right


-- 移動をリストで指定
advances :: Status -> [Direction] -> Status
advances = foldl' (==>)

f7 = advances ((0,0),North) [Forward, Forward, Left, Forward, Right]


-- 経路探索問題解きたくなってきた
permutations' ::  [a] -> Int -> [[a]]
permutations' = func []
  where
    func acc _ 0 = acc
    func [] xs i = func [[]] xs i
    func acc xs i = (func [ (x:ys) | x <- xs, ys <- acc ] xs (i-1)) ++ acc

-- ある場所にいけるn手以内の全経路（アルゴリズムは力技）
routes :: Status -> (Int,Int) -> Int -> [[Direction]]
routes start goal max = filter (\ds-> (advances start ds) `isReachable` goal )
                        $ permutations' [Forward ..] max
  where
    isReachable ((x,y),_) (x',y') = ( x == x' && y == y' )

-- (0,0)北向 から (2,4) にいける 7手以内の全経路
f8 = routes ((0,0),North) (2,3) 7





--
-- 非決定的な計算への対応
--
forward' :: Monad m => Status -> m Status
forward' = \x -> return $ forward x
backward' :: Monad m => Status -> m Status
backward' = \x -> return $ backward x
left' :: Monad m => Status -> m Status
left' = \x -> return $ left x
right' :: Monad m => Status -> m Status
right' = \x -> return $ right x

f10 :: Monad m => m Status
f10 = return((0,0),North) >>= forward' >>= forward' >>= left' >>= forward' >>= right'



go :: Monad m => (Status -> m Status) -> (m Status)
go adv = return((0,0),North) >>= forward' >>= forward' >>= adv >>= forward' >>= right'

-- [POINT] 途中で解なしになるかもしれない計算(範囲外など)
f11 :: Maybe Status
f12 :: Maybe Status
f11 = go $ \st-> Just (left st)
f12 = go $ \st-> Nothing

-- [POINT] 解が複数ある計算
f13 :: [Status]
f14 :: [Status]
f15 :: [Status]
f13 = go $ \st-> [left st,right st]
f14 = go $ \st-> [left st]
f15 = go $ \st-> []

-- [POINT] 外部からの入力で解が変わる計算
f16 :: IO Status
f16 = go $ \st-> getLine >>= \line -> return (read line :: Direction) >>= \dir -> return ( advance dir st )
f16' :: IO Status
f16' = go $ \st-> do
  line <- getLine
  return ( advance (read line :: Direction) st )


main :: IO()
main = do
  print f1
  print f2
  print f3
  print f4
  print f5
  print f6
  print f7
  print $ length f8
  f10 >>= \st -> print st
  print f11
  print f12
  print f13
  print f14
  print f15
  f16 >>= \st -> print st

