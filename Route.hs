import Prelude hiding( Left, Right )
import Data.List(foldl')

-- "向き"の型
data Heading = North
             | East
             | West
             | South
             deriving( Show, Read )

-- 現在の状態の型 ( (X座標, Y座標), "向き" )
type Status = ( (Int, Int), Heading )

-- 移動方向の型
data Direction = Forward
               | Backward
               | Left
               | Right
               deriving( Enum, Show, Read )


--
-- [New!!] if や switch を使わずにパターンを網羅できる
--

-- 移動 :: 方向 -> 旧状態 -> 新状態
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



-- 移動してみる
ex1a = advance Forward ((0,0),North)
-- ((0,1),North)

ex1b = advance Left ((0,0),North)
-- ((-1,0),West)


-- 連続移動してみる( 前 - 前 - 右 - 前 - 左 )
ex1c = advance Left (　advance Forward (　advance Right ( advance Forward ( advance Forward ((0,0),North) ) ) ) )
-- ((2,3),North)




-- 連続移動が読みにくいので関数の引数を固定してみる

-- 前進 :: 旧状態 -> 新状態
forward :: Status -> Status
forward  = advance Forward

-- 後退 :: 旧状態 -> 新状態
backward :: Status -> Status
backward = advance Backward

-- 左折 :: 旧状態 -> 新状態
left :: Status -> Status
left = advance Left

-- 右折 :: 旧状態 -> 新状態
right :: Status -> Status
right = advance Right


ex2a = forward ((0,0),North)
-- ((0,1),North)

ex2b = left ((0,0),North)
-- ((-1,0),West)


-- 少し読みやすくなった( 前 - 前 - 右 - 前 - 左 )
ex2c = left ( forward ( right ( forward ( forward ((0,0),North) ) ) ) )
-- ((2,3),North)


--
-- [New!!] 演算子を使って式を評価できるのでカッコが省略できる
--
-- さらに読みやすくなった( 前 - 前 - 右 - 前 - 左 )
ex2d = left $ forward $ right $ forward $ forward ((0,0),North)
-- ((2,3),North)




-- 引数の順番を入れ替えてみる

-- 移動' :: 旧状態 -> 方向 -> 新状態
advance':: Status -> Direction -> Status
advance' = flip advance

--
-- [New!!] 関数を演算子として使う
--
-- 長くなったけど順番は見やすい( 前 - 前 - 右 - 前 - 左 )
ex3a = ((0,0),North) `advance'` Forward `advance'` Forward `advance'` Right `advance'` Forward `advance'` Left
-- ((2,3),North)


--
-- [New!!] 独自演算子の定義(オーバーロードではなく定義)
--
(==>):: Status -> Direction -> Status
(==>) = advance'

-- そうとう見やすくなった( 前 - 前 - 右 - 前 - 左 )
ex3b = ((0,0),North) ==> Forward ==> Forward ==> Right ==> Forward ==> Left
-- ((2,3),North)


-- あるいは
($>) = flip ($)

-- 同じくらい見やすい( 前 - 前 - 右 - 前 - 左 )
ex3c = ((0,0),North) $> forward $> forward $> right $> forward $> left
-- ((2,3),North)





-- 移動指示をリストで指定できるようにしてみる
advances :: Status -> [Direction] -> Status
advances = foldl' (==>)

--
-- [New!!] こんなに簡単に?
--
-- リストで移動指示( 前 - 前 - 右 - 前 - 左 )
ex4a = advances ((0,0),North) [Forward, Forward, Right, Forward, Left]
-- ((2,3),North)




-- 経路探索してみたくなった
permutations' ::  [a] -> Int -> [[a]]
permutations' = func [[]]
  where
    func acc _ 0 = acc
    func acc xs i = acc ++ (func [ (x:ys) | x <- xs, ys <- acc ] xs (i-1))

-- こんな関数を作った
ex4b = permutations' [1,2,3] 4
-- [[],[1],[2],[3],[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3],[1,1,1],[1,1,2],[1,1,3],[1,2,1],[1,2,2],[1,2,3],[1,3,1],[1,3,2],[1,3,3],[2,1,1],[2,1,2],[2,1,3],[2,2,1],[2,2,2],[2,2,3],[2,3,1],[2,3,2],[2,3,3],[3,1,1],[3,1,2],[3,1,3],[3,2,1],[3,2,2],[3,2,3],[3,3,1],[3,3,2],[3,3,3],[1,1,1,1],[1,1,1,2],[1,1,1,3],[1,1,2,1],[1,1,2,2],[1,1,2,3],[1,1,3,1],[1,1,3,2],[1,1,3,3],[1,2,1,1],[1,2,1,2],[1,2,1,3],[1,2,2,1],[1,2,2,2],[1,2,2,3],[1,2,3,1],[1,2,3,2],[1,2,3,3],[1,3,1,1],[1,3,1,2],[1,3,1,3],[1,3,2,1],[1,3,2,2],[1,3,2,3],[1,3,3,1],[1,3,3,2],[1,3,3,3],[2,1,1,1],[2,1,1,2],[2,1,1,3],[2,1,2,1],[2,1,2,2],[2,1,2,3],[2,1,3,1],[2,1,3,2],[2,1,3,3],[2,2,1,1],[2,2,1,2],[2,2,1,3],[2,2,2,1],[2,2,2,2],[2,2,2,3],[2,2,3,1],[2,2,3,2],[2,2,3,3],[2,3,1,1],[2,3,1,2],[2,3,1,3],[2,3,2,1],[2,3,2,2],[2,3,2,3],[2,3,3,1],[2,3,3,2],[2,3,3,3],[3,1,1,1],[3,1,1,2],[3,1,1,3],[3,1,2,1],[3,1,2,2],[3,1,2,3],[3,1,3,1],[3,1,3,2],[3,1,3,3],[3,2,1,1],[3,2,1,2],[3,2,1,3],[3,2,2,1],[3,2,2,2],[3,2,2,3],[3,2,3,1],[3,2,3,2],[3,2,3,3],[3,3,1,1],[3,3,1,2],[3,3,1,3],[3,3,2,1],[3,3,2,2],[3,3,2,3],[3,3,3,1],[3,3,3,2],[3,3,3,3]]


-- ある場所にいけるn手以内の全経路（アルゴリズムは総当たりです...）
-- 経路 :: 初期状態 -> 目的地@(X座標,Y座標) -> 最大手数 -> [経路]
routes :: Status -> (Int,Int) -> Int -> [[Direction]]
routes start goal max = filter (\ds-> (advances start ds) `isReachable` goal )
                        $ permutations' [Forward ..] max
  where
    isReachable ((x,y),_) (x',y') = ( x == x' && y == y' )


-- ((0,0),北向) から ((2,3),[向き問わず]) に行ける 10手以内の経路を3つ探索
ex4c = take 3 $ routes ((0,0),North) (2,3) 10
-- [[Forward,Forward,Forward,Right,Forward],[Forward,Forward,Right,Forward,Left],[Forward,Forward,Right,Left,Right]]






--
-- [New!!] 関数をパワーアップするといろんな計算に対応できる!!
--
forwardM :: Monad m => Status -> m Status
forwardM = \st -> return $ forward st

backwardM :: Monad m => Status -> m Status
backwardM = \st -> return $ backward st

leftM :: Monad m => Status -> m Status
leftM = \st -> return $ left st

rightM :: Monad m => Status -> m Status
rightM = \st -> return $ right st


-- 今までと同じようにも使える( 前 - 前 - 右 - 前 - 左 )
ex5a :: Monad m => m Status
ex5a = return((0,0),North) >>= forwardM >>= forwardM >>= rightM >>= forwardM >>= leftM
-- ((2,3),North)



-- ターニングポイントを設置してみる( 前 - 前 - ? - 前 - 左 )
-- ある地点での計算を引数にとる関数
go :: Monad m => (Status -> m Status) -> m Status
go turn = return((0,0),North) >>= forwardM >>= forwardM >>= turn >>= forwardM >>= leftM


--
-- [New!!] 答えがないかもしれない計算（ターニングポイントが未定なら結果も未定）
--
-- 移動できる範囲を決める場合などに範囲外に出たケースなどで使えそう
--
ex6a :: Maybe Status
ex6b :: Maybe Status
ex6c :: Maybe Status

ex6a = go $ \st -> Just (right st)
-- Just ((2,3),North)

ex6b = go $ \st -> Just (backward st)
-- Just ((1,0),East)

ex6c = go $ \st -> Nothing
-- Nothing



--
-- [New!!] 答えが複数ある計算（複数のターニングポイントを一挙に計算）
--
ex7a :: [Status]
ex7b :: [Status]
ex7c :: [Status]

ex7a = go $ \st -> [backward st, right st, left st]
-- [((1,0),East),((2,3),North),((-2,1),South)]

ex7b = go $ \st -> [right st]
-- [((2,3),North)]

ex7c = go $ \st -> []
-- []



---
--- ユーザーの入力をターニングポイントに反映
---

-- 文字列入力を "移動方向"型に変換
getDirection :: IO Direction
getDirection = getLine >>= \line -> return ( read line :: Direction )

ex8a :: IO Status
ex8a = go $ \st -> getDirection >>= \dir -> return (advance dir st)
-- 入力待ちになるので "Forward" "Backward" "Left" "Right" などを入力
-- ターニングポイントが入力した移動方向だったときの計算が行われる

-- わかりやすく書くとこう
ex8b :: IO Status
ex8b = go $ \st-> do
  line <- getLine
  let dir = read line :: Direction
  return (advance dir st)







main :: IO()
main = do

  print ex1a
  print ex1b
  print ex1c

  print ex2a
  print ex2b
  print ex2c
  print ex2d

  print ex3a
  print ex3b
  print ex3c
  
  print ex4a
  print ex4b
  print ex4c
  
  print =<< ex5a

  print ex6a
  print ex6b
  print ex6c
  
  print ex7a
  print ex7b
  print ex7c

  print =<< ex8a
  print =<< ex8b

