

-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}

oneList :: [String] -> [String] -> [[String]]
oneList [] _ = []
oneList _ [] = []
oneList (x:xs) (y:ys) = [[x,y]] ++ (oneList xs ys)

operation :: [Int] -> String
operation l = printf "%.2f" ((fromIntegral (foldr (+) 0 l))/(fromIntegral 8) :: Float)  

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : (oneList (map head (tail m)) (map operation (map (map (read::String->Int)) (map tail (tail m)))))

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = goals (map (foldr (+) 0) (map (map (read::String->Int)) (map tail (tail m))))

goals l = length(filter (>1000) l)

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m))/(fromIntegral (length(tail m)))


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (fromIntegral (foldr (+) 0 (map (foldr (+) 0) (map (map (read::String->Int)) (map tail (tail m))))))/(fromIntegral (length(tail m)))

-- Task 3

printString :: [Float] -> [String]
printString l = map (\x -> printf "%.2f" x) l

get_num :: Table -> [[Int]]
get_num m = map (map (read::String->Int)) (map tail (tail m))

get_sum :: [[Int]] -> [Float]
get_sum m = map (/(fromIntegral (length m))) (map (fromIntegral) (foldr (\xs ys -> zipWith (+) xs ys) [0,0,0,0,0,0,0,0] m))

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : (printString ((get_sum (get_num m))) : [])

-- Task 4

printString2 :: [Float] -> [String]
printString2 l = map (\x -> printf "%.0f" x) l

get3 :: Table -> [[Int]]
get3 m = map (map (read::String->Int)) (map (drop 3) (tail m))

count_workout :: [Int] -> [[Char]]
count_workout m = printString2 (map fromIntegral [range1 m,range2 m,range3 m])

range1 l = length(filter (>=0) (filter (<50) l))
range2 l = length(filter (<100) (filter (>=50) l)) 
range3 l = length(filter (<500) (filter (>=100) l))  

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"]:(["VeryActiveMinutes"] ++ count_workout (map head (get3 m))):(["FairlyActiveMinutes"] ++ count_workout (map (!! 1) (get3 m))):(["LightlyActiveMinutes"] ++ count_workout (map last (get3 m))):[]

-- Task 5

cmp_steps a b  
  | ((read::String->Int) (((!! 1)) a)) < ((read::String->Int) (((!! 1)) b)) = LT
  | ((read::String->Int) (((!! 1)) a)) > ((read::String->Int) (((!! 1)) b)) = GT
  | otherwise = cmp_people a b

cmp_people a b
  | (head a) < (head b) = LT
  | (head a) > (head b) = GT


get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : (sortBy cmp_steps (map (take 2) (tail m)))

cmp_diff a b  
  | ((read::String->Double) (((!! 3)) a)) < ((read::String->Double) (((!! 3)) b)) = LT
  | ((read::String->Double) (((!! 3)) a)) > ((read::String->Double) (((!! 3)) b)) = GT
  | otherwise = cmp_people a b

-- Task 6
operation1 :: [Int] -> Float
operation1 l = ((fromIntegral (foldr (+) 0 l))/(fromIntegral 4) :: Float)  

get_first :: [[String]] -> [[Int]]
get_first m = map(map (read::String->Int)) (map (take 4) m)

get_last :: [[String]] -> [[Int]]
get_last m = map(map (read::String->Int)) (map (drop 4) m)

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : (sortBy cmp_diff (zipWith (++) (oneList (map head (tail m)) (printString (map operation1 (get_first (map tail (tail m)))))) (oneList (printString (map operation1 (get_last (map tail (tail m))))) (printString (map abs (zipWith (-) (map operation1 (get_first (map tail (tail m)))) (map operation1 (get_last (map tail (tail m))))))))))

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f m)

get_sleep_total :: Row -> Row
get_sleep_total r = [head r] ++ (printString [(foldr (+) 0 (map (read::String->Float) (tail r)))]) ++ []


{-
    TASK SET 2
-}

-- Task 1

get_position :: ColumnName -> Table -> Int
get_position column table
  | elemIndex column (head table) /= Nothing = (read::String->Int) (maybe "" show (elemIndex column (head table)))
  | otherwise = -1

cmp_columns a b
  | ((!! ((read::String->Int) ((!! 0) a) + 1)) a) == "" = LT
  | ((!! ((read::String->Int) ((!! 0) a) + 1)) b) == "" = GT
  | ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) a)) < ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) b)) = LT
  | ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) a)) > ((read::String->Float) (((!! ((read::String->Int) ((!! 0) a) + 1))) b)) = GT
  | otherwise = cmp_first a b

cmp_first a b
  | ((!! 1) a) == "" = LT
  | ((!! 1) b) == "" = GT  
  | (((!! 1)) a) < (((!! 1)) b) = LT
  | (((!! 1)) a) > (((!! 1)) b) = GT
  | otherwise = EQ

tsort :: ColumnName -> Table -> Table
tsort column table = (head table) : (map tail (sortBy cmp_columns (map ((printf "%d" (get_position column table)):) (tail table))))

-- Task 2

list_compare :: (Eq a) => [a] -> [a] -> Bool
list_compare x y = null (x \\ y) && null (y \\ x)

vunion :: Table -> Table -> Table
vunion t1 t2
  | list_compare (head t1) (head t2) == True = t1 ++ (tail t2)
  | otherwise = t1

-- Task 3

generate2 :: Int -> Row
generate2 size
  | (size == 0 ) = []
  | (size /= 0 ) = "" : generate2 (size-1)

generate1 :: Table -> Int -> Table
generate1 table size
  | (size == 0 ) = []
  | (size /= 0 ) = generate2 (length (head table)) : generate1 table (size-1)

hunion :: Table -> Table -> Table
hunion t1 t2
  | (length t1 < length t2) = zipWith (++) (reverse ((generate1 t1 (length t2 - length t1)) ++ (reverse t1))) t2
  | (length t2 < length t1) = zipWith (++) t1 (reverse ((generate1 t2 (length t1 - length t2)) ++ (reverse t2)))
  | otherwise = zipWith (++) t1 t2

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [["Name","TotalSteps","TotalDistance","VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","10","11","12","13","14","15","16","17"]]

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : [ new_row_function l1 l2 | l1 <- (tail t1), l2 <- (tail t2)]

-- Task 6

--get_position1 :: ColumnName -> [ColumnName] -> Int
get_position1 column list
    | elemIndex column list /= Nothing = (read::String->Int) (maybe "" show (elemIndex column list))
    | otherwise = -1

get_pos :: [ColumnName] -> [ColumnName] -> [Int]
get_pos (x : l1) l2
  | (length l1==0) = [get_position1 x l2]
  | (length l1/=0) = (get_position1 x l2) : (get_pos l1 l2)

prj :: [Int] -> Table -> Table
prj (x : l) table
  | (length l==0) = [map (!! x) table]
  | (length l/=0) = (map (!! x) table) : prj l table

generate :: Int -> Table
generate size
  | (size == 0 ) = []
  | (size /= 0 ) = [] : generate (size-1)

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = foldr (\xs ys -> zipWith (:) xs ys) (generate (length (head (prj (get_pos columns_to_extract (head t)) t)))) (prj (get_pos columns_to_extract (head t)) t)

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (filter (\xs -> condition (((!! (get_position key_column [head t])) xs)) ) (tail t))


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromTable t) = Table t
    eval (AsList colname (FromTable t)) = List (map (!!(get_position colname t)) (tail t))
    eval (Sort colname (FromTable t)) = Table (tsort colname t)
    eval (ValueMap op (FromTable t)) = Table (vmap op t)
    eval (RowMap op colnames (FromTable t)) = Table (colnames : (map op (tail t)))
    eval (VUnion (FromTable t1) (FromTable t2)) = Table (vunion t1 t2)
    eval (HUnion (FromTable t1) (FromTable t2)) = Table (hunion t1 t2)
    eval (TableJoin colname (FromTable t1) (FromTable t2)) = Table (t1)
    eval (Cartesian op colnames (FromTable t1) (FromTable t2)) = Table (cartesian op colnames t1 t2)
    eval (Projection colnames (FromTable t)) = Table (projection colnames t)
    eval (Graph func (FromTable t)) = Table t
    eval (Filter (Eq name nr) (FromTable t)) = Table t
    eval (Filter (Lt name nr) (FromTable t)) = Table t
    eval (Filter (Gt name nr) (FromTable t)) = Table t
    eval (Filter (In str l) (FromTable t)) = Table t
    eval (Filter (FieldEq str1 str2) (FromTable t)) = Table t
    eval (Filter (FNot (Eq name nr )) (FromTable t)) = Table t

-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
  feval colnames (Eq name nr) row = (read::String->Float) ((!!(get_position1 name colnames)) row) == nr
  feval colnames (Lt name nr) row = (read::String->Float) ((!!(get_position1 name colnames)) row) < nr
  feval colnames (Gt name nr) row = (read::String->Float) ((!!(get_position1 name colnames)) row) > nr
  feval colnames (In name list) row = (read::String->Float) ((!!(get_position1 name colnames)) row) `elem` list
  feval colnames (FNot cond) row = not (feval colnames cond row)
  feval colnames (FieldEq name1 name2) row = (read::String->Float) ((!!(get_position1 name1 colnames)) row) == (read::String->Float) ((!!(get_position1 name2 colnames)) row)

instance FEval String where
  feval colnames (Eq name nr) row = ((!!(get_position1 name colnames)) row) == nr
  feval colnames (Lt name nr) row = ((!!(get_position1 name colnames)) row) < nr
  feval colnames (Gt name nr) row = ((!!(get_position1 name colnames)) row) > nr
  feval colnames (In name list) row = ((!!(get_position1 name colnames)) row) `elem` list
  feval colnames (FNot cond) row = not (feval colnames cond row)
  feval colnames (FieldEq name1 name2) row = ((!!(get_position1 name1 colnames)) row) == ((!!(get_position1 name2 colnames)) row)

-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

newList :: [String] -> [String] -> [String]
newList [] _ = []
newList _ [] = []
newList (x:xs) (y:ys) = [x,y] ++ (newList xs ys)

{-
t=physical_activity
a = group_list (length (tail t) - 1) (cartProd2 new_op (tail t) (tail t))
b=map (map convert) a
c= map clear b           
pos=pos_table c b        
pos1 = map (map show) pos
c1 = map (map show) c
names=map head(tail t)    
d=zipWith newList pos1 c1
e=map (group_list 2) d
-}



new_op (n1:l1:_) (n2:l2:_)
  | l1 == l2 = Just l1
  | otherwise = Nothing

convert :: Maybe [Char] -> Int
convert Nothing = -999
convert (Just x) = (read::String->Int) x

clear :: [Int] -> [Int]
clear [] = []
clear (x:xs)
  | x == -999 = clear xs
  | otherwise = x:clear xs

find_pos :: [Int] -> [Int] -> Int -> [Int]
find_pos _ [] acc = []
find_pos [] _ acc = []
find_pos (x:xs) (y:ys) acc
  | acc == 0 = get_position1 x (y:ys) : find_pos xs (drop (get_position1 x (y:ys)+1) (y:ys)) (acc+get_position1 x (y:ys)+1)
  | otherwise = (acc+get_position1 x (y:ys)) : find_pos xs (drop (get_position1 x (y:ys)+1) (y:ys)) (acc+get_position1 x (y:ys)+1)

pos_table :: [[Int]] -> [[Int]] -> [[Int]]
pos_table _ [] = []
pos_table [] _ = []
pos_table (x:xs) (y:ys) = (find_pos x y 0) : (pos_table xs ys)

cartProd2 new_op xs ys = [new_op x y | x <- xs, y <- ys,x/=y]

use_op :: Table -> EdgeOp -> [Maybe Int]
use_op t op = undefined

edgeop [_,_,z] [_,_,c]
   | z == c = Just c
   | otherwise = Nothing

-- 3.5
similarities_query :: Query
similarities_query = FromTable [["a","b","c"]]

-- 3.6 (Typos)

-- functie ce intoarce distanta intre oricare 2 cuvinte 
cartProd xs ys = [distance x y | x <- xs, y <- ys]

--functie ce transfoma o lista mai mare in liste de dimensiuni dorite
group_list :: Int -> [a] -> [[a]]
group_list _ [] = []
group_list n l = (take n l) : (group_list n (drop n l))

--functie ce adauga fiecar element dintr-o lista la inceputul randului din tabel
share :: [String] -> Table -> Table
share [] [] = []
share (x:xs) [] = []
share [] (y:ys) = y:ys
share (x:xs) (y:ys) = (x:y) : share xs ys

swap :: [String] -> Table -> Table
swap col t
  | head col == head (head t) = share col (map tail t)
  | otherwise = share (map head t) (swap col (map tail t))

--functie ce intoarce pozitia uni element itr-o lista
int_pos :: Eq a => a -> [a] -> Int
int_pos column list
    | elemIndex column list /= Nothing = (read::String->Int) (maybe "" show (elemIndex column list))
    | otherwise = -1

--functie ce intoarce numele persoanelor de pe pozitiile date din tabela
get_name :: [Int] -> [String] -> [String]
get_name (x:xs) [] = []
get_name [] (y:ys) = []
get_name (x:xs) (y:ys) = ((!! x) (y:ys)) : get_name xs (y:ys)

--functie ce calculeaza "distanta" dintre 2 cuvinte 
distance :: Eq a => [a] -> [a] -> Int
distance xs ys = table ! (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
    
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]

{-
a = tail (map (!!(get_position "Name" t1)) t1)
b = tail (map (!!(get_position "Name" t2)) t2)
groups = group_list (length b) (cartProd a  b)
pos = initial_pos (a \\ b) a
mins = map minimum (group_list (length b) (cartProd a b))
poz = zipWith int_pos mins groups
names = get_name poz b
swap ("Name":names) t1  
-}

-- in realizarea task-ului am urmat intocmai pasii descrisi in tema
-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = swap (col : (get_name (zipWith int_pos (map minimum (group_list (length b) (cartProd a b))) (group_list (length b) (cartProd a  b))) b)) csv1 
  where
    a = tail (map (!!(get_position col csv1)) csv1)
    b = tail (map (!!(get_position col csv2)) csv2)

