import System.Environment
import System.IO

main = do
    putStrLn "Enter a formula to parse:"
    formula <- getLine
    putStrLn $ process $ replace formula

process :: String -> String
process s
  | not parsed = "Sorry, the string you entered isn't well-formed"
  | otherwise = "\nFormula: " ++ s ++ "\nMain operator: " ++ (mainop tree) ++ "\nImmediate subformulas: "++(subformulas tree)++"\nParse tree: " ++ show tree ++"\n"
  where
    parsed = parse s
    tree = parseTr s

-- PARSING LIBRARY

myelem :: (Eq a) => a -> [a] -> Bool
myelem y (x:xs) = if y==x then True else myelem y xs
myelem y [] = False

tostring :: Char -> String
tostring x = x:[]

isname :: Char -> Bool
isname x = myelem x ['a'..'t']

isvar :: Char -> Bool
isvar x = myelem x "uwxyz"

istrm :: Char -> Bool
istrm x = (isname x) || (isvar x)

isbinop :: Char -> Bool
isbinop x = myelem x "&v>$"

isunop :: Char -> Bool
isunop x = myelem x "~"

isparen :: Char -> Bool
isparen x = myelem x "()"

isQ :: Char -> Bool
isQ x = (x== 'E')||(x=='A')

isprop :: Char -> Bool
isprop x = myelem x ['A'..'Z']

isat :: String -> Bool
isat "" = False
isat (x:[]) = isprop x
isat (x:xs) = isprop x && (ckargs xs)
  where
    ckargs (x:[]) = istrm x
    ckargs (x:xs) = (istrm x) && (ckargs xs)


-- takes a string and returns a list of pairs containing each string char with its index
idx :: String -> [(Char, Int)]
idx str = idx' str 0 []
  where
    idx' [] i out = out
    idx' (x:[]) i out = out++[(x,i)]
    idx' (x:xs) i out = idx' xs (i+1) (out++[(x,i)])


-- takes a string and returns a list of pairs containing each string char with its depth
-- i.e. how many opening parentheses it is in the scope of
depth :: String -> [(Char, Int)]
depth str = depth' str 0 []
  where
    depth' [] i out = []
    depth' (x:[]) i out
      | x=='(' = out++[(x,i+1)]
      | x==')' = out++[(x,i)]
      | otherwise = out++[(x,i)]
    depth' (x:xs) i out
      | x=='(' = depth' xs (i+1) (out++[(x,i+1)])
      | x==')' = depth' xs (i-1) (out++[(x,i)])
      | otherwise = depth' xs i (out++[(x,i)])

charAt :: String -> Int -> Char
charAt [] n = error "Empty string"
charAt ls n =
  let flt = [x | (x,y) <- (idx ls), y==n]
  in if flt==[] then error "Index out of bounds" else head flt


-- takes a string and returns a list of triples where the first element is
-- the character, the second is its index in the string, the third represents
-- its depth, i.e. how many opening parentheses it is in the scope of
split :: String -> [(Char, Int, Int)]
split s = comb (idx s) (depth s) []
  where
    comb [] y out = out
    comb (x:xs) (y:ys) out = comb xs ys (out++[(fst x, snd x, snd y)])

-- finds any binary connectives with depth of 1
mainbc :: String -> [(Char,Int)]
mainbc s = [(a,b) | (a,b,c) <- (split s), (isbinop a), c==1]

-- determines if the parentheses in a string are balanced
-- NB this doesn't get used, included here just because
bparen :: String -> Bool
bparen s = bp s []
  where
    bp [] stk = (stk == [])
    bp (x:xs) stk
      | x=='(' = bp xs (x:stk)
      | x==')' = if stk==[] then False else bp xs (tail stk)
      | otherwise = bp xs stk


-- takes a string and an int and breaks string at the int index, with the
-- char at the index going into the second returned string
-- returns [""] if given empty string or index out of bounds
brkat :: String -> Int -> [String]
brkat "" n = [""] -- empty string input
brkat s 0 = ["",s]
brkat s n = br s "" n 0
  where
    br s1 s2 i c
      | i==c = [s2, s1]
      | s1==[] = [""] -- index out of bounds
      | otherwise = br (tail s1) (s2++[head s1]) i (c+1)

-- takes a string and cleans it up: replacing "<>" with "$", "^" with "&", and
-- removing white space
replace :: String -> String
replace s = rep s ""
  where
    rep s1 s2
      | s1==[] = s2
      | (head s1) == '<' && (head (tail s1))=='>' = rep (tail (tail s1)) (s2++"$")
      | (head s1) == '^' = rep (tail s1) (s2++"&")
      | (head s1) == ' ' = rep (tail s1) s2
      | otherwise = rep (tail s1) (s2++[head s1])


-- Takes a string, assumed to begin with '(' and end with ')' and attempts to
-- find a binary connective enclosed by those parentheses along with the
-- strings to its left and right.  Returns [""] if the attempt fails
bcfrm :: String -> [String]
bcfrm s
  | m==[] = [""]
  | v==[""] = [""]
  | (length v1)<=1 = [""]
  | (length v2)<=2 = [""]
  | otherwise = [s1, s2, s3]
  where
    m = mainbc s
    v = brkat s (snd (head m))
    v1 = head v
    v2 = head (tail v)
    s1 = tail v1
    s2 = tostring (head v2)
    s3 = init (tail v2)

-- Takes a string and determines if its a wff of FOL
parse :: String -> Bool
parse s = parse' (replace s)
  where
    parse' "" = False
    parse' s@(x:xs)
      | xs==[] = isat (tostring x)
      | (x=='(' && (last xs)==')') = let f = bcfrm s in if f==[""] then False else (parse' (f!!0)) && (parse' (f!!2))
      | x=='~' = parse' xs
      | (isQ x) = if isvar (head xs) then parse' (tail xs) else False
      | otherwise = isat s


data Tree a = EmptyTree | Leaf a | Node (Tree a) (Tree a) (Tree a) deriving (Read, Eq)

parseTr :: String -> Tree String
parseTr "" = EmptyTree
parseTr s@(x:xs)
    | isat s = Leaf s
    | x=='(' = let f = bcfrm s in Node (parseTr (f!!0)) (Leaf (f!!1)) (parseTr (f!!2))
    | x=='~' = Node (Leaf "~") (parseTr xs) EmptyTree
    | isQ x  = Node (Leaf (tostring x ++ tostring (head xs))) (parseTr (tail xs)) EmptyTree
    | otherwise = EmptyTree


instance (Show a) => Show (Tree a) where
  show EmptyTree = ""
  show (Leaf a) = show a
  show (Node t1 t2 EmptyTree) = "["++(show t1)++" "++(show t2)++"]"
  show (Node t1 t2 t3) = "["++(show t1)++" "++(show t2)++" "++(show t3)++"]"

mainop :: Tree String -> String
mainop EmptyTree = ""
mainop (Leaf a) = "none"
mainop (Node a _ EmptyTree) = show a
mainop (Node _ b _) = show b

unparse :: Tree String -> String
unparse EmptyTree = ""
unparse (Leaf a) = a
unparse (Node a b EmptyTree) = (unparse a) ++ (unparse b)
unparse (Node a b c) = "("++(unparse a)++(unparse b)++(unparse c)++")"

subformulas :: Tree String -> String
subformulas (Leaf a) = "none"
subformulas (Node a b EmptyTree) = unparse b
subformulas (Node a b c) = (unparse a)++", "++(unparse c)
