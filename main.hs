module Expressions where

import System.Environment (getArgs)
import System.IO

data TreeNode
  = Const Char
  | And TreeNode TreeNode
  | Or TreeNode TreeNode
  | Not TreeNode
  | Implication TreeNode TreeNode
  | Eq TreeNode TreeNode

eval :: TreeNode -> (Bool, Bool) -> Bool
eval (Const 'p') (p, q) = p
eval (Const 'q') (p, q) = q
eval (And a b) (p, q) = eval a (p, q) && eval b (p, q)
eval (Or a b) (p, q) = eval a (p, q) || eval b (p, q)
eval (Not a) (p, q) = not (eval a (p, q))
eval (Implication a b) (p, q) = eval a (p, q) <= eval b (p, q)
eval (Eq a b) (p, q) = eval a (p, q) == eval b (p, q)
eval _ _ = error "Incorrect evaluation input"

doubleTreeNode :: Char -> TreeNode -> TreeNode -> TreeNode
doubleTreeNode '+' a b = Or a b
doubleTreeNode '*' a b = And a b
doubleTreeNode '>' a b = Implication a b
doubleTreeNode '#' a b = Eq a b
doubleTreeNode x _ _ = error "Incorrect doubleTreeNode input"

singleTreeNode :: Char -> TreeNode -> TreeNode
singleTreeNode '!' a = Not a
singleTreeNode x _ = error "Incorrect singleTreeNode input"

constTreeNode :: Char -> TreeNode
constTreeNode = Const

priority :: Char -> Int
priority '!' = 5
priority '+' = 4
priority '*' = 3
priority '>' = 2
priority '#' = 1
priority x
  | isOperand x = 5
  | otherwise = 0

isUnaryOperator :: Char -> Bool
isUnaryOperator '!' = True
isUnaryOperator _ = False

isDoubleOperator :: Char -> Bool
isDoubleOperator x = isOperator x && not (isUnaryOperator x)

isLeftJoiningOperator :: Char -> Bool
isLeftJoiningOperator '+' = True
isLeftJoiningOperator '*' = True
isLeftJoiningOperator _ = False

isRightJoiningOperator :: Char -> Bool
isRightJoiningOperator '#' = True
isRightJoiningOperator '>' = True
isRightJoiningOperator '!' = True
isRightJoiningOperator _ = False

isOperator :: Char -> Bool
isOperator '*' = True
isOperator '+' = True
isOperator '!' = True
isOperator '>' = True
isOperator '#' = True
isOperator _ = False

isBracket :: Char -> Bool
isBracket '(' = True
isBracket ')' = True
isBracket _ = False

isOperand :: Char -> Bool
isOperand 'q' = True
isOperand 'p' = True
isOperand _ = False

data State = Zero | One | Two

validateExpression :: State -> Int -> String -> Bool
validateExpression One margins [] = margins == 0
validateExpression _ margins [] = False
validateExpression Zero margins (x : xs)
  | margins < 0 = False
  | x == '(' = validateExpression Zero (margins + 1) xs
  | isUnaryOperator x = validateExpression Two margins xs
  | isOperand x = validateExpression One margins xs
  | otherwise = False
validateExpression One margins (x : xs)
  | margins < 0 = False
  | x == '(' = False
  | x == ')' = validateExpression One (margins - 1) xs
  | isDoubleOperator x = validateExpression Zero margins xs
  | otherwise = False
validateExpression Two margins (x : xs)
  | margins < 0 = False
  | x == '(' = validateExpression Zero (margins + 1) xs
  | isUnaryOperator x = validateExpression Two margins xs
  | isOperand x = validateExpression One margins xs
  | otherwise = False

popOne :: [a] -> ([a], a)
popOne x = (init x, last x)

popTwo :: [a] -> ([a], a, a)
popTwo x = (init (init x), last (init x), last x)

buildTree :: [TreeNode] -> String -> TreeNode
buildTree stack [] = last stack
buildTree stack (x : xs)
  | isUnaryOperator x = buildTree (singlePoppedStack ++ [singleTreeNode x child]) xs
  | isDoubleOperator x = buildTree (doublePoppedStack ++ [doubleTreeNode x left right]) xs
  | isOperand x = buildTree (stack ++ [constTreeNode x]) xs
  | otherwise = error "Incorrect character."
  where
    (doublePoppedStack, left, right) = popTwo stack
    (singlePoppedStack, child) = popOne stack

infixToPostfix :: [Char] -> String -> String -> String
infixToPostfix [] result [] = result
infixToPostfix stack result [] = infixToPostfix (init stack) (result ++ [last stack]) []
infixToPostfix stack result (x : xs)
  | isOperand x = infixToPostfix stack (result ++ [x]) xs
  | x == '(' = infixToPostfix (stack ++ [x]) result xs
  | isLeftJoiningOperator x = infixToPostfix (leftProcessedStack ++ [x]) leftProcessedResult xs
  | isRightJoiningOperator x = infixToPostfix (rightProcessedStack ++ [x]) rightProcessedResult xs
  | x == ')' = infixToPostfix (init bracketProcessedStack) bracketProcessedResult xs
  | otherwise = ""
  where
    (leftProcessedStack, leftProcessedResult) = processLeftJoining x stack result
    (rightProcessedStack, rightProcessedResult) = processRightJoining x stack result
    (bracketProcessedStack, bracketProcessedResult) = processBracket stack result

processLeftJoining :: Char -> [Char] -> String -> ([Char], String)
processLeftJoining symbol [] result = ([], result)
processLeftJoining symbol stack result
  | priority (last stack) >= priority symbol = processLeftJoining symbol (init stack) (result ++ [last stack])
  | otherwise = (stack, result)

processRightJoining :: Char -> [Char] -> String -> ([Char], String)
processRightJoining symbol [] result = ([], result)
processRightJoining symbol stack result
  | priority (last stack) > priority symbol = processRightJoining symbol (init stack) (result ++ [last stack])
  | otherwise = (stack, result)

processBracket :: [Char] -> String -> ([Char], String)
processBracket [] result = ([], result)
processBracket stack result
  | last stack == '(' = (stack, result)
  | otherwise = processBracket (init stack) (result ++ [last stack])

multiplyWords :: [String] -> [String] -> [String]
multiplyWords a b = (++) <$> a <*> b

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | x <- l, y <- l]

iter :: (Int, a -> a) -> (a -> a)
iter (1, f) x = x
iter (n, f) x = f (iter (n - 1, f) x)

allWords :: Int -> [String] -> [String]
allWords n alphabet = iter (n, multiplyWords alphabet) alphabet

isTautology :: TreeNode -> Bool
isTautology t = all (eval t) (pairs [True, False])

isExpressionACorrectTautology :: String -> Bool
isExpressionACorrectTautology e = validateExpression Zero 0 e && isTautology (buildTree [] (infixToPostfix [] [] e))

allTautologies :: Int -> [String]
allTautologies 0 = []
allTautologies n = allTautologies (n - 1) ++ filter isExpressionACorrectTautology (allWords n ["p", "q", "*", "+", ">", "#", "!", "(", ")"])

main = do
  (maxLength : fileName : _) <- getArgs
  let n = read maxLength :: Int
  fileHandle <- openFile fileName WriteMode
  hPrint fileHandle (allTautologies n)
  hClose fileHandle
