data TreeNode
  = Const Char
  | And TreeNode TreeNode
  | Or TreeNode TreeNode
  | Not TreeNode
  | Implication TreeNode TreeNode
  | Eq TreeNode TreeNode

eval :: Bool -> Bool -> TreeNode -> Bool
eval p q (Const 'p') = p
eval p q (Const 'q') = q
eval p q (And a b) = eval p q a && eval p q b
eval p q (Or a b) = eval p q a || eval p q b
eval p q (Not a) = not (eval p q a)
eval p q (Implication a b) = eval p q a <= eval p q b
eval p q (Eq a b) = eval p q a == eval p q b
eval _ _ _ = error "Incorrect evaluation input"

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
priority _ = error "Incorrect priority input"

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

validateExpression :: String -> State -> Int -> Bool
validateExpression [] One _ = False
validateExpression [] _ margins
  | margins == 0 = True
  | otherwise = False
validateExpression (x : xs) Zero margins
  | margins < 0 = False
  | x == '(' = validateExpression xs Zero (margins + 1)
  | isUnaryOperator x = validateExpression xs Two margins
  | isOperand x = validateExpression xs One margins
  | otherwise = False
validateExpression (x : xs) One margins
  | margins < 0 = False
  | x == '(' = False
  | x == ')' = validateExpression xs One (margins - 1)
  | isOperator x = validateExpression xs Zero margins
  | otherwise = False
validateExpression (x : xs) Two margins
  | margins < 0 = False
  | x == '(' = False
  | x == ')' = validateExpression xs One (margins - 1)
  | isOperator x = validateExpression xs Zero margins
  | otherwise = False

popOne :: [a] -> ([a], a)
popOne x = (init x, last x)

popTwo :: [a] -> ([a], a, a)
popTwo x = (init (init x), last (init x), last x)

buildTree :: String -> [TreeNode] -> TreeNode
buildTree [] stack = last stack
buildTree (x : xs) stack
  | isUnaryOperator x = buildTree xs (singlePoppedStack ++ [singleTreeNode x child])
  | isDoubleOperator x = buildTree xs (doublePoppedStack ++ [doubleTreeNode x left right])
  | isOperand x = buildTree xs (stack ++ [constTreeNode x])
  | otherwise = error "Incorrect character."
  where
    (doublePoppedStack, left, right) = popTwo stack
    (singlePoppedStack, child) = popOne stack

infixToPostfix :: String -> [Char] -> String -> String
infixToPostfix [] [] result = result
infixToPostfix [] stack result = infixToPostfix [] (init stack) (result ++ [last stack])
infixToPostfix (x : xs) stack result
  | isOperand x = infixToPostfix xs stack (result ++ [x])
  | x == '(' = infixToPostfix xs (stack ++ [x]) result
  | isLeftJoiningOperator x = infixToPostfix xs (leftProcessedStack ++ [x]) leftProcessedResult
  | isRightJoiningOperator x = infixToPostfix xs (rightProcessedStack ++ [x]) rightProcessedResult
  | x == ')' = infixToPostfix xs (init bracketProcessedStack) bracketProcessedResult
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
  | priority (last stack) > priority symbol = processLeftJoining symbol (init stack) (result ++ [last stack])
  | otherwise = (stack, result)

processBracket :: [Char] -> String -> ([Char], String)
processBracket [] result = ([], result)
processBracket stack result
  | last stack == '(' = (stack, result)
  | otherwise = processBracket (init stack) (result ++ [last stack])
