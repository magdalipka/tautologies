data TreeNode
  = Const Bool
  | And TreeNode TreeNode
  | Or TreeNode TreeNode
  | Not TreeNode
  | Implication TreeNode TreeNode
  | Eq TreeNode TreeNode

eval :: TreeNode -> Bool
eval (Const a) = a
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Not a) = not (eval a)
eval (Implication a b) = eval b || not (eval a) && not (eval b)
eval (Eq a b) = eval a && eval b || not (eval a) && not (eval b)

isUnaryOperator :: Char -> Bool
isUnaryOperator '!' = True
isUnaryOperator _ = False

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

buildTree :: String -> [String] -> TreeNode
buildTree [] stack = Const True
buildTree [x] stack = if x == '1' then Const True else Const False
buildTree (x : xs) stack
  | isOperator x = Const True
  | isBracket x = Const True
  | isOperand x = Const True
  | otherwise = error "Incorrect character."
