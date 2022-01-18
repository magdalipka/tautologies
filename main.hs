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

buildTree :: String -> [String] -> TreeNode
buildTree [] stack = Const True
buildTree [x] stack = if x == '1' then Const True else Const False
buildTree (x : xs) stack
  | isOperator x = Const True
  | isBracket x = Const True
  | isOperand x = Const True
  | otherwise = error "Incorrect character."
