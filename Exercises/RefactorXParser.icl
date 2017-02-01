implementation module RefactorXParser

import StdEnv
import StdDebug
import StdMaybe
import RefactorX

Start :: [Expr]
Start = map fromString [E1,E2,E3,E4,E5]

E1 = "(let x = 42 - 3 in x / 0) + (let y = 6 in y * y)"
E2 = "let x = 42 in x + (let x = 58 in x)"
E3 = "let x = 1 in let y = 2 in let x = 3 in 4"
E4 = "let x = 1 in x + y"
E5 = "(let x = 1 in x) * x"

instance fromString Expr
where fromString str 
        = case fromString str of Nothing = abort "Parse error!"
                                 Just x  = x

instance fromString (Maybe Expr)
where fromString str 
        = case parseExpr (fromString str) of 
	          (Just e,[]) = Just e
	          _ = Nothing

/* =================== PARSER ============== */

:: Parser a :== [Char] -> (Maybe a, [Char])
:: Prio :== Int

parseExpr :: Parser Expr
parseExpr = parseExprLevel 1

parseToken :: (Char->Bool) [Char] -> (Maybe String, [Char])
parseToken pred [c:cs] | pred c = (Just (toString [c:takeWhile pred cs]), strip (dropWhile pred cs))
parseToken pred cs = (Nothing, cs)

parseNum :: Parser Int
parseNum = produce (\s->s o toInt) :=> parseSign :=> parseToken (\x->isDigit x)

parseSign :: Parser (Int->Int)
parseSign = either [ produce sign :=> parseToken (\x->isMember x ['+-']), empty ]
where sign s = (*) if (isEven (length (filter ((==) '-') (fromString s)))) +1 -1

parseVar :: Parser Name
parseVar = parseToken isAlphanum

parseExprLevel :: Prio -> Parser Expr
parseExprLevel n = produce (\t f->f t) :=> parseTerm n :=> parseDyadTail n

parseTerm :: Prio -> Parser Expr
parseTerm n | n>=0 = parseExprLevel (n-1)
parseTerm _ = either [ empty  :-> expect ['('] :=> parseExpr :-> expect [')']
                     , produce LET :-> expect ['let'] :=> parseVar :-> expect ['='] :=> parseExpr :-> expect ['in'] :=> parseExpr
                     , produce NR  :=> parseNum 
                     , produce VAR :=> parseVar
                     ]

parseDyadTail :: Prio -> Parser (Expr->Expr)
parseDyadTail n = either [ produce leftAssociative :=> parseOp n :=> parseTerm n :=> parseDyadTail n
                         , empty 
                         ]

parseOp :: Prio -> Parser Operator
parseOp 0 = either [ produce DIV  :-> expect ['/']
                   , produce MUL  :-> expect ['*'] 
                   ]
parseOp 1 = either [ produce PLUS :-> expect ['+']
                   , produce MIN  :-> expect ['-'] 
                   ]
parseOp _ = fail :-> expect ['the spanish inquisition']

// 

:: Void = Void
expect :: [Char] [Char] -> (Maybe Void, [Char])
expect lit str 
| take (length lit) str` == lit 
  = (Just (abort "concrete syntax"), strip (drop (length lit) str`))
| otherwise 
  = (Nothing, str)
where str` = strip str

leftAssociative :: Operator Expr (Expr->Expr) Expr -> Expr
leftAssociative op rhs f lhs = f (OP lhs op rhs)
rightAssociative :: Operator Expr (Expr->Expr) Expr -> Expr
rightAssociative op rhs f lhs = OP lhs op (f rhs)

// 

produce :: f -> Parser f
produce f = \str->(Just f, strip str)

fail :: Parser a
fail = \str->(Nothing, str)

empty :: Parser (a->a)
empty = produce id

either :: [Parser a] -> Parser a
either ps = \str->hd (filter (isJust o fst) (map (\f->f str) ps) ++ [fail str])

(:->) infixl 8 :: (Parser a) (Parser b) -> Parser a
(:->) pa pb = produce const :=> pa :=> pb

(:=>) infixl 8 :: (Parser (a->b)) (Parser a) -> Parser b
(:=>) pab pa = \str -> case pab str of (ab, str`) = ap str ab (pa str`)
where
    ap :: [Char] (Maybe (a->b)) (Maybe a,[Char]) -> (Maybe b, [Char])
    ap str Nothing  _             = (Nothing, str)
    ap _   (Just f) (Just x,rest) = (Just (f x), rest)
    ap _   _        (_,str)       = (Nothing, str)

strip :: ([Char] -> [Char])
strip = dropWhile ((==) ' ')

/*
E1 = (let x = 42 - 3 in x / 0) + (let y = 6 in y * y)
E2 = let x = 42 in x + (let x = 58 in x)
E3 = let x = 1 in let y = 2 in let x = 3 in 4
E4 = let x = 1 in x + y
E5 = (let x = 1 in x) * x
*/

