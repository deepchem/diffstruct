import Data.Function
import Data.List


type Var = String

infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
  = C Int           --- Constant
  | V Var           --- Variable
  | Exp :+: Exp     --- Addition
  | Exp :-: Exp     --- Subtraction
  | Exp :*: Exp     --- Multiplication
  | Exp :/: Exp     --- Division

infix 1 := 

data Stmt
  = Var := Exp      --- Assignment
  | While Exp Stmt  --- Loop
  | Seq [Stmt]      --- Sequence

type Prog = Stmt

type Val = Int
type Store = [(Var, Val)]

eval :: Exp -> Store -> Val
eval (C n) r       = n
eval (V x) r       = case lookup x r of
                       Nothing -> error ("unbound variable `" ++ x ++ "'")
                       Just v  -> v
eval (e1 :+: e2) r = eval e1 r + eval e2 r
eval (e1 :-: e2) r = eval e1 r - eval e2 r
eval (e1 :*: e2) r = eval e1 r * eval e2 r
eval (e1 :/: e2) r = eval e1 r `div` eval e2 r

exec :: Stmt -> Store -> Store
exec (x := e) r                    = (x, eval e r) : r
exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
                   | otherwise     = r
exec (Seq []) r                    = r
exec (Seq (s:ss)) r                = exec (Seq ss) (exec s r)

run :: Prog -> Store -> Store
run p r = nubBy ((==) `on` fst) (exec p r)
