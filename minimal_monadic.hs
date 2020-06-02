{- This file implements a simple monadic interpreter.
 -}

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import qualified Control.Monad.Fail as Fail

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

newtype Interp a = Interp { runInterp :: Store -> Either String (a, Store) }

instance Functor Interp where
  fmap = liftM -- imported from Control.Monad

instance Applicative Interp where
  pure  = return
  (<*>) = ap -- imported from Control.Monad

instance Monad Interp where
  return x = Interp $ \r -> Right (x, r)
  i >>= k  = Interp $ \r -> case runInterp i r of
               Left msg      -> Left msg
               Right (x, r') -> runInterp (k x) r'

instance Fail.MonadFail Interp where
  fail msg = Interp $ \_ -> Left msg

rd :: Var -> Interp Val
rd x = Interp $ \r -> case lookup x r of
         Nothing -> Left ("unbound variable `" ++ x ++ "'")
         Just v  -> Right (v, r)


wr :: Var -> Val -> Interp ()
wr x v = Interp $ \r -> Right ((), (x, v) : r)

eval :: Exp -> Interp Val
eval (C n)       = do return n
eval (V x)       = do rd x
eval (e1 :+: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (e1 :-: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 - v2)
eval (e1 :*: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval (e1 :/: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0
                        then fail "division by zero"
                        else return (v1 `div` v2)

exec :: Stmt -> Interp ()
exec (x := e)       = do v <- eval e
                         wr x v
exec (While e s)    = do v <- eval e
                         when (v /= 0) (exec (Seq [s, While e s]))
exec (Seq [])       = do return ()
exec (Seq (s : ss)) = do exec s
                         exec (Seq ss)

run :: Prog -> Store -> Either String Store
run p r = case runInterp (exec p) r of
            Left msg      -> Left msg
            Right (_, r') -> Right (nubBy ((==) `on` fst) r')
