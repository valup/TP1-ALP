module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado nulo
-- Completar la definición
initState :: State
initState = (M.empty, " - ")

f :: (a, b) -> a
f = Prelude.fst

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v $ f s of
                Just n -> Right n
                _      -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x v (s, t) = (M.insert x v s, t)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace str (s, t) = (s, t ++ str)

-- Funcion auxiliar para añadir a la traza
trcStr :: Variable -> Int -> String
trcStr v n = "Let " ++ v ++ " " ++ show n ++ " - "

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s =  case stepComm c s of
                        Right (c' :!: s') -> stepCommStar c' s'
                        Left err          -> Left err

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)

stepComm Skip s                 = Right (Skip :!: s)

stepComm (Let v e) s            = case evalExp e s of
                                    Right (n :!: s') -> let s''  = update v n s'
                                                            trc  = trcStr v n
                                                            s''' = addTrace trc s''
                                                        in Right (Skip :!: s''')
                                    Left err         -> Left err

stepComm (Seq Skip c1) s        = Right (c1 :!: s)

stepComm (Seq c0 c1) s          = case stepComm c0 s of
                                    Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')
                                    Left err         -> Left err

stepComm (IfThenElse p c0 c1) s = case evalExp p s of
                                    Right (b :!: s') -> if b then Right (c0 :!: s')
                                                             else Right (c1 :!: s')
                                    Left err         -> Left err

stepComm r@(Repeat c b) s       = let ite = IfThenElse b Skip r
                                  in Right (Seq c ite :!: s)


-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)

evalExp (Const n) s     = Right (n :!: s)

evalExp (Var x) s       = case lookfor x s of
                            Right n  -> Right (n :!: s)
                            _        -> Left UndefVar

evalExp (UMinus e) s    = case evalExp e s of
                            Right (n :!: s') -> Right (- n :!: s')
                            Left err         -> Left err

evalExp (Plus e0 e1) s  = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 + n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (Minus e0 e1) s = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 - n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (Times e0 e1) s = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 * n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (Div e0 e1) s   = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') ->
                                        if n1 == 0
                                            then Left DivByZero 
                                            else Right (div n0 n1 :!: s'')
                                    Left err -> Left err
                            Left err -> Left err

evalExp (ESeq e0 e1) s  = case evalExp e0 s of
                            Right (_ :!: s') -> evalExp e1 s'
                            Left err         -> Left err

evalExp (EAssgn v e) s  = case evalExp e s of
                            Right (n :!: s') ->
                                let s'' = update v n s'
                                    trc = trcStr v n
                                    s''' = addTrace trc s''
                                in Right (n :!: s'')
                            Left err -> Left err

evalExp BTrue s       = Right (True :!: s)

evalExp BFalse s      = Right (False :!: s)

evalExp (Lt e0 e1) s  = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 < n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (Gt e0 e1) s  = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 > n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (And p0 p1) s = case evalExp p0 s of
                            Right (b0 :!: s') ->
                                case evalExp p1 s' of
                                    Right (b1 :!: s'') -> Right (b0 && b1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (Or p0 p1) s  = case evalExp p0 s of
                            Right (b0 :!: s') ->
                                case evalExp p1 s' of
                                    Right (b1 :!: s'') -> Right ((b0 || b1) :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (Not p) s     = case evalExp p s of
                            Right (b :!: s') -> Right (not b :!: s')
                            Left err         -> Left err

evalExp (Eq e0 e1) s  = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 == n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err

evalExp (NEq e0 e1) s = case evalExp e0 s of
                            Right (n0 :!: s') ->
                                case evalExp e1 s' of
                                    Right (n1 :!: s'') -> Right (n0 /= n1 :!: s'')
                                    Left err           -> Left err
                            Left err -> Left err


