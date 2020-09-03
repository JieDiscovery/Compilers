module Interpreter where

import Control.Monad
import Control.Exception

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import CMM.Abs
import CMM.Print
import CMM.ErrM

type Env = (Sig, [Context])
type Sig = Map Id Def
type Context = Map Id Val

data Val 
    = VInt Integer 
    | VDouble Double 
    | VBool Bool 
    | Void
    deriving (Eq, Ord, Read)

instance Show Val where
   show (VInt integer)   = show integer
   show (VDouble double) = show double
   show (VBool boolean)  = show boolean
   show Void             = show ""

data Myexception = ERROR String deriving (Show, Typeable)
instance Exception Myexception

interpret :: Program -> IO ()
interpret (PDefs (defs)) = do
    env <- addBuildin emptyEnv defs
    (DFuns _ id args stms) <- lookupFun env (Id "main") 
    execBlock (newBlock env) stms
    return ()

-- | Add Functions into the environment
addFuns :: Env -> [Def] -> IO Env
addFuns env []                           = return env
addFuns env ((DFuns t id args stms):defs) = do
    env1 <- updateFun env (DFuns t id args stms)
    addFuns env1 defs

-- | Add build-in functions
addBuildin :: Env -> [Def] -> IO Env
addBuildin env defs = do
    env1 <- updateFun env  (DFuns Tvoid (Id "printInt") ((ADecl (Tint) (Id "x")):[]) [])
    env2 <- updateFun env1 (DFuns Tvoid (Id "printDouble") ((ADecl (Tdouble) (Id "x")):[]) [])
    env3 <- updateFun env2 (DFuns Tint (Id "readInt") [] [])
    env4 <- updateFun env3 (DFuns Tdouble (Id "readDouble") [] [])
    addFuns env4 defs

-- | Check the value of a variable
lookupVar :: Env -> Id -> IO Val
lookupVar (sig, [])   id = throw (ERROR ("uninitialized variable " ++ show id))
lookupVar (sig, c:cs) id = case Map.lookup id c of
    Just val -> case val of 
                Void -> throw (ERROR ("uninitialized variable " ++ show id))
                otherwise -> return val
    Nothing -> lookupVar (sig, cs) id

-- | Check the definition of a function
lookupFun :: Env -> Id -> IO Def
lookupFun (sig, c) id = case Map.lookup id sig of
    Just def ->  return def  
    Nothing -> throw (ERROR ("Function " ++ (show id) 
                                  ++ " has not been declared"))

-- | add a new Variable to the current context
addVar :: Env -> Id -> Val -> IO Env
addVar (sig, [])   id val = return (sig, Map.insert id val Map.empty:[])
addVar (sig, c:cs) id val = return (sig, Map.insert id val c:cs)

updateVar :: Env -> Id -> Val -> IO Env
updateVar (sig, [])   id val = return (sig,[])
updateVar (sig, c:cs) id val = case Map.lookup id c of
                                Just x  -> do
                                    return(sig, Map.insert id val c:cs)
                                   
                                Nothing -> do 
                                (sig1, cs1) <- updateVar (sig, cs) id val
                                return (sig1, (c:cs1))

updateReturn :: Env -> Id -> Val -> IO Env
updateReturn (sig, [])   id val = return (sig,[])
updateReturn (sig, c:cs) id val = case Map.lookup id c of
                                Just x  -> do
                                    return(sig, c:cs)
                                   
                                Nothing -> do addVar (sig, c:cs) id val
                              
-- | Insert a new function if not present
updateFun :: Env -> Def -> IO Env
updateFun (sig, c) (DFuns t id args stms) = case Map.lookup id sig of
    Just c1 -> throw (ERROR ("Function " ++ (show id) 
                                  ++ " has not been declared"))
    Nothing -> return ((Map.insert id (DFuns t id args stms) sig), c)

-- | Introduce a new block context for a given function
newBlock  :: Env -> Env
newBlock (sig, cs) = (sig, Map.empty:cs)

-- | Create an empty environment
emptyEnv  :: Env
emptyEnv = (Map.empty, []) 

-- | Execute a statement
execStm :: Env -> Stm -> IO Env
execStm env stm = case stm of
    SExp exp  -> do
        (val, env) <- evalExp env exp
        return env
    SDecls typ [] -> do
        return env
    SDecls typ (i:is) -> do  
        env1 <- addVar env i Void      
        execStm env1 (SDecls typ is)
    SWhile exp stm  -> do
        (VBool bool, env1) <- evalExp env exp
        if bool then do
            (sig, c:cs) <- execStm (newBlock env1) stm
            case Map.lookup (Id "return") c of
                Just val -> propagateReturn (sig, c:cs)
                Nothing  -> execStm (sig, cs) (SWhile exp stm) 
        else return env1;
    SInit typ id exp -> do 
        (val, env1) <- evalExp env exp
        env2 <- addVar env1 id val
        updateVar env2 id val

    SReturn exp -> do 
        (val, (sig, c:cs)) <- evalExp env exp
        let env2 = (sig, Map.insert (Id "return") val c:cs)
        return env2
        
    SBlock  stms -> do
        env1 <- execBlock (newBlock env) stms
        propagateReturn env1
   
    SIfElse exp stm1 stm2 -> do 
        (VBool bool, env1) <- evalExp env exp
        if bool then do
            envif <-  execStm (newBlock env1) stm1
            propagateReturn(envif)
        else do
            envelse <- execStm (newBlock env1) stm2
            propagateReturn(envelse)

-- | Propagate the return statement
propagateReturn :: Env -> IO Env
propagateReturn (sig, c:cs) = case Map.lookup (Id "return") c of
    Just val -> do 
        updateReturn(sig, cs) (Id "return") val

    Nothing  -> return (sig, cs)

-- | Execute a block of statements

execBlock :: Env -> [Stm] -> IO Env
execBlock env []     = return env
execBlock env (s:ss) = do
                        (sig, c:cs) <- execStm env s
                        case Map.lookup (Id "return") c of
                            Just val -> return (sig, c:cs)
                            Nothing  -> execBlock (sig, c:cs) ss 

-- | Evaluate an expression
evalExp :: Env -> Exp -> IO (Val, Env)
evalExp env exp = case exp of
    ETrue  -> return (VBool True, env)
    EFalse -> return (VBool False, env)
    EInt integer   -> return (VInt integer, env)
    EDouble double -> return (VDouble double, env)
    EId id -> do
              val <- lookupVar env id
              return (val, env)
    ECall id exps -> execFun env id exps
    EPIncr id -> evalUna env (EPIncr id)
    EPDecr id -> evalUna env (EPDecr id)
    EIncr  id -> evalUna env (EIncr  id)
    EDecr  id -> evalUna env (EDecr  id)
    EMul exp1 exp2 -> evalBin env (EMul exp1 exp2)
    EDiv exp1 exp2 -> evalBin env (EDiv exp1 exp2)
    EAdd exp1 exp2 -> evalBin env (EAdd exp1 exp2)
    ESub exp1 exp2 -> evalBin env (ESub exp1 exp2)
    ELt  exp1 exp2 -> evalBin env (ELt  exp1 exp2)
    EGt  exp1 exp2 -> evalBin env (EGt  exp1 exp2)
    ELEq exp1 exp2 -> evalBin env (ELEq exp1 exp2)
    EGEq exp1 exp2 -> evalBin env (EGEq exp1 exp2)
    EEq  exp1 exp2 -> evalBin env (EEq  exp1 exp2)
    ENEq exp1 exp2 -> evalBin env (ENEq exp1 exp2)
    EAnd exp1 exp2 -> evalBin env (EAnd exp1 exp2)
    EOr  exp1 exp2 -> evalBin env (EOr  exp1 exp2)
    EAss id exp -> do
        (val, env1) <- evalExp env exp
        env2 <- updateVar env1 id val
        return (val, env2)

-- | Evaluate unary operations
evalUna :: Env -> Exp -> IO (Val, Env)
evalUna env exp = case exp of
                    EPIncr id -> do 
                        val <- lookupVar env id
                        case val of 
                            VDouble double -> do
                                env1 <- updateVar env id (VDouble (double + 1.0))
                                return (val, env1)
                            VInt integer -> do
                                env2 <- updateVar env id (VInt (integer + 1))
                                return (val, env2)
                    EPDecr id -> do 
                        val <- lookupVar env id
                        case val of 
                            VDouble double -> do
                                env1 <- updateVar env id (VDouble (double - 1.0))
                                return (val, env1)
                            VInt integer -> do
                                env2 <- updateVar env id (VInt (integer - 1))
                                return (val, env2)                  
                    EIncr id -> do 
                        val <- lookupVar env id
                        case val of 
                            VDouble double -> do
                                env1 <- updateVar env id (VDouble (double + 1.0))
                                return (VDouble (double + 1.0), env1)
                            VInt integer -> do
                                env2 <- updateVar env id (VInt (integer + 1))
                                return (VInt (integer + 1), env2)
                    EDecr id -> do 
                        val <- lookupVar env id
                        case val of 
                            VDouble double -> do
                                env1 <- updateVar env id (VDouble (double - 1.0))
                                return (VDouble (double - 1.0), env1)
                            VInt integer -> do
                                env2 <- updateVar env id (VInt (integer - 1))
                                return (VInt (integer - 1), env2)

-- | Evaluate binary operations
evalBin :: Env -> Exp -> IO (Val, Env)
evalBin env exp = case exp of 
                        EMul exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VInt (val3 * val4), env2)
                                (VDouble val3, VDouble val4) -> return (VDouble (val3 * val4), env2)
                                (VInt val3,    VDouble val4) -> return (VDouble ((fromIntegral val3) * val4), env2)
                                (VDouble val3, VInt val4)    -> return (VDouble (val3 * (fromIntegral val4)), env2)

                        EDiv exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> if val4 == 0 then throw (ERROR "Values cannot be divided by 0")
                                                                else return (VInt (val3 `div` val4), env2)
                                (VDouble val3, VDouble val4) -> if val4 == 0 then throw (ERROR "Values cannot be divided by 0")
                                                                else return (VDouble (val3 / val4), env2)
                                (VInt val3,    VDouble val4) -> if val4 == 0 then throw (ERROR "Values cannot be divided by 0")
                                                                else return (VDouble ((fromIntegral val3) / val4), env2)
                                (VDouble val3, VInt val4)    -> if val4 == 0 then throw (ERROR "Values cannot be divided by 0")
                                                                else return (VDouble (val3 / (fromIntegral val4)), env2)                           
                        EAdd exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VInt (val3 + val4), env2)
                                (VDouble val3, VDouble val4) -> return (VDouble (val3 + val4), env2)
                                (VInt val3,    VDouble val4) -> return (VDouble ((fromIntegral val3) + val4), env2)
                                (VDouble val3, VInt val4)    -> return (VDouble (val3 + (fromIntegral val4)), env2)

                        ESub exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VInt (val3 - val4), env2)
                                (VDouble val3, VDouble val4) -> return (VDouble (val3 - val4), env2)
                                (VInt val3,    VDouble val4) -> return (VDouble ((fromIntegral val3) - val4), env2)
                                (VDouble val3, VInt val4)    -> return (VDouble (val3 - (fromIntegral val4)), env2)

                        ELt exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VBool (val3 < val4), env2)
                                (VDouble val3, VDouble val4) -> return (VBool (val3 < val4), env2)
                                (VInt val3,    VDouble val4) -> return (VBool ((fromIntegral val3) < val4), env2)
                                (VDouble val3, VInt val4)    -> return (VBool (val3 < (fromIntegral val4)), env2)

                        EGt exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VBool (val3 > val4), env2)
                                (VDouble val3, VDouble val4) -> return (VBool (val3 > val4), env2)
                                (VInt val3,    VDouble val4) -> return (VBool ((fromIntegral val3) > val4), env2)
                                (VDouble val3, VInt val4)    -> return (VBool (val3 > (fromIntegral val4)), env2)

                        ELEq exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VBool (val3 <= val4), env2)
                                (VDouble val3, VDouble val4) -> return (VBool (val3 <= val4), env2)
                                (VInt val3,    VDouble val4) -> return (VBool ((fromIntegral val3) <= val4), env2)
                                (VDouble val3, VInt val4)    -> return (VBool (val3 <= (fromIntegral val4)), env2)

                        EGEq exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VBool (val3 >= val4), env2)
                                (VDouble val3, VDouble val4) -> return (VBool (val3 >= val4), env2)
                                (VInt val3,    VDouble val4) -> return (VBool ((fromIntegral val3) >= val4), env2)
                                (VDouble val3, VInt val4)    -> return (VBool (val3 >= (fromIntegral val4)), env2)

                        EEq exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VBool (val3 == val4), env2)
                                (VDouble val3, VDouble val4) -> return (VBool (val3 == val4), env2)
                                (VInt val3,    VDouble val4) -> return (VBool ((fromIntegral val3) == val4), env2)
                                (VBool val3,   VBool val4)   -> return (VBool (val3 == val4), env2)
                                (VDouble val3, VInt val4)    -> return (VBool (val3 == (fromIntegral val4)), env2)

                        ENEq exp1 exp2 -> do
                            (val1, env1) <- evalExp env exp1
                            (val2, env2) <- evalExp env1 exp2
                            case (val1, val2) of
                                (VInt val3,    VInt val4)    -> return (VBool (val3 /= val4), env2)
                                (VDouble val3, VDouble val4) -> return (VBool (val3 /= val4), env2)
                                (VInt val3,    VDouble val4) -> return (VBool ((fromIntegral val3) /= val4), env2)
                                (VBool val3,   VBool val4)   -> return (VBool (val3 /= val4), env2)
                                (VDouble val3, VInt val4)    -> return (VBool (val3 /= (fromIntegral val4)), env2)

                        EAnd exp1 exp2 -> do
                            ((VBool val1), env1) <- evalExp env exp1
                            if not val1 then return (VBool False, env1)
                            else do
                                (VBool val2, env2) <- evalExp env1 exp2
                                return (VBool val2, env2)

                        EOr exp1 exp2 -> do
                            (VBool val1, env1) <- evalExp env exp1
                            if val1 then return (VBool True, env1)
                            else do
                                (VBool val2, env2) <- evalExp env1 exp2
                                return (VBool val2, env2)

-- | Execute a function call

execFun :: Env -> Id -> [Exp] -> IO (Val, Env)

execFun env (Id "printInt") exps = case exps of
                                    (exp1:[]) -> do
                                        (val, env1) <- evalExp env exp1
                                        putStrLn $ show val
                                        return (Void, env1)
                                    otherwise -> throw (ERROR "printInt() has too many arguments")
execFun env (Id "printDouble") exps = case exps of
                                       (exp1:[]) -> do
                                            case exp1 of
                                                EInt i -> do
                                                        (val, env1) <- evalExp env (EDouble (fromIntegral i))
                                                        putStrLn $ show val
                                                        return (Void, env1)
                                                _      -> do
                                                        (val, env1) <- evalExp env exp1
                                                        putStrLn $ show val
                                                        return (Void, env1)
                                       otherwise -> throw (ERROR "printDouble() has too many arguments")
execFun env (Id "readInt") exps = case exps of
                                    [] -> do
                                        str <- getLine
                                        return ((VInt (read str)), env)
                                    otherwise -> throw (ERROR "readInt() does not require any arguments")
execFun env (Id "readDouble") exps = case exps of
                                        [] -> do
                                            str <- getLine
                                            return ((VDouble (read str)), env)
                                        otherwise -> throw (ERROR "readDouble() does not require any arguments")
-- execFun env f ()
execFun env id exps = do 
                        (DFuns t id1 args stms) <- lookupFun env id
                        env1 <- evalArgs (newBlock env) args exps
                    
                        (sig,c:cs) <- execBlock env1 stms

                        case Map.lookup (Id "return") c of
                            Just val -> return (val, (sig, cs))
                            Nothing -> return (Void, (sig, cs))

-- ] Bind arguments to the function
bindArgs :: Env -> [Arg] -> [Exp] -> IO Env
bindArgs env [] [] = return env
bindArgs (sig, c:cs) ((ADecl typ id):args) (exp:exps) = do 
                                                        (val, env1) <- evalExp (sig,cs) exp
                                                        bindArgs (sig, Map.insert id val c:cs) args exps

-- | Evaluate a function's arguments
evalArgs :: Env -> [Arg] -> [Exp] -> IO Env
evalArgs env [] [] = return env
evalArgs (sig, c:cs) ((ADecl typ id):args) (exp:exps) = do
                                                        (val, (sig,con)) <- evalExp (sig,cs) exp
                                                        env2 <- addVar (sig, c:con) id val
                                                        evalArgs env2 args exps
                                










