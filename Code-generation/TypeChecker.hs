{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Type checker for C--, producing typed syntax from ASTs.

module TypeChecker where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CMM.Abs as C
import CMM.Print as CP
import CMM.ErrM  (Err(Ok, Bad))

import qualified Annotated as A

type Env = (Sig,[Context]) -- functions and context stack 
type Sig = Map Id ([Type],Type) -- function type signature 
type Context = Map Id Type -- variables with their types

typecheck :: C.Program -> Err A.Program
typecheck (C.PDefs [])   = Bad "This is an empty program"
typecheck (C.PDefs defs) = 
    if checkMain defs then do 
                    env <- addFuns defs
                    adefs <- checkDef env defs
                    return (A.PDefs adefs)
    else do Bad ("The program does not have a main function")

-- | Check if main function exists
checkMain :: [C.Def] -> Bool
checkMain [] = False
checkMain ((C.DFuns t (C.Id "main") args _):defs) = True
checkMain ((C.DFuns t _ args _):defs)           = checkMain defs

-- | Add Functions into the environment
addFuns :: [C.Def] -> Err Env
addFuns [] = return addBuildin
addFuns ((C.DFuns t (C.Id "main") args _):defs) = case (t, args) of
                        (C.Tint, []) -> do
                                    env <- addFuns defs
                                    updateFun env (C.Id "main") ((map (\(C.ADecl t2 id) -> t2) args),t)
                        _ -> Bad "The main function must have Int as the return type"

addFuns ((C.DFuns t id args _):defs) = do
    env <- addFuns defs
    updateFun env id ((map (\(C.ADecl t2 id) -> t2) args),t)

-- | Add build-in functions
addBuildin :: Env
addBuildin = case emptyEnv of
    (sig, cons) -> (
                    (Map.insert (C.Id "printInt") ([C.Tint], C.Tvoid)
                    (Map.insert (C.Id "printDouble") ([C.Tdouble], C.Tvoid)
                    (Map.insert (C.Id "readInt") ([], C.Tint)
                    (Map.insert (C.Id "readDouble") ([], C.Tdouble)
                    sig))))
                    , cons)

-- | Check the type of a variable
lookupVar :: Env -> C.Id -> Err (A.Type)
lookupVar (_, [])     id = Bad ("Variable " ++ show id ++ " is not found")
lookupVar (sig, c:cs) id = case Map.lookup id c of
                           Just t  -> return (modifyType t)
                           Nothing -> lookupVar (sig, cs) id

-- | Check the type signature of a function
lookupFun :: Env -> C.Id -> Err ([C.Type],C.Type)
lookupFun (sig, _) id = case Map.lookup id sig of
                        Just f  -> return f
                        Nothing -> Bad ("Function " ++ show id ++ " is not found")

-- | Insert a new variable if it not present
updateVar :: Env -> C.Id -> C.Type -> Err Env
updateVar (sig, []) id   t = return (sig, (Map.insert id t Map.empty):[])
updateVar (sig, c:cs) id t = case Map.lookup id c of
                             Just t1 -> Bad ("The variable " ++ show id ++ " has been declared")
                             Nothing -> return(sig, (Map.insert id t c):cs)

-- | Insert a new function if not present
updateFun :: Env -> C.Id -> ([C.Type],C.Type) -> Err Env
updateFun (sig, c) id t = case Map.lookup id sig of 
                          Just t1 -> Bad ("The function " ++ show id ++ " has been declared")
                          Nothing -> return(Map.insert id t sig, c)

-- | Introduce a new block context for a given function
newBlock  :: Env -> Env
newBlock (sig, cs) = (sig, Map.empty:cs)

-- | Create an empty environment
emptyEnv  :: Env
emptyEnv = (Map.empty, [])

-- | Infer type for expressions 
inferExp :: Env -> C.Exp -> Err (A.Type, A.Exp)
inferExp env x = case x of
    C.ETrue     -> return (A.Tbool, (A.ETrue))
    C.EFalse    -> return (A.Tbool, (A.EFalse))
    C.EInt    n -> return (A.Tint, (A.EInt n))
    C.EDouble d -> return (A.Tdouble, (A.EDouble d))
    C.EId   id       -> do
                        atyp <- lookupVar env id
                        return (atyp, A.EId (modifyId id))
    C.ECall id slist -> case lookupFun env id of
                        Ok (ts, t) -> do
                            aexps <- checkArgs env ts slist t
                            return ((modifyType t), (A.ECall (modifyId id) aexps))
                        Bad s -> Bad s
    C.EPIncr id -> inferUna [A.Tint, A.Tdouble] env id (A.EPIncr (modifyId id))
    C.EPDecr id -> inferUna [A.Tint, A.Tdouble] env id (A.EPDecr (modifyId id))
    C.EIncr id  -> inferUna [A.Tint, A.Tdouble] env id (A.EIncr (modifyId id))
    C.EDecr id  -> inferUna [A.Tint, A.Tdouble] env id (A.EDecr (modifyId id))
    C.EMul exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (atyp, (A.EMul atyp aexp1 aexp2))
    C.EDiv exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (atyp, (A.EDiv atyp aexp1 aexp2))
    C.EAdd exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (atyp, (A.EAdd atyp aexp1 aexp2))
    C.ESub exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (atyp, (A.ESub atyp aexp1 aexp2))
    C.ELt exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (A.Tbool, (A.ELt atyp aexp1 aexp2))
    C.EGt exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (A.Tbool, (A.EGt atyp aexp1 aexp2))
    C.ELEq exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (A.Tbool, (A.ELEq atyp aexp1 aexp2))
    C.EGEq exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble] env exp1 exp2
                        return (A.Tbool, (A.EGEq atyp aexp1 aexp2))
    C.EEq exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble, A.Tbool] env exp1 exp2
                        return (A.Tbool, (A.EEq atyp aexp1 aexp2))
    C.ENEq exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tint, A.Tdouble, A.Tbool] env exp1 exp2
                        return (A.Tbool, (A.ENEq atyp aexp1 aexp2))
    C.EAnd exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tbool] env exp1 exp2
                        return (A.Tbool, (A.EAnd aexp1 aexp2))
    C.EOr exp1 exp2 -> do 
                        (atyp, aexp1, aexp2) <- inferBin [A.Tbool] env exp1 exp2
                        return (A.Tbool, (A.EOr aexp1 aexp2))
    C.EAss id exp -> do 
                   t1 <- lookupVar env id
                   (atyp, aexp) <- inferExp env exp

                   if checkSubtype atyp t1 then case (t1, atyp) of
                                                    (A.Tdouble, A.Tint) -> return (t1, (A.EAss (modifyId id) (A.EConv t1 aexp)))
                                                    otherise        -> return (t1, (A.EAss (modifyId id) aexp))
 
                   else Bad ("wrong type of expression " ++ CP.printTree exp)
-- | Check if arguments of a function call are valid  
checkArgs :: Env -> [C.Type] -> [C.Exp] -> C.Type -> Err [A.Exp]
checkArgs env []   []       t = Ok []
checkArgs env []   (y:ys)   t = Bad "Too few arguments"
checkArgs env (x:xs) []     t = Bad "Too few arguments"
checkArgs env (x:xs) (y:ys) t = do 
                        aexp <- checkExp env (modifyType x) y
                        aexps <- (checkArgs env xs ys t)
                        return (aexp:aexps)

-- | Infer expressions for unary operations
inferUna :: [A.Type] -> Env -> C.Id -> A.Exp -> Err (A.Type, A.Exp)
inferUna types env id aexp = do
    atyp <- lookupVar env id
    if elem atyp types
        then
            return (atyp, aexp)
        else 
            Bad ("wrong type of expression " ++ show id)

-- | Infer expressions for binary operations
inferBin :: [A.Type] -> Env -> C.Exp -> C.Exp -> Err (A.Type, A.Exp, A.Exp)
inferBin types env exp1 exp2 = do
    (atyp1, aexp1) <- inferExp env exp1
    (atyp2, aexp2) <- inferExp env exp2
    if (elem atyp1 types) && (elem atyp2 types)
        then case (atyp1, atyp2) of
            (A.Tint , A.Tdouble ) -> return (atyp2, (A.EConv A.Tdouble aexp1), aexp2)
            (A.Tdouble , A.Tint ) -> return (atyp1, aexp1, (A.EConv A.Tdouble aexp2))
            (_,_) -> do
                    aexp <- checkExp env atyp1 exp2
                    return (atyp1, aexp1, aexp)
    else
            Bad ("wrong type of expression " ++ show exp1)

-- | Check Subtype relationship
checkSubtype:: A.Type -> A.Type -> Bool
checkSubtype A.Tbool A.Tbool     = True
checkSubtype A.Tint A.Tint       = True
checkSubtype A.Tdouble A.Tdouble = True
checkSubtype A.Tvoid A.Tvoid     = True
checkSubtype A.Tint A.Tdouble    = True
checkSubtype _ _                 = False

-- | Check validity of definitions
checkDef :: Env -> [C.Def] -> Err [A.Def]
checkDef env [] = do return []
checkDef env ((C.DFuns t id args stms):defs) = case (addArgs(newBlock env) args) of
    Ok env -> do
        (env1,astms) <- checkStms env t stms
        adefs <- checkDef (exitBlock env1) defs
        return ((A.DFuns (modifyType t) (modifyId id) (map modifyArg args) astms):adefs)
    Bad msg -> Bad msg

-- | Add function arguments into the environment
addArgs :: Env -> [C.Arg] -> Err Env
addArgs env [] = do return env
addArgs (sig,[]) ((C.ADecl t id):args)    = if t == C.Tvoid then Bad "Variable cannot have the type Void"
                                          else addArgs (sig, (Map.insert id t Map.empty):[]) args
addArgs (sig, c:cs) ((C.ADecl t id):args) = if t == C.Tvoid then Bad "Variable cannot have the type Void"
                                          else case Map.lookup id c of
                                                   Just t -> Bad ("The argument " ++ show id ++ " has already been declared")
                                                   _ -> addArgs (sig, (Map.insert id t c):cs) args

-- | Check expressions in terms of type inference
checkExp :: Env -> A.Type -> C.Exp -> Err A.Exp
checkExp env typ exp = do
    (atyp, aexp) <- inferExp env exp
    if (checkSubtype atyp typ) then
        
        case (atyp, typ) of
            (A.Tint, A.Tdouble) -> case aexp of 
                                        (A.EInt i) -> return (A.EDouble (fromIntegral i))
                                        --(A.Id  id) -> 
                                        otherwise  -> return (A.EConv typ aexp)
            _                   -> return aexp
        {-   
        case (typ, aexp) of 
            (A.Tdouble, A.EInt i) -> return (A.EDouble (fromIntegral i))
            _                     -> return aexp
            -}
    else
        Bad ("type of " ++ CP.printTree exp ++
        " expected " ++ A.printTree typ ++
        " but found " ++ A.printTree atyp)

-- | Check validity of statements
checkStm :: Env -> C.Type -> C.Stm -> Err (Env, A.Stm)
checkStm env val x = case x of
    C.SExp exp  -> do 
        (atyp, aexp) <- inferExp env exp
        return (env, (A.SExp atyp aexp))

    C.SDecls typ [] -> do
        return (env, (A.SDecls (modifyType typ) []))

    C.SDecls typ (i:is) ->
        if typ == C.Tvoid then 
            do Bad ("Variables cannot have Void type")
        else do 
                env1 <- updateVar env i typ
                (env2, res) <- checkStm env1 val (C.SDecls typ is)
                let (A.SDecls atyp aexps) = res
                let newRes = (A.SDecls atyp ((modifyId i):aexps))
                return (env2, newRes)

    C.SWhile exp stm  -> do
        aexp <- checkExp env A.Tbool exp
        (env1,astms) <- checkStm (newBlock env) val stm
        return ((exitBlock env1), (A.SWhile aexp astms))

    C.SInit typ id exp -> do 

        env1 <- updateVar env id typ
        aexp <- checkExp env1 (modifyType typ) exp
        return (env1, (A.SInit (modifyType typ) (modifyId id) aexp))
      
    C.SReturn exp -> do 
        aexp <- checkExp env (modifyType val) exp
        return (env, (A.SReturn (modifyType val) aexp))

    C.SBlock  stms -> do
        (env1, astms) <- checkStms (newBlock env) val stms
        return ((exitBlock env1), (A.SBlock astms))

    C.SIfElse exp stm1 stm2 -> do 
        aexp <- checkExp env A.Tbool exp
        (env1, astms1) <- checkStm (newBlock env) val stm1
        (env2, astms2) <- checkStm (newBlock (exitBlock env1)) val stm2
        return ((exitBlock env2), (A.SIfElse aexp astms1 astms2))

-- | Exit the block
exitBlock :: Env -> Env
exitBlock (sig, c:cs) = (sig,cs)

-- | Check validity of statement lists
checkStms :: Env -> C.Type -> [C.Stm] -> Err (Env, [A.Stm])
checkStms env t stms = case stms of
    [] -> return (env, [])
    x : rest -> do
        (env1, astm) <- checkStm env t x
        (env2, astms) <- checkStms env1 t rest
        return (env2, astm:astms)

modifyArg :: C.Arg -> A.Arg
modifyArg (C.ADecl t id) = (A.ADecl (modifyType t) (modifyId id))

modifyId :: C.Id -> A.Id
modifyId (C.Id s) = (A.Id s)

modifyType :: C.Type -> A.Type
modifyType typ = case typ of
  C.Tbool -> A.Tbool
  C.Tdouble -> A.Tdouble
  C.Tint -> A.Tint
  C.Tvoid -> A.Tvoid



