module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

type Env = (Sig,[Context]) -- functions and context stack 
type Sig = Map Id ([Type],Type) -- function type signature 
type Context = Map Id Type -- variables with their types

typecheck :: Program -> Err ()
typecheck (PDefs [])   = Bad "This is an empty program"
typecheck (PDefs defs) = 
    if checkMain defs then do 
                    env <- addFuns defs
                    checkDef env defs
    else do Bad ("The program does not have a main function")

-- | Check if main function exists
checkMain :: [Def] -> Bool
checkMain [] = False
checkMain ((DFuns t (Id "main") args _):defs) = True
checkMain ((DFuns t _ args _):defs)           = checkMain defs

-- | Add Functions into the environment
addFuns :: [Def] -> Err Env
addFuns [] = return addBuildin
addFuns ((DFuns t (Id "main") args _):defs) = case (t, args) of
                        (Tint, []) -> do
                                    env <- addFuns defs
                                    updateFun env (Id "main") ((map (\(ADecl t2 id) -> t2) args),t)
                        _ -> Bad "The main function must have Int as the return type"

addFuns ((DFuns t id args _):defs) = do
    env <- addFuns defs
    updateFun env id ((map (\(ADecl t2 id) -> t2) args),t)

-- | Add build-in functions
addBuildin :: Env
addBuildin = case emptyEnv of
    (sig, cons) -> (
                    (Map.insert (Id "printInt") ([Tint], Tvoid)
                    (Map.insert (Id "printDouble") ([Tdouble], Tvoid)
                    (Map.insert (Id "readInt") ([], Tint)
                    (Map.insert (Id "readDouble") ([], Tdouble)
                    sig))))
                    , cons)

-- | Check the type of a variable
lookupVar :: Env -> Id -> Err Type
lookupVar (_, [])     id = Bad ("Variable " ++ show id ++ " is not found")
lookupVar (sig, c:cs) id = case Map.lookup id c of
                           Just t  -> return t
                           Nothing -> lookupVar (sig, cs) id

-- | Check the type signature of a function
lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun (sig, _) id = case Map.lookup id sig of
                        Just f  -> return f
                        Nothing -> Bad ("Function " ++ show id ++ " is not found")

-- | Insert a new variable if it not present
updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, []) id   t = return (sig, (Map.insert id t Map.empty):[])
updateVar (sig, c:cs) id t = case Map.lookup id c of
                             Just t1 -> Bad ("The variable " ++ show id ++ " has been declared")
                             Nothing -> return(sig, (Map.insert id t c):cs)

-- | Insert a new function if not present
updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, c) id t = case Map.lookup id sig of 
                          Just t1 -> Bad ("The function " ++ show id ++ " has been declared")
                          Nothing -> return(Map.insert id t sig, c)

-- | Introduce a new block context for a given function
newBlock  :: Env -> Env
newBlock (sig, cs) = (sig, Map.empty:cs)

-- | Create an empty environment
emptyEnv  :: Env
emptyEnv = (Map.empty, [])

checkArgs :: [Type] -> [Type] -> Bool
checkArgs []   [] = True
checkArgs [] typs = False
checkArgs typs [] = False
checkArgs (typ1:typs1) (typ2:typs2) = if (checkSubtype typ1 typ2) then checkArgs typs1 typs2
                                        else False

-- | Infer type for expressions 
inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
    ETrue     -> return Tbool
    EFalse    -> return Tbool
    EInt    n -> return Tint
    EDouble d -> return Tdouble
    EId   id       -> lookupVar env id
    ECall id slist -> do
                      (ts, t) <- lookupFun env id
                      ts2 <- mapM (inferExp env) slist

                      if (checkArgs ts2 ts)
                          then return t
                      else Bad (printTree id 
                                  ++ " are supplied with the following arguments " 
                                  ++ show slist
                                  ++ ", however, the required arguments are: "
                                  ++ show ts )
    EPIncr id -> inferUna [Tint, Tdouble] env id
    EPDecr id -> inferUna [Tint, Tdouble] env id
    EIncr id  -> inferUna [Tint, Tdouble] env id
    EDecr id  -> inferUna [Tint, Tdouble] env id
    EMul exp1 exp2 ->
        inferBin [Tint, Tdouble] env exp1 exp2
    EDiv exp1 exp2 ->
        inferBin [Tint, Tdouble] env exp1 exp2
    EAdd exp1 exp2 ->
        inferBin [Tint, Tdouble] env exp1 exp2
    ESub exp1 exp2 ->
        inferBin [Tint, Tdouble] env exp1 exp2
    ELt exp1 exp2 -> 
        inferBin [Tint, Tdouble] env exp1 exp2 >> return Tbool
    EGt exp1 exp2 -> 
        inferBin [Tint, Tdouble] env exp1 exp2 >> return Tbool
    ELEq exp1 exp2 -> 
        inferBin [Tint, Tdouble] env exp1 exp2 >> return Tbool
    EGEq exp1 exp2 -> 
        inferBin [Tint, Tdouble] env exp1 exp2 >> return Tbool
    EEq exp1 exp2 -> 
        inferBin [Tint, Tdouble, Tbool] env exp1 exp2 >> return Tbool
    ENEq exp1 exp2 ->
        inferBin [Tint, Tdouble, Tbool] env exp1 exp2 >> return Tbool
    EAnd exp1 exp2 -> 
        inferBin [Tbool] env exp1 exp2 >> return Tbool
    EOr exp1 exp2 -> 
        inferBin [Tbool] env exp1 exp2 >> return Tbool
    EAss id exp -> do 
                   t1 <- lookupVar env id
                   t2 <- inferExp env exp
                   if checkSubtype t2 t1 then return t1
                   else Bad ("wrong type of expression " ++ printTree exp)



-- | Infer expressions for unary operations
inferUna :: [Type] -> Env -> Id -> Err Type
inferUna types env id = do
    typ <- lookupVar env id
    if elem typ types
        then
            return typ
        else 
            Bad ("wrong type of expression " ++ show id)

-- | Infer expressions for binary operations
inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
    typ1 <- inferExp env exp1
    typ2 <- inferExp env exp2
    if (elem typ1 types) && (elem typ2 types)
        then case (typ1, typ2) of
            (Tint, Tdouble) -> return Tdouble
            (Tdouble, Tint) -> return Tdouble
            (_,_) -> do
                    t <- checkExp env typ1 exp2
                    return t
    else
            Bad ("wrong type of expression " ++ show exp1)

-- | Check Subtype relationship
checkSubtype:: Type -> Type -> Bool
checkSubtype Tbool Tbool     = True
checkSubtype Tint Tint       = True
checkSubtype Tdouble Tdouble = True
checkSubtype Tvoid Tvoid     = True
checkSubtype Tint Tdouble    = True
checkSubtype _ _             = False

-- | Check validity of definitions
checkDef :: Env -> [Def] -> Err ()
checkDef env [] = do return()
checkDef env ((DFuns t id args stms):defs) = case (addArgs(newBlock env) args) of
    Ok (sig,c:cs) -> do
        checkStms (sig, c:cs) t stms
        checkDef(sig,cs) defs
    Bad msg -> Bad msg

-- | Add function arguments into the environment
addArgs :: Env -> [Arg] -> Err Env
addArgs env [] = do return env
addArgs (sig,[]) ((ADecl t id):args)    = if t == Tvoid then Bad "Variable cannot have the type Void"
                                          else addArgs (sig, (Map.insert id t Map.empty):[]) args
addArgs (sig, c:cs) ((ADecl t id):args) = if t == Tvoid then Bad "Variable cannot have the type Void"
                                          else case Map.lookup id c of
                                                   Just t -> Bad ("The argument " ++ show id ++ " has already been declared")
                                                   _ -> addArgs (sig, (Map.insert id t c):cs) args

-- | Check expressions in terms of type inference
checkExp :: Env -> Type -> Exp -> Err Type
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (checkSubtype typ2 typ) then
        return typ
    else
        Bad ("type of " ++ printTree exp ++
        " expected " ++ printTree typ ++
        " but found " ++ printTree typ2)

-- | Check validity of statements
checkStm :: Env -> Type -> Stm -> Err Env
checkStm env val x = case x of
    SExp exp  -> do 
        inferExp env exp
        return env

    SDecls typ [] -> do
        return env

    SDecls typ (i:is) ->
        if typ == Tvoid then 
            do Bad ("Variables cannot have Void type")
        else do 
                env1 <- updateVar env i typ
                checkStm env1 val (SDecls typ is)

    SWhile exp stm  -> do
        checkExp env Tbool exp
        env1 <- checkStm (newBlock env) val stm
        return (exitBlock env1)

    SInit typ id exp -> do 

        env1 <- updateVar env id typ
        checkExp env1 typ exp
        return env1
      
    SReturn exp -> do 
        checkExp env val exp
        return env 

    SBlock  stms -> do
        env1 <- checkStms (newBlock env) val stms
        return (exitBlock env1)

    SIfElse exp stm1 stm2 -> do 
        checkExp env Tbool exp
        env1 <- checkStm (newBlock env) val stm1
        env2 <- checkStm (newBlock (exitBlock env1)) val stm2
        return (exitBlock env2)

-- | Exit the block
exitBlock :: Env -> Env
exitBlock (sig, c:cs) = (sig,cs)

-- | Check validity of statement lists
checkStms :: Env -> Type -> [Stm] -> Err Env
checkStms env t stms = case stms of
    [] -> return env
    x : rest -> do
        env1 <- checkStm env t x
        checkStms env1 t rest




