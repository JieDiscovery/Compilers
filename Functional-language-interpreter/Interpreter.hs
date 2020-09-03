-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

{-# LANGUAGE LambdaCase #-}

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.Print

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- |  Basic type of value: Int； Program containing functions

data Value
  = VInt Integer
  | VFun Ident Exp Env

-- | Structure of context
data Ctxt = Ctxt
  { cStrategy :: Strategy  
  , cSig      :: Sig       
  , cEnv      :: Env       
  }
-- | Error monad.

type Err = Except String

-- | type construct signature
type Sig = Map Ident Exp
emptyEnv= Map.empty

-- | type construct environment
type Env = Map Ident Value

type Eval = ReaderT Ctxt Err
-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  --throwError $ "TODO: implement interpreter"
  let signature   = Map.fromList $ map def2Sig defs
  let context     = Ctxt strategy signature emptyEnv
  returnValue <- eval mainExp `runReaderT` context
  -- Return value check
  case returnValue of
        VInt i -> return i
        _      -> error $"Invalid return value: return value should be an integer but not"
  
-- | translate defs to exp
def2Sig :: Def -> (Ident, Exp)
def2Sig (DDef fun v m ) = (fun, foldr EAbs m v) 

-- | Evaluation in given Environment.
gevalEnv :: Env -> Eval a -> Eval a
gevalEnv env eval = do
  local (\ctxt -> ctxt { cEnv = env}) eval
 
-- | Evaluation.
eval :: Exp -> Eval Value
eval e = case e of
  EVar x    -> do
    env <- asks cEnv
    case Map.lookup x env of
      Just v ->  do
        stg <- asks cStrategy
        case stg of
          CallByValue -> return v
          CallByName -> case v of
            VInt{} -> return v
            VFun x f env' -> do
                gevalEnv env' $ eval f
      Nothing -> do
        sig <- asks cSig
        case Map.lookup x sig of
          Just e  -> gevalEnv emptyEnv $ eval e
          Nothing -> error $ "Unbound identifier " ++ printTree x

  EInt x   -> return $ VInt x

  EApp f e  -> do
    vf <- eval f
    case vf of
      VFun x f' env -> do
        stg <- asks cStrategy
        case stg of
          CallByValue -> do
            ve <- eval e
            gevalEnv (Map.insert x ve env) $ eval f'
          CallByName -> do
            ce <- eval (EAbs x e)
            gevalEnv (Map.insert x ce env) $ eval f'
      _ -> error $ "Non-function applied"

  EAdd e m ->do
    v  <- eval e
    w <- eval m
    case (v,w) of
        ((VInt v),(VInt w)) -> return $ VInt(v+w)
        _ -> error $ "Only integers support EAdd" 

  ESub e e' -> do
    v  <- eval e
    w <- eval e'
    case (v,w) of
        ((VInt v),(VInt w)) -> return $ VInt(v-w)
        _ -> error $ "Only integers support ESub" 

  ELt  e e' -> do
    v  <- eval e
    w  <- eval e'
    case (v,w) of
        ((VInt v),(VInt w)) -> if v < w then return $ VInt 1 else return $ VInt 0
        _ -> error $ "Only integers support Elt" 
    
  EIf c t e -> do
    (con) <- eval c
    case con of 
        VInt con -> if eq (VInt con) (VInt 1) then eval t else eval e 
        _ -> error $ "Condition of if is not assigned value or does not have boolean value"
    where eq (VInt m) (VInt n) = m == n 

  EAbs x e  -> do
    env <- asks cEnv
    return $ VFun x e env


