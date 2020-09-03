-- Optional: turn on warnings.
-- {-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module Compiler where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


import Annotated

data FunType = FunType Type [Type]

data St = St
  { cxt           :: Cxt   -- ^ Context.
  , limitLocals   :: Int   -- ^ Maximal size for locals encountered.
  , currentStack  :: Int   -- ^ Current stack size.
  , limitStack    :: Int   -- ^ Maximal stack size encountered.
  , nextLabel     :: Label -- ^ Next jump label (persistent part of state).
  }

type Sig = Map Id Fun

-- | Function names bundled with their type.
data Fun = Fun { funId :: Id, funFunType :: FunType }

type Cxt = [Map Id Var]
type Var = (Addr, Type)

newtype Label = L { theLabel :: Int }
  deriving (Eq, Enum)

instance Show Label where
    show (L i) = "L" ++ show i

initSt :: St
initSt = St
  { cxt = [Map.empty]
  , limitLocals   = 0
  , currentStack  = 0
  , limitStack    = 0
  , nextLabel     = L 0
  }

type Output = [String]

type Compile = RWS Sig Output St

-- | Builtin-functions
builtin :: [(Id, Fun)]
builtin =
  [ (Id "printInt"   , Fun (Id "Runtime/printInt"   ) $ FunType Tvoid [Tint]),
    (Id "readInt"    , Fun (Id "Runtime/readInt"    ) $ FunType Tint []),
    (Id "printDouble", Fun (Id "Runtime/printDouble") $ FunType Tvoid [Tdouble]),
    (Id "readDouble" , Fun (Id "Runtime/readDouble" ) $ FunType Tdouble [])
    ]

-- | Entry point.

compile
  :: String  -- ^ Class name.
  -> Program -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile name prg@(PDefs defs) = unlines w
  where
  sigEntry def@(DFuns _ f@(Id x) _ _ ) = (f, Fun (Id $ name ++ "/" ++ x) $ funType def)
  sig = Map.fromList $ builtin ++ map sigEntry defs
  w   = snd $ evalRWS (compileProgram name prg) sig initSt

compileProgram :: String -> Program -> Compile ()
compileProgram name (PDefs defs) = do
  tell header
  mapM_ compileFun defs
  where
  header =
    [ ";; BEGIN HEADER"
    , ""
    , ".class public " ++ name
    , ".super java/lang/Object"
    , ""
    , ".method public <init>()V"
    , "  .limit locals 1"
    , ""
    , "  aload_0"
    , "  invokespecial java/lang/Object/<init>()V"
    , "  return"
    , ""
    , ".end method"
    , ""
    , ".method public static main([Ljava/lang/String;)V"
    , "  .limit locals 1"
    , "  .limit stack  1"
    , ""
    , "  invokestatic " ++ name ++ "/main()I"
    , "  pop"
    , "  return"
    , ""
    , ".end method"
    , ""
    , ";; END HEADER"
    ]

compileFun :: Def -> Compile ()
compileFun def@(DFuns t f args ss) = do
  -- function header
  tell [ "", ".method public static " ++ toJVM (Fun f $ funType def) ]

  -- prepare environment
  lab <- gets nextLabel
  put initSt{ nextLabel = lab }
  mapM_ (\ (ADecl t' x) -> newVar x t') args

  -- compile statements
  inNewBlock

  w <- grabOutput $ do
    mapM_ compileStm ss

  exitBlock

  -- output limits
  ll <- gets limitLocals
  ls <- gets limitStack
  tell [ "  .limit locals " ++ show ll
       , "  .limit stack  " ++ show ls
       ]

  -- output code, indented by 2
  tell $ map (\ s -> if null s then s else "  " ++ s) w

  
  --TODO
  case t of
    Tvoid -> tell ["  return"]
    otherwise -> if f == (Id "main") then tell ["  ireturn"] else tell[""]

  -- function footer
  tell [ "", ".end method"]


compileStm :: Stm -> Compile ()
compileStm s = do

  -- Printing a comment
  let top = stmTop s
  unless (null top) $ do
    tell $ map (";; " ++) $ lines top

  -- message for NYI
  let nyi = error $ "The following statement has not been implemented yet: " ++ top

  case s of

    SInit t x e -> do
      compileExp e
      newVar x t
      (a, _) <- lookupVar x
      emit $ Store t a

    SExp t e -> do
      compileExp e
      emit $ Pop t

    SWhile e s1 -> do
      start <- newLabel
      done  <- newLabel
      emit $ Label start
      compileExp e
      emit $ IfEq done
      inNewBlock
      compileStm s1
      exitBlock
      emit $ Goto start
      emit $ Label done

    SReturn typ exp -> do  
      case typ of
        Tdouble -> do
                    val <- gets currentStack

                    when (val < 2) (modify $ \ st -> st { currentStack = 2 })

        _        -> return ()
      compileExp exp
      emit (Return typ)

    SDecls typ ids -> do
      mapM_ (\id -> newVar id typ) ids

    SBlock stms -> do
        inNewBlock
        mapM_ compileStm stms
        exitBlock

    SIfElse exp stm1 stm2 -> do
        true  <- newLabel
        false <- newLabel
        compileExp exp
        emit (IfEq false)
        inNewBlock
        compileStm stm1
        exitBlock
        emit(Goto true)
        emit(Label false)
        inNewBlock
        compileStm stm2
        exitBlock
        emit(Label true)


compileExp :: Exp -> Compile ()
compileExp e = do
  -- message for NYI
  let nyi = error $ "The following expression has not been implemented yet: " ++ show e
  case e of

    ETrue -> emit (IConst 1)

    EFalse -> emit (IConst 0)

    EInt i -> do
      emit $ IConst i

    EDouble d -> do
        emit $ DConst d

    EConv typ exp -> do

        case exp of
            (EInt i) -> emit (DConst (fromIntegral i))
            _        -> do
                        compileExp exp
                        --updateStackLimit 1
                        emit (I2D)

    EId x -> do
      (a, t) <- lookupVar x
      emit $ Load t a

    ECall f es -> do
      mapM_ compileExp es
      sig <- ask
      let fun = fromMaybe (error "unbound function") $  Map.lookup f sig
      emit $ Call fun

    EPIncr id -> do
        (add, typ) <- lookupVar id
        emit(Load typ add)

        case typ of

            Tint    -> do
                        emit Dup
                        emit (IConst 1)

            Tdouble -> do
                        emit Dup2
                        emit (DConst 1.0)

        emit(Add typ)
        emit(Store typ add)

    EPDecr id -> do
        (add, typ) <- lookupVar id
        emit(Load typ add)

        case typ of

            Tint    -> do
                        emit Dup
                        emit (IConst 1)

            Tdouble -> do
                        emit Dup2
                        emit (DConst 1.0)

        emit(Sub typ)
        emit(Store typ add)

    EIncr id -> do
        (add, typ) <- lookupVar id
        emit(Load typ add)

        case typ of
            Tint    -> emit (IConst 1)

            Tdouble -> emit (DConst 1.0)

        emit(Add typ)
        emit(Store typ add)
        emit(Load typ add)

    EDecr id -> do
        (add, typ) <- lookupVar id
        emit(Load typ add)

        case typ of
            Tint    -> emit (IConst 1)

            Tdouble -> emit (DConst 1.0)

        emit(Sub typ)
        emit(Store typ add)
        emit(Load typ add)
    
    EMul typ exp1 exp2 -> do
        compileExp exp1
        compileExp exp2
        emit (Mul typ)

    EDiv typ exp1 exp2 -> do
        compileExp exp1
        compileExp exp2
        emit (Div typ)

    EAdd typ exp1 exp2 -> do
        compileExp exp1
        compileExp exp2
        emit (Add typ)

    ESub typ exp1 exp2 -> do
        compileExp exp1
        compileExp exp2
        emit (Sub typ)

    ELt typ exp1 exp2 -> do

        case typ of
            Tint -> do
                    true <- newLabel
                    emit (IConst 1)
                    compileExp exp1
                    compileExp exp2 
                    emit (IfCmpLt true)
                    emit $ Pop typ
                    emit (IConst 0)
                    emit (Label true)
            Tdouble -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2
                        emit (IfCmpg)
                        emit (IConst 0)
                        emit (IfCmpLt true)
                        emit $ Pop Tint
                        emit (IConst 0)
                        emit (Label true)


    EGt typ exp1 exp2 -> do

        case typ of
            Tint -> do
                    true <- newLabel
                    emit (IConst 1)
                    compileExp exp1
                    compileExp exp2 
                    emit (IfCmpGt true)
                    emit $ Pop typ
                    emit (IConst 0)
                    emit (Label true)
            Tdouble -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2
                        emit (IfCmpg)
                        emit (IConst 0)
                        emit (IfCmpGt true)
                        emit $ Pop Tint
                        emit (IConst 0)
                        emit (Label true)

    ELEq typ exp1 exp2 -> do

        case typ of
            Tint -> do
                    true <- newLabel
                    emit (IConst 1)
                    compileExp exp1
                    compileExp exp2 
                    emit (IfCmpLe true)
                    emit $ Pop typ
                    emit (IConst 0)
                    emit (Label true)
            Tdouble -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2
                        emit (IfCmpg)
                        emit (IConst 0)
                        emit (IfCmpLe true)
                        emit $ Pop Tint
                        emit (IConst 0)
                        emit (Label true)

    EGEq typ exp1 exp2 -> do

        case typ of
            Tint -> do
                    true <- newLabel
                    emit (IConst 1)
                    compileExp exp1
                    compileExp exp2 
                    emit (IfCmpGe true)
                    emit $ Pop typ
                    emit (IConst 0)
                    emit (Label true)
            Tdouble -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2
                        emit (IfCmpg)
                        emit (IConst 0)
                        emit (IfCmpGe true)
                        emit $ Pop Tint
                        emit (IConst 0)
                        emit (Label true)

    EEq typ exp1 exp2 -> do

        case typ of
            Tdouble -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2
                        emit (IfCmpg)
                        emit (IConst 0)
                        emit (IfCmpEq true)
                        emit $ Pop Tint
                        emit (IConst 0)
                        emit (Label true)
            _       -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2 
                        emit (IfCmpEq true)
                        emit $ Pop typ
                        emit (IConst 0)
                        emit (Label true)                   

    ENEq typ exp1 exp2 -> do

        case typ of
            Tdouble -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2
                        emit (IfCmpg)
                        emit (IConst 0)
                        emit (IfCmpNe true)
                        emit $ Pop Tint
                        emit (IConst 0)
                        emit (Label true)
            _       -> do
                        true <- newLabel
                        emit (IConst 1)
                        compileExp exp1
                        compileExp exp2 
                        emit (IfCmpNe true)
                        emit $ Pop typ
                        emit (IConst 0)
                        emit (Label true) 

    EAnd exp1 exp2 -> do
                        false <- newLabel
                        end <- newLabel
                        compileExp exp1
                        emit (IfEq false)
                        compileExp exp2
                        emit (Goto end)
                        emit (Label false)
                        emit (IConst 0)
                        emit (Label end)

    EOr exp1 exp2 -> do
                        false <- newLabel
                        end <- newLabel
                        compileExp exp1
                        emit (IfEq false)
                        emit (IConst 1)
                        emit (Goto end)
                        emit (Label false)
                        compileExp exp2
                        emit (Label end)

    EAss id exp2 -> do
                        compileExp exp2
                        (a, t) <- lookupVar id

                        case t of
                            Tdouble -> emit Dup2
                            _ -> emit Dup
                            
                        emit (Store t a)


-- * Instructions and emitting

type Addr = Int

data Code
  = Store Type Addr  -- ^ Store stack content of type @Type@ in local variable @Addr@.
  | Load  Type Addr  -- ^ Push stack content of type @Type@ from local variable @Addr@.

  | IConst Integer   -- ^ Put integer constant on the stack.
  | DConst Double
  | Pop Type         -- ^ Pop something of type @Type@ from the stack.
  | Return Type      -- ^ Return from method of type @Type@.
  | Add Type         -- ^ Add 2 top values of stack.
  | Sub Type
  | Mul Type
  | Div Type


  | Call Fun         -- ^ Call function.
  | Dup
  | Dup2

  | Label Label      -- ^ Define label.
  | Goto Label       -- ^ Jump to label.

  | IfEq Label
  | IfCmpEq Label
  | IfCmpNe Label
  | IfCmpLt Label
  | IfCmpLe Label
  | IfCmpGt Label
  | IfCmpGe Label

  | IfCmpg

  | I2D


-- | Print a single instruction.  Also update stack limits
emit :: Code -> Compile ()

emit (Store Tvoid _) = return()
emit (Load  Tvoid _) = return()
emit (Pop   Tvoid  ) = return()

emit c = do

        tell [toJVM c]

        case c of
            Store t _ -> updateStackLimit (negate (size t))
            Load  t _ -> updateStackLimit (size t)
            IConst  _ -> updateStackLimit 1
            DConst  _ -> updateStackLimit 2
            Pop     t -> updateStackLimit (negate (size t))
            Return  t -> updateStackLimit (negate (size t))
            Call    f -> updateStackLimit (negate (size f))
            Add     t -> updateStackLimit (negate (size t))
            Div     t -> updateStackLimit (negate (size t))
            Mul     t -> updateStackLimit (negate (size t))
            Sub     t -> updateStackLimit (negate (size t))
            Dup       -> updateStackLimit 1
            Dup2      -> updateStackLimit 2
            _         -> return()

class Size a where
  size :: a -> Int

instance Size Type where
  size t = case t of
    Tint    -> 1
    Tdouble -> 2
    Tbool   -> 1
    Tvoid   -> 0

instance Size FunType where
  size (FunType t ts) = sum (map size ts) - size t

instance Size Fun where
  size (Fun _ ft) = size ft

updateStackLimit :: Int -> Compile ()
updateStackLimit n = do
    new <- (n +) <$> gets currentStack
    modify $ \ st -> st { currentStack = new }
    old <- gets limitStack
    when (new > old) $
        modify $ \ st -> st { limitStack = new }

class ToJVM a where
    toJVM :: a -> String

instance ToJVM Type where
  toJVM t = case t of
    Tint    -> "I"
    Tvoid   -> "V"
    Tdouble -> "D"
    Tbool   -> "Z"

instance ToJVM FunType where
  toJVM (FunType t ts) = "(" ++ (toJVM =<< ts) ++ ")" ++ toJVM t

instance ToJVM Fun where
  toJVM (Fun (Id f) t) = f ++ toJVM t

instance ToJVM Label where
  toJVM (L l) = "L" ++ show l

instance ToJVM Code where
  toJVM c = case c of

    Store t n -> prefix t ++ "store " ++ show n
    Load  t n -> prefix t ++ "load " ++ show n

    Pop t     -> "pop" ++ suffix t
    Return t  -> prefix t ++ "return"
    Call f    -> "invokestatic " ++ toJVM f
    Dup       -> "dup"
    Dup2      -> "dup2"

    IConst i  -> "ldc " ++ show i
    DConst d  -> "ldc2_w " ++ show d

    Add t     -> prefix t ++ "add"
    Sub t     -> prefix t ++ "sub"
    Mul t     -> prefix t ++ "mul"
    Div t     -> prefix t ++ "div"

    Label l     -> show l ++ ":"
    IfEq l    -> "ifeq " ++ show l

    Goto l    -> "goto " ++ show l

    IfCmpEq l -> "if_icmpeq " ++ show l
    IfCmpLt l -> "if_icmplt " ++ show l
    IfCmpLe l -> "if_icmple " ++ show l
    IfCmpGt l -> "if_icmpgt " ++ show l
    IfCmpGe l -> "if_icmpge " ++ show l
    IfCmpNe l -> "if_icmpne " ++ show l
    IfCmpg    -> "dcmpg"

    I2D       -> "i2d"


-- | Type-prefix for JVM instructions.
prefix :: Type -> String
prefix t = case t of
  Tdouble -> "d"
  Tint    -> "i"
  Tbool   -> "i"
  Tvoid   -> ""


-- | Type-suffix for JVM instructions.
suffix :: Type -> String
suffix t = case t of
  Tdouble -> "2"
  _ -> ""


-- * Labels

newLabel :: Compile Label
newLabel = do
  l <- gets nextLabel
  modify $ \ st -> st { nextLabel = succ l }
  return $ l

-- | Print top part of statement (for comments)

stmTop :: Stm -> String
stmTop s =
  case s of
    SWhile e _ -> "while (" ++ printTree e ++ ")"
    _ -> printTree s


-- * Auxiliary functions

grabOutput :: Compile () -> Compile Output
grabOutput m = do
  r <- ask
  s  <- get
  let ((), s', w) = runRWS m r s
  put s'
  return w

-- * Auxiliary functions

funType :: Def -> FunType
funType (DFuns t _ args _) = FunType t $ map (\ (ADecl t' _) -> t') args

--type Cxt = [Map Id Var]
--type Var = (Addr, Type)

newVar :: Id -> Type -> Compile()
newVar id typ = do
    ll <- gets limitLocals
    m <- gets cxt

    case m of
        []     -> return()
        mp:mps -> modify $ \ st -> st {cxt = ((Map.insert id (ll, typ) mp):mps),
                    limitLocals = case typ of
                                    Tdouble -> ll + 2
                                    _       -> ll + 1
                                  } 


lookupVar :: Id -> Compile(Addr,Type)
lookupVar id = do
    con <- gets cxt
    case lookupHelper con id of
        Just(addr, typ) -> return (addr, typ)
        Nothing         -> fail $ "The variable has not been declared yet"

lookupHelper :: Cxt -> Id -> Maybe (Addr, Type)
lookupHelper []     id = Nothing
lookupHelper (c:cs) id = case Map.lookup id c of
                        Just(addr, typ) -> return (addr, typ)
                        Nothing         -> lookupHelper cs id

inNewBlock :: Compile()
inNewBlock = do
    con <- gets cxt
    modify $ \ st -> st {cxt = (Map.empty:con)}

exitBlock :: Compile ()
exitBlock = do
    con <- gets cxt
    case con of
        []     -> return ()
        (c:cs) -> modify $ \ st -> st {cxt = cs}

