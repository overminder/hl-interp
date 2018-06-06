{-# LANGUAGE TemplateHaskell #-}

module BaseMachine where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as M
import Control.Lens

-- Unlimited regs
type Reg = Int

type RegMap t = M.Map Reg (VT t)
type HeapMap t = M.Map Int (VT t, t) -- (value+tag, loc tag)

-- A parameterized State monad to contain all the machine states,
-- including the AP and the rule coprocessor state.
type MachM s a = StateT s (Either HaltReason) a
-- p: Policy state type, t: tag type
type MachPolicyM p t a = MachM (p, Machine t) a

throwMach :: HaltReason -> MachM s t
throwMach = lift . Left 

shouldntHappen :: String -> MachM s t
shouldntHappen = throwMach . ShouldntHappen

class TagMach p t | t -> p where
  tagRule :: t -> t                            -- pc, inst tags
          -> InstTagIn t                       -- input tags
          -> MachM p (t, InstTagOut (Maybe t)) -- pc, output tags
  defaultRegT :: MachM p t
  defaultHeapT :: MachM p t

liftPolicy :: MachM p a -> MachPolicyM p t a
liftPolicy m0 = do
  (p, s) <- get
  case runStateT m0 p of
    Right (b, p') -> put (p', s) *> pure b
    Left x -> throwMach x

-- Machine word
type Val = Int

-- Tagged machine word
type VT a = (Val, a)

-- Per-inst record to pack up the tags.
-- XXX: Can we somehow couple its variant with Inst?
data InstTagIn a
  = NopT
  | HaltT
  | ConstT { _rdTagIn :: a }
  | MovT { _rsTagIn :: a, _rdTagIn :: a }
  | StoreT { _rpTagIn :: a, _rsTagIn :: a, _memTagIn :: a, _locTagIn :: a }
  | LoadT { _rpTagIn :: a, _rdTagIn :: a, _memTagIn :: a, _locTagIn :: a }
  | BinOpT { _r1TagIn :: a, _r2TagIn :: a, _rdTagIn :: a }
  | JalT { _rdTagIn :: a, _rlinkTagIn :: a }
  | JumpT { _rdTagIn :: a }
  | BnzT { _rsTagIn :: a }
  deriving (Show, Eq)

data InstTagOut a
  = NopO
  | HaltO
  | ConstO { _rdTagOut :: a }
  | MovO { _rdTagOut :: a }
  | LoadO { _rdTagOut :: a }
  | StoreO { _memTagOut :: a, _locTagOut :: a }
  | JalO { _rlinkTagOut :: a }
  | BinOpO { _rdTagOut :: a }
  | JumpO
  | BnzO
  deriving (Show, Eq, Functor)

-- GNU syntax, e.g. inst r_s r_d
data Inst
  = NopI
  | HaltI
  | ConstI Val Reg
  | MovI Reg Reg          -- r_s, r_d
  | LoadI Reg Reg         -- r_p, r_d
  | StoreI Reg Reg        -- r_p, r_s
  | BinOpI Op Reg Reg Reg -- r_1, r_2, r_d
  | JumpI Reg             -- r_d
  | JalI Reg Reg          -- r_d, r_link
  | BnzI Reg Int          -- r_s, imm
  deriving (Show, Eq)

data Op
  = AddO
  | SubO
  | MulO
  | DivO
  | LtO
  | GtO
  | EqO
  deriving (Show, Eq, Ord)

type Prog a = M.Map Int (Inst, a)

data Machine a = Machine {
  _machRegs :: RegMap a,
  _machHeap :: HeapMap a,
  _machPc :: VT a,
  _machProg :: Prog a
}
  deriving (Show, Eq)

data HaltReason
  = ExecutedHaltInst
  | NoApplicableRule
  | CodeAddrNotMapped
  | ShouldntHappen String
  | ExplicitFailure String
  deriving (Show, Eq)

makeLenses ''Machine

emptyMachine' :: t -> Machine t
emptyMachine' t0 = Machine M.empty M.empty (0, t0) M.empty

readReg :: TagMach s t => Reg -> RegMap t -> MachM s (VT t)
readReg r m = do
  rt <- defaultRegT
  pure $ M.findWithDefault (0, rt) r m

readHeap :: TagMach s t => Int -> HeapMap t -> MachM s (VT t, t)
readHeap addr m = do
  ht <- defaultHeapT
  pure $ M.findWithDefault ((0, ht), ht) addr m

writeReg :: Reg -> VT a -> RegMap a -> RegMap a
writeReg = M.insert

writeHeap :: Int -> (VT a, a) -> HeapMap a -> HeapMap a
writeHeap = M.insert

noSuchRule = throwMach NoApplicableRule

