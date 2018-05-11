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

class Tag t where
  defaultT :: t

-- A parameterized State monad to contain all the machine states,
-- including the AP and the rule coprocessor state.
type MachM s t = StateT s (Either HaltReason) t
-- p: Policy state type, t: tag type
type MachPolicyM p t a = MachM (p, Machine t) a

throwMach :: HaltReason -> MachM s t
throwMach = lift . Left 

shouldntHappen :: String -> MachM s t
shouldntHappen = throwMach . ShouldntHappen

class Tag t => TagMach p t where
  tagRule :: t -> t                     -- pc, inst tags
          -> InstTagIn t                -- input tags
          -> MachM p (t, InstTagOut t)  -- pc, output tags

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
  deriving (Show, Eq)

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

emptyMachine :: Tag t => Machine t
emptyMachine = Machine M.empty M.empty (0, defaultT) M.empty

readReg :: Tag a => Reg -> RegMap a -> VT a
readReg = M.findWithDefault (0, defaultT)

readHeap :: Tag a => Int -> HeapMap a -> (VT a, a)
readHeap = M.findWithDefault ((0, defaultT), defaultT)

writeReg :: Tag a => Reg -> VT a -> RegMap a -> RegMap a
writeReg = M.insert

writeHeap :: Tag a => Int -> (VT a, a) -> HeapMap a -> HeapMap a
writeHeap = M.insert

noSuchRule = throwMach NoApplicableRule
