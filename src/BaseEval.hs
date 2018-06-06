module BaseEval where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import BaseMachine
import Control.Lens

putMach (x, y, z) = do
  _2.machRegs .= x
  _2.machHeap .= y
  _2.machPc .= z

-- Simplification:
--   * separated code and heap/stack storage: inst fetched from elsewhere
--   * Tag rule match failure: different monadic behavior per-tag?
--   * Halt: another monadic effect.
--   Current:  just halt mach on every failure, but distinguish
step :: TagMach p a => MachPolicyM p a ()
step = do
  Machine regs heap pc@(pcv, pcTag) prog <- use _2
  (inst, instTag) <- maybe (throwMach CodeAddrNotMapped) pure (M.lookup pcv prog)
  let
    runRule = runRule0 pcTag instTag
    doJump pc tag = _2.machPc .= (pc, tag)
    bumpPc pcTag' = doJump (pcv + 1) pcTag'
    modifyR f = _2.machRegs %= f
    modifyH f = _2.machHeap %= f
  -- Instruction dispatch
  case inst of
    NopI -> do
      (pcTag', NopO) <- runRule NopT
      bumpPc pcTag'

    HaltI -> do
      (pcTag', HaltO) <- runRule HaltT
      throwMach ExecutedHaltInst

    ConstI imm rd -> do
      let (_, rdTag) = readReg rd regs
      (pcTag', ConstO rdTag') <- runRule (ConstT rdTag)
      modifyR $ writeReg rd (imm, fromMaybe rdTag rdTag')
      bumpPc pcTag'

    MovI rs rd -> do
      let (rsv, rsTag) = readReg rs regs
          (_, rdTag) = readReg rd regs
      (pcTag', MovO rdTag') <- runRule (MovT rsTag rdTag)
      modifyR $ writeReg rd (rsv, fromMaybe rdTag rdTag')
      bumpPc pcTag'

    LoadI rp rd -> do
      let (_, rdTag) = readReg rd regs
          (rpv, rpTag) = readReg rp regs
          ((memv, memTag), locTag) = readHeap rpv heap
      (pcTag', LoadO rdTag') <- runRule (LoadT rpTag rdTag memTag locTag)
      modifyR $ writeReg memv (rd, fromMaybe rdTag rdTag')
      bumpPc pcTag'

    StoreI rp rs -> do
      let (rpv, rpTag) = readReg rp regs
          ((memv, memTag), locTag) = readHeap rpv heap
          (rsv, rsTag) = readReg rs regs
      (pcTag', StoreO memTag' locTag') <- runRule (StoreT rpTag rsTag memTag locTag)
      modifyH $ writeHeap rpv ((memv, fromMaybe memTag memTag'),
        fromMaybe locTag locTag')
      bumpPc pcTag'

    BinOpI op r1 r2 rd -> do
      let (r1v, r1Tag) = readReg r1 regs
          (r2v, r2Tag) = readReg r2 regs
          (_, rdTag) = readReg rd regs
      (pcTag', BinOpO rdTag') <- runRule (BinOpT r1Tag r2Tag rdTag)
      modifyR $ writeReg rd (runOp op r1v r2v,
        fromMaybe rdTag rdTag')
      bumpPc pcTag'

    JalI rd rlink -> do
      let (rdv, rdTag) = readReg rd regs
      let (_, rlinkTag) = readReg rlink regs
      (pcTag', JalO rlinkTag') <- runRule (JalT rdTag rlinkTag)
      modifyR $ writeReg rlink (pcv + 1,
        fromMaybe rlinkTag rlinkTag')
      doJump rdv pcTag'

    JumpI rd -> do
      let (rdv, rdTag) = readReg rd regs
      (pcTag', JumpO) <- runRule (JumpT rdTag)
      doJump rdv pcTag'

    BnzI rs offset -> do
      let (rdv, rdTag) = readReg rs regs
      (pcTag', BnzO) <- runRule (BnzT rdTag)
      doJump (if rdv /= 0 then pcv + offset else pcv + 1) pcTag'
  -- Done
  pure ()
 where
  runRule0 pcTag instTag extra = liftPolicy $
    tagRule pcTag instTag extra

runOp = \case
  AddO -> (+)
  SubO -> (-)
  MulO -> (*)
  DivO -> div
  LtO -> b (<)
  GtO -> b (>)
  EqO -> b (==)
 where
  b op x y = if op x y then 1 else 0

runMach :: TagMach p t => p -> Machine t -> (p, Machine t, HaltReason)
runMach = go
 where
  go p m = case execStateT step (p, m) of
    Left x -> (p, m, x)
    Right (p', m') -> go p' m'
