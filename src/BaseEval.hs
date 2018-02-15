module BaseEval where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as M
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
    runRule = runRule0 inst pcTag instTag
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
      modifyR $ writeReg rd (imm, rdTag')
      bumpPc pcTag'

    StoreI rp rs -> do
      let (rpv, rpTag) = readReg rp regs
          ((memv, memTag), locTag) = readHeap rpv heap
          (rsv, rsTag) = readReg rs regs
      (pcTag', StoreO memTag' locTag') <- runRule (StoreT rpTag rsTag memTag locTag)
      modifyH $ writeHeap rpv ((memv, memTag'), locTag')
      bumpPc pcTag'

    JalI rd rlink -> do
      let (rdv, rdTag) = readReg rd regs
      let (_, rlinkTag) = readReg rlink regs
      (pcTag', JalO rlinkTag') <- runRule (JalT rdTag rlinkTag)
      modifyR $ writeReg rlink (pcv + 1, rlinkTag')
      doJump rdv pcTag'

    JumpI rd -> do
      let (rdv, rdTag) = readReg rd regs
      (pcTag', JumpO) <- runRule (JumpT rdTag)
      doJump rdv pcTag'
  -- Done
  pure ()
 where
  runRule0 inst pcTag instTag extra = liftPolicy $
    tagRule inst pcTag instTag extra

runMach :: TagMach p t => p -> Machine t -> (p, Machine t, HaltReason)
runMach = go
 where
  go p m = case execStateT step (p, m) of
    Left x -> (p, m, x)
    Right (p', m') -> go p' m'

