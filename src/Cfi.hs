module Cfi where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Map as M
import qualified Data.Set as S

import BaseMachine

data CodeLoc
  = BotC
  | AddrC Int
  deriving (Show, Eq)

pattern CodeAddrT x = CodeT (AddrC x)
pattern CodeBotT = CodeT BotC

data CfiTag
  = CodeT CodeLoc
  | DataT
  | NoT  -- `other registers are always tagged \textbullet?`
  deriving (Show, Eq)

newtype CfiJumps = CfiJumps { unCfiJumps :: S.Set (Int, Int) }
  deriving (Show, Eq)

instance Tag CfiTag where
  defaultT = NoT

type CfiM a = MachM CfiJumps a

hasJump :: (Int, Int) -> CfiM Bool
hasJump x = gets (S.member x . unCfiJumps)

noSuchRule = throwMach NoApplicableRule

instance TagMach CfiJumps CfiTag where
  tagRule inst pcTag instTag extra = do
    let
      checkCfi origRes = case (pcTag, instTag) of
        -- If the last instr is a jump (so pcTag is Code any), this instTag
        -- will always be a
        -- jump destination. This will be recorded in the allowed jump set
        -- so we can check its validity.
        -- In all other cases, pcTag will be Code bot.
        (CodeAddrT src, CodeAddrT dst) -> checkCfiSrcDst origRes src dst
        (CodeBotT, CodeT _) -> pure origRes
        _ -> noSuchRule
      checkCfiSrcDst origRes src dst = do
        ok <- hasJump (src, dst)
        if ok then pure origRes else noSuchRule
    -- Rule dispatch 
    case (inst, extra) of
      -- Nop: returns (pc, _, _)
      (NopI, NopT) ->
        checkCfi (CodeBotT, NopO)

      (HaltI, HaltT) ->
        checkCfi (CodeBotT, HaltO)

      -- Const: returns (pc, rd, _)
      (ConstI {}, ConstT {..}) ->
        checkCfi (CodeBotT, ConstO { _rdTagOut = _rdTagIn })

      -- Store: returns (pc mem loc)
      (StoreI {}, StoreT {..}) -> do
        let
          out = StoreO {
            _memTagOut = _rsTagIn,
            _locTagOut = _locTagIn
          }
        case _locTagIn of
          DataT -> checkCfi (CodeBotT, out)
          _ -> noSuchRule

      -- Jal: returns (pc rlink)
      (JalI {}, JalT {..}) ->
        checkCfi (instTag, JalO { _rlinkTagOut = pcTag })

      (JumpI {}, JumpT {..}) ->
        checkCfi (instTag, JumpO)

      _ ->
        shouldntHappen "Inst and InstTagIn mismatch"
