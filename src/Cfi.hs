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

data CfiTag
  = CodeT CodeLoc
  | DataT
  -- | Default tag for all other registers.
  | NoT
  deriving (Show, Eq)

pattern CodeAddrT x = CodeT (AddrC x)
pattern CodeBotT = CodeT BotC

newtype CfiJumps = CfiJumps { unCfiJumps :: S.Set (Int, Int) }
  deriving (Show, Eq)

type CfiM a = MachM CfiJumps a

hasJump :: (Int, Int) -> CfiM Bool
hasJump x = gets (S.member x . unCfiJumps)

instance TagMach CfiJumps CfiTag where
  defaultRegT = pure NoT
  defaultHeapT = pure NoT

  tagRule pcTag instTag extra = do
    let
      checkCfi origRes = case (pcTag, instTag) of
        -- If the last instr is a jump (so pcTag is Code any), this instTag
        -- will always be a
        -- jump destination. This will be recorded in the allowed jump set
        -- so we can check its validity.
        -- In all other cases, pcTag will be Code bot.
        (CodeAddrT src, CodeAddrT dst) -> checkCfiSrcDst origRes src dst
        (CodeBotT, CodeT _) -> resOk origRes
        _ -> noSuchRule

      checkCfiSrcDst origRes src dst = do
        ok <- hasJump (src, dst)
        if ok then resOk origRes else noSuchRule

      resOk (pcTag', instTags') = pure (pcTag', fmap Just instTags')

    -- Rule dispatch 
    case extra of
      -- Nop: returns (pc, _, _)
      NopT ->
        checkCfi (CodeBotT, NopO)

      HaltT ->
        checkCfi (CodeBotT, HaltO)

      -- Const: returns (pc, rd, _)
      ConstT {..} ->
        checkCfi (CodeBotT, ConstO { _rdTagOut = _rdTagIn })

      MovT {..} ->
        checkCfi (CodeBotT, MovO { _rdTagOut = _rsTagIn })

      LoadT {..} ->
        checkCfi (instTag, LoadO { _rdTagOut = _memTagIn })

      -- Store: returns (pc mem loc)
      StoreT {..} -> do
        let
          out = StoreO {
            _memTagOut = _rsTagIn,
            _locTagOut = _locTagIn
          }
        case _locTagIn of
          DataT -> checkCfi (CodeBotT, out)
          _ -> noSuchRule

      -- Jal: returns (pc rlink)
      JalT {..} ->
        checkCfi (instTag, JalO { _rlinkTagOut = pcTag })

      JumpT {..} ->
        checkCfi (instTag, JumpO)

      BnzT {..} ->
        checkCfi (instTag, BnzO)

      BinOpT {..} ->
        checkCfi (instTag, BinOpO { _rdTagOut = NoT })

