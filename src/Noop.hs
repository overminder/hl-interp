module Noop where

import BaseMachine

newtype NoopTag = NoopTag ()
  deriving (Show, Eq)
newtype NoopState = NoopState ()
  deriving (Show, Eq)

noopTag = NoopTag ()
noopState = NoopState ()

checkNoop = pure

instance Tag NoopTag where
  defaultT = noopTag

instance TagMach NoopState NoopTag where
  tagRule inst pcTag instTag extra = do
    -- Rule dispatch 
    case (inst, extra) of
      -- Nop: returns (pc, _, _)
      (NopI, NopT) ->
        checkNoop (defaultT, NopO)

      (HaltI, HaltT) ->
        checkNoop (defaultT, HaltO)

      -- Const: returns (pc, rd, _)
      (ConstI {}, ConstT {..}) ->
        checkNoop (defaultT, ConstO { _rdTagOut = _rdTagIn })

      -- Store: returns (pc mem loc)
      (StoreI {}, StoreT {..}) -> do
        let
          out = StoreO {
            _memTagOut = _rsTagIn,
            _locTagOut = _locTagIn
          }
        checkNoop (defaultT, out)

      -- Jal: returns (pc rlink)
      (JalI {}, JalT {..}) ->
        checkNoop (instTag, JalO { _rlinkTagOut = pcTag })

      (JumpI {}, JumpT {..}) ->
        checkNoop (instTag, JumpO)

      _ ->
        shouldntHappen "Inst and InstTagIn mismatch"
