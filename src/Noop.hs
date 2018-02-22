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
  tagRule pcTag instTag extra = do
    -- Rule dispatch 
    case extra of
      -- Nop: returns (pc, _, _)
      NopT ->
        checkNoop (defaultT, NopO)

      HaltT ->
        checkNoop (defaultT, HaltO)

      MovT {..} ->
        checkNoop (defaultT, MovO { _rdTagOut = _rsTagIn })

      LoadT {..} ->
        checkNoop (defaultT, LoadO { _rdTagOut = _memTagIn })

      -- Const: returns (pc, rd, _)
      ConstT {..} ->
        checkNoop (defaultT, ConstO { _rdTagOut = _rdTagIn })

      -- Store: returns (pc mem loc)
      StoreT {..} -> do
        let
          out = StoreO {
            _memTagOut = _rsTagIn,
            _locTagOut = _locTagIn
          }
        checkNoop (defaultT, out)

      -- Jal: returns (pc rlink)
      JalT {..} ->
        checkNoop (instTag, JalO { _rlinkTagOut = pcTag })

      JumpT {..} ->
        checkNoop (instTag, JumpO)

      BnzT {..} ->
        checkNoop (instTag, BnzO)

      BinOpT {..} ->
        checkNoop (instTag, BinOpO { _rdTagOut = defaultT })

