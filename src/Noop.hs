module Noop where

import BaseMachine

newtype NoopTag = NoopTag ()
  deriving (Show, Eq)
newtype NoopState = NoopState ()
  deriving (Show, Eq)

noopTag = NoopTag ()
noopState = NoopState ()

checkNoop (pcTag, instTags) = pure (pcTag, fmap Just instTags)

instance TagMach NoopState NoopTag where
  defaultRegT = pure noopTag
  defaultHeapT = pure noopTag

  tagRule pcTag instTag extra = do
    -- Rule dispatch 
    case extra of
      -- Nop: returns (pc, _, _)
      NopT ->
        checkNoop (noopTag, NopO)

      HaltT ->
        checkNoop (noopTag, HaltO)

      MovT {..} ->
        checkNoop (noopTag, MovO { _rdTagOut = _rsTagIn })

      LoadT {..} ->
        checkNoop (noopTag, LoadO { _rdTagOut = _memTagIn })

      -- Const: returns (pc, rd, _)
      ConstT {..} ->
        checkNoop (noopTag, ConstO { _rdTagOut = _rdTagIn })

      -- Store: returns (pc mem loc)
      StoreT {..} -> do
        let
          out = StoreO {
            _memTagOut = _rsTagIn,
            _locTagOut = _locTagIn
          }
        checkNoop (noopTag, out)

      -- Jal: returns (pc rlink)
      JalT {..} ->
        checkNoop (instTag, JalO { _rlinkTagOut = pcTag })

      JumpT {..} ->
        checkNoop (instTag, JumpO)

      BnzT {..} ->
        checkNoop (instTag, BnzO)

      BinOpT {..} ->
        checkNoop (instTag, BinOpO { _rdTagOut = noopTag })

