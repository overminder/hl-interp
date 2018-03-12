module Sealing where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Map as M
import qualified Data.Set as S

import BaseMachine

data STag
  = Data
  | Sealed Int
  | Key Int
  -- | A virtual instruction that represents a service
  | VirtInst Service
  deriving (Show, Eq)
  
data Service
  = MkKey
  | Seal
  | Unseal
  deriving (Show, Eq)

newtype SState = SState Int
  deriving (Show, Eq)

emptySState = SState 1

nextKey (SState i) = (i, SState $ i + 1)

instance Tag STag where
  defaultT = Data

type SealingM a = MachM SState a

instance TagMach SState STag where
  tagRule pcTag instTag extra = do
    let
      checkS origRes extra = checkS0 origRes $ [pcTag, instTag] ++ extra
      checkS' origRes = checkS origRes []
      -- Simply ensure that pc, inst and all args are not sealed.
      checkS0 origRes extra = if all (== Data) extra
        then pure origRes
        else noSuchRule

    -- Rule dispatch 
    case extra of
      NopT ->
        checkS' (Data, NopO)

      HaltT ->
        checkS' (Data, HaltO)

      ConstT {..} -> case instTag of
        VirtInst MkKey -> do
          k <- state nextKey
          pure (Data, ConstO { _rdTagOut = Key k })
        _ -> checkS' (Data, ConstO { _rdTagOut = _rdTagIn })

      MovT {..} ->
        checkS' (Data, MovO { _rdTagOut = _rsTagIn })

      LoadT {..} ->
        checkS (Data, LoadO { _rdTagOut = _memTagIn }) [_locTagIn]

      StoreT {..} ->
        checkS (Data, StoreO { _memTagOut = _rsTagIn, _locTagOut = _locTagIn })
               [_rpTagIn]

      JalT {..} ->
        checkS (Data, JalO { _rlinkTagOut = pcTag }) [_rdTagIn]

      JumpT {..} ->
        checkS (Data, JumpO) [_rdTagIn]

      BnzT {..} ->
        checkS (Data, BnzO) [_rsTagIn]

      -- Both seal and unseal are binary operators.
      BinOpT {..} -> case (instTag, _r1TagIn, _r2TagIn) of
        (VirtInst Seal, Data, Key k) ->
          pure (Data, BinOpO { _rdTagOut = Sealed k })
        (VirtInst Unseal, Sealed k0, Key k) -> if k0 == k
          then pure (Data, BinOpO { _rdTagOut = Data })
          else noSuchRule
        _ -> checkS (Data, BinOpO { _rdTagOut = _r1TagIn })
                    [_r1TagIn, _r2TagIn]

