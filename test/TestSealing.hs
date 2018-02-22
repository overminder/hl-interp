module TestSealing where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens

import BaseMachine
import BaseEval
import Sealing

tests = describe "BaseEval+Sealing" $ do
  it "can't even execute HaltI if tagged incorrectly" $ do
    let
      mach = (machPc._2 .~ Sealed 1)
           $ mkMach [(0, HaltI)]
    runMach ps0 mach `shouldBe` (ps0, mach, NoApplicableRule)
    let
      mach = (machProg.ix 0._2 .~ Sealed 1)
           $ mkMach [(0, HaltI)]
    runMach ps0 mach `shouldBe` (ps0, mach, NoApplicableRule)

  it "executes HaltI correctly" $ do
    let
      mach = mkMach [(0, HaltI)]
    runMach ps0 mach `shouldBe` (ps0, mach, ExecutedHaltInst)

  it "executes ConstI and JumpI correctly" $ do
    let
      mach = mkMach [ (0, (ConstI 100 0))
                    , (1, (JumpI 0))
                    , (100, (HaltI))
                    ]
      finalMach = (machRegs %~ M.insert 0 (100, defaultT))
                . (machPc .~ (100, defaultT))
                $ mach 
    runMach ps0 mach `shouldBe` (ps0, finalMach, ExecutedHaltInst)

  it "calls mkKey correctly" $ do
    let
      mach = mkMach' [ (0, (ConstI 0 0, VirtInst MkKey))
                     , (1, (HaltI, defaultT))
                     ]
      finalMach = (machRegs %~ M.insert 0 (0, Key 1))
                . (machPc .~ (1, defaultT))
                $ mach
      ps1 = SState 2
    runMach ps0 mach `shouldBe` (ps1, finalMach, ExecutedHaltInst)

  it "calls seal correctly" $ do
    let
      mach = mkMach' [ (0, (ConstI 0 0, VirtInst MkKey))
                     , (1, (ConstI 42 1, defaultT))
                     , (2, (BinOpI AddO 1 0 2, VirtInst Seal))
                     , (3, (HaltI, defaultT))
                     ]
      finalMach = (machRegs %~ M.insert 0 (0, Key 1)
                  . M.insert 1 (42, Data)
                  . M.insert 2 (42, Sealed 1))
                . (machPc .~ (3, defaultT))
                $ mach
      ps1 = SState 2
    runMach ps0 mach `shouldBe` (ps1, finalMach, ExecutedHaltInst)

  it "calls unseal correctly" $ do
    let
      mach = mkMach' [ (0, (ConstI 0 0, VirtInst MkKey))
                     , (1, (ConstI 42 1, defaultT))
                     , (2, (BinOpI AddO 1 0 2, VirtInst Seal))
                     , (3, (BinOpI AddO 2 0 1, VirtInst Unseal))
                     , (4, (HaltI, defaultT))
                     ]
      finalMach = (machRegs %~ M.insert 0 (0, Key 1)
                  . M.insert 1 (42, Data)
                  . M.insert 2 (42, Sealed 1))
                . (machPc .~ (4, defaultT))
                $ mach
      ps1 = SState 2
    runMach ps0 mach `shouldBe` (ps1, finalMach, ExecutedHaltInst)

  it "halts when trying to adding a sealed word with another data word" $ do
    let
      mach = mkMach' [ (0, (ConstI 0 0, VirtInst MkKey))
                     , (1, (ConstI 42 1, defaultT))
                     , (2, (BinOpI AddO 1 0 2, VirtInst Seal))
                     , (3, (BinOpI AddO 2 1 0, defaultT))
                     ]
      finalMach = (machRegs %~ M.insert 0 (0, Key 1)
                  . M.insert 1 (42, Data)
                  . M.insert 2 (42, Sealed 1))
                . (machPc .~ (3, defaultT))
                $ mach
      ps1 = SState 2
    runMach ps0 mach `shouldBe` (ps1, finalMach, NoApplicableRule)

  it "halts when trying to adding a key with another data word" $ do
    let
      mach = mkMach' [ (0, (ConstI 0 0, VirtInst MkKey))
                     , (1, (ConstI 42 1, defaultT))
                     , (2, (BinOpI AddO 0 1 2, defaultT))
                     ]
      finalMach = (machRegs %~ M.insert 0 (0, Key 1)
                  . M.insert 1 (42, Data))
                . (machPc .~ (2, defaultT))
                $ mach
      ps1 = SState 2
    runMach ps0 mach `shouldBe` (ps1, finalMach, NoApplicableRule)

  {-
  it "clears tag after jumping to an landing inst" $ do
    let
      mach = mkMach' [ (0, (ConstI 100 0, CodeBotT))
                     , (1, (JumpI 0, CodeAddrT 1))
                     , (100, (NopI, CodeAddrT 100))
                     , (101, (HaltI, CodeBotT))
                     ]
      finalMach = (machRegs %~ M.insert 0 (100, defaultT))
                . (machPc .~ (101, CodeBotT))
                $ mach 
      ps1 = CfiJumps (S.fromList [(1, 100)])
    runMach ps1 mach `shouldBe` (ps1, finalMach, ExecutedHaltInst)

  it "runs simple function with dst-src jumps" $ do
    let
      mach = mkMach' [ (0, (ConstI 100 0, CodeBotT))
                     , (1, (JalI 0 1, CodeAddrT 1))
                     , (2, (HaltI, CodeAddrT 2))
                     , (100, (JumpI 1, CodeAddrT 100))
                     ]
      finalMach = (machRegs %~ M.union (M.fromList [(0, (100, NoT)), (1, (2, CodeBotT))]))
                . (machPc .~ (2, CodeAddrT 100))
                $ mach 
      ps1 = CfiJumps (S.fromList [(1, 100), (100, 2)])
    runMach ps1 mach `shouldBe` (ps1, finalMach, ExecutedHaltInst)
    -}
 where
  mkMach x = (machProg .~ M.fromList (tag x))
           . (machPc .~ (0, defaultT))
           $ (emptyMachine @ STag)
  tag = map $ \(ix, inst) -> (ix, (inst, defaultT))
  mkMach' x = (machProg .~ M.fromList x)
            . (machPc .~ (0, defaultT))
            $ (emptyMachine @ STag)
  ps0 = emptySState

