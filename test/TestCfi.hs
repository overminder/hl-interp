module TestCfi where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens

import BaseMachine
import BaseEval
import Cfi

tests = describe "BaseEval+Cfi" $ do
  it "can't even execute HaltI if tagged incorrectly" $ do
    let
      mach = (machPc._2 .~ CodeAddrT 10) 
           . (machProg.ix 0._2 .~ CodeAddrT 20)
           $ mkMach [(0, HaltI)]
    runMach ps0 mach `shouldBe` (ps0, mach, NoApplicableRule)

  it "executes HaltI correctly" $ do
    let
      mach = mkMach [(0, HaltI)]
    runMach ps0 mach `shouldBe` (ps0, mach, ExecutedHaltInst)

  it "executes ConstI and JumpI correctly" $ do
    let
      mach = mkMach' [ (0, (ConstI 100 0, CodeBotT))
                     , (1, (JumpI 0, CodeAddrT 1))
                     , (100, (HaltI, CodeAddrT 100))
                     ]
      finalMach = (machRegs %~ M.insert 0 (100, defaultT))
                . (machPc .~ (100, CodeAddrT 1))
                $ mach 
      ps1 = CfiJumps (S.fromList [(1, 100)])
    runMach ps1 mach `shouldBe` (ps1, finalMach, ExecutedHaltInst)

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
 where
  mkMach x = (machProg .~ M.fromList (tag x))
           . (machPc .~ (0, CodeBotT))
           $ (emptyMachine @ CfiTag)
  tag = map $ \(ix, inst) -> (ix, (inst, CodeBotT))
  mkMach' x = (machProg .~ M.fromList x)
            . (machPc .~ (0, CodeBotT))
            $ (emptyMachine @ CfiTag)
  ps0 = CfiJumps S.empty
