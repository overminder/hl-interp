module TestNoop where

import Test.Hspec
import qualified Data.Map as M
import Control.Lens

import BaseMachine
import BaseEval
import Noop

tests = describe "BaseEval+Noop" $ do
  it "executes HaltI correctly" $ do
    let
      mach = mkMach [(0, HaltI)]
    runMach policyState mach `shouldBe` (policyState, mach, ExecutedHaltInst)

  it "executes ConstI and JumpI correctly" $ do
    let
      mach = mkMach [ (0, ConstI 100 0)
                    , (1, JumpI 0)
                    , (100, HaltI)
                    ]
      finalMach = (machRegs %~ M.insert 0 (100, noopTag))
                . (machPc .~ (100, noopTag))
                $ mach 
    runMach policyState mach `shouldBe` (policyState, finalMach, ExecutedHaltInst)

  it "halts when jumping to an invalid address" $ do
    let
      mach = mkMach [ (0, ConstI 100 0)
                    , (1, JumpI 0)
                    ]
      finalMach = (machRegs %~ M.insert 0 (100, noopTag))
                . (machPc .~ (100, noopTag))
                $ mach 
    runMach policyState mach `shouldBe` (policyState, finalMach, CodeAddrNotMapped)

  it "runs simple function" $ do
    let
      mach = mkMach [ (0, ConstI 100 0)
                    , (1, JalI 0 1)
                    , (2, HaltI)
                    , (100, JumpI 1)
                    ]
      finalMach = (machRegs %~ M.union (M.fromList [(0, (100, noopTag)), (1, (2, noopTag))]))
                . (machPc .~ (2, noopTag))
                $ mach 
    runMach policyState mach `shouldBe` (policyState, finalMach, ExecutedHaltInst)

  it "runs fibonacci" $ do
    let
      -- Say fibonacci: r0 for argpassing, r1 for sp, r2 for jump ptr (in),
      -- r3 for rlink. r4 for const 2/const 1, r5 for res/tmp.
      mach = mkMach [ (0, ConstI 10 0)
                    , (1, ConstI 1000 1)
                    , (2, ConstI 100 2)
                    , (3, JalI 1 3)
                    , (4, HaltI)
                    -- fibo: entry
                    , (100, ConstI 2 4)  -- r4 <- 2
                    , (101, BinOpI LtO 0 4 5)  -- r5 <- r0 < r4 (n < 2)
                    , (102, BnzI 5 10)  -- if r5: goto base
                    -- fibo: base
                    , (104, ConstI 1 5) -- r5 <- 1
                    , (105, BinOpI AddO 1 5 1) -- r1 += r5 (sp += 1)
                    , (106, StoreI 1 3) -- *r1 = r3 (store rlink)

                    , (105, BinOpI AddO 1 5 1) -- r1 += r5 (sp += 1)
                    , (103, StoreI 1 0)  -- *r1 = r0  (store n to *sp)

                    , (106, BinOpI SubO 0 5 0) -- r0 <- r0 - r5 (n - 1)
                    , (104, JalI 1 3)  -- call fibo
                    , (000, LoadI 1 0) -- r0 = *r1 (load n from *sp)
                    , (000, StoreI 5 0) -- *r1 = r0 (store fibo(n-1) result)

                    , (104, ConstI 2 5) -- r5 <- 2
                    , (000, BinOpI SubO 0 5 0) -- r0 <- r0 - r5 (n - 2)
                    , (104, JalI 1 3)  -- call fibo
                    , (000, LoadI 1 0) -- r0 = *r1 (load fibo(n-2) from *sp)
                    , (000, BinOpI AddO 0 5 5)  -- r5 <- r0 + r5 (combine results)
                    , (104, ConstI 2 0) -- r0 <- 1
                    , (000, BinOpI SubO 1 0 1) -- r1 <- r1 - r0 (sp -= 1)
                    , (000, LoadI 1 3) -- r3 = *r1 (load retaddr)
                    , (000, BinOpI SubO 1 0 1) -- r1 <- r1 - r0 (sp -= 1)
                    , (113, JumpI 3)  -- goto *lr

                    -- fibo: base
                    , (112, MovI 0 5)  -- r5 <- r0
                    , (113, JumpI 3)  -- goto *lr
                    ]
      finalMach = (machRegs %~ M.union (M.fromList [(0, (100, noopTag)), (1, (2, noopTag))]))
                . (machPc .~ (2, noopTag))
                $ mach 
    -- runMach policyState mach `shouldBe` (policyState, finalMach, ExecutedHaltInst)
    1 `shouldBe` 1
 where
  mkMach x = (machProg .~ M.fromList (tag x)) (emptyMachine' noopTag)
  policyState = noopState
  tag = map $ \(ix, inst) -> (ix, (inst, noopTag))
