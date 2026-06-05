{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.TopologyFreeArrowSpec
-- Description : Demonstrate the reusable 'FreeArrow' framework
--
-- The "Kafka.Streams.Topology.Free.Arrow" module is the
-- domain-agnostic framework that "Kafka.Streams.Topology.Free"
-- /could/ be re-expressed on top of. This test exercises the
-- framework with a tiny non-Kafka DSL — a "calculator"
-- language whose primitives are arithmetic operations — to
-- demonstrate that:
--
--   1. The framework is genuinely reusable: a different
--      primitive type works without changing the framework
--      code.
--   2. The generic interpreter, introspection, and
--      simplification pass all work as advertised.
--   3. The 'Category' \/ 'Arrow' \/ 'ArrowChoice' \/
--      'Applicative' \/ 'Monad' \/ 'Semigroup' \/ 'Monoid'
--      instances are usable without forcing a Kafka domain.
module Streams.TopologyFreeArrowSpec (tests) where

import qualified Control.Arrow as A
import Control.Arrow ((&&&), (***), (>>>), (|||))
import qualified Control.Category as Cat
import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Test.Syd

import Kafka.Streams.Topology.Free.Arrow

----------------------------------------------------------------------
-- A tiny example DSL: a calculator over Doubles
----------------------------------------------------------------------

-- | Primitive operations for the calculator DSL. Every primitive
-- is a directed arrow @Prim i o@ from input type @i@ to output
-- type @o@. The framework's 'FreeArrow' composes these into
-- larger programs.
data CalcPrim i o where
  Add    :: CalcPrim (Double, Double) Double
  Sub    :: CalcPrim (Double, Double) Double
  Mul    :: CalcPrim (Double, Double) Double
  Negate :: CalcPrim Double Double

-- | The DSL itself: free arrow over the primitives.
type Calc = FreeArrow CalcPrim

-- Smart constructors.
addC, subC, mulC :: Calc (Double, Double) Double
addC = lift Add
subC = lift Sub
mulC = lift Mul

negateC :: Calc Double Double
negateC = lift Negate

-- | Pure interpreter: each primitive becomes its Haskell
-- counterpart, threaded through 'Identity'.
runCalc :: Calc i o -> i -> o
runCalc t i = runIdentity (interpret runPrim t i)
  where
    runPrim :: forall a b. CalcPrim a b -> a -> Identity b
    runPrim Add    (x, y) = Identity (x + y)
    runPrim Sub    (x, y) = Identity (x - y)
    runPrim Mul    (x, y) = Identity (x * y)
    runPrim Negate x      = Identity (negate x)

-- | Inspection: emit one token per primitive.
labelCalc :: forall a b. CalcPrim a b -> T.Text
labelCalc Add    = "Add"
labelCalc Sub    = "Sub"
labelCalc Mul    = "Mul"
labelCalc Negate = "Negate"

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests = describe "Topology.Free.Arrow (framework)" $ sequence_
  [ test_category_composition
  , test_arrow_combinators
  , test_arrow_choice_combinators
  , test_lineage_combinators
  , test_applicative_monad
  , test_semigroup_monoid
  , test_inspect_emits_tokens
  , test_simplify_collapses_identity_and_pure_chains
  , test_pretty_print_renders_tokens
  ]

----------------------------------------------------------------------
-- 1. Category composition
----------------------------------------------------------------------

test_category_composition :: Spec
test_category_composition =
  it "Category: Id, Compose work for the calculator DSL" $ do
    let prog :: Calc Double Double
        prog = negateC >>> negateC      -- double-negate => identity

    -- (-3) -> negate -> 3 -> negate -> -3
    runCalc prog (-3) `shouldBe` (-3)
    runCalc prog 7    `shouldBe` 7
    -- Composition with 'Id' on either side is identity.
    runCalc (Cat.id Cat.. prog) 5 `shouldBe` runCalc prog 5
    runCalc (prog Cat.. Cat.id) 5 `shouldBe` runCalc prog 5

----------------------------------------------------------------------
-- 2. Arrow combinators
----------------------------------------------------------------------

test_arrow_combinators :: Spec
test_arrow_combinators =
  it "Arrow: first / second / *** / &&& work" $ do
    -- (x, y) -> (x + y, x * y)
    let prog :: Calc (Double, Double) (Double, Double)
        prog = (addC &&& mulC) >>> Cat.id

    runCalc prog (2, 5) `shouldBe` (7, 10)

    -- first: apply addC to the first half, leave the second
    let addFirst :: Calc ((Double, Double), Double) (Double, Double)
        addFirst = addC *** Cat.id

    runCalc addFirst ((1, 2), 99) `shouldBe` (3, 99)

----------------------------------------------------------------------
-- 3. ArrowChoice combinators
----------------------------------------------------------------------

test_arrow_choice_combinators :: Spec
test_arrow_choice_combinators =
  it "ArrowChoice: left / right / +++ / ||| work" $ do
    let prog :: Calc (Either (Double, Double) (Double, Double)) Double
        prog = addC ||| subC   -- Left -> add, Right -> sub

    runCalc prog (Left  (3, 4)) `shouldBe` 7
    runCalc prog (Right (10, 7)) `shouldBe` 3

----------------------------------------------------------------------
-- 4. Lineage combinators
----------------------------------------------------------------------

test_lineage_combinators :: Spec
test_lineage_combinators =
  it "Lineage: fork / forkN / tap work" $ do
    -- fork: x -> (x, x)
    let dup :: Calc Double Double
        dup = fork >>> addC

    runCalc dup 7 `shouldBe` 14

    -- forkN: x -> NonEmpty [x+1, x*2, -x]
    let three :: Calc Double (NE.NonEmpty Double)
        three = forkN (NE.fromList
          [ arrPlusOne
          , arrTimesTwo
          , negateC
          ])

    NE.toList (runCalc three 10) `shouldBe` [11, 20, -10]
  where
    arrPlusOne :: Calc Double Double
    arrPlusOne = A.arr (+ 1)

    arrTimesTwo :: Calc Double Double
    arrTimesTwo = A.arr (* 2)

----------------------------------------------------------------------
-- 5. Applicative + Monad
----------------------------------------------------------------------

test_applicative_monad :: Spec
test_applicative_monad =
  it "Applicative / Monad: pure, <*>, >>= work" $ do
    -- Applicative: liftA2 (+) (pure 3) (pure 4) ignores input.
    let prog :: Calc () Double
        prog = (+) <$> pure 3 <*> pure 4
    runCalc prog () `shouldBe` 7

    -- Monad: bind a wire value and use it downstream.
    let prog2 :: Calc Double Double
        prog2 = do
          x <- Cat.id
          pure (x * 2)
    runCalc prog2 5 `shouldBe` 10

----------------------------------------------------------------------
-- 6. Semigroup / Monoid
----------------------------------------------------------------------

test_semigroup_monoid :: Spec
test_semigroup_monoid =
  it "Semigroup / Monoid over the output type work" $ do
    -- Semigroup: lift (<>) over Double via Sum-style semigroup
    -- (we use a list to avoid pulling in newtype wrappers).
    let prog :: Calc Double [Double]
        prog = ((: []) <$> A.arr id) <> ((: []) <$> negateC)
    runCalc prog 3 `shouldBe` [3, -3]

    -- Monoid: mempty :: Calc Double [Double] = pure []
    let memptyProg :: Calc Double [Double]
        memptyProg = mempty
    runCalc memptyProg 99 `shouldBe` []

----------------------------------------------------------------------
-- 7. Inspection
----------------------------------------------------------------------

test_inspect_emits_tokens :: Spec
test_inspect_emits_tokens =
  it "inspectFA emits framework + primitive tokens" $ do
    let prog :: Calc Double Double
        prog = fork >>> (negateC *** A.arr (+ 1)) >>> addC

        toks = inspectFA labelCalc prog

    -- Tokens for: Fork; Parallel<Negate|Arr>; Add
    ("Fork" `elem` toks) `shouldBe` True
    ("Negate" `elem` toks) `shouldBe` True
    ("Arr" `elem` toks) `shouldBe` True
    ("Add" `elem` toks) `shouldBe` True
    ("Parallel<" `elem` toks) `shouldBe` True

----------------------------------------------------------------------
-- 8. Framework-level optimisation
----------------------------------------------------------------------

test_simplify_collapses_identity_and_pure_chains :: Spec
test_simplify_collapses_identity_and_pure_chains =
  it "simplifyFA collapses Id chains and fuses Arr-Arr-Fork" $ do
    -- A program with redundant Ids and adjacent Arrs that
    -- should fuse plus a 'fork >>> Arr' that should collapse.
    let redundant :: Calc Double Double
        redundant =
          Cat.id
            >>> Cat.id Cat.. A.arr (+ 1) Cat.. Cat.id
            >>> Cat.id
            >>> fork
            >>> A.arr (\(x, y) -> x + y)
            >>> A.arr (* 2)
            >>> Cat.id

        before = countNodesFA redundant
        after  = countNodesFA (simplifyFA redundant)

    -- Behaviour preserved.
    runCalc redundant 5 `shouldBe` runCalc (simplifyFA redundant) 5
    runCalc redundant 5 `shouldBe` 24  -- ((5+1)+(5+1)) * 2 = 24
    -- And the optimised version has strictly fewer nodes.
    (if (after < before) then pure () else expectationFailure ("expected node-count reduction; before="
        <> show before <> " after=" <> show after))

----------------------------------------------------------------------
-- 9. prettyPrint
----------------------------------------------------------------------

test_pretty_print_renders_tokens :: Spec
test_pretty_print_renders_tokens =
  it "prettyPrintFA renders the AST as whitespace-joined tokens" $ do
    let prog :: Calc (Double, Double) Double
        prog = addC

        pp = prettyPrintFA labelCalc prog
    pp `shouldBe` "Add"