module ActivitySpec where

import TestUtils
import Workflow (
  CustomerAddress (..), 
  CustomerDistance (..),
  OrderConfirmation (..),
  Pizza (..),
  PizzaOrder (..),
  billCustomer,
  getDistance,
  )

import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Testing.MockActivityEnvironment (mkMockActivityEnvironment, runMockActivity)

spec :: Spec
spec = describe "test Activities" $ do
  describe "getDistance" $ do
    it "gets distance for a two-line address" $ do
      let input = CustomerAddress "701 Mission Street" "Apartment 9C"
      let expected = CustomerDistance 20
      mockEnv <- mkMockActivityEnvironment ()
      res <- runMockActivity mockEnv $ getDistance input
      res `shouldBe` expected

    it "gets distance for a one-line address" $ do
      let input = CustomerAddress "917 Delores Street" ""
      let expected = CustomerDistance 8
      mockEnv <- mkMockActivityEnvironment ()
      res <- runMockActivity mockEnv $ getDistance input
      res `shouldBe` expected

  describe "billCustomer" $ do
    it "returns the expected results for a basic test order" $ do
      mockEnv <- mkMockActivityEnvironment ()
      res <- runMockActivity mockEnv $ billCustomer mkBasicTestOrder
      res.ocOrderNumber `shouldBe` mkBasicTestOrder.poOrderNumber
      res.ocAmountCents `shouldBe` 2600

-- | A basic test order. Please create specific orders to test weekly
-- specials
mkBasicTestOrder :: PizzaOrder
mkBasicTestOrder = PizzaOrder addr 12345 [lgCheese, lgCheese]
  where
    addr = CustomerAddress "917 Delores Street" ""
    lgCheese = Pizza "Large, with cheese" 1300

