{-# LANGUAGE OverloadedLabels, DataKinds,TypeFamilies #-}
module PolyDictSpec(spec) where

import PolyDict(Assoc,Dict)
import qualified PolyDict as D

import Test.Hspec.Core.Spec
import Test.Hspec.Expectations
import Lens.Micro

data Test
type instance Assoc Test "foo" = String
type instance Assoc Test "bar" = Bool
type instance Assoc Test "baz" = Dict Test

dict0 :: Dict Test
dict0 = D.empty

dict1 = dict0 & (D.access #foo ?~ "Foo")
              . (D.access #bar ?~ True )
              . (D.access #baz ?~ dict0)

specDict0 :: Spec
specDict0 = do
  describe "empty" $ do
    it "lookup always be Nothing" $ do
        D.lookup #foo dict0 `shouldBe` Nothing
        D.lookup #bar dict0 `shouldBe` Nothing
        D.lookup #baz dict0 `shouldBe` Nothing

specDict1 :: Spec
specDict1 = do
  describe "insert" $ do
    it "has" $ do
      D.lookup #foo dict1 `shouldBe` (Just "Foo")
      D.lookup #bar dict1 `shouldBe` (Just True)
      D.lookup #baz dict1 `shouldBe` (Just dict0)

spec :: Spec
spec = do
    specDict0
    specDict1
