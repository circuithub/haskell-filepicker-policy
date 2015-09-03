{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Filepicker.PolicySpec (spec) where


import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BL
import           Data.Time
import           Network.Filepicker.Policy
import           Test.Hspec
import qualified Text.RawString.QQ         as RAW



expiryDate :: UTCTime
expiryDate = UTCTime (fromGregorian 2020 0 1) 0

spec :: Spec
spec = do
  describe "policy json" $ do
    it "read policy" $ do
      (A.encode . setHandle "AAAAAAAAAAAAAAAAAAAA" . addCall Read . expiryToPolicy $ expiryDate)
        `shouldBe`
        [RAW.r|{"expiry":1577836800,"handle":"AAAAAAAAAAAAAAAAAAAA","call":["read"]}|]

  describe "signed policy URL parameters" $ do
    it "read policy" $ do
      (encodePolicyS "OOOOOOOOOOOOOOOOOOOOOOOOO" . setHandle "AAAAAAAAAAAAAAAAAAAA" . addCall Read . expiryToPolicy $ expiryDate)
        `shouldBe`
        [RAW.r|signature=c4cf11c73fa1511b558f1182547079ad54e9976837f25cdf820eb3c04a402dd6&policy=eyJleHBpcnkiOjE1Nzc4MzY4MDAsImhhbmRsZSI6IkFBQUFBQUFBQUFBQUFBQUFBQUFBIiwiY2FsbCI6WyJyZWFkIl19|]


