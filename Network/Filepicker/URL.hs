{-# LANGUAGE OverloadedStrings #-}
module Network.Filepicker.URL
  ( -- Re-exports
    FilepickerHandle (FilepickerHandle, unFilepickerHandle)
    -- Exports
  , FilepickerUrl (unFilepickerUrl)
  , FilepickerSignedUrl (unFilepickerSignedUrl)
  , filepickerUrl
  , filepickerSignedUrl
  ) where

import           Data.Aeson.Types          (ToJSON, toJSON)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Network.Filepicker.Handle
import           Network.Filepicker.Policy

newtype FilepickerUrl = FilepickerUrl { unFilepickerUrl :: ByteString } deriving (Eq, Show)
newtype FilepickerSignedUrl = FilepickerSignedUrl { unFilepickerSignedUrl :: ByteString } deriving (Eq, Show)
instance ToJSON FilepickerUrl where
  toJSON = toJSON . TE.decodeUtf8 . unFilepickerUrl
instance ToJSON FilepickerSignedUrl where
  toJSON = toJSON . TE.decodeUtf8 . unFilepickerSignedUrl

filepickerUrl :: FilepickerHandle -> FilepickerUrl
filepickerUrl h = FilepickerUrl ("https://www.filepicker.io/api/file/" <> unFilepickerHandle h)

filepickerSignedUrl :: ByteString -> Policy -> FilepickerHandle -> FilepickerSignedUrl
filepickerSignedUrl secret policy h = FilepickerSignedUrl ((unFilepickerUrl . filepickerUrl) h <> encodePolicyParamBS secret policy)

