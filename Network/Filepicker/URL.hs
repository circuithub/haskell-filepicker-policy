{-# LANGUAGE OverloadedStrings #-}
module Network.Filepicker.URL
  ( -- Re-exports
    FilepickerHandle (FilepickerHandle, unFilepickerHandle)
  , Policy (..)
    -- Exports
  , FilepickerUrl (unFilepickerUrl)
  , FilepickerSignedUrl (unFilepickerSignedUrl)
  , filepickerUrl
  , filepickerSignedUrl
  , handleFromUrlBS
  , handleFromUrlText
  , handleFromUrlS
  ) where

import           Data.Aeson.Types          (ToJSON, toJSON)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC8
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
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

filepickerRoot :: ByteString
filepickerRoot = "https://www.filepicker.io/api/file/"

filepickerUrl :: FilepickerHandle -> FilepickerUrl
filepickerUrl h = FilepickerUrl (filepickerRoot <> unFilepickerHandle h)

filepickerSignedUrl :: ByteString -> Policy -> FilepickerHandle -> FilepickerSignedUrl
filepickerSignedUrl secret policy h = FilepickerSignedUrl ((unFilepickerUrl . filepickerUrl) h <> "?" <> encodePolicyParamBS secret policy)

handleFromUrlBS :: ByteString -> Maybe FilepickerHandle
handleFromUrlBS url = if filepickerRoot `B.isPrefixOf` url then (Just . FilepickerHandle . BC8.takeWhile (/= '?') . B.drop (B.length filepickerRoot)) url else Nothing

handleFromUrlText :: Text -> Maybe FilepickerHandle
handleFromUrlText = handleFromUrlBS . TE.encodeUtf8

handleFromUrlS :: String -> Maybe FilepickerHandle
handleFromUrlS = handleFromUrlBS . BC8.pack

