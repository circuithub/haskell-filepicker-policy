{-# LANGUAGE OverloadedStrings #-}
module Network.Filepicker.URL
  ( -- Re-exports
    FilepickerHandle (FilepickerHandle, unFilepickerHandle)
  , Policy (..)
    -- Exports
  , FilepickerUrl (unFilepickerUrl)
  , FilepickerSignedUrl (unFilepickerSignedUrl)
  , ConversionOption
  , filepickerUrl
  , filepickerSignedUrl
  , filepickerConvert
  , filepickerSignedConvert
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

-- TODO: Improve support
data ConversionOption = Width Word
                      | Height Word
                      -- Fit
                      -- Crop
                      -- Format
                      -- Filter
                      -- Quality
                      -- Rotate
                      -- Watermark
                      -- WatermarkSize
                      -- WatermarkPosition

filepickerRoot :: ByteString
filepickerRoot = "https://www.filepicker.io/api/file/"

filepickerUrl :: FilepickerHandle -> FilepickerUrl
filepickerUrl handle = FilepickerUrl (filepickerRoot <> unFilepickerHandle handle)

filepickerSignedUrl :: ByteString -> Policy -> FilepickerHandle -> FilepickerSignedUrl
filepickerSignedUrl secret policy handle =
  FilepickerSignedUrl
  $ unFilepickerUrl (filepickerUrl handle)
    <> "?"
    <> encodePolicyParamBS secret policy

filepickerConvert :: FilepickerHandle -> [ConversionOption] -> FilepickerUrl
filepickerConvert handle options =
  FilepickerUrl
  $ unFilepickerUrl (filepickerUrl handle)
    <> "/convert?"
    <>  BC8.intercalate "&" (map encodeOption options)
  where
    encodeOption (Width w) = "w=" <> (BC8.pack . show) w
    encodeOption (Height h) = "h=" <> (BC8.pack . show) h

filepickerSignedConvert :: ByteString -> Policy -> FilepickerHandle -> [ConversionOption] -> FilepickerSignedUrl
filepickerSignedConvert secret policy handle options =
  FilepickerSignedUrl
  $ unFilepickerUrl (filepickerConvert handle options)
    <> (if null options then "?" else "&")
    <> encodePolicyParamBS secret policy

handleFromUrlBS :: ByteString -> Maybe FilepickerHandle
handleFromUrlBS url = if filepickerRoot `B.isPrefixOf` url then (Just . FilepickerHandle . BC8.takeWhile (/= '?') . B.drop (B.length filepickerRoot)) url else Nothing

handleFromUrlText :: Text -> Maybe FilepickerHandle
handleFromUrlText = handleFromUrlBS . TE.encodeUtf8

handleFromUrlS :: String -> Maybe FilepickerHandle
handleFromUrlS = handleFromUrlBS . BC8.pack

