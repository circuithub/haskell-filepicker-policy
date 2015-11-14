module Network.Filepicker.Handle
  ( FilepickerHandle (FilepickerHandle, unFilepickerHandle)
  ) where

import           Data.Aeson.Types   (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import qualified Data.Text.Encoding as TE

newtype FilepickerHandle = FilepickerHandle { unFilepickerHandle :: ByteString } deriving (Eq, Show, Read)
instance ToJSON FilepickerHandle where
  toJSON = toJSON . TE.decodeUtf8 . unFilepickerHandle
instance FromJSON FilepickerHandle where
  parseJSON = fmap (FilepickerHandle . TE.encodeUtf8) . parseJSON

