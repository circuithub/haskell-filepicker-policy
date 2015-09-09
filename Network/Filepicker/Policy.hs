{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Filepicker.Policy
  ( Policy(..)
  , Call(..)
  , expiryToPolicy
  , addCall
  , setHandle
  , encodePolicyBS
  , encodePolicyText
  , encodePolicyS
  , encodePolicyParamBS
  , encodePolicyParamText
  , encodePolicyParamS
  ) where

import           Crypto.Hash                (Digest)
import           Crypto.Hash.Algorithms     (SHA256)
import           Crypto.MAC.HMAC            (hmac, hmacGetDigest)
import qualified Data.Aeson.Encode          as A
import           Data.Aeson.Types           (ToJSON)
import qualified Data.Aeson.Types           as A
import           Data.ByteArray             (convert)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.UTF8       (fromString)
import qualified Data.Char                  as C
import           Data.Maybe
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           GHC.Generics

import           Debug.Trace

-- | https://www.filepicker.com/documentation/security/create-policy

data Policy = Policy
  { expiry    :: Int                -- ^ Seconds since the Epoch (1970).
  , call      :: Maybe (Set Call)   -- ^ Allowed operations
  , handle    :: Maybe (Text)       -- ^ A Filepicker file URL like https://www.filepicker.io/api/file/KW9EJhYtS6y48Whm2S6D has a handle of KW9EJhYtS6y48Whm2S6D
  , maxSize   :: Maybe Int          -- ^ The maximum size that can be stored into your s3. This only applies to the store command. Default to no limit.
  , minSize   :: Maybe Int          -- ^ The minimum size that can be stored into your s3. This only applies to the store command. Together with maxSize, this forms a range. The value of minSize should be smaller then maxSize. Default to 0.
  , path      :: Maybe Text         -- ^ For policies that store files, a perl-like regular expression that must match the path that the files will be stored under. Defaults to allowing any path ('.*').
  , container :: Maybe Text         -- ^ For policies that store files, a perl-like regular expression that must match the container that the files will be stored under. Defaults to allowing any container ('.*').
  } deriving (Show, Read, Eq, Generic)

-- | Create a policy given its expiry time, all other fields are left blank
expiryToPolicy :: UTCTime -> Policy
expiryToPolicy t = Policy (truncate . utcTimeToPOSIXSeconds $ t) Nothing Nothing Nothing Nothing Nothing Nothing

-- | Add a call type to a policy
addCall :: Call -> Policy -> Policy
addCall c p = p {call = Just . maybe (S.singleton c) (S.insert c) . call $ p}

-- | Set the handle of the policy
setHandle :: Text -> Policy -> Policy
setHandle h p = p {handle = Just h}

-- | Set the maxSize of the policy
setMaxSize :: Int -> Policy -> Policy
setMaxSize i p = p {maxSize = Just i}

-- | Set the minSize of the policy
setMinSize :: Int -> Policy -> Policy
setMinSize i p = p {minSize = Just i}

-- | Set the path of the policy
setPath :: Text -> Policy -> Policy
setPath t p = p {path = Just t}

-- | Set the container of the policy
setContainer :: Text -> Policy -> Policy
setContainer t p = p {container = Just t}

aesonPolicyOptions :: A.Options
aesonPolicyOptions = A.defaultOptions
  { A.fieldLabelModifier      = id
  , A.constructorTagModifier  = id
  , A.allNullaryToStringTag   = True
  , A.omitNothingFields       = True
  , A.sumEncoding             = A.defaultTaggedObject
  }

instance ToJSON Policy where toJSON = A.genericToJSON aesonPolicyOptions

-- | The type of call that is allowed
data Call
  = Pick
  | Read
  | Stat
  | Write
  | WriteUrl
  | Store
  | Convert
  | Remove
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

aesonCallOptions :: A.Options
aesonCallOptions = A.defaultOptions
  { A.fieldLabelModifier      = id
  , A.constructorTagModifier  = \t -> (C.toLower . head $ t) : tail t
  , A.allNullaryToStringTag   = True
  , A.omitNothingFields       = False
  , A.sumEncoding             = A.defaultTaggedObject
  }

instance ToJSON Call where toJSON = A.genericToJSON aesonCallOptions

-- Helper function for over pair
both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

-- | Given the secret and the policy return the signature and policy encoded as URL parameters
encodePolicyBS :: ByteString -> Policy -> (ByteString, ByteString)
encodePolicyBS secret p = (sig,jsonB64)
  where
    jsonB64 = B64.encode . B.pack . BL.unpack . A.encode $ p
    sig = fromString . show $ (hmacGetDigest (hmac secret jsonB64) :: Digest SHA256)

-- | Given the secret and the policy return the signature and policy encoded as URL parameters
encodePolicyText :: ByteString -> Policy -> (Text, Text)
encodePolicyText secret = both T.decodeUtf8 . encodePolicyBS secret

-- | Given the secret and the policy return the signature and policy encoded as URL parameters
encodePolicyS :: ByteString -> Policy -> (String, String)
encodePolicyS secret = both BC8.unpack . encodePolicyBS secret

-- | Given the secret and the policy return the signature and policy encoded as URL parameters
encodePolicyParamBS :: ByteString -> Policy -> ByteString
encodePolicyParamBS secret = (\(sig,jsonB64) -> "signature=" <> sig <> "&policy=" <> jsonB64) . encodePolicyBS secret

-- | Given the secret and the policy return the signature and policy encoded as URL parameters
encodePolicyParamText :: ByteString -> Policy -> Text
encodePolicyParamText secret = T.decodeUtf8 . encodePolicyParamBS secret

-- | Given the secret and the policy return the signature and policy encoded as URL parameters
encodePolicyParamS :: ByteString -> Policy -> String
encodePolicyParamS secret = BC8.unpack . encodePolicyParamBS secret

