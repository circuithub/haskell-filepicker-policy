{-# LANGUAGE DeriveGeneric #-}
module Network.Filepicker.Policy
  ( Policy(..)
  , Call(..)
  , expiryToPolicy
  , addCall
  , setHandle
  ) where

import           Data.Aeson.Types      (ToJSON)
import qualified Data.Aeson.Types      as A
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           GHC.Generics

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
  , A.constructorTagModifier  = id
  , A.allNullaryToStringTag   = True
  , A.omitNothingFields       = False
  , A.sumEncoding             = A.defaultTaggedObject
  }

instance ToJSON Call where toJSON = A.genericToJSON aesonCallOptions

encodePolicy :: Policy -> Text
encodePolicy p = undefined
