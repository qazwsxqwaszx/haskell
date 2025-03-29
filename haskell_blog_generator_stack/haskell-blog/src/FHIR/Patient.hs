{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FHIR.Patient where

import Prelude
import Data.Maybe
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Data.Aeson

-- 🔹 性別 Gender
data Gender = Male | Female | Other | Unknown
  deriving (Show, Eq, Generic)

instance ToJSON Gender where
  toJSON Male    = String "male"
  toJSON Female  = String "female"
  toJSON Other   = String "other"
  toJSON Unknown = String "unknown"

instance FromJSON Gender where
  parseJSON = withText "Gender" $ \t -> case t of
    "male"    -> return Male
    "female"  -> return Female
    "other"   -> return Other
    "unknown" -> return Unknown
    _         -> fail "Invalid gender value"

-- 🔹 人名
data HumanName = HumanName
  { humanNameUse    :: Maybe Text
  , humanNameFamily :: Maybe Text
  , humanNameGiven  :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON HumanName
instance FromJSON HumanName

-- 🔹 聯絡方式
data ContactPoint = ContactPoint
  { contactPointSystem :: Maybe Text
  , contactPointValue  :: Maybe Text
  , contactPointUse    :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ContactPoint
instance FromJSON ContactPoint

-- 🔹 地址
data Address = Address
  { addressUse        :: Maybe Text
  , addressLine       :: [Text]
  , addressCity       :: Maybe Text
  , addressState      :: Maybe Text
  , addressPostalCode :: Maybe Text
  , addressCountry    :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Address
instance FromJSON Address

-- 🔹 緊急聯絡人
data Contact = Contact
  { contactName    :: Maybe HumanName
  , contactTelecom :: [ContactPoint]
  , contactGender  :: Maybe Gender
  } deriving (Show, Eq, Generic)

instance ToJSON Contact
instance FromJSON Contact

-- 🔹 病人 Patient
data Patient = Patient
  { patientResourceType  :: Text
  , patientId            :: Maybe Text
  , patientActive        :: Maybe Bool
  , patientName          :: [HumanName]
  , patientTelecom       :: [ContactPoint]
  , patientGender        :: Maybe Gender
  , patientBirthDate     :: Maybe Day
  , patientDeceased      :: Maybe Bool
  , patientAddress       :: [Address]
  , patientMaritalStatus :: Maybe Text
  , patientContact       :: [Contact]
  } deriving (Show, Eq, Generic)

instance ToJSON Patient
instance FromJSON Patient
