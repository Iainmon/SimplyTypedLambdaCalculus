{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Serialize (
  encode,
  encodeJson
) where

import Data.Text hiding (unpack)
import Data.Text.Lazy (unpack)
-- import Data.Text hiding (unpack)

import Data.Aeson hiding (encode)
import Data.Aeson.Text
import Data.Aeson.TH

import Syntax (Typing,Name,Expr(..))

-- data Person = Person
--   { name :: String
--   , age :: Int
--   , skill :: String -- you should probably use a more structured type here
--   }
-- deriveJSON defaultOptions ''Person

-- data NewPerson = NP
--   { name' :: String
--   , siblings :: [NewPerson]
--   }
-- deriveJSON defaultOptions ''NewPerson

-- data Option = Some String | None
-- deriveJSON defaultOptions ''Option

-- data Rec = Reca { h :: Int } | Recb { g :: Float }
-- deriveJSON defaultOptions ''Rec

-- data Expr
--   = App Expr Expr Typing
--   | Lam Name Expr Typing
--   | Lit Name Typing
--   deriving (Eq,Ord)
-- deriveJSON defaultOptions ''Expr

encodeJson :: ToJSON a => a -> String
encodeJson = unpack . encodeToLazyText
encode :: ToJSON a => a -> String
encode = encodeJson

tex :: Text -> Text
tex = id
str :: String -> String
str = id

ruleText :: Text
ruleText = "rule"

instance ToJSON Expr where
  toJSON (Lit v _)     = object [ruleText .= str "Lit", "value" .= v]
  toJSON (Lam v b _)   = object [ruleText .= str "Lam", "param" .= v, "body" .= b]
  toJSON (App l r _)   = object [ruleText .= str "App", "left" .= l, "right" .= r]

  -- toEncoding (Lit v _) = pairs ("rule" .= "Lit" <> "value" .= v)
  -- toEncoding (Lam v b _) = pairs ("rule" .= "Lit" <> "param" .= v <> "body" .= b)
