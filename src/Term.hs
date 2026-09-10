module Term (Term (..), Name, packName, unpackName, nameText) where

import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)

type Name = ByteString

packName :: String -> Name
packName = encodeUtf8 . T.pack

unpackName :: Name -> String
unpackName = T.unpack . nameText

nameText :: Name -> Text
nameText = decodeUtf8Lenient

data Term = V !Name | F !Name [Term] deriving (Eq)

instance Show Term where
  show (V x) = unpackName x
  show (F f ts) = unpackName f ++ (if length ts > 0 then "(" ++ intercalate "," [show t | t <- ts] ++ ")" else "")
