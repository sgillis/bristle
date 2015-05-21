{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding (map)
import GHC.Generics
import Data.Text
import Text.Parsec (parse)
import Text.Bristle
import Text.Bristle.Context
import Text.Bristle.Types
import Text.Bristle.ContextGenerators
import Control.Monad.IO.Class

data Name = Name
    { name :: Text } deriving Generic

instance ContextGenerator IO Name

data Tickets = Tickets
    { amount  :: Int
    , concert :: Text
    } deriving Generic

instance ContextGenerator IO Tickets

data Rec = Rec
    { world   :: Text
    , shout   :: Text -> Text
    , n       :: Int
    , blabber :: Bool
    , names   :: [Name]
    , tickets :: SubContext Tickets
    } deriving Generic

instance ContextGenerator IO Rec

data Hero = SuperHero { heroName :: String }
          | EvilHero  { heroName :: String }
          deriving (Show, Generic)

instance ContextGenerator IO Hero

main :: IO ()
main = do
    template <- readFile "example.mustache"
    let em  = parse parseMustache "" (pack template)
        ctx = (Rec "Europe" toUpper 1 False
                   [Name "me", Name "myself", Name "I"]
                   (SubContext (Tickets 2 "Gojira")))
              <++> (mkContext "friends" (ContextList []))
              <++> (SuperHero "Dr. Manhattan")
              <++> (mkContext "html" (ContextText "<b>html</b>"))
              <++> (mkContext "trees" (ContextLiteralList ["Pine", "Oak"]))
              <++> defaultContext
    case em of
         Left e -> print e
         Right m -> do
            evaluateTemplate fromEnv m >>= putStr . unpack
            evaluateTemplate ctx m >>= putStr . unpack
            evaluateTemplate fromHttp m >>= putStr . unpack
