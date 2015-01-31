{-# LANGUAGE DeriveGeneric #-}

import Data.Char
import GHC.Generics
import Text.Parsec (parse)
import Text.Bristle
import Text.Bristle.Context
import Text.Bristle.Types

data Name = Name
    { name :: String } deriving Generic

instance ContextGenerator Name

data Tickets = Tickets
    { amount  :: Int
    , concert :: String
    } deriving Generic

instance ContextGenerator Tickets

data Rec = Rec
    { world   :: String
    , shout   :: String -> String
    , n       :: Int
    , blabber :: Bool
    , names   :: [Name]
    , tickets :: SubContext Tickets
    } deriving Generic

instance ContextGenerator Rec

main :: IO ()
main = do
    template <- readFile "example.mustache"
    let em = parse parseMustache "" template
        rec = Rec "Europe" (map toUpper) 1 False
                  [Name "me", Name "myself", Name "I"]
                  (SubContext (Tickets 2 "Gojira"))
    case em of
         Left e -> print e
         Right m -> putStr $ evaluateTemplate rec m
