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

data Rec = Rec
    { world   :: String
    , shout   :: String -> String
    , n       :: Int
    , blabber :: Bool
    , names   :: [Name]
    } deriving Generic

instance ContextGenerator Rec

main :: IO ()
main = do
    let em = parse parseMustache "" $ concat
            [ "{{#shout}}hello {{world}}{{/shout}}\n"
            , "You're number {{n}}!\n"
            , "{{#blabber}}"
            , "Don't start blabbering\n"
            , "{{/blabber}}"
            , "{{#names}}{{name}},\n{{/names}}"
            ]
        rec = Rec "Europe" (map toUpper) 1 False
                  [Name "me", Name "myself", Name "I"]
    case em of
         Left e -> print e
         Right m -> putStr $ evaluateTemplate rec m

{- Output:
HELLO EUROPE
You're number 1!
me,
myself,
I,
-}
