{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import System.Environment

data TmplPart = Literal String
              | Variable String
              deriving Show

type Template = [TmplPart]

specialCharacters :: String
specialCharacters = "{}"

variable :: Parser String
variable = between (string "{{" <* spaces) (string "}}")
                   (many1 $ noneOf specialCharacters <* spaces)

parseTmplPart :: Parser TmplPart
parseTmplPart = Literal <$> (many1 $ noneOf specialCharacters)
         <|> Variable <$> variable

parseTemplate :: Parser Template
parseTemplate = many parseTmplPart

toLiteral :: TmplPart -> IO TmplPart
toLiteral (Variable s) = do
    mval <- lookupEnv s
    case mval of
         Just val -> return $ Literal val
         Nothing -> return $ Literal ""
toLiteral x = return x

toString :: TmplPart -> IO String
toString p = do
    Literal s <- toLiteral p
    return s

main :: IO ()
main = do
    let parsedTmpl = parse parseTemplate "" "Hello {{ name }}"
    case parsedTmpl of
         Left e -> print e
         Right tmpl -> do
            filledTmpl <- mapM toString tmpl
            print $ concat filledTmpl
