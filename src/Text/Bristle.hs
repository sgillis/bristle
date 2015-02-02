{-# LANGUAGE OverloadedStrings #-}

module Text.Bristle where

-- Parse mustache(5) templates
--
-- Variable: {{name}}
--   - Search for key in current context, parent context recursively
--   - Nothing is rendered if key not found
--   - HTML is escaped
--
-- Variable: {{{name}}}
--   - Same as variable, but HTML is not escaped
--
-- Sections: {{#section}}{{/section}}
--   - False value or empty list -> text not displayed
--   - Non-empty list -> Context of the block is rendered for every item in the
--                       list
--   - Lambdas -> Not supported now
--   - Non-false values -> value is used as the context for a single rendering
--
-- Inverted sections: {{^section}}{{/section}}
--   - Render the text if the key doesn't exist, is false or an empty list
--
-- Comments: {{! comment }}
--
-- Partials: {{> box}}
--
-- Set delimiter:
--   - Not supported

import Prelude hiding (lookup)
import Data.Text
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Applicative ((<$>), (<*>))
import Text.Bristle.Types


parseMustache :: Parser Mustache
parseMustache = manyTill parseMustacheNode eof

parseMustacheNode :: Parser MustacheNode
parseMustacheNode =
        try partial
    <|> try section
    <|> try invSection
    <|> try comment
    <|> try noEscapeVar
    <|> try ampVar
    <|> try var
    <|> try text
      where partial = MustachePartial <$>
                (mustache $ string "> " >> key) >>= stripNewline
            section = prefixSection '#' >>= \(name, mustache) ->
              return $ MustacheSection name mustache
            invSection = prefixSection '^' >>= \(name, mustache) ->
              return $ MustacheSectionInv name mustache
            comment = mustache (char '!' >> key) >> return MustacheComment
            noEscapeVar = MustacheVar False <$>
              (mustache $ between (char '{') (char '}') key)
            ampVar = MustacheVar False <$> (mustache $ string "& " >> key)
            var = MustacheVar True <$> (mustache key)
            text = MustacheText <$>
              (many1Till anyChar (try eof <|> startToken) >>= return . pack)

startToken :: Parser ()
startToken = lookAhead $ try $ string "{{" >> return ()

endToken :: Parser ()
endToken = lookAhead $ try $ string "}}" >>  return ()

key :: Parser Text
key = manyTill anyChar (lookAhead $ try (string "}}")) >>= return . pack

mustache :: Parser a -> Parser a
mustache f = between (string "{{") (string "}}") f

prefixSection :: Char -> Parser (Text, Mustache)
prefixSection prefix = do
    sectionName <- mustache $ char prefix >> key >>= return . unpack
    n           <- optionMaybe newline
    mustache    <- manyTill parseMustacheNode $ lookAhead $ try $ sectionEnd sectionName
    _           <- sectionEnd sectionName
    case n of
         Just _ -> newline >> return ()
         Nothing   -> return ()
    return (pack sectionName, mustache)
      where sectionEnd name = mustache $ char '/' >> string name

many1Till :: Parser a -> Parser b -> Parser [a]
many1Till f end = do
    x  <- f
    xs <- manyTill f end
    return (x:xs)

stripNewline :: a -> Parser a
stripNewline x = optional newline >> return x
