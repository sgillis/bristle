{-# LANGUAGE OverloadedStrings #-}

module Text.Bristle.Context where

import Prelude hiding (lookup, concat, concatMap)
import Data.Text hiding (map)
import Control.Monad
import System.Environment
import Text.Parsec
import Text.Bristle.Types
import Text.Bristle

combineContext :: (ContextGenerator m a, ContextGenerator m b)
               => a -> b -> Context m
combineContext c c' = \s -> case clookup c s of
                                 Nothing -> clookup c' s
                                 ma      -> ma

(<++>) :: (ContextGenerator m a, ContextGenerator m b) => a -> b -> Context m
(<++>) = combineContext

defaultContext :: Context m
defaultContext = \_ -> return Nothing

mkContext :: Text -> ContextNode m -> Context m
mkContext s n = \s' -> if s' == s then Just n else Nothing

htmlEscape :: Text -> Text
htmlEscape = concatMap proc
  where
    proc '&'  = "&amp;"
    proc '\\' = "&#92;"
    proc '"'  = "&quot;"
    proc '\'' = "&#39;"
    proc '<'  = "&lt;"
    proc '>'  = "&gt;"
    proc h    = singleton h

textToContext :: Text -> Context m
textToContext t = mkContext "." $ ContextText t

{-| Evaluate |-}
evaluateTemplate :: ContextGenerator m a => a -> Mustache -> IO Text
evaluateTemplate c mu = do
    mapM (evaluateNode c) mu >>= return . concat

evaluateNode :: ContextGenerator m a => a -> MustacheNode -> IO Text
evaluateNode c (MustacheText s) = return s

evaluateNode c (MustacheVar escape s) = do
    let value = case clookup c s of
                     Just (ContextText s) -> s
                     _                    -> empty
    if escape then return (htmlEscape value) else return value

evaluateNode c MustacheComment = return empty

evaluateNode c (MustacheSection s m) =
    case clookup c s of
         Just (ContextLambda f)       -> evaluateTemplate c m >>= return . f
         Just (ContextBool b)         ->
           if b then evaluateTemplate c m else return empty
         Just (ContextList [])        -> return empty
         Just (ContextList xs)        ->
           mapM (flip evaluateTemplate m) xs >>= return . concat
         Just (ContextLiteralList xs) ->
           mapM (flip evaluateTemplate m) (map textToContext xs) >>=
               return . concat
         Just (ContextSub sc)         -> evaluateTemplate sc m
         _                            -> evaluateTemplate c m

evaluateNode c (MustacheSectionInv s m) =
    case clookup c s of
         Just (ContextBool b)   -> if b then return empty else evaluateTemplate c m
         Just (ContextList [])  -> evaluateTemplate c m
         _                      -> return empty

evaluateNode c (MustachePartial s) = do
    partialMustache <- readFile (unpack s ++ ".mustache")
    let em = parse parseMustache "" (pack partialMustache)
    case em of
         Left e -> return $ pack $ show e
         Right m -> evaluateTemplate c m
