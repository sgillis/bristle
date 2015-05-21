{-# LANGUAGE OverloadedStrings #-}

module Text.Bristle.Context where

import Prelude hiding (lookup, concat, concatMap)
import Data.Text hiding (map)
import Control.Monad
import System.Environment
import Text.Parsec
import Text.Bristle.Types
import Text.Bristle
import Control.Monad.IO.Class

combineContext :: (ContextGenerator m a, ContextGenerator m b, MonadIO m)
               => a -> b -> Context m
combineContext c c' = \s -> do
    mcontext <- clookup c s
    case mcontext of
         Nothing -> clookup c' s
         ma      -> return ma

(<++>) :: (ContextGenerator m a, ContextGenerator m b, MonadIO m)
          => a -> b -> Context m
(<++>) = combineContext

defaultContext :: MonadIO m => Context m
defaultContext = \_ -> return Nothing

mkContext :: MonadIO m => Text -> ContextNode m -> Context m
mkContext s n = \s' -> if s' == s then return (Just n) else return Nothing

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

textToContext :: MonadIO m => Text -> Context m
textToContext t = mkContext "." $ ContextText t

{-| Evaluate |-}
evaluateTemplate :: (ContextGenerator m a, MonadIO m) => a -> Mustache -> m Text
evaluateTemplate c mu = do
    mapM (evaluateNode c) mu >>= return . concat

evaluateNode :: (ContextGenerator m a, MonadIO m) => a -> MustacheNode -> m Text
evaluateNode c (MustacheText s) = return s

evaluateNode c (MustacheVar escape s) = do
    mvalue <- clookup c s
    let value = case mvalue of
                     Just (ContextText s) -> s
                     _                    -> empty
    if escape then return (htmlEscape value) else return value

evaluateNode c MustacheComment = return empty

evaluateNode c (MustacheSection s m) = do
    mvalue <- clookup c s
    case mvalue of
         Just (ContextLambda f)       -> evaluateTemplate c m >>= return . f
         Just (ContextBool b)         ->
           if b then evaluateTemplate c m else return empty
         Just (ContextList [])        -> return empty
         Just (ContextList xs)        ->
           mapM (flip evaluateTemplate m) xs >>= return . concat
         Just (ContextLiteralList xs) -> do
           let ctxs = map textToContext xs
           mapM (flip evaluateTemplate m) ctxs >>= return . concat
         Just (ContextSub sc)         -> evaluateTemplate sc m
         _                            -> evaluateTemplate c m

evaluateNode c (MustacheSectionInv s m) = do
    mvalue <- clookup c s
    case mvalue of
         Just (ContextBool b)   -> if b then return empty else evaluateTemplate c m
         Just (ContextList [])  -> evaluateTemplate c m
         _                      -> return empty

evaluateNode c (MustachePartial s) = do
    partialMustache <- liftIO $ readFile (unpack s ++ ".mustache")
    let em = parse parseMustache "" (pack partialMustache)
    case em of
         Left e -> return $ pack $ show e
         Right m -> evaluateTemplate c m
