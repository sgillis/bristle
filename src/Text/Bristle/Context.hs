{-# LANGUAGE LambdaCase #-}

module Text.Bristle.Context where

import Prelude hiding (lookup)
import System.Environment
import Text.Bristle.Types

combineContext :: (ContextGenerator a, ContextGenerator b)
               => a -> b -> Context
combineContext c c' = \s -> case clookup c s of
                                 Nothing -> clookup c' s
                                 ma      -> ma

(<++>) :: (ContextGenerator a, ContextGenerator b) => a -> b -> Context
(<++>) = combineContext

defaultContext :: Context
defaultContext = \_ -> Nothing

{-| Evaluate |-}
empty :: String
empty = ""

evaluateTemplate :: ContextGenerator a => a -> Mustache -> String
evaluateTemplate c mu =
    concat $ map (evaluateNode c) mu

evaluateNode :: ContextGenerator a => a -> MustacheNode -> String
evaluateNode c (MustacheText s) = s

evaluateNode c (MustacheVar escape s) =
    case clookup c s of
         Just (ContextText s) -> s
         _                    -> empty

evaluateNode c MustacheComment = empty

evaluateNode c (MustacheSection s m) =
    case clookup c s of
         Just (ContextLambda f) -> f $ evaluateTemplate c m
         Just (ContextBool b)   -> if b then evaluateTemplate c m else empty
         Just (ContextList [])  -> empty
         Just (ContextList xs)  -> concat $ map (flip evaluateTemplate m) xs
         Just (ContextSub sc)   -> evaluateTemplate sc m
         _                      -> evaluateTemplate c m
