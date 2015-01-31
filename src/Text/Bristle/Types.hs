{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Text.Bristle.Types where

import Prelude hiding (lookup)
import GHC.Generics
import Data.Typeable

type Mustache = [MustacheNode]
type Escape = Bool

data MustacheNode = MustacheText String
                  | MustacheVar Escape String
                  | MustacheSection String Mustache
                  | MustacheSectionInv String Mustache
                  | MustachePartial String
                  | MustacheComment
                  deriving Show

data ContextNode = ContextText String
                 | ContextLambda (String -> String)
                 | ContextBool Bool
                 | ContextList [String -> Maybe ContextNode]
                 | ContextSub (String -> Maybe ContextNode)

data SubContext c = SubContext { getContext :: c }

instance ContextGenerator (String -> Maybe ContextNode) where
    clookup a s = a s

{-| ContextGenerator |-}
class ContextGenerator a where
    clookup :: a -> String -> Maybe ContextNode
    default clookup :: (Generic a, GContextGenerator (Rep a))
                   => a -> String -> Maybe ContextNode
    clookup = glookup . from

{-| Generic ContextGenerator |-}
class GContextGenerator f where
    glookup :: f a -> String -> Maybe ContextNode

instance (GContext f, Selector c) => GContextGenerator (S1 c f) where
    glookup m@(M1 x) s | selName m == s = Just $  gcontext x
                       | otherwise      = Nothing

instance (GContextGenerator f) => GContextGenerator (D1 c f) where
    glookup (M1 x) = glookup x

instance (GContextGenerator f) => GContextGenerator (C1 c f) where
    glookup (M1 x) = glookup x

instance ContextGenerator a => GContextGenerator (K1 i a) where
    glookup (K1 x) s = Nothing

instance GContextGenerator U1 where
    glookup _ _ = Nothing

instance (GContextGenerator f, GContextGenerator g) => GContextGenerator (f :*: g) where
    glookup (x :*: y) s = case glookup x s of
                               Nothing -> glookup y s
                               ms -> ms

instance (GContextGenerator f, GContextGenerator g) => GContextGenerator (f :+: g) where
    glookup (L1 x) s = glookup x s
    glookup (R1 y) s = glookup y s

{-| GContext |-}
class GContext f where
    gcontext :: f a -> ContextNode

instance GContext (K1 i String) where
    gcontext (K1 x) = ContextText x

instance GContext (K1 i (String -> String)) where
    gcontext (K1 x) = ContextLambda x

instance GContext (K1 i Bool) where
    gcontext (K1 x) = ContextBool x

instance (Show c) => GContext (K1 i c) where
    gcontext (K1 x) = ContextText $ show x

instance (ContextGenerator c) => GContext (K1 i [c]) where
    gcontext (K1 xs) = ContextList $ map clookup xs

instance (ContextGenerator c) => GContext (K1 i (SubContext c)) where
    gcontext (K1 x) = ContextSub $ clookup $ getContext x
