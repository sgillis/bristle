{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Bristle.Types where

import Prelude
import GHC.Generics
import Data.Text hiding (map)

type Mustache = [MustacheNode]
type Escape = Bool

data MustacheNode = MustacheText Text
                  | MustacheVar Escape Text
                  | MustacheSection Text Mustache
                  | MustacheSectionInv Text Mustache
                  | MustachePartial Text
                  | MustacheComment
                  deriving Show

data ContextNode = ContextText Text
                 | ContextLambda (Text -> Text)
                 | ContextBool Bool
                 | ContextList [Context]
                 | ContextLiteralList [Text]
                 | ContextSub Context

newtype SubContext c = SubContext { getContext :: c }

type Context = (Text -> Maybe ContextNode)

instance ContextGenerator Context where
    clookup a s = a s

instance (ContextGenerator a) => ContextGenerator [a] where
    clookup [] s = Nothing
    clookup (a:as) s = case clookup a s of
                            Nothing -> clookup as s
                            ma      -> ma

{-| ContextGenerator |-}
class ContextGenerator a where
    clookup :: a -> Text -> Maybe ContextNode
    default clookup :: (Generic a, GContextGenerator (Rep a))
                   => a -> Text -> Maybe ContextNode
    clookup = glookup . from

{-| Generic ContextGenerator |-}
class GContextGenerator f where
    glookup :: f a -> Text -> Maybe ContextNode

instance (GContext f, Selector c) => GContextGenerator (S1 c f) where
    glookup m@(M1 x) s | pack (selName m) == s = Just $ gcontext x
                       | otherwise             = Nothing

instance (GContextGenerator f) => GContextGenerator (D1 c f) where
    glookup (M1 x) = glookup x

instance (GContextGenerator f) => GContextGenerator (C1 c f) where
    glookup (M1 x) = glookup x

instance ContextGenerator a => GContextGenerator (K1 i a) where
    glookup (K1 x) s = Nothing

instance GContextGenerator U1 where
    glookup _ _ = Nothing

instance (GContextGenerator f, GContextGenerator g)
         => GContextGenerator (f :*: g) where
    glookup (x :*: y) s = case glookup x s of
                               Nothing -> glookup y s
                               ms -> ms

instance (GName f, GName g, GContextGenerator f, GContextGenerator g)
         => GContextGenerator (f :+: g) where
    glookup (L1 x) s = case gname x == s of
                              True -> Just $ ContextSub $ glookup x
                              False -> Nothing
    glookup (R1 y) s = case gname y == s of
                              True -> Just $ ContextSub $ glookup y
                              False -> Nothing

{-| GContext |-}
class GContext f where
    gcontext :: f a -> ContextNode

instance GContext (K1 i String) where
    gcontext (K1 x) = ContextText $ pack x

instance GContext (K1 i Text) where
    gcontext (K1 x) = ContextText x

instance GContext (K1 i (Text -> Text)) where
    gcontext (K1 x) = ContextLambda x

instance GContext (K1 i Bool) where
    gcontext (K1 x) = ContextBool x

instance GContext (K1 i [Text]) where
    gcontext (K1 xs) = ContextLiteralList xs

instance (Show c) => GContext (K1 i c) where
    gcontext (K1 x) = ContextText $ pack $ show x

instance (ContextGenerator c) => GContext (K1 i [c]) where
    gcontext (K1 xs) = ContextList $ map clookup xs

instance (ContextGenerator c) => GContext (K1 i (SubContext c)) where
    gcontext (K1 x) = ContextSub $ clookup $ getContext x

{-| GName |-}
class GName f where
    gname :: f a -> Text

instance (Constructor c) => GName (M1 C c f) where
    gname = pack . conName
