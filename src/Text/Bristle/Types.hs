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
{-# LANGUAGE MultiParamTypeClasses #-}

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

data ContextNode m = ContextText Text
                   | ContextLambda (Text -> Text)
                   | ContextBool Bool
                   | ContextList [Context m]
                   | ContextLiteralList [Text]
                   | ContextSub (Context m)

newtype SubContext c = SubContext { getContext :: c }

type Context m = (Text -> m (Maybe (ContextNode m)))

instance (Monad m) => ContextGenerator m (Context m) where
    clookup a s = a s

instance (Monad m, ContextGenerator m a) => ContextGenerator m [a] where
    clookup [] s = return Nothing
    clookup (a:as) s = do
        mctx <- clookup a s
        case mctx of
             Nothing -> clookup as s
             ma      -> return ma

{-| ContextGenerator |-}
class ContextGenerator m a where
    clookup :: a -> Text -> m (Maybe (ContextNode m))
    default clookup :: (Generic a, GContextGenerator m (Rep a))
                   => a -> Text -> m (Maybe (ContextNode m))
    clookup = glookup . from

{-| Generic ContextGenerator |-}
class GContextGenerator m f where
    glookup :: f a -> Text -> m (Maybe (ContextNode m))

instance (GContext m f, Selector c, Monad m)
         => GContextGenerator m (S1 c f) where
    glookup m@(M1 x) s
        | pack (selName m) == s = do contextnode <- gcontext x
                                     return $ Just contextnode
        | otherwise             = return Nothing

instance (GContextGenerator m f) => GContextGenerator m (D1 c f) where
    glookup (M1 x) = glookup x

instance (GContextGenerator m f) => GContextGenerator m (C1 c f) where
    glookup (M1 x) = glookup x

instance (ContextGenerator m a, Monad m) => GContextGenerator m (K1 i a) where
    glookup (K1 x) s = return Nothing

instance Monad m => GContextGenerator m U1 where
    glookup _ _ = return Nothing

instance (GContextGenerator m f, GContextGenerator m g, Monad m)
         => GContextGenerator m (f :*: g) where
    glookup (x :*: y) s = do
        mcontextnode <- glookup x s
        case mcontextnode of
             Nothing -> glookup y s
             ms -> return $ ms

instance (GName f, GName g, GContextGenerator m f, GContextGenerator m g
         , Monad m) => GContextGenerator m (f :+: g) where
    glookup (L1 x) s = case gname x == s of
                              True -> return $ Just $ ContextSub $ glookup x
                              False -> return $ Nothing
    glookup (R1 y) s = case gname y == s of
                              True -> return $ Just $ ContextSub $ glookup y
                              False -> return $ Nothing

{-| GContext |-}
class GContext m f where
    gcontext :: f a -> m (ContextNode m)

instance Monad m => GContext m (K1 i String) where
    gcontext (K1 x) = return $ ContextText $ pack x

instance Monad m => GContext m (K1 i Text) where
    gcontext (K1 x) = return $ ContextText x

instance Monad m => GContext m (K1 i (Text -> Text)) where
    gcontext (K1 x) = return $ ContextLambda x

instance Monad m => GContext m (K1 i Bool) where
    gcontext (K1 x) = return $ ContextBool x

instance Monad m => GContext m (K1 i [Text]) where
    gcontext (K1 xs) = return $ ContextLiteralList xs

instance (Monad m, Show c) => GContext m (K1 i c) where
    gcontext (K1 x) = return $ ContextText $ pack $ show x

instance (ContextGenerator m c, Monad m) => GContext m (K1 i [c]) where
    gcontext (K1 xs) = return $ ContextList $ map clookup xs

instance (ContextGenerator m c, Monad m)
         => GContext m (K1 i (SubContext c)) where
    gcontext (K1 x) = do
        let context = clookup (getContext x)
        return $ ContextSub context

{-| GName |-}
class GName f where
    gname :: f a -> Text

instance (Constructor c) => GName (M1 C c f) where
    gname = pack . conName
