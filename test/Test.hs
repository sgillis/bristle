{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Text.Parsec (parse)
import Data.Text

import Text.Bristle (parseMustacheNode)
import Text.Bristle.Types


tests = [ testCase "Test mustache variable parser" testMustache ]

testMustache =
    case p of
         Left e -> error "Failed to parse"
         Right mn -> mn @=? MustacheVar False "test"
        where p = parse parseMustacheNode "" (pack "{{test}}")

main = defaultMain tests
