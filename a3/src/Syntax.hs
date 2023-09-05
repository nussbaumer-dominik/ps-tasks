{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.GI.Base

data SyntaxRule = SyntaxRule
    { pattern  :: T.Text
    , styleTag :: String
    }

keywords :: [T.Text]
keywords = ["plus", "minus", "mult", "div", "cont", "each"]

keywordsPattern :: T.Text
keywordsPattern = T.concat ["(?<!\\w)(" , T.intercalate "|" keywords , ")(?!\\w)"]

variablePattern :: T.Text
variablePattern = T.concat ["(?<!\\w)(?!(?:" , keywordsPattern , "))(\\w+)(?!\\[\\(\\{\\w])"]

valuePattern :: T.Text
valuePattern = "\\b[0-9]+\\b"

syntaxRules :: [SyntaxRule]
syntaxRules = [ SyntaxRule keywordsPattern  "keyword"
              , SyntaxRule valuePattern     "value"
              , SyntaxRule variablePattern  "variable"
              ]

initTagTable :: Gtk.TextTagTable -> IO ()
initTagTable tagTable = do
    keywordTag <- Gtk.new Gtk.TextTag [#name := "keyword", #foreground := "blue"]
    _ <- Gtk.textTagTableAdd tagTable keywordTag

    variableTag <- Gtk.new Gtk.TextTag [#name := "variable", #foreground := "pink"]
    _ <- Gtk.textTagTableAdd tagTable variableTag

    valueTag <- Gtk.new Gtk.TextTag [#name := "value", #foreground := "orange"]
    _ <- Gtk.textTagTableAdd tagTable valueTag
    return ()