{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import GI.Gdk.Structs.RGBA
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

commentPattern :: T.Text
commentPattern = "--[^\n]*$" -- everything after -- until end of line

syntaxRules :: [SyntaxRule]
syntaxRules = [ SyntaxRule keywordsPattern  "keyword"
              , SyntaxRule valuePattern     "value"
              , SyntaxRule variablePattern  "variable"
              , SyntaxRule commentPattern   "comment"
              ]

initTagTable :: Gtk.TextTagTable -> IO ()
initTagTable tagTable = do
    _ <- createAndAddTag "keyword" 0 51 179 1 tagTable
    _ <- createAndAddTag "variable" 0 0 0 1 tagTable
    _ <- createAndAddTag "value" 42 172 184 1 tagTable
    _ <- createAndAddTag "comment" 140 140 140 1 tagTable
    return ()

createTag :: T.Text -> Int -> Int -> Int -> Int -> IO Gtk.TextTag
createTag tagName r g b a = do
    color <- newGdkRGBA r g b a
    Gtk.new Gtk.TextTag [#name := tagName, #foregroundRgba := color]

addTag :: Gtk.TextTagTable -> Gtk.TextTag -> IO ()
addTag tagTable tag = do
    _ <- Gtk.textTagTableAdd tagTable tag
    return ()

createAndAddTag :: T.Text -> Int -> Int -> Int -> Int -> Gtk.TextTagTable -> IO Gtk.TextTag
createAndAddTag tagName r g b a tagTable =
    createTag tagName r g b a >>= \tag -> addTag tagTable tag >> return tag

newGdkRGBA :: Int -> Int -> Int -> Int -> IO RGBA
newGdkRGBA r g b a = Gtk.new RGBA [ #red := fromIntegral r / 255
                                  , #green := fromIntegral g / 255
                                  , #blue := fromIntegral b / 255
                                  , #alpha := fromIntegral a
                                  ]

