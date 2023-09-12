{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax
  ( SyntaxRule (..),
    StyleTag (..),
    syntaxRules,
    initTagTable,
  )
where

import Data.GI.Base
import qualified Data.Text as T
import GI.Gdk.Structs.RGBA
import qualified GI.Gtk as Gtk

data SyntaxRule = SyntaxRule
  { pattern :: T.Text,
    styleTag :: StyleTag
  }

keywords :: [T.Text]
keywords = ["plus", "minus", "mult", "div", "cont", "each"]

keywordsPattern :: T.Text
keywordsPattern = T.concat ["(?<!\\w)(", T.intercalate "|" keywords, ")(?!\\w)"]

variablePattern :: T.Text
variablePattern = T.concat ["(?<!\\w)(?!(?:", keywordsPattern, "))(\\w+)(?!\\[\\(\\{\\w])"]

valuePattern :: T.Text
valuePattern = "\\b[0-9]+\\b"

commentPattern :: T.Text
commentPattern = "#[^\n]*$" -- everything after "#" until end of line

syntaxRules :: [SyntaxRule]
syntaxRules =
  [ SyntaxRule keywordsPattern Keyword,
    SyntaxRule valuePattern Value,
    SyntaxRule variablePattern Variable,
    SyntaxRule commentPattern Comment
  ]

data StyleTag = Keyword | Value | Variable | Comment | WordHighlight | BracketHighlight | UnmatchedBracket
  deriving (Show)

data StyleType = Foreground | Background

initTagTable :: Gtk.TextTagTable -> IO ()
initTagTable tagTable = do
  _ <- createAndAddTag Keyword 0 51 179 1 Foreground tagTable
  _ <- createAndAddTag Variable 0 0 0 1 Foreground tagTable
  _ <- createAndAddTag Value 42 172 184 1 Foreground tagTable
  _ <- createAndAddTag Comment 140 140 140 1 Foreground tagTable
  _ <- createAndAddTag WordHighlight 237 235 252 1 Background tagTable
  _ <- createAndAddTag BracketHighlight 237 235 252 1 Background tagTable
  _ <- createAndAddTag UnmatchedBracket 255 0 0 1 Foreground tagTable
  return ()

createTag :: StyleTag -> Int -> Int -> Int -> Int -> StyleType -> IO Gtk.TextTag
createTag tagName r g b a styleType = do
  color <- newGdkRGBA r g b a
  case styleType of
    Foreground -> Gtk.new Gtk.TextTag [#name := T.pack (show tagName), #foregroundRgba := color]
    Background -> Gtk.new Gtk.TextTag [#name := T.pack (show tagName), #backgroundRgba := color]

addTag :: Gtk.TextTagTable -> Gtk.TextTag -> IO Bool
addTag tagTable tag = Gtk.textTagTableAdd tagTable tag

createAndAddTag :: StyleTag -> Int -> Int -> Int -> Int -> StyleType -> Gtk.TextTagTable -> IO Gtk.TextTag
createAndAddTag tagName r g b a styleType tagTable =
  createTag tagName r g b a styleType >>= \tag -> addTag tagTable tag >> return tag

newGdkRGBA :: Int -> Int -> Int -> Int -> IO RGBA
newGdkRGBA r g b a =
  Gtk.new
    RGBA
    [ #red := fromIntegral r / 255,
      #green := fromIntegral g / 255,
      #blue := fromIntegral b / 255,
      #alpha := fromIntegral a
    ]
