{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines a syntax and associated style tags for a custom language (as defined in a2).
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

-- | Data structure representing a rule for syntax highlighting.
data SyntaxRule = SyntaxRule
  { pattern :: T.Text,
    styleTag :: StyleTag
  }

-- | A list of predefined keywords for the a2 language.
keywords :: [T.Text]
keywords = ["plus", "minus", "mult", "div", "cont", "each", "print", "modulo", "cond", "eq", "lt", "gt", "and", "or", "not"]

-- | Pattern for keywords. It ensures that keywords are not part of other words (exact matches).
keywordsPattern :: T.Text
keywordsPattern = T.concat ["(?<!\\w)(", T.intercalate "|" keywords, ")(?!\\w)"]

-- | List of predefined signs for the a2 language.
signs :: [T.Text]
signs = ["=", ":", ">"]

-- | Pattern for signs.
signPattern :: T.Text
signPattern = T.concat ["(", T.intercalate "|" signs, ")"]

-- | Pattern for variables. Excludes keywords.
variablePattern :: T.Text
variablePattern = T.concat ["(?<!\\w)(?!(?:", keywordsPattern, "))(\\w+)(?!\\[\\(\\{\\w])"]

-- | Pattern for integer values.
valuePattern :: T.Text
valuePattern = "\\b[0-9]+\\b"

-- | Pattern for comments. Everything after "#" until end of line is a comment.
commentPattern :: T.Text
commentPattern = "#[^\n]*$"

-- | List of 'SyntaxRule's for the a2 language.
syntaxRules :: [SyntaxRule]
syntaxRules =
  [ SyntaxRule keywordsPattern Keyword,
    SyntaxRule signPattern Sign,
    SyntaxRule valuePattern Value,
    SyntaxRule variablePattern Variable,
    SyntaxRule commentPattern Comment
  ]

-- | Data structure representing different style tags for syntax highlighting.
data StyleTag = Keyword | Sign | Value | Variable | Comment | WordHighlight | BracketHighlight | UnmatchedBracket
  deriving (Show)

-- | Type to differentiate between foreground and background styles.
data StyleType = Foreground | Background

-- | Initializes a given 'Gtk.TextTagTable' with syntax style tags.
initTagTable 
  :: Gtk.TextTagTable -- ^ The 'Gtk.TextTagTable' to initialize 
  -> IO () -- ^ Returns nothing
initTagTable tagTable = do
  _ <- createAndAddTag Keyword 0 51 179 1 Foreground tagTable
  _ <- createAndAddTag Sign 150 61 239 1 Foreground tagTable
  _ <- createAndAddTag Variable 0 0 0 1 Foreground tagTable
  _ <- createAndAddTag Value 42 172 184 1 Foreground tagTable
  _ <- createAndAddTag Comment 140 140 140 1 Foreground tagTable
  _ <- createAndAddTag WordHighlight 237 235 252 1 Background tagTable
  _ <- createAndAddTag BracketHighlight 237 235 252 1 Background tagTable
  _ <- createAndAddTag UnmatchedBracket 255 0 0 1 Foreground tagTable
  return ()

-- | Creates a 'Gtk.TextTag' based on the given style parameters.
createTag 
  :: StyleTag       -- ^ The name of the tag 
  -> Int            -- ^ Red value in the range [0, 255]
  -> Int            -- ^ Green value in the range [0, 255]
  -> Int            -- ^ Blue value in the range [0, 255]
  -> Int            -- ^ Alpha value in the range [0, 1]
  -> StyleType      -- ^ StyleType defining where the color should be applied
  -> IO Gtk.TextTag -- ^ Returns the created 'Gtk.TextTag'
createTag tagName r g b a styleType = do
  color <- newGdkRGBA r g b a
  case styleType of
    Foreground -> Gtk.new Gtk.TextTag [#name := T.pack (show tagName), #foregroundRgba := color]
    Background -> Gtk.new Gtk.TextTag [#name := T.pack (show tagName), #backgroundRgba := color]

-- | Adds a 'Gtk.TextTag' to a 'Gtk.TextTagTable'.
addTag 
  :: Gtk.TextTagTable -- ^ The 'Gtk.TextTagTable' to add the tag to 
  -> Gtk.TextTag      -- ^ The 'Gtk.TextTag' to add
  -> IO Bool          -- ^ Returns True if the tag was added successfully
addTag tagTable tag = Gtk.textTagTableAdd tagTable tag

-- | Helper function to create a 'Gtk.TextTag' and add it to a 'Gtk.TextTagTable'.
createAndAddTag 
  :: StyleTag         -- ^ The name of the tag 
  -> Int              -- ^ Red value in the range [0, 255]
  -> Int              -- ^ Green value in the range [0, 255]
  -> Int              -- ^ Blue value in the range [0, 255]
  -> Int              -- ^ Alpha value in the range [0, 1]
  -> StyleType        -- ^ StyleType defining where the color should be applied
  -> Gtk.TextTagTable -- ^ The 'Gtk.TextTagTable' to add the tag to
  -> IO Gtk.TextTag   -- ^ Returns the created 'Gtk.TextTag'
createAndAddTag tagName r g b a styleType tagTable =
  createTag tagName r g b a styleType >>= \tag -> addTag tagTable tag >> return tag

-- | Helper function to create a new RGBA color based on the given parameters.
newGdkRGBA 
  :: Int      -- ^ Red value in the range [0, 255] 
  -> Int      -- ^ Green value in the range [0, 255]
  -> Int      -- ^ Blue value in the range [0, 255]
  -> Int      -- ^ Alpha value in the range [0, 1]
  -> IO RGBA  -- ^ Returns a Gtk RGBA object
newGdkRGBA r g b a =
  Gtk.new
    RGBA
    [ #red := fromIntegral r / 255,
      #green := fromIntegral g / 255,
      #blue := fromIntegral b / 255,
      #alpha := fromIntegral a
    ]
