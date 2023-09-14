{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module TextView (createTextView, createTextViewWithNumbers) where

import Control.Monad (unless, when)
import Data.Char (isLetter)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import GtkHelpers (getTextFromBuffer)
import Highlighting
import Syntax (StyleTag (..))
import SyntaxCore

-- | Create a new TextView with a given TextBuffer
-- This function creates a 'Gtk.TextView', attaches the given 'Gtk.TextBuffer'
-- and also attaches two event handlers to it, namely
-- * 'highlightSyntax' to highlight the syntax of the text on every change
-- * 'handleMarkSet' to highlight the matching bracket on every cursor movement
createTextView
  :: Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' to be attached to the 'Gtk.TextView'
  -> IO Gtk.TextView -- ^ Returns the created 'Gtk.TextView'
createTextView textBuffer = do
  textview <-
    Gtk.new
      Gtk.TextView
      [ #buffer Gtk.:= textBuffer,
        #indent Gtk.:= 4
      ]

  refreshSyntaxHighlighting textBuffer
  _ <- Gtk.on textBuffer #changed (refreshSyntaxHighlighting textBuffer)

  _ <- Gtk.on textBuffer #markSet (handleMarkSet textBuffer)

  return textview

-- | Create a new 'Gtk.TextView' that holds the line numbers for a given 'Gtk.TextBuffer'
createTextViewWithNumbers
  :: Gtk.TextBuffer   -- ^ The 'Gtk.TextBuffer' to be attached to the 'Gtk.TextView'
  -> IO Gtk.TextView  -- ^ Returns the created 'Gtk.TextView'
createTextViewWithNumbers textBuffer = do
  lineNumberBuffer <- Gtk.new Gtk.TextBuffer []
  lineNumberView <-
    Gtk.new
      Gtk.TextView
      [ #buffer Gtk.:= lineNumberBuffer,
        #editable Gtk.:= False,
        #cursorVisible Gtk.:= False,
        #wrapMode Gtk.:= Gtk.WrapModeNone,
        #leftMargin Gtk.:= 5,
        #rightMargin Gtk.:= 15
      ]

  updateLineNumbers textBuffer lineNumberBuffer
  _ <- Gtk.on textBuffer #changed (updateLineNumbers textBuffer lineNumberBuffer)

  return lineNumberView

-- | Update the line numbers of a given 'Gtk.TextBuffer'
updateLineNumbers
  :: Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' to be attached to the 'Gtk.TextView'
  -> Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' that holds the line numbers
  -> IO ()          -- ^ Returns nothing
updateLineNumbers mainBuffer lineNumberBuffer = do
  totalLines <- Gtk.textBufferGetLineCount mainBuffer
  let lineNumbers = T.unlines . map T.pack $ show <$> [1 .. totalLines]
  Gtk.textBufferSetText lineNumberBuffer lineNumbers (-1)

-- | Refresh the syntax highlighting of a given 'Gtk.TextBuffer'
refreshSyntaxHighlighting
  :: Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' to be attached to the 'Gtk.TextView'
  -> IO ()          -- ^ Returns nothing
refreshSyntaxHighlighting textBuffer = do
  removeAllHighlights textBuffer
  text <- getTextFromBuffer textBuffer
  let highlights = highlightSyntax text
  applyHighlightsToBuffer textBuffer highlights

-- | Remove all highlighting tags from a given 'Gtk.TextBuffer'
removeAllHighlights
  :: Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' to be attached to the 'Gtk.TextView'
  -> IO ()          -- ^ Returns nothing
removeAllHighlights buffer = do
  (start, end) <- getBufferBounds buffer
  Gtk.textBufferRemoveAllTags buffer start end

-- | Handle the 'Gtk.TextBuffer' 'markSet' event
handleMarkSet
  :: Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' to be attached to the 'Gtk.TextView'
  -> Gtk.TextIter   -- ^ The 'Gtk.TextIter' of the cursor position
  -> Gtk.TextMark   -- ^ The 'Gtk.TextMark' of the cursor position
  -> IO ()          -- ^ Returns nothing
handleMarkSet textBuffer iter mark = do
  markName <- Gtk.textMarkGetName mark
  when (markName == Just "insert") $ do
    removeHighlighting textBuffer
    entireText <- getTextFromBuffer textBuffer
    cursorPos <- fromIntegral <$> Gtk.textIterGetOffset iter

    -- Highlight word occurrences
    let wordToHighlight = identifyWord entireText cursorPos
    unless (T.null wordToHighlight) $
      when (T.count wordToHighlight entireText > 1) $
        applyHighlightsToBuffer textBuffer (highlightWordOccurrences entireText wordToHighlight)

    -- Highlight matching brackets
    mapM_ (applyHighlightsToBuffer textBuffer) (highlightMatchingBracket entireText cursorPos)

-- | Identifies the word at the given position in a text.
identifyWord
  :: T.Text -- ^ The text to be searched through
  -> Int    -- ^ The position of the cursor
  -> T.Text -- ^ Returns the word at the cursor position
identifyWord text pos
  | T.null text = T.empty
  | otherwise = T.reverse (T.takeWhile isLetter (T.reverse beginning)) `T.append` T.takeWhile isLetter remainder
  where
    (beginning, remainder) = T.splitAt pos text

-- | Remove two highlighting tags from a given 'Gtk.TextBuffer', namely:
-- * WordHighlight - The tag that highlights all occurrences of a word at the cursor position
-- * BracketHighlight - The tag that highlights the matching bracket of the cursor
removeHighlighting
  :: Gtk.TextBuffer -- ^ The 'Gtk.TextBuffer' where the highlighting tags should be removed
  -> IO ()          -- ^ Returns nothing
removeHighlighting buffer = do
  (start, end) <- getBufferBounds buffer
  Gtk.textBufferRemoveTagByName buffer (T.pack $ show WordHighlight) start end
  Gtk.textBufferRemoveTagByName buffer (T.pack $ show BracketHighlight) start end
