{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module TextView where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import Highlighting
import Syntax (StyleTag (..))
import SyntaxCore

-- | Create a new TextView with a given TextBuffer
-- This function creates a 'Gtk.TextView', attaches the given 'Gtk.TextBuffer'
-- and also attaches two event handlers to it, namely
-- * 'highlightSyntax' to highlight the syntax of the text on every change
-- * 'handleMarkSet' to highlight the matching bracket on every cursor movement
createTextView :: Gtk.TextBuffer -> IO Gtk.TextView
createTextView textBuffer = do
  textview <-
    Gtk.new
      Gtk.TextView
      [ #buffer Gtk.:= textBuffer,
        #indent Gtk.:= 4
      ]

  refreshSyntaxHighlighting textBuffer
  _ <- Gtk.on textBuffer #changed (refreshSyntaxHighlighting textBuffer)

  --_ <- Gtk.on textBuffer #markSet (handleMarkSet textBuffer)

  return textview

-- | Create a new 'Gtk.TextView' that holds the line numbers for a given 'Gtk.TextBuffer'
createTextViewWithNumbers :: Gtk.TextBuffer -> IO Gtk.TextView
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
updateLineNumbers :: Gtk.TextBuffer -> Gtk.TextBuffer -> IO ()
updateLineNumbers mainBuffer lineNumberBuffer = do
  totalLines <- Gtk.textBufferGetLineCount mainBuffer
  let lineNumbers = T.unlines . map T.pack $ show <$> [1 .. totalLines]
  Gtk.textBufferSetText lineNumberBuffer lineNumbers (-1)

refreshSyntaxHighlighting :: Gtk.TextBuffer -> IO ()
refreshSyntaxHighlighting textBuffer = do
  text <- getTextFromBuffer textBuffer
  let highlights = highlightSyntax text
  applyHighlightsToBuffer textBuffer highlights

-- | Remove two highlighting tags from a given 'Gtk.TextBuffer', namely:
-- * 'word-highlight' - The tag that highlights all occurrences of a word at the cursor position
-- * 'bracket-highlight' - The tag that highlights the matching bracket of the cursor
removeHighlighting :: Gtk.TextBuffer -> IO ()
removeHighlighting buffer = do
  (start, end) <- getBufferBounds buffer
  Gtk.textBufferRemoveTagByName buffer (T.pack $ show WordHighlight) start end
  Gtk.textBufferRemoveTagByName buffer (T.pack $ show BracketHighlight) start end

-- And similar procedures for word occurrences or bracket matching:
highlightWord :: Gtk.TextBuffer -> T.Text -> IO ()
highlightWord textBuffer word = do
  text <- getTextFromBuffer textBuffer
  let highlights = highlightWordOccurrences text word
  applyHighlightsToBuffer textBuffer highlights

highlightBracketAtPos :: Gtk.TextBuffer -> Int -> IO ()
highlightBracketAtPos textBuffer pos = do
  text <- getTextFromBuffer textBuffer
  case highlightMatchingBracket text pos of
    Just (h1, h2) -> applyHighlightsToBuffer textBuffer [h1, h2]
    Nothing -> return ()

-- | Save given file content to file
saveFile :: Gtk.TextBuffer -> FilePath -> IO ()
saveFile textBuffer filename = do
  text <- getTextFromBuffer textBuffer
  TIO.writeFile filename text

getTextFromBuffer :: Gtk.TextBuffer -> IO T.Text
getTextFromBuffer textBuffer = do
  startIter <- Gtk.textBufferGetStartIter textBuffer
  endIter <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferGetText textBuffer startIter endIter False
