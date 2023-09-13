{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module TextView
  ( createTextView,
    createTextViewWithNumbers,
    saveFile,
  )
where

import Control.Monad (unless, void, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import Highlighting
  ( getBufferBounds,
    highlightMatchingBracket,
    highlightSyntax,
    highlightWordOccurrences,
    isBracket,
  )
import Syntax (StyleTag (..))

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

  highlightSyntax textBuffer
  _ <- Gtk.on textBuffer #changed (highlightSyntax textBuffer)

  _ <- Gtk.on textBuffer #markSet (handleMarkSet textBuffer)

  return textview

-- | Handle the 'markSet' event of the 'Gtk.TextBuffer'
-- This function is called on every cursor movement and highlights any matching brackets or words
-- Arguments:
-- * 'Gtk.TextBuffer' - The 'Gtk.TextBuffer' that triggered the event
-- * 'Gtk.TextIter' - The 'Gtk.TextIter' of the cursor
-- * 'Gtk.TextMark' - The 'Gtk.TextMark' that was set
handleMarkSet :: Gtk.TextBuffer -> Gtk.TextIter -> Gtk.TextMark -> IO ()
handleMarkSet textBuffer iter mark = do
  markName <- Gtk.textMarkGetName mark
  when (markName == Just "insert") $ do
    removeHighlighting textBuffer

    -- Get the current word
    wordToHighlight <- identifyWord iter
    unless (T.null wordToHighlight) $ do
      highlightWordOccurrences textBuffer wordToHighlight

    charRightOfCursor <- Gtk.textIterGetChar iter
    iterLeftOfCursor <- Gtk.textIterCopy iter
    canMoveBack <- Gtk.textIterBackwardChar iterLeftOfCursor
    charLeftOfCursor <-
      if canMoveBack
        then Gtk.textIterGetChar iterLeftOfCursor
        else return '\0'

    when (isBracket charRightOfCursor) $ do
      highlightMatchingBracket textBuffer iter

    when (isBracket charLeftOfCursor) $ do
      highlightMatchingBracket textBuffer iterLeftOfCursor

identifyWord :: Gtk.TextIter -> IO T.Text
identifyWord cursor = do
  startWordIter <- Gtk.textIterCopy cursor
  endWordIter <- Gtk.textIterCopy cursor

  startsWord <- Gtk.textIterStartsWord cursor
  insideWord <- Gtk.textIterInsideWord cursor
  endsWord <- Gtk.textIterEndsWord cursor

  adjustIters startsWord insideWord endsWord startWordIter endWordIter
  Gtk.textIterGetText startWordIter endWordIter

-- | Adjust the given 'Gtk.TextIter's to the beginning and end of the current word
-- Arguments:
-- * 'Bool' - Whether the cursor is at the beginning of a word
-- * 'Bool' - Whether the cursor is inside a word
-- * 'Bool' - Whether the cursor is at the end of a word
-- * 'Gtk.TextIter' - The 'Gtk.TextIter' to adjust to the beginning of the word
-- * 'Gtk.TextIter' - The 'Gtk.TextIter' to adjust to the end of the word
adjustIters :: Bool -> Bool -> Bool -> Gtk.TextIter -> Gtk.TextIter -> IO ()
adjustIters True _ _ _ end = void $ Gtk.textIterForwardWordEnd end
adjustIters _ True _ start end = do
  void $ Gtk.textIterBackwardWordStart start
  void $ Gtk.textIterForwardWordEnd end
adjustIters _ _ True start _ = void $ Gtk.textIterBackwardWordStart start
adjustIters _ _ _ _ _ = return ()

-- | Remove two highlighting tags from a given 'Gtk.TextBuffer', namely:
-- * 'word-highlight' - The tag that highlights all occurrences of a word at the cursor position
-- * 'bracket-highlight' - The tag that highlights the matching bracket of the cursor
removeHighlighting :: Gtk.TextBuffer -> IO ()
removeHighlighting buffer = do
  (start, end) <- getBufferBounds buffer
  Gtk.textBufferRemoveTagByName buffer (T.pack $ show WordHighlight) start end
  Gtk.textBufferRemoveTagByName buffer (T.pack $ show BracketHighlight) start end

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

-- | Save given file content to file
saveFile :: Gtk.TextBuffer -> FilePath -> IO ()
saveFile textBuffer filename = do
  startIter <- Gtk.textBufferGetStartIter textBuffer
  endIter <- Gtk.textBufferGetEndIter textBuffer
  text <- Gtk.textBufferGetText textBuffer startIter endIter False
  TIO.writeFile filename text
