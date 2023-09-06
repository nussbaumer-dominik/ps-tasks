{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TextView (createTextViewWithNumbers, saveFile) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import Control.Monad (when)
import Highlighting (highlightSyntax, highlightWordOccurrences, highlightMatchingBracket)

-- create TextView with given TextBuffer and attach highlighting handler
createTextView :: Gtk.TextBuffer -> IO Gtk.TextView
createTextView textBuffer = do
    textview <- Gtk.new Gtk.TextView [ #buffer Gtk.:= textBuffer
                                     , #indent Gtk.:= 4
                                     ]

    highlightSyntax textBuffer
    _ <- Gtk.on textBuffer #changed (highlightSyntax textBuffer)

    _ <- Gtk.on textBuffer #markSet (handleMarkSet textBuffer)

    return textview

handleMarkSet :: Gtk.TextBuffer -> Gtk.TextIter -> Gtk.TextMark -> IO ()
handleMarkSet textBuffer iter mark = do
    markName <- Gtk.textMarkGetName mark
    when (markName == Just "insert") $ do
        updateHighlighting textBuffer

updateHighlighting :: Gtk.TextBuffer -> IO ()
updateHighlighting textBuffer = do
    mark <- Gtk.textBufferGetInsert textBuffer
    iter <- Gtk.textBufferGetIterAtMark textBuffer mark

    removeHighlighting textBuffer

    wordToHighlight <- identifyWord iter
    when (T.length wordToHighlight > 0) $
        highlightWordOccurrences textBuffer wordToHighlight

identifyWord :: Gtk.TextIter -> IO T.Text
identifyWord iter = do
    startWordIter <- Gtk.textIterCopy iter
    _ <- Gtk.textIterBackwardWordStart startWordIter
    endWordIter <- Gtk.textIterCopy iter
    _ <- Gtk.textIterForwardWordEnd endWordIter
    Gtk.textIterGetText startWordIter endWordIter

removeHighlighting :: Gtk.TextBuffer -> IO ()
removeHighlighting buffer = do
    putStrLn "removeHighlighting"
    start <- Gtk.textBufferGetStartIter buffer
    end <- Gtk.textBufferGetEndIter buffer
    Gtk.textBufferRemoveTagByName buffer "word-highlight" start end
    Gtk.textBufferRemoveTagByName buffer "bracket-highlight" start end

-- create TextView and attach line numbers
createTextViewWithNumbers :: Gtk.TextBuffer -> IO Gtk.Box
createTextViewWithNumbers textBuffer = do
    -- Create main TextView
    textview <- createTextView textBuffer

    -- Create line number TextView
    lineNumberBuffer <- Gtk.new Gtk.TextBuffer []
    lineNumberView <- Gtk.new Gtk.TextView [ #buffer Gtk.:= lineNumberBuffer
                                       , #editable Gtk.:= False
                                       , #cursorVisible Gtk.:= False
                                       , #wrapMode Gtk.:= Gtk.WrapModeNone
                                       , #leftMargin Gtk.:= 5
                                       , #rightMargin Gtk.:= 15
                                       ]

    -- Pack both TextViews into a horizontal box
    hbox <- Gtk.new Gtk.Box [ #orientation Gtk.:= Gtk.OrientationHorizontal ]
    separator <- Gtk.new Gtk.Separator [ #orientation Gtk.:= Gtk.OrientationVertical ]
    Gtk.boxPackStart hbox lineNumberView False True 0
    Gtk.boxPackStart hbox separator False False 0
    Gtk.boxPackStart hbox textview True True 0

    _ <- Gtk.on textBuffer #changed (updateLineNumbers textBuffer lineNumberBuffer)

    updateLineNumbers textBuffer lineNumberBuffer

    return hbox

updateLineNumbers :: Gtk.TextBuffer -> Gtk.TextBuffer -> IO ()
updateLineNumbers mainBuffer lineNumberBuffer = do
    totalLines <- Gtk.textBufferGetLineCount mainBuffer
    let lineNumbers = T.unlines . map T.pack $ show <$> [1..totalLines]
    Gtk.textBufferSetText lineNumberBuffer lineNumbers (-1)

-- save given file content to file
saveFile :: Gtk.TextBuffer -> FilePath -> IO ()
saveFile textBuffer filename = do
    startIter <- Gtk.textBufferGetStartIter textBuffer
    endIter <- Gtk.textBufferGetEndIter textBuffer
    text <- Gtk.textBufferGetText textBuffer startIter endIter False
    TIO.writeFile filename text

-- helper
isBracket :: Char -> Bool
isBracket ch = ch `elem` ("[]{}()" :: String)

isMatchingBracket :: Char -> Char -> Bool
isMatchingBracket '(' ')' = True
isMatchingBracket '{' '}' = True
isMatchingBracket '[' ']' = True
isMatchingBracket _ _     = False

hasMatchingBrackets :: String -> Bool
hasMatchingBrackets = go []
  where
    go [] [] = True
    go _  [] = False
    go stack (c:cs)
      | c `elem` ("([{" :: String) = go (c:stack) cs
      | c `elem` (")]}" :: String) && not (null stack) && isMatchingBracket (head stack) c = go (tail stack) cs
      | otherwise = False
