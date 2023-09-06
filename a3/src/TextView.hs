{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TextView (createTextViewWithNumbers, saveFile) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import Control.Monad (when, unless, void)
import Highlighting ( highlightSyntax
                    , highlightWordOccurrences
                    , highlightMatchingBracket
                    , isBracket
                    )

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
        -- Remove the existing highlighting first
        removeHighlighting textBuffer

        -- Get the current word
        wordToHighlight <- identifyWord iter
        unless (T.null wordToHighlight) $ do
            highlightWordOccurrences textBuffer wordToHighlight

        charRightOfCursor <- Gtk.textIterGetChar iter
        iterLeftOfCursor <- Gtk.textIterCopy iter
        canMoveBack <- Gtk.textIterBackwardChar iterLeftOfCursor
        charLeftOfCursor <- if canMoveBack
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

    -- Extract the word
    Gtk.textIterGetText startWordIter endWordIter

adjustIters :: Bool -> Bool -> Bool -> Gtk.TextIter -> Gtk.TextIter -> IO ()
adjustIters True  _    _    _     end = void $ Gtk.textIterForwardWordEnd end
adjustIters _    True  _    start end = do
    void $ Gtk.textIterBackwardWordStart start
    void $ Gtk.textIterForwardWordEnd end
adjustIters _    _    True  start _   = void $ Gtk.textIterBackwardWordStart start
adjustIters _    _    _     _     _   = return ()

removeHighlighting :: Gtk.TextBuffer -> IO ()
removeHighlighting buffer = do
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