{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TextView (createTextViewWithNumbers, saveFile) where

import qualified Data.Text as T
import GI.Gtk
import Highlighting (highlightSyntax)
import qualified Data.Text.IO as TIO

-- create TextView and load file content
createTextView :: TextBuffer -> IO TextView
createTextView textbuffer = do
    textview <- new TextView [ #buffer := textbuffer
                             , #indent := 4
                             ]

    highlightSyntax textbuffer

    _ <- on textbuffer #changed (highlightSyntax textbuffer)
    return textview


createTextViewWithNumbers :: TextBuffer -> IO Box
createTextViewWithNumbers textBuffer = do
    -- Create main TextView
    textview <- createTextView textBuffer

    -- Create line number TextView
    lineNumberBuffer <- new TextBuffer []
    lineNumberView <- new TextView [ #buffer := lineNumberBuffer
                                   , #editable := False
                                   , #cursorVisible := False
                                   , #wrapMode := WrapModeNone
                                   , #leftMargin := 5
                                   , #rightMargin := 15
                                   ]

    -- Pack both TextViews into a horizontal box
    hbox <- new Box [ #orientation := OrientationHorizontal ]
    separator <- new Separator [ #orientation := OrientationVertical ]
    #packStart hbox lineNumberView False True 0
    #packStart hbox separator False False 0
    #packStart hbox textview True True 0

    -- Update line numbers every time main TextView content changes
    _ <- on textBuffer #changed (updateLineNumbers textBuffer lineNumberBuffer)

    -- Initial line number update
    updateLineNumbers textBuffer lineNumberBuffer

    return hbox

updateLineNumbers :: TextBuffer -> TextBuffer -> IO ()
updateLineNumbers mainBuffer lineNumberBuffer = do
    totalLines <- #getLineCount mainBuffer
    let lineNumbers = T.unlines . map T.pack $ show <$> [1..totalLines]
    textBufferSetText lineNumberBuffer lineNumbers (-1)

-- save given file content to file
saveFile :: TextBuffer -> FilePath -> IO ()
saveFile textbuffer filename = do
    startIter <- #getStartIter textbuffer
    endIter <- #getEndIter textbuffer
    text <- #getText textbuffer startIter endIter False
    TIO.writeFile filename text