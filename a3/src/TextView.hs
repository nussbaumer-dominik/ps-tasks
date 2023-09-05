{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TextView (createTextView, saveFile) where

import GI.Gtk
import Highlighting (highlightSyntax)
import Data.Text.IO as TIO

-- create TextView and load file content
createTextView :: TextBuffer -> FilePath -> IO TextView
createTextView textbuffer filename = do
    textview <- new TextView [#buffer := textbuffer]
    filecontent <- TIO.readFile filename
    textBufferSetText textbuffer filecontent (-1)
    highlightSyntax textbuffer
    _ <- on textbuffer #changed (highlightSyntax textbuffer)
    return textview

-- save given file content to file
saveFile :: TextBuffer -> FilePath -> IO ()
saveFile textbuffer filename = do
    startIter <- #getStartIter textbuffer
    endIter <- #getEndIter textbuffer
    text <- #getText textbuffer startIter endIter False
    TIO.writeFile filename text
