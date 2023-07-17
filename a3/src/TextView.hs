{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TextView (createTextView, saveFile) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text.IO as TIO

createTextView :: Gtk.TextBuffer -> FilePath -> IO Gtk.TextView
createTextView textbuffer filename = do
    textview <- new Gtk.TextView [#buffer := textbuffer]
    filecontent <- TIO.readFile filename
    Gtk.textBufferSetText textbuffer filecontent (-1)
    return textview

saveFile :: Gtk.TextBuffer -> FilePath -> IO ()
saveFile textbuffer filename = do
    startIter <- #getStartIter textbuffer
    endIter <- #getEndIter textbuffer
    text <- #getText textbuffer startIter endIter False
    TIO.writeFile filename text
