{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import FileOpener (openFileDialog)
import Syntax (initTagTable)

main :: IO ()
main = do
    _ <- Gtk.init Nothing

    initialWindow <- new Gtk.Window [
        #title := "File Opener",
        #defaultWidth := 800,
        #defaultHeight := 600
        ]
    _ <- on initialWindow #destroy Gtk.mainQuit

    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add initialWindow box

    -- create TextBuffer
    buffer <- new Gtk.TextBuffer []
    -- get TagTable and initiate it
    tagTable <- Gtk.textBufferGetTagTable buffer
    initTagTable tagTable

    button <- new Gtk.Button [#label := "Open"]
    _ <- on button #clicked (openFileDialog initialWindow box buffer)
    #setCenterWidget box (Just button)

    #showAll initialWindow

    Gtk.main