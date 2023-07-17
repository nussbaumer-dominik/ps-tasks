{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Lib
    ( someFunc
    ) where

import GI.Gtk as Gtk
import Data.GI.Base
import Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    _ <- Gtk.init Nothing

    win <- Gtk.new Gtk.Window [#title := "Text Editor"]
    _ <- Gtk.on win #destroy Gtk.mainQuit

    grid <- Gtk.new Gtk.Grid []
    Gtk.containerAdd win grid

    textview <- Gtk.new Gtk.TextView []
    textbuffer <- Gtk.textViewGetBuffer textview

    scrolledWindow <- Gtk.new Gtk.ScrolledWindow []
    Gtk.containerAdd scrolledWindow textview
    Gtk.containerAdd grid scrolledWindow

    button <- Gtk.new Gtk.Button [#label := "Open"]
    _ <- Gtk.on button #clicked (openFileDialog win textbuffer)
    Gtk.containerAdd grid button

    Gtk.widgetShowAll win

    Gtk.main

openFileDialog :: Gtk.Window -> Gtk.TextBuffer -> IO ()
openFileDialog win textbuffer = do
    dialog <- Gtk.new Gtk.FileChooserDialog [#title := "Open File", #action := Gtk.FileChooserActionOpen]
    Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
    Gtk.dialogAddButton dialog "Open" (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
    Gtk.windowSetTransientFor dialog (Just win)
    response <- Gtk.dialogRun dialog
    case response of
        x | x == fromIntegral (fromEnum Gtk.ResponseTypeAccept) -> do
            Just filename <- Gtk.fileChooserGetFilename dialog
            filecontent <- TIO.readFile filename
            Gtk.textBufferSetText textbuffer filecontent (-1)
            Gtk.widgetHide dialog
        _ -> Gtk.widgetHide dialog

