{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module FileHandler (openFile, saveAsFile) where

import Data.GI.Base
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import TextView (saveFile)

openFile :: Gtk.Window -> Gtk.TextBuffer -> IO ()
openFile win textbuffer = do
  dialog <- Gtk.new Gtk.FileChooserDialog [#title := "Open File", #action := Gtk.FileChooserActionOpen]
  _ <- Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
  _ <- Gtk.dialogAddButton dialog "Open" (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
  Gtk.windowSetTransientFor dialog (Just win)
  response <- Gtk.dialogRun dialog
  case response of
    x | x == fromIntegral (fromEnum Gtk.ResponseTypeAccept) -> do
      Just filename <- Gtk.fileChooserGetFilename dialog
      filecontent <- TIO.readFile filename
      Gtk.textBufferSetText textbuffer filecontent (-1)
      Gtk.widgetHide dialog
    _ -> Gtk.widgetHide dialog

saveAsFile :: Gtk.Window -> Gtk.TextBuffer -> IO ()
saveAsFile win textbuffer = do
  dialog <- Gtk.new Gtk.FileChooserDialog [#title := "Save File As", #action := Gtk.FileChooserActionSave]
  _ <- Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
  _ <- Gtk.dialogAddButton dialog "Save" (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
  Gtk.windowSetTransientFor dialog (Just win)
  response <- Gtk.dialogRun dialog
  case response of
    x | x == fromIntegral (fromEnum Gtk.ResponseTypeAccept) -> do
      Just filename <- Gtk.fileChooserGetFilename dialog
      Gtk.widgetHide dialog
      saveFile textbuffer filename
    _ -> Gtk.widgetHide dialog
