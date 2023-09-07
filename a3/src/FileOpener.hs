{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module FileOpener (openFileDialog) where

import Data.GI.Base
import Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import TextEditor (openEditor)

openFileDialog :: Gtk.Window -> Gtk.Box -> Gtk.TextBuffer -> IO ()
openFileDialog win grid textBuffer = do
  dialog <- Gtk.new Gtk.FileChooserDialog [#title := "Open File", #action := Gtk.FileChooserActionOpen]
  _ <- Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
  _ <- Gtk.dialogAddButton dialog "Open" (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
  Gtk.windowSetTransientFor dialog (Just win)
  response <- Gtk.dialogRun dialog
  case response of
    x | x == fromIntegral (fromEnum Gtk.ResponseTypeAccept) -> do
      Just filename <- Gtk.fileChooserGetFilename dialog
      fileContent <- TIO.readFile filename
      Gtk.textBufferSetText textBuffer fileContent (-1)
      Gtk.widgetHide dialog
      children <- #getChildren grid
      mapM_ (#remove grid) children
      openEditor win grid textBuffer filename
    _ -> Gtk.widgetHide dialog
