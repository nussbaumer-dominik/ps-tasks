{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module FileHandler (openFile, saveFile) where

import Data.GI.Base
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import GtkHelpers (getTextFromBuffer)

-- | Opens a file using a file chooser dialog and loads its content into a 'Gtk.TextBuffer'.
--
-- This function presents a file chooser dialog to the user. If the user selects a file
-- and clicks "Open", the content of the file is read and loaded into the provided 'Gtk.TextBuffer'.
-- If the user clicks "Cancel" or closes the dialog, no action is taken.
--
-- Arguments:
-- * The parent 'Gtk.Window' for the file chooser dialog.
-- * The 'Gtk.TextBuffer' where the file content should be loaded.
openFile :: Gtk.Window -> Gtk.TextBuffer -> IO ()
openFile win textBuffer = do
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
    _ -> Gtk.widgetHide dialog

-- | Save given file content to file
saveFile :: Gtk.TextBuffer -> FilePath -> IO ()
saveFile textBuffer filename = do
  text <- getTextFromBuffer textBuffer
  TIO.writeFile filename text
