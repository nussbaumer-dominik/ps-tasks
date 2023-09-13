module GtkHelpers (getTextFromBuffer) where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk

getTextFromBuffer :: Gtk.TextBuffer -> IO T.Text
getTextFromBuffer textBuffer = do
  startIter <- Gtk.textBufferGetStartIter textBuffer
  endIter <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferGetText textBuffer startIter endIter False
