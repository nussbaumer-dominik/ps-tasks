module GtkHelpers (getTextFromBuffer, getBufferBounds) where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk

-- | Fetches the text from a given 'Gtk.TextBuffer'.
getTextFromBuffer 
  :: Gtk.TextBuffer -- ^ TextBuffer where operation should be done 
  -> IO T.Text      -- ^ Returns the text from the buffer
getTextFromBuffer textBuffer = do
  startIter <- Gtk.textBufferGetStartIter textBuffer
  endIter <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferGetText textBuffer startIter endIter False

-- | Fetches the start and end iterators of a given 'Gtk.TextBuffer'.
getBufferBounds 
  :: Gtk.TextBuffer                   -- ^ TextBuffer where operation should be done 
  -> IO (Gtk.TextIter, Gtk.TextIter)  -- ^ Returns a tuple containing the start and end iterators.
getBufferBounds buffer = do
  startIter <- Gtk.textBufferGetStartIter buffer
  endIter <- Gtk.textBufferGetEndIter buffer
  return (startIter, endIter)