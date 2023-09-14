{-# LANGUAGE OverloadedStrings #-}

module Highlighting
  ( applyHighlightsToBuffer,
    getBufferBounds,
  )
where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import GtkHelpers (getBufferBounds)
import Syntax (StyleTag (..))
import SyntaxCore (Highlight (..), Range (..))

-- | Takes a 'Gtk.TextBuffer' and a list of Highlights, and applies the corresponding tags to the buffer.
applyHighlightsToBuffer
  :: Gtk.TextBuffer -- ^ TextBuffer where highlights should be applied
  -> [Highlight]    -- ^ List of Highlights to be applied
  -> IO ()          -- ^ Returns nothing
applyHighlightsToBuffer textBuffer highlights = do
  tagTable <- Gtk.textBufferGetTagTable textBuffer
  forM_ highlights $ \(Highlight style (Range (start, end))) -> do
    tag <- getTag tagTable style
    sIter <- getIterAtOffset textBuffer start
    eIter <- getIterAtOffset textBuffer end
    Gtk.textBufferApplyTag textBuffer tag sIter eIter

-- | Retrieves or creates a 'Gtk.TextTag' from a 'Gtk.TextTagTable' by name
getTag
  :: Gtk.TextTagTable -- ^ TagTable where all available tags are stored
  -> StyleTag         -- ^ StyleTag name to be retrieved
  -> IO Gtk.TextTag   -- ^ Returns the found Tag or creates it if it doesn't
getTag tagTable style = do
  let tagName = T.pack $ show style
  mTag <- Gtk.textTagTableLookup tagTable tagName
  case mTag of
    Nothing -> do
      newTag <- Gtk.textTagNew (Just tagName)
      _ <- Gtk.textTagTableAdd tagTable newTag
      return newTag
    Just tag -> return tag

-- | Helper function that retrieves a 'Gtk.TextIter' for a specific position in a 'Gtk.TextBuffer'
getIterAtOffset
  :: Gtk.TextBuffer   -- ^ TextBuffer where iterator should be retrieved
  -> Int              -- ^ Integer offset position
  -> IO Gtk.TextIter  -- ^ Returns the found iterator
getIterAtOffset textBuffer offset = Gtk.textBufferGetIterAtOffset textBuffer (fromIntegral offset)
