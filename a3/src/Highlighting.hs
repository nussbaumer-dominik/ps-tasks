{-# LANGUAGE OverloadedStrings #-}

module Highlighting
  ( applyHighlightsToBuffer,
    getBufferBounds,
  )
where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Syntax (StyleTag (..))
import SyntaxCore (Highlight (..), Range (..))

-- | Takes a Gtk.TextBuffer and a list of Highlights, and applies the corresponding tags to the buffer.
applyHighlightsToBuffer :: Gtk.TextBuffer -> [Highlight] -> IO ()
applyHighlightsToBuffer textBuffer highlights = do
  tagTable <- Gtk.textBufferGetTagTable textBuffer
  forM_ highlights $ \(Highlight style (Range (start, end))) -> do
    tag <- getTag tagTable style
    sIter <- getIterAtOffset textBuffer start
    eIter <- getIterAtOffset textBuffer end
    Gtk.textBufferApplyTag textBuffer tag sIter eIter

getTag :: Gtk.TextTagTable -> StyleTag -> IO Gtk.TextTag
getTag tagTable style = do
  let tagName = T.pack $ show style
  mTag <- Gtk.textTagTableLookup tagTable tagName
  case mTag of
    Nothing -> do
      newTag <- Gtk.textTagNew (Just tagName)
      _ <- Gtk.textTagTableAdd tagTable newTag
      return newTag
    Just tag -> return tag

getIterAtOffset :: Gtk.TextBuffer -> Int -> IO Gtk.TextIter
getIterAtOffset textBuffer offset = Gtk.textBufferGetIterAtOffset textBuffer (fromIntegral offset)

-- | Fetches the start and end iterators of a given 'Gtk.TextBuffer'.
-- Returns a tuple containing the start and end iterators.
getBufferBounds :: Gtk.TextBuffer -> IO (Gtk.TextIter, Gtk.TextIter)
getBufferBounds buffer = do
  startIter <- Gtk.textBufferGetStartIter buffer
  endIter <- Gtk.textBufferGetEndIter buffer
  return (startIter, endIter)
