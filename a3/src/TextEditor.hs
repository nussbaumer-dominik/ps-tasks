{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module TextEditor (openEditor) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import Menu (createMenuBar)
import TextView (createTextView, createTextViewWithNumbers)

-- | Create a new TextView with a given TextBuffer
-- This function creates a 'Gtk.TextView', attaches the given 'Gtk.TextBuffer'
-- and also shows the line numbers
openEditor :: Gtk.Window -> Gtk.Box -> Gtk.TextBuffer -> FilePath -> IO ()
openEditor win grid textBuffer filename = do
  menubar <- createMenuBar win textBuffer filename
  Gtk.containerAdd grid menubar
  Gtk.widgetSetVexpand menubar False
  Gtk.widgetSetValign menubar Gtk.AlignStart
  textView <- createTextView textBuffer
  lineNumberView <- createTextViewWithNumbers textBuffer
  hBox <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationHorizontal]
  separator <- Gtk.new Gtk.Separator [#orientation Gtk.:= Gtk.OrientationVertical]
  Gtk.boxPackStart hBox lineNumberView False True 0
  Gtk.boxPackStart hBox separator False False 0
  Gtk.boxPackStart hBox textView True True 0

  scrolledWindow <- new Gtk.ScrolledWindow []
  Gtk.containerAdd scrolledWindow hBox
  Gtk.containerAdd grid scrolledWindow
  Gtk.widgetSetVexpand scrolledWindow True

  Gtk.widgetShowAll grid
