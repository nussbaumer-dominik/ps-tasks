{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module TextEditor (openEditor) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import Menu (createMenuBar)
import TextView (createTextViewWithNumbers)

-- TODO: refactor this function to compose the textview with the line numbers textview instead of doing it inside
-- TODO: of the createTextViewWithNumbers function
openEditor :: Gtk.Window -> Gtk.Box -> Gtk.TextBuffer -> FilePath -> IO ()
openEditor win grid textbuffer filename = do
  menubar <- createMenuBar win textbuffer filename
  Gtk.containerAdd grid menubar
  Gtk.widgetSetVexpand menubar False
  Gtk.widgetSetValign menubar Gtk.AlignStart

  textview <- createTextViewWithNumbers textbuffer
  scrolledWindow <- new Gtk.ScrolledWindow []
  Gtk.containerAdd scrolledWindow textview
  Gtk.containerAdd grid scrolledWindow
  Gtk.widgetSetVexpand scrolledWindow True

  Gtk.widgetShowAll grid
