{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TextEditor (openEditor) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Menu (createMenuBar)
import TextView (createTextView)

openEditor :: Gtk.Window -> Gtk.Box -> Gtk.TextBuffer -> FilePath -> IO ()
openEditor win grid textbuffer filename = do
    menubar <- createMenuBar win textbuffer filename
    Gtk.containerAdd grid menubar
    Gtk.widgetSetVexpand menubar False
    Gtk.widgetSetValign menubar Gtk.AlignStart

    textview <- createTextView textbuffer filename
    scrolledWindow <- new Gtk.ScrolledWindow []
    Gtk.containerAdd scrolledWindow textview
    Gtk.containerAdd grid scrolledWindow
    Gtk.widgetSetVexpand scrolledWindow True

    Gtk.widgetShowAll grid

