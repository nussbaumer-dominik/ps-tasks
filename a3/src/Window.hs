{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Window (createEditorWindow) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Menu (createMenuBar)
import TextView (createTextView)

createEditorWindow :: Gtk.Window -> Gtk.Box -> Gtk.TextBuffer -> FilePath -> IO ()
createEditorWindow win box textbuffer filename = do
    menubar <- createMenuBar win textbuffer filename
    #packStart box menubar False False 0

    textview <- createTextView textbuffer filename
    scrolledWindow <- new Gtk.ScrolledWindow []
    #add scrolledWindow textview
    #packStart box scrolledWindow True True 0
