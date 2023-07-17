{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Menu (createMenuBar) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import FileHandler (openFile)
import TextView (saveFile)

createMenuBar :: Gtk.Window -> Gtk.TextBuffer -> FilePath -> IO Gtk.MenuBar
createMenuBar win textbuffer filepath = do
    menubar <- new Gtk.MenuBar []

    filemenu <- new Gtk.Menu []

    file <- new Gtk.MenuItem [#label := "File"]
    #setSubmenu file (Just filemenu)
    #append menubar file

    open <- new Gtk.MenuItem [#label := "Open"]
    _ <- on open #activate (openFile win textbuffer)
    #append filemenu open

    save <- new Gtk.MenuItem [#label := "Save"]
    _ <- on save #activate (saveFile textbuffer filepath)
    #append filemenu save

    close <- new Gtk.MenuItem [#label := "Close"]
    _ <- on close #activate (#destroy win)
    #append filemenu close

    return menubar
