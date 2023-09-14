{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Menu (createMenuBar) where

import Data.GI.Base
import FileHandler (openFile, saveFile)
import qualified GI.Gtk as Gtk

-- | Create the menu bar with multiple options
createMenuBar
  :: Gtk.Window     -- ^ Window where the menu should be integrated into
  -> Gtk.TextBuffer -- ^ TextBuffer where the contents should be stored
  -> FilePath       -- ^ FilePath where the opened file should be saved
  -> IO Gtk.MenuBar -- ^ Returns a fully initialized 'Gtk.MenuBar'
createMenuBar win textBuffer filepath = do
  menubar <- new Gtk.MenuBar []

  fileMenu <- new Gtk.Menu []

  file <- new Gtk.MenuItem [#label := "File"]
  #setSubmenu file (Just fileMenu)
  #append menubar file

  open <- new Gtk.MenuItem [#label := "Open"]
  _ <- on open #activate (openFile win textBuffer)
  #append fileMenu open

  save <- new Gtk.MenuItem [#label := "Save"]
  _ <- on save #activate (saveFile textBuffer filepath)
  #append fileMenu save

  close <- new Gtk.MenuItem [#label := "Close"]
  _ <- on close #activate (#destroy win)
  #append fileMenu close

  return menubar
