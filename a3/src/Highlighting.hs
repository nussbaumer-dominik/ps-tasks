{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Highlighting
    ( highlightSyntax
    ) where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Control.Monad (forM_)
import Text.Regex.PCRE
import Syntax

highlightSyntax :: Gtk.TextBuffer -> IO ()
highlightSyntax buffer = do
    putStrLn "highlightSyntax"

    startIter <- #getStartIter buffer
    endIter <- #getEndIter buffer
    text <- Gtk.textBufferGetText buffer startIter endIter False

    let tokenPositions = getTokenPositions text syntaxRules
    putStrLn $ show tokenPositions
    tagTable <- Gtk.textBufferGetTagTable buffer

    forM_ tokenPositions $ \(styleName, (startPos, endPos)) -> do
        mTagMaybe <- getTag tagTable styleName
        case mTagMaybe of
            Just tag -> do
                start <- getIterAtOffset buffer startPos
                end <- getIterAtOffset buffer endPos
                Gtk.textBufferApplyTag buffer tag start end
            Nothing -> return ()
    return ()

getTokenPositions :: T.Text -> [SyntaxRule] -> [(String, (Int, Int))]
getTokenPositions text rules =
    concatMap (\rule -> findMatches text rule) rules

findMatches :: T.Text -> SyntaxRule -> [(String, (Int, Int))]
findMatches t rule =
    let matches = getAllMatches $ T.unpack t =~ T.unpack (pattern rule) :: [(Int, Int)]
    in map (\(start, len) -> (styleTag rule, (start, start + len))) matches

getTag :: Gtk.TextTagTable -> String -> IO (Maybe Gtk.TextTag)
getTag tagTable name = Gtk.textTagTableLookup tagTable (T.pack name)

getIterAtOffset :: Gtk.TextBuffer -> Int -> IO Gtk.TextIter
getIterAtOffset buffer offset = Gtk.textBufferGetIterAtOffset buffer (fromIntegral offset)