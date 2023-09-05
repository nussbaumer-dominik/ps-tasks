{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Highlighting
    ( highlightSyntax
    ) where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Text.Megaparsec
import Control.Monad (forM_)
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

-- helper functions
--getTokenPositions :: T.Text -> [SyntaxRule] -> [(String, (Int, Int))]
--getTokenPositions text rules =
--concatMap (findMatches text) rules
--where
--    findMatches :: T.Text -> SyntaxRule -> [(String, (Int, Int))]
--    findMatches t rule = case parse (manyTill parserWithPos eof) "" t of
--        Left _ -> []
--        Right matches -> matches
--      where
--        parserWithPos = do
--          startPos <- getSourcePos
--          _ <- parser rule
--          endPos <- getSourcePos
--          let style = styleTag rule
--              startOffset = sourcePosToOffset t startPos
--              endOffset = sourcePosToOffset t endPos
--          return (style, (startOffset, endOffset))

getTokenPositions :: T.Text -> [SyntaxRule] -> [(String, (Int, Int))]
getTokenPositions text rules = go text 0
    where
        go :: T.Text -> Int -> [(String, (Int, Int))]
        go t offset
            | T.null t = []
            | otherwise =
                let matches = concatMap (findMatch t offset) rules
                in case matches of
                     []     -> go (T.drop 1 t) (offset + 1)  -- No match, skip one character
                     (m:_)  -> m : go (T.drop (snd (snd m) - offset + 1) t) (snd (snd m))  -- Match found, jump to the end of the match

        findMatch :: T.Text -> Int -> SyntaxRule -> [(String, (Int, Int))]
        findMatch t offset rule = case parse (parserWithPos rule) "" t of
            Left _       -> []
            Right match  -> [(styleTag rule, (fst match + offset, snd match + offset))]

        parserWithPos :: SyntaxRule -> Parser (Int, Int)
        parserWithPos rule = do
            startPos <- getOffset
            _ <- parser rule
            endPos <- getOffset
            return (startPos, endPos)

        getOffset = length <$> manyTill anySingle (lookAhead (try anyRule <|> (eof >> return T.empty)))
        anyRule = foldl1 (<|>) (parser <$> rules)


getTag :: Gtk.TextTagTable -> String -> IO (Maybe Gtk.TextTag)
getTag tagTable name = Gtk.textTagTableLookup tagTable (T.pack name)

getIterAtOffset :: Gtk.TextBuffer -> Int -> IO Gtk.TextIter
getIterAtOffset buffer offset = Gtk.textBufferGetIterAtOffset buffer (fromIntegral offset)

sourcePosToOffset :: T.Text -> SourcePos -> Int
sourcePosToOffset txt pos =
    let precedingLines = take (unPos (sourceLine pos) - 1) (T.lines txt)
        precedingChars = sum $ map T.length precedingLines
    in precedingChars + (unPos (sourceColumn pos) - 1) + length precedingLines -- Add length for newline chars