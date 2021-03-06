module Main where

import Control.Monad ((>=>))
import Data.Functor ((<$>))
import Data.List (foldl')
import System.IO (isEOF)


languageMoves =
    [
        (1, [
            ('a', 2),
            ('b', 3),
            ('c', 4)
        ]),
        (2, [('b', 1)]),
        (3, [('c', 1)]),
        (4, [('b', 5)])
    ]

languageStartState      =  1
languageAcceptingStates = [5]

chain :: Monad m => [a -> m a] -> a -> m a
chain = foldl' (>=>) return

recognizeLanguage :: String -> Bool
recognizeLanguage input = (isAccepting <$> finalState) == Just True where

    move :: Char -> Int -> Maybe Int
    move c s = lookup s languageMoves >>= lookup c

    finalState :: Maybe Int
    finalState = flip chain languageStartState $ map move input

    isAccepting :: Int -> Bool
    isAccepting = flip elem languageAcceptingStates

main :: IO ()
main = do
    putStrLn "Welcome to the regular language \"(ab|bc)*cb\" recognizer!"
    putStrLn "Only put your test inside terminal:"
    putStrLn "(send EOF (Ctrl+D) to terminate)"
    interactByLine recognizeLanguage'
  where
    interactByLine :: (String -> String) -> IO ()
    interactByLine f = isEOF >>= interactByLine'eofHandler
      where
        interactByLine'eofHandler :: Bool -> IO ()
        interactByLine'eofHandler True  = putStrLn "Bye!"
        interactByLine'eofHandler False = interactByLine' f

        interactByLine' :: (String -> String) -> IO ()
        interactByLine' f = f <$> getLine >>= putStrLn >> interactByLine f

    recognizeLanguage' :: String -> String
    recognizeLanguage' = showResult . recognizeLanguage where
        showResult True  = "[GOOD] Input is sentence of this language!"
        showResult False = "[BAD]  Input is not sentence of this language."
