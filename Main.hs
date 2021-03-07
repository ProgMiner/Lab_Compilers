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

data RecognitionError = BadChar Int | NotEnded

showRecognitionError :: String -> RecognitionError -> String
showRecognitionError input (BadChar i) = "bad char at position " ++ show i ++ ": \""
        ++ take i input ++ '>':head (drop i input):'<':drop (i + 1) input ++ "\""

showRecognitionError input NotEnded    = "too short input"

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..length xs] xs

chain :: Monad m => [a -> m a] -> a -> m a
chain = foldl' (>=>) return

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Right a) = Nothing
leftToMaybe (Left  a) = Just a

recognizeLanguage :: String -> Maybe RecognitionError
recognizeLanguage input = leftToMaybe $ finalState >>= isAccepting where

    move :: Char -> Int -> Maybe Int
    move c s = lookup s languageMoves >>= lookup c

    finalState :: Either RecognitionError Int
    finalState = flip chain languageStartState $ map mapMove $ enumerate input where

        mapMove :: (Int, Char) -> Int -> Either RecognitionError Int
        mapMove (i, c) = mapMove' . move c where

            mapMove' :: Maybe Int -> Either RecognitionError Int
            mapMove'  Nothing = Left $ BadChar i
            mapMove' (Just s) = Right s

    isAccepting :: Int -> Either RecognitionError ()
    isAccepting s = if isAccepting' s then Right () else Left NotEnded where

        isAccepting' :: Int -> Bool
        isAccepting' = flip elem languageAcceptingStates

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
    recognizeLanguage' input = showResult $ recognizeLanguage input where
        showResult Nothing  = "[GOOD] Input is sentence of this language!"
        showResult (Just e) = "[BAD]  Input is not sentence of this language: "
                ++ showRecognitionError input e ++ "."
