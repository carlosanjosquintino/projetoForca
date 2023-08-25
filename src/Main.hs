import System.Random
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (transpose)
import System.IO (hFlush, stdout)



data GameState = GameState
    { guesses :: [Char]
    , targetWord :: String
    , attempts :: Int
    }

initialState :: String -> GameState
initialState word = GameState
    { guesses = []
    , targetWord = word
    , attempts = 0
    }


forca :: [[String]]
forca =
    transpose
        [ ["   ", " O ", " O ", " O ", " O ", "_O ", "_O_","_O/_"]
        , ["   ", "   ", " | ", " | ", " | ", " | ", " | ","/| "]
        , ["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\","/ \\"]
        ]


imagemCompleta :: GameState -> [String]
imagemCompleta gameState =
    "=========" :
    "|    |" :
    map ("|   " ++) img ++
    ["|"] ++
    replicate (length (head img) - length word) "|   _" ++
    ["|"]
  where
    img = forca !! (attempts gameState)
    word = targetWord gameState
        

readWords :: FilePath -> IO [String]
readWords path = do
    content <- readFile path
    return $ lines content


chooseRandomWord :: [String] -> IO String
chooseRandomWord wordsList = do
    randomIndex <- randomRIO (0, length wordsList - 1)
    return $ wordsList !! randomIndex

maxAttempts :: Int
maxAttempts = length forca - 1  -- Define o número máximo de tentativas




main :: IO ()
main = do
    wordsList <- readWords "palavras.txt"
    randomWord <- chooseRandomWord wordsList
    let [word1, word2] = splitOn ":" randomWord

    let gameState = initialState word1
    putStrLn "Jogo de Forca Iniciado!"
    putStrLn $ "Dica: " ++ word2
    let len = length forca
    let inicial = 0
    putStrLn $ unlines $ imagemCompleta gameState

    playGame gameState
    
playGame :: GameState -> IO ()
playGame gameState@(GameState guesses targetWord attempts) = do
    if attempts >= maxAttempts
        then putStrLn "Você perdeu"
        else do
            putStr "Digite uma letra: "
            hFlush stdout
            guess <- getLine
            let newGuesses = if length guess == 1 then guess ++ guesses else guesses

            let newAttempts = if head guess `elem` targetWord then attempts else attempts + 1
            let newGameState = gameState { guesses = newGuesses, attempts = newAttempts }

            if all (`elem` newGuesses) targetWord
                then putStrLn $ "Parabéns, você adivinhou a palavra: " ++ targetWord
                else do
                    putStrLn $ unlines $ imagemCompleta newGameState
                    putStrLn $ "Palavra: " ++ displayWord targetWord newGuesses
                    playGame newGameState


displayWord :: String -> [Char] -> String
displayWord word guesses = map (\c -> if c `elem` guesses then c else '_') word