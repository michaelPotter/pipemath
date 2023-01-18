import Text.Read (readMaybe)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStr "missing arg"
        [equationString] -> do
            stdin <- getContents
            let results = main' equationString stdin
            case results of
                Left error     -> failProgram error
                Right results' -> mapM_ print results'
        _ -> failProgram "Too many arguments given"

main' :: String -> String -> Either String [Float]
main' equationString stdin = do
    eqn <- parseEquation equationString
    nums <- parseLines . lines $ stdin
    Right $ eqn nums

failProgram :: String -> IO ()
failProgram s = hPutStrLn stderr s >> exitFailure


type Eqn = ([Float] -> [Float])

parseEquation :: String -> Either String Eqn
parseEquation text = go $ tokenize text
    where
        go :: [String] -> Either String Eqn
        -- Arithmetic
        go ["*", n] = Right (map (* (read n :: Float)))
        go [n, "*"] = Right (map (* (read n :: Float)))
        go ["+", n] = Right (map (+ (read n :: Float)))
        go [n, "+"] = Right (map (+ (read n :: Float)))
        go ["/", n] = Right (map (\x -> x / (read n :: Float)))
        go [n, "/"] = Right (map (\x -> (read n :: Float) / x))
        go ["-", n] = Right (map (\x -> x - (read n :: Float)))
        go [n, "-"] = Right (map (\x -> (read n :: Float) - x))
        -- Aggregate
        go ["sum"]  = Right $ \x -> [ sum x ]
        go ["avg"]  = Right $ \x -> [ sum x / fromIntegral (length x) ]
        -- Rounding
        go ["round"]   = Right (map $ fromIntegral . round)
        go ["floor"]   = Right (map $ fromIntegral . floor)
        go ["ceil"]    = Right (map $ fromIntegral . ceiling)
        go ["ceiling"] = Right (map $ fromIntegral . ceiling)
        go _ = Left $ "Error: Could not parse equation: '" ++ text ++ "'"

-- TODO support things like "+2" or "8-" without a space btwn number and operator
tokenize :: String -> [String]
tokenize = words

parseLines :: [String] -> Either String [Float]
parseLines = mapM parseLine . zip [1..]
    where
        parseLine :: (Int, String) -> Either String Float
        parseLine (i, line) = let parsed = readMaybe line
            in case parsed of
                Nothing -> Left $ "Error: line " ++ show i ++ ": Could not parse as number: " ++ line
                Just x -> Right x
