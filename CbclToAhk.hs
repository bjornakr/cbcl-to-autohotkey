import Data.List.Split
import System.IO
import System.Environment
import System.Exit

type Response = Maybe Int
type CbclRow = [String]
type Header = [String]
data ActivityType = SportsHobbies | GroupsJobs

data Options = Options {
    fileName :: String,
    respId :: String,
    verbose :: Bool
}

startVariable = "CBCL4_2"
numberOfCbclVariables = 211

openEndedIndexes = [1, 3, 5, 12, 14, 16, 18, 25, 27, 29, 31, 35, 37, 39, 41, 51, 52, 59,
    61, 63, 65, 67, 69, 71, 72, 73, 83, 104, 116, 123, 137, 142, 145, 154, 159, 163, 168,
    171, 176, 178, 180, 188, 197, 203]

toKeyString :: [Response] -> String
toKeyString [] = ""
toKeyString (Nothing:xs) = "{down}" ++ (toKeyString xs)
toKeyString ((Just x):xs) = (show x) ++ (toKeyString xs)

toResponse :: String -> Response
toResponse "" = Nothing
toResponse s = Just ((read s)::Int)

createScript :: String -> String
createScript keyString = "SetKeyDelay, 300\n^b::\n   Send, " ++ keyString ++ "\nReturn"

findStartIndex' :: [(Int, String)] -> Maybe Int
findStartIndex' [] = Nothing
findStartIndex' ((i, s):xs)
    | s == startVariable = Just i
    | otherwise = findStartIndex' xs

findStartIndex :: Header -> Maybe Int
findStartIndex h = findStartIndex' (zip [0..] h)

dropEmptyRows :: [CbclRow] -> [CbclRow]
dropEmptyRows [] = []    
dropEmptyRows (("":_):rs) = dropEmptyRows rs
dropEmptyRows (r:rs) = (r:dropEmptyRows rs)

dropOpenEnded :: Int -> CbclRow -> CbclRow
dropOpenEnded _ [] = []
dropOpenEnded index (item:items) =        
    if elem index openEndedIndexes then dropOpenEnded (index+1) items
    else (item:(dropOpenEnded (index+1) items))

splitRow :: String -> [String]
splitRow r = splitOn ";" r

extractCbcl :: Int -> [String] -> CbclRow
extractCbcl startIndex row = dropOpenEnded 0 (take numberOfCbclVariables (drop startIndex row))

prettyPrint :: (Int, String, String) -> String
prettyPrint (i, var, val) = (show i) ++ ":\t" ++ var ++ "    \t" ++ val

sumResponses :: [Response] -> Int
sumResponses [] = 0
sumResponses (Nothing:rs) = sumResponses rs
sumResponses((Just x):rs) = x + (sumResponses rs)


filterOutEmptyActivities :: ActivityType -> Int -> [Response] -> [Response]
filterOutEmptyActivities _ 0 _ = []
filterOutEmptyActivities SportsHobbies n@noOfActivities r@responses = 
    (filterOutEmptyActivities SportsHobbies (n-1) r) ++ ((r !! (n-1)):(r !! (n+2)):[])
filterOutEmptyActivities GroupsJobs n@noOfActivities r@responses =
    (filterOutEmptyActivities GroupsJobs (n-1) r) ++ ((r !! (n-1)):[])


processActivity :: ActivityType -> [Response] -> [Response]
processActivity activityType activityResponses =
    let noOfActivities = (sumResponses (take 3 activityResponses)) in
        Just noOfActivities:(map rescaleActivity (filterOutEmptyActivities activityType noOfActivities (drop 3 activityResponses)))


toResponses :: CbclRow -> [Response]
toResponses row =
    let raw = map toResponse row in
        (processActivity SportsHobbies (take 9 raw))
        ++ (processActivity SportsHobbies (take 9 (drop 9 raw)))
        ++ (processActivity GroupsJobs (take 6 (drop 18 raw))) 
        ++ (processActivity GroupsJobs (take 6 (drop 24 raw)))
        ++ (map toBaseZeroScale (take 18 (drop 30 raw)))
        ++ (drop 48 raw)


rescaleActivity :: Response -> Response
rescaleActivity Nothing = Nothing
rescaleActivity (Just 4) = Just 0
rescaleActivity r = r

toBaseZeroScale :: Response -> Response
toBaseZeroScale Nothing = Nothing
toBaseZeroScale (Just x) = Just (x-1)

dealWithMaybe :: Maybe Int -> String -> IO Int
dealWithMaybe Nothing msg = do
    putStrLn msg
    exitFailure
dealWithMaybe (Just a) _ = return a

process :: Options -> [[String]] -> IO ()
process options rows = do
    let onlyRespRow = concat $ filter (\x -> (x !! 1) == (respId options)) rows
    if (length onlyRespRow == 0) 
        then error $ "ERROR: Could not find data for RespId = " ++ (respId options) ++ "."
        else return ()
    let orderedRows = reverse (dropEmptyRows rows)

    startIndex <- dealWithMaybe 
        (findStartIndex (head orderedRows))
        ("Could not find CBCL items (starting with " ++ startVariable ++ ").")
    
    let headerCbcl = extractCbcl startIndex (head orderedRows)
    let respCbcl = extractCbcl startIndex (onlyRespRow)
    let script = createScript (toKeyString (toResponses respCbcl))

    if (verbose options) 
        then do
            mapM_ putStrLn (map prettyPrint (zip3 [0..] headerCbcl respCbcl))
            putStrLn $ script
        else 
            return ()
    
    let outfileName = "cbcl-" ++ (respId options) ++ ".ahk"
    putStrLn $ "> Writing AutoHotKey script to: " ++ outfileName
    writeToFile outfileName script
    return ()


writeToFile :: String -> String -> IO ()
writeToFile fileName content = do
    fileHandle <- openFile fileName WriteMode
    hPutStrLn fileHandle content
    hClose fileHandle
    return ()

parse :: Handle -> [[String]] -> IO [CbclRow]
parse inHandle rows = do
    isEof <- hIsEOF inHandle
    if isEof
    then 
        return rows
    else do
        row <- hGetLine inHandle
        parse inHandle ((splitRow row):rows)

parseAndProcess options = do
    let infileName = fileName options
    putStrLn $ "> Reading from: " ++ infileName
    fileHandle <- openFile (fileName options) ReadMode
    rows <- parse fileHandle []
    hClose fileHandle
    process options rows

main = do
    putStrLn "\nCBCL TO AutoHotKey v0.9.0"
    putStrLn "-------------------------"
    args <- getArgs
    if ((length args) < 2)
        then 
            putStrLn "USAGE: cbcl2ahk <input file> <respid> [--verbose]"
        else do
            let options = Options {
                fileName = args !! 0,
                respId = args !! 1,
                verbose = (length args) >= 3 && (args !! 2 == "--verbose")
            }
            parseAndProcess options