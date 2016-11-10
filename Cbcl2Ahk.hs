import Data.List.Split
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import System.IO
import System.Environment (getArgs, getProgName)
import System.Exit
import Control.Monad (when)

version = "1.0.1"

type Response = Maybe Int
type CbclRow = [String]
type Header = [String]
data ActivityType = SportsHobbies | GroupsJobs
data FormType = Cbcl | Ysr

data Options = Options {
    formType :: String,
    fileName :: String,
    respId :: String,
    verbose :: Bool
}

startVariable Cbcl = "CBCL4_2"
startVariable Ysr = "q7_2"
respVariable = "Resp_For_or_Child"


numberOfCbclVariables Cbcl = 212
numberOfCbclVariables Ysr = 204

openEndedIndexes Cbcl = [1, 3, 5, 12, 14, 16, 18, 25, 27, 29, 31, 35, 37, 39, 41, 51, 52, 59,
    61, 63, 65, 67, 69, 71, 72, 73, 83, 104, 116, 123, 137, 142, 145, 154, 159, 163, 168,
    171, 176, 178, 180, 188, 197, 203]
openEndedIndexes Ysr = [1, 3, 5, 12, 14, 16, 18, 25, 27, 29, 31, 35, 37, 39, 41,
    51, 52, 59, 61, 63, 64, 65, 66, 69, 77, 98, 110, 117, 131, 136, 139,
    148, 153, 161, 164, 169, 171, 173, 181, 190, 196]

toKeyString :: [Response] -> String
toKeyString [] = ""
toKeyString (Nothing:xs) = "{down}" ++ (toKeyString xs)
toKeyString ((Just x):xs) = (show x) ++ (toKeyString xs)

toResponse :: String -> Response
toResponse "" = Nothing
toResponse s = Just ((read s)::Int)

createScript :: String -> String
createScript keyString = "SetKeyDelay, 300\n^b::\n   Send, " ++ keyString ++ "\nReturn"

findStartIndex' :: FormType -> [(Int, String)] -> Maybe Int
findStartIndex' _ [] = Nothing
findStartIndex' formType ((i, s):xs)
    | s == startVariable formType = Just i
    | otherwise = findStartIndex' formType xs

findStartIndex :: FormType -> Header -> Maybe Int
findStartIndex formType h = findStartIndex' formType (zip [0..] h)

findIndex :: String -> Header -> Maybe Int
findIndex var header =
    let indexedHeader = zip [0..] header in
    case (dropWhile (\(i, x) -> x /= var) indexedHeader) of
        [] -> Nothing
        ((i2, x2):xs) -> Just i2



dropEmptyRows :: [CbclRow] -> [CbclRow]
dropEmptyRows [] = []    
dropEmptyRows (("":_):rs) = dropEmptyRows rs
dropEmptyRows (r:rs) = (r:dropEmptyRows rs)

dropOpenEnded :: FormType -> Int -> CbclRow -> CbclRow
dropOpenEnded _ _ [] = []
dropOpenEnded formType index (item:items) =        
    if elem index (openEndedIndexes formType) then dropOpenEnded formType (index+1) items
    else (item:(dropOpenEnded formType (index+1) items))

splitRow :: String -> [String]
splitRow r = splitOn ";" r

extractCbcl :: FormType -> Int -> [String] -> CbclRow
extractCbcl formType startIndex row = 
    dropOpenEnded formType 0 (take (numberOfCbclVariables formType) (drop startIndex row))

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


processActivity :: (Response -> Response) -> ActivityType -> [Response] -> [Response]
processActivity scaleMethod activityType activityResponses =
    let noOfActivities = (sumResponses (take 3 activityResponses)) in
        Just noOfActivities:(map scaleMethod (filterOutEmptyActivities activityType noOfActivities (drop 3 activityResponses)))


toResponses :: FormType -> CbclRow -> [Response]
toResponses Cbcl row =
    let raw = map toResponse row
        proc = processActivity wrapScale in
            (proc SportsHobbies (take 9 raw)) ++
            (proc SportsHobbies (take 9 (drop 9 raw))) ++
            (proc GroupsJobs (take 6 (drop 18 raw))) ++
            (proc GroupsJobs (take 6 (drop 24 raw))) ++
            (map toBaseZeroScale (take 18 (drop 30 raw))) ++
            (drop 48 raw)

toResponses Ysr row =
    let raw = map toResponse row
        proc = processActivity dropNotApplicable in
            (proc SportsHobbies (take 9 raw)) ++
            (proc SportsHobbies (take 9 (drop 9 raw))) ++
            (proc GroupsJobs (take 6 (drop 18 raw)))  ++
            (proc GroupsJobs (take 6 (drop 24 raw))) ++
            (map toBaseZeroScale (take 2 (drop 30 raw))) ++
            (map toBaseZeroScale (map dropNotApplicable (take 1 (drop 32 raw)))) ++
            (map toBaseZeroScale (take 11 (drop 33 raw))) ++
            (drop 44 raw)


wrapScale :: Response -> Response
wrapScale (Just 4) = Just 0
wrapScale r = r

dropNotApplicable :: Response -> Response
dropNotApplicable (Just 4) = Nothing
dropNotApplicable r = r

--rescaleActivity :: FormType -> Response -> Response
--rescaleActivity _ Nothing = Nothing
--rescaleActivity Cbcl (Just 4) = Just 0
--rescaleActivity Ysr (Just 4) = Nothing
--rescaleActivity r = r

toBaseZeroScale :: Response -> Response
toBaseZeroScale Nothing = Nothing
toBaseZeroScale (Just x) = Just (x-1)

valueOrError :: Maybe Int -> String -> IO Int
valueOrError Nothing msg = do
    putStrLn msg
    exitFailure
valueOrError (Just a) _ = return a

process :: Options -> [[String]] -> IO ()
process options rows = do
    let formName = map toLower (formType options)
    let fType = case formName of
                    "cbcl" -> Cbcl
                    "ysr" -> Ysr
                    invalid -> error $ "ERROR: Invalid form type '" ++ invalid ++ "'. 'cbcl' or 'ysr' required."

    let respType formType = case formType of
                                Cbcl -> "F"
                                Ysr  -> "C"
    let orderedRows = reverse (dropEmptyRows rows)
    let header = head orderedRows

    let respVarIndex =
            fromMaybe
                (error $ "Could not find column " ++ respVariable)
                (findIndex respVariable header)
        

    let childrenOrParentsRows = filter (\x -> (x !! respVarIndex) == (respType fType)) rows
    let onlyRespRow = concat $ filter (\x -> (x !! 15) == (respId options)) childrenOrParentsRows
    if (length onlyRespRow == 0) 
        then error $ "ERROR: Could not find data for ID = " ++ (respId options) ++ "."
        else return ()

    startIndex <- valueOrError 
        (findStartIndex fType header)
        ("Could not find CBCL items (starting with " ++ (startVariable fType) ++ ").")
    
    let headerCbcl = extractCbcl fType startIndex (head orderedRows)
    let respCbcl = extractCbcl fType startIndex (onlyRespRow)
    let script = createScript (toKeyString (toResponses fType respCbcl))

    when (verbose options) $ do
        mapM_ putStrLn (map prettyPrint (zip3 [0..] headerCbcl respCbcl))
        putStrLn $ script
    
    let outfileName = formName ++ "-" ++ (respId options) ++ ".ahk"
    putStrLn $ "> Writing AutoHotKey script to: " ++ outfileName
    writeToFile outfileName script
    return ()


writeToFile :: String -> String -> IO ()
writeToFile fileName content = do
    withFile fileName WriteMode (\handle -> do hPutStrLn handle content)
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
    putStrLn $ "\nCBCL TO AutoHotKey v" ++ version
    putStrLn "-------------------------"
    args <- getArgs
    if ((length args) < 3)
        then do
            progName <- getProgName
            putStrLn $ "USAGE: " ++ progName ++ " <cbcl|ysr> <input file> <respid> [--verbose]\n"
            putStrLn $ "EXAMPLE:"
            putStrLn $ "> " ++ progName ++ " cbcl data.csv 120"
            putStrLn $ "Creates an AutoHotKey script for CBCL for respondent 120."
        else do
            let options = Options {                
                formType = args !! 0,
                fileName = args !! 1,
                respId = map toUpper (args !! 2),
                verbose = (length args) >= 4 && (args !! 3 == "--verbose")
            }
            parseAndProcess options