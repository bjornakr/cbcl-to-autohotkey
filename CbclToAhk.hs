--module CbclToAhk where
    import Data.List.Split
    import System.IO
    import System.Environment
    import System.Exit

    type Response = Maybe Int
    type CbclRow = [String]
    type Header = [String]
    --type Options = (String, String, Bool)
    data ActivityType = SportsHobbies | GroupsJobs

    data Options = Options {
        fileName :: String,
        respId :: String,
        verbose :: Bool
    }

    startVariable = "CBCL4_2"
    numberOfCbclVariables = 211
    --openEndedIndexes = []

    openEndedIndexes = [1, 3, 5, 12, 14, 16, 18, 25, 27, 29, 31, 35, 37, 39, 41, 51, 52, 59,
        61, 63, 65, 67, 69, 71, 72, 73, 83, 104, 116, 123, 137, 142, 145, 154, 159, 163, 168,
        171, 176, 178, 180, 188, 197, 203]

    toKeyString :: [Response] -> String
    toKeyString [] = ""
    toKeyString (Nothing:xs) = "{down}" ++ (toKeyString xs)
    toKeyString ((Just x):xs) = (show x) ++ (toKeyString xs)

    
    toResponse :: Char -> Response
    toResponse ' ' = Nothing
    toResponse c = Just ((read (c:[]))::Int)
    
    --toResponses :: String -> [Response]
    --toResponses [] = []
    --toResponses ";" = []
    --toResponses (';':xs) = Nothing:(toResponses xs)
    --toResponses (x:[]) = (toResponse x):[]
    --toResponses (x:';':';':xs) = (toResponse x):Nothing:(toResponses xs)
    --toResponses (x:';':xs) = (toResponse x):(toResponses xs)
    --toResponses (x:xs) = error $ "Error trying to parse: " ++ (x:xs)

    toResponse2 :: String -> Response
    toResponse2 "" = Nothing
    toResponse2 s = Just ((read s)::Int)

    parseActivities :: Int -> [Response] -> [Response]
    parseActivities fieldPerActivity ((Just noOfJobs):xs) = (Just noOfJobs):(take (fieldPerActivity*noOfJobs) xs)
    parseActivities _ (Nothing:xs) = [Nothing]
    parseActivities _ _ = error "Error parsing sports/jobs"
    
    createScript :: String -> String
    createScript keyString = "SetKeyDelay, 300\n^b::\n   Send, " ++ keyString ++ "\nReturn"


    findStartIndex' :: [(Int, String)] -> Maybe Int
    findStartIndex' [] = Nothing
    findStartIndex' ((i, s):xs)
        | s == startVariable = Just i
        -- | s == "CBCL1t1_1" = Just i
        | otherwise = findStartIndex' xs

    findStartIndex :: Header -> Maybe Int
    findStartIndex h = findStartIndex' (zip [0..] h)

    dropEmptyRows :: [CbclRow] -> [CbclRow]
    dropEmptyRows [] = []    
    dropEmptyRows (("":cs):rs) = dropEmptyRows rs
    dropEmptyRows (r:rs) = (r:dropEmptyRows rs)

    dropOpenEnded :: Int -> CbclRow -> CbclRow
    dropOpenEnded _ [] = []
    dropOpenEnded index (item:items) =        
        --let openEndedIndexes = [5, 7, 9, 18, 20, 22, 31, 33, 35, 41, 43, 45, 56, 63, 65, 67, 69, 73, 75] in
        --let openEndedIndexes = [9, 30, 42, 49, 63, 68, 71, 80, 85, 89, 94, 97, 102, 104, 106, 114, 123, 129] in
        if elem index openEndedIndexes then dropOpenEnded (index+1) items
        else (item:(dropOpenEnded (index+1) items))

    splitRow :: String -> [String]
    splitRow r = splitOn ";" r

    extractCbcl :: Int -> [String] -> CbclRow
    extractCbcl startIndex row = dropOpenEnded 0 (take numberOfCbclVariables (drop startIndex row))
        -- dropOpenEnded 0 (take numberOfCbclVariables (drop startIndex row))


    --prettyPrint :: Int -> [String] -> IO ()
    --prettyPrint _ [] = return ()
    --prettyPrint index (s:ss) = do
    --    putStrLn $ (show index) ++ ": " ++ s
    --    prettyPrint (index+1) ss

    --prettyPrint2 :: Int -> String -> String
    --prettyPrint2 i s = (show i) ++ ": " ++ s

    --createCbclRow :: Int -> [String] -> [CbclRow]
    --createCbclRows startIndex row = map (extractCbcl startIndex) (dropEmptyRows rows)

    --selectRespRow :: String -> [CbclRow] -> Either String CbclRow
    --selectRespRow respId = 

    --cbclResult :: Either String [CbclRow] -> IO ()
    --cbclResult (Left msg) = error $ "ERROR: " ++ msg
    --cbclResult (Right cbclRows) = mapM_ putStrLn (map prettyPrint (zip [0..] (cbclRows !! 0)))


    prettyPrint :: (Int, String, String) -> String
    prettyPrint (i, var, val) = (show i) ++ ":\t" ++ var ++ "    \t" ++ val

    printScript :: Either String [CbclRow] -> IO()
    printScript (Left msg) = error $ "ERROR: " ++ msg
    printScript (Right cbclRows) = 
        --putStrLn $ createScript (toKeyString (map toResponse2 (cbclRows !! 0)))
        putStrLn $ show $ toResponses (cbclRows !! 0)


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

        mapM_ putStrLn (map prettyPrint (zip3 [0..] headerCbcl respCbcl))

        --let a = toResponses respCbcl
        let a = toResponses respCbcl
        putStrLn $ show a
        --putStrLn $ show a

        let b = createScript (toKeyString a)

        putStrLn $ b
        --let responses = toResponses cbclRows
        --if (isSetVerbose)
            --then cbclResult cbclRows
            --else return ()
        --printScript cbclRows
        return ()


    dealWithEither :: Either String [CbclRow] -> IO [CbclRow]
    dealWithEither (Left msg) = do
        putStrLn msg
        exitFailure
    dealWithEither (Right a) = return a
        
    dealWithMaybe :: Maybe Int -> String -> IO Int
    dealWithMaybe Nothing msg = do
        putStrLn msg
        exitFailure
    dealWithMaybe (Just a) _ = return a


    parse :: Options -> Handle -> [[String]] -> IO ()
    parse options inHandle rows = do
        isEof <- hIsEOF inHandle
        if isEof
        then do
            process options rows
        else do
            row <- hGetLine inHandle
            parse options inHandle ((splitRow row):rows)



    beginParsing options = do
        fileHandle <- openFile (fileName options) ReadMode
        parse options fileHandle []
        hClose fileHandle

    main = do
        --putStrLn $ toKeyString (parseActivities 1 (toResponses ["3", "1", "2"]))
        args <- getArgs
        if ((length args) < 2)
            then putStrLn "USAGE: cbcl2ahk <input file> <respid> [--verbose]"
            else do
                let options = Options {
                    fileName = args !! 0,
                    respId = args !! 1,
                    verbose = (length args) >= 3 && (args !! 2 == "--verbose")
                }
                --let verbose = ((length args) >= 3 && (args !! 2 == "--verbose"))
                beginParsing options




    sumResponses :: [Response] -> Int
    sumResponses [] = 0
    sumResponses (Nothing:rs) = sumResponses rs
    sumResponses((Just x):rs) = x + (sumResponses rs)


    filterOutEmptyActivities :: ActivityType -> Int -> [Response] -> [Response]
    filterOutEmptyActivities _ 0 _ = []
    filterOutEmptyActivities SportsHobbies n@noOfActivities r@responsesForActivity = 
        (filterOutEmptyActivities SportsHobbies (n-1) r) ++ ((r !! (n-1)):(r !! (n+2)):[])
    filterOutEmptyActivities GroupsJobs n@noOfActivities r@responsesForActivity =
        (filterOutEmptyActivities GroupsJobs (n-1) r) ++ ((r !! (n-1)):[])


    --procAct 3 resp = (resp !! 5):(resp !! 8):(procAct 2 resp)
    --procAct 2 resp = (resp !! 4):(resp !! 7):(procAct 1 resp)
    --procAct 1 resp = (resp !! 3):(resp !! 6):(procAct 0 resp)

    processActivity :: ActivityType -> [Response] -> [Response]
    processActivity activityType activityResponses =
        let noOfActivities = (sumResponses (take 3 activityResponses)) in
            Just noOfActivities:(map rescaleActivity (filterOutEmptyActivities activityType noOfActivities (drop 3 activityResponses)))


    toResponses :: CbclRow -> [Response]
    toResponses row =
        let raw = map toResponse2 row in
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

--START_INDEX = "CBCL1t1_1"

--GRAVEYARD

            --let z = toResponse2 (cbclRows !! 50)
            --if (z == Left msg)
            --    then putStrLn msg
            --    else putStrLn $ createScript (toKeyString z)
            --toResponse2 
            --if (cbclRows == Left a) 
            --    then putStrLn a
            --    else
            --        let a = map prettyPrint (zip [0..] (head cbclRows))

                --extractCbcl dropEmptyRows reverse rows
            --parse inHandle ((extractCbcl (parseRow row)):rows)
            --prettyPrint 0 (head (reverse (dropEmptyRows rows)))
            --let a = map prettyPrint (zip [0..] (head (reverse (dropEmptyRows rows))))
            --mapM_ putStrLn a
            --prettyPrint 0 (dropOpenEnded 0 (head (reverse (dropEmptyRows rows))))

        --let a = toResponses "0;1;2;;0;1;2; ;0;1;2;0;1;2;3;1;2;3;4;1;2;3;1;2;3;1;4;2;3;2"
        --let b = parseActivities 2 (toResponses "3;;2;1;2;3;4;5;6;9;9;")
        --putStrLn $ toKeyString a
        --putStrLn $ toKeyString b
        --putStrLn $ createScript $ toKeyString a
