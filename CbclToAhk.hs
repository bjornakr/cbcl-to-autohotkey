    import Data.List.Split
    import System.IO
    import System.Environment

    type Response = Maybe Int
    type CbclRow = [String]
    type Header = [String]

    toKeyString :: [Response] -> String
    toKeyString [] = ""
    toKeyString (Nothing:xs) = "{down}" ++ (toKeyString xs)
    toKeyString ((Just x):xs) = (show x) ++ (toKeyString xs)

    
    toResponse :: Char -> Response
    toResponse ' ' = Nothing
    toResponse c = Just ((read (c:[]))::Int)
    
    toResponses :: String -> [Response]
    toResponses [] = []
    toResponses ";" = []
    toResponses (';':xs) = Nothing:(toResponses xs)
    toResponses (x:[]) = (toResponse x):[]
    toResponses (x:';':';':xs) = (toResponse x):Nothing:(toResponses xs)
    toResponses (x:';':xs) = (toResponse x):(toResponses xs)
    toResponses (x:xs) = error $ "Error trying to parse: " ++ (x:xs)

    toResponse2 :: String -> Response
    toResponse2 "" = Nothing
    toResponse2 s = Just ((read s)::Int)

    parseActivities :: Int -> [Response] -> [Response]
    parseActivities n ((Just noOfJobs):xs) = (Just noOfJobs):(take (n*noOfJobs) xs)
    parseActivities n (Nothing:xs) = [Nothing]
    parseActivities _ _ = error "Error parsing sports/jobs"
    
    createScript :: String -> String
    createScript keyString = "SetKeyDelay, 300\n^b::\n   Send, " ++ keyString ++ "\nReturn"


    findStartIndex' :: [(Int, String)] -> Maybe Int
    findStartIndex' [] = Nothing
    findStartIndex' ((i, s):xs)
        | s == "CBCL1t1_1" = Just i
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
        let openEndedIndexes = [9, 30, 42, 49, 63, 68, 71, 80, 85, 89, 94, 97, 102, 104, 106, 114, 123, 129] in
        if elem index openEndedIndexes then dropOpenEnded (index+1) items
        else (item:(dropOpenEnded (index+1) items))

    splitRow :: String -> [String]
    splitRow r = splitOn ";" r

    extractCbcl :: Int -> [String] -> CbclRow
    extractCbcl startIndex row = dropOpenEnded 0 (take 137 (drop startIndex row))


    --prettyPrint :: Int -> [String] -> IO ()
    --prettyPrint _ [] = return ()
    --prettyPrint index (s:ss) = do
    --    putStrLn $ (show index) ++ ": " ++ s
    --    prettyPrint (index+1) ss

    --prettyPrint2 :: Int -> String -> String
    --prettyPrint2 i s = (show i) ++ ": " ++ s

    createCbclRows :: Maybe Int -> [[String]] -> Either String [CbclRow]
    createCbclRows Nothing _ = Left "Could not find CBCL items (starting with CBCL1t1_1)"
    createCbclRows (Just startIndex) rows = 
        Right $ map (extractCbcl startIndex) (dropEmptyRows rows)

    --selectRespRow :: String -> [CbclRow] -> Either String CbclRow
    --selectRespRow respId = 

    cbclResult :: Either String [CbclRow] -> IO ()
    cbclResult (Left a) = putStrLn a
    cbclResult (Right []) = putStrLn "Couldn't find row with RespId."
    cbclResult (Right cbclRows) = mapM_ putStrLn (map prettyPrint (zip [0..] (cbclRows !! 0)))


    prettyPrint :: (Int, String) -> String
    prettyPrint (i, s) = (show i) ++ ": " ++ s

    printScript :: Either String [CbclRow] -> IO()
    printScript (Left msg) = putStrLn msg
    printScript (Right cbclRows) = 
        putStrLn $ createScript (toKeyString (map toResponse2 (cbclRows !! 0)))

    parse :: String -> Handle -> [[String]] -> IO ()
    parse respId inHandle rows = do
        isEof <- hIsEOF inHandle
        if isEof
        then do
            let onlyRespRow = filter (\x -> (x !! 1) == respId) rows
            let orderedRows = reverse rows
            let cbclRows = createCbclRows (findStartIndex (head orderedRows)) onlyRespRow
            cbclResult cbclRows

            printScript cbclRows
            return ()
        else do
            row <- hGetLine inHandle
            parse respId inHandle ((splitRow row):rows)



    beginParsing respId = do
        fileHandle <- openFile "Book2.csv" ReadMode
        parse respId fileHandle []
        hClose fileHandle

    main = do
        args <- getArgs
        if ((length args) < 1)
            then putStrLn "USAGE: cbcl2ahk <file> <respid>"
            else beginParsing (args !! 0)



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
