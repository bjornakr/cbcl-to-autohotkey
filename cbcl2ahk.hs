type Response = Maybe Int

toKeyString :: [Response] -> String
toKeyString [] = ""
toKeyString (Nothing:xs) = "{down}" ++ (toKeyString xs)
toKeyString ((Just x):xs) = (show x) ++ (toKeyString xs)

createScript :: String -> String
createScript keyString = "SetKeyDelay, 300\n^b::\n   Send, " ++ keyString ++ "\nReturn"

toResponse :: Char -> Response
toResponse ' ' = Nothing
toResponse c = Just ((read (c:[]))::Int)


toResponses :: String -> [Response]
toResponses [] = []
toResponses ";" = []
toResponses (x:[]) = (toResponse x):[]
toResponses (x:';':xs) = (toResponse x):(toResponses xs)

parseSports :: Int -> String -> [Response]
parseSports 0 xs = []
parseSports n (x:';':y:';':xs) = (toResponses (x:';':y:';':[])) ++ (parseSports (n-1) xs)

parseJob :: Int -> String -> [Response]
parseJob 0 xs = []
parseJob n (x:';':xs) = (toResponse x):(parseJob (n-1) xs)

parseActivities :: (Int -> String -> [Response]) -> String -> [Response]
parseActivities parse (noOfJobs:';':xs) = (toResponse noOfJobs):(parse ((read (noOfJobs:[]))::Int) xs)
parseActivities _ _ = error "Error parsing sports/jobs"


main = do
    let a = toResponses "0;1;2; ;0;1;2; ;0;1;2"
    let b = parseActivities parseJob "3;1;2;3;4;5;6;9;9;"
    putStrLn $ toKeyString b
