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
toResponses (';':xs) = Nothing:(toResponses xs)
toResponses (x:[]) = (toResponse x):[]
toResponses (x:';':xs) = (toResponse x):(toResponses xs)
toResponses (x:xs) = error $ "Error trying to parse: " ++ (x:xs)

parseActivities :: Int -> [Response] -> [Response]
parseActivities n ((Just noOfJobs):xs) = (Just noOfJobs):(take (n*noOfJobs) xs)
parseActivities n (Nothing:xs) = [Nothing]
parseActivities _ _ = error "Error parsing sports/jobs"

main = do
    let a = toResponses "0;1;2;;0;1;2; ;0;1;2;0;1;2;3;1;2;3;4;1;2;3;1;2;3;1;4;2;3;2"
    let b = parseActivities 2 (toResponses "3;;2;1;2;3;4;5;6;9;9;")
    putStrLn $ toKeyString a
    putStrLn $ toKeyString b
    putStrLn $ createScript $ toKeyString a
