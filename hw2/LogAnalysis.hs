
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


-- useful to keep in mind that we are more closely programming what a thing IS rather than HOW it is made

parseMessage :: String -> LogMessage
parseMessage = parseMessageWordList . stringToList  
    where 
    parseMessageWordList :: [String] -> LogMessage
    parseMessageWordList ("I":stamp:message) = LogMessage Info (toInt stamp) (listToString message)
    parseMessageWordList ("W":stamp:message) = LogMessage Warning (toInt stamp) (listToString message) 
    parseMessageWordList ("E":level:stamp:message) = LogMessage (Error (toInt level)) (toInt stamp) (listToString message)
    parseMessageWordList _ = Unknown "Unkown message type"
    
    toInt str = read str :: Int -- parse to int
    listToString = unwords
    stringToList = words

parse :: String -> [LogMessage]
parse = parseLines
    where parseLines = map parseMessage . linesToList
          linesToList = lines



insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown _) tree = tree
insert toInsert Leaf = Node Leaf toInsert Leaf
insert toInsert@(LogMessage _ insertStamp _) (Node left subRoot@(LogMessage _ rootStamp _) right) =
    if insertStamp < rootStamp  -- since bound to the stamp values
    then Node (insert toInsert left) subRoot right
    else Node left subRoot (insert toInsert right)


build :: [LogMessage] -> MessageTree 
build [] = Leaf
build (x:xs) = insert x (build xs)  --recursivly inserting next LogMessage

inOrder :: MessageTree -> [LogMessage]  -- want to traverse from rightmost to leftmost
inOrder Leaf = []
inOrder (Node _ (Unknown _) _) = []
inOrder (Node left subRoot right) = (inOrder left) ++ [subRoot] ++ (inOrder right)



whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  (map getMessage) . filterSevere  
    where getMessage (LogMessage _ _ message) = message
          getMessage _ = []
          filterSevere = (filter isSevereError) . orderMessages
              where isSevereError (LogMessage (Error level) _ _) = level > 50
                    isSevereError _ = False
                    orderMessages = inOrder . build

main :: IO()
main = return()        
