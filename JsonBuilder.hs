module JsonBuilder(parseJson, writeJson) where

import JsonObject (JsonValue(..))
import Data.List (dropWhileEnd, intercalate)
import Data.Char (isDigit, isSpace, toLower)
import Data.Maybe (isJust)

writeJson :: Maybe JsonValue -> String
writeJson (Just (JString s)) = "\"" ++ s ++ "\""
writeJson (Just (JBool b)) = if b then "true" else "false"
writeJson (Just (JNumber n)) = show n
writeJson (Just (JList l)) = "[" ++ intercalate ", " (map writeJson l) ++ "]"
writeJson (Just (JObject o)) = "{" ++ createFromObject o ++ "}"
writeJson (Just JNil) = ""
writeJson _ = []

parseJson :: String -> Maybe JsonValue
parseJson [] = Nothing
parseJson s
        | isOpenCharObject s && isClosedCharObject s =
                Just (JObject (map buildTuple (splitAcc ',' (extractObject (Just s) '{') [])))
        | otherwise = Nothing

parseString :: String -> Maybe JsonValue
parseString "" = Nothing
parseString ('\"' : xs) = Just (JString (takeWhile (/= '\"') xs))
parseString _ = Nothing

parseBool :: String -> Maybe JsonValue
parseBool ('f' : 'a' : 'l' : 's' : 'e' : _) = Just (JBool False)
parseBool ('t' : 'r' : 'u' : 'e' : _) = Just (JBool True)
parseBool _ = Nothing

parseNumber :: String -> Maybe JsonValue
parseNumber n = if isDigit' n then Just (JNumber (read n)) else Nothing

parseList :: String -> Maybe JsonValue
parseList [] = Nothing
parseList xs = Just (JList (parseList' xs))

parseList' :: String -> [Maybe JsonValue]
parseList' [] = [Just JNil]
parseList' xs = if isOpenCharList xs
        then map getJValue (splitList ',' (extractObject (Just xs) '[') [])
        else [Nothing]

showNode :: (String, Maybe JsonValue) -> String
showNode (name,value) = "\"" ++ trim name ++ "\": " ++ writeJson value

createFromObject :: [(String, Maybe JsonValue)] -> String
createFromObject [] = ""
createFromObject xs = intercalate ", " (map showNode xs)

buildTuple :: String -> (String, Maybe JsonValue)
buildTuple [] = ("", Just (JString ""))
buildTuple s = let x:y:_ = splitObject ':' (Just s) [] in (trim x, getJValue (trim y))

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' [x] = isDigit x
isDigit' ('-':xs) = isDigit' xs
isDigit' (x:xs) = isDigit x && isDigit' xs

isOpenCharList :: String -> Bool
isOpenCharList [] = False
isOpenCharList (x:xs)
        | x == '[' = True
        | otherwise = isOpenCharList xs

isOpenCharObject :: String -> Bool
isOpenCharObject [] = False
isOpenCharObject (x:xs)
        | x == '{' = True
        | otherwise = isOpenCharObject xs

isClosedCharObject :: String -> Bool
isClosedCharObject [] = False
isClosedCharObject (x:xs)
        | x == '}' = True
        | otherwise = isClosedCharObject xs

getJValue :: String -> Maybe JsonValue
getJValue [] = Nothing
getJValue str@(x:_)
        | x == '\"' = parseString str
        | x == '[' = parseList str
        | x == '{' = parseJson str
        | isDigit' str = parseNumber str
        | otherwise = if Data.Maybe.isJust (parseBool (map toLower str))
                then parseBool (map toLower str)
                else Nothing

allQuotesClosed :: Int -> String -> Bool
allQuotesClosed n [] = even n
allQuotesClosed n (x:xs) = if x == '\"' then allQuotesClosed (n + 1) xs else allQuotesClosed n xs

allListsClosed :: Int -> Int -> Char -> Char -> String -> Bool
allListsClosed a b _ _ [] = a == b
allListsClosed a b c d (x:xs)
                | x == c = allListsClosed (a + 1) b c d xs
                | x == d = allListsClosed a (b + 1) c d xs
                | otherwise = allListsClosed a b c d xs

splitList :: Char -> Maybe String -> String -> [String]
splitList _ Nothing [] = []
splitList _ Nothing (_:_) = []
splitList _ (Just []) [] = []
splitList _ (Just []) ys = [reverse ys]
splitList c (Just(x:xs)) ys
                | x == c = if allListsClosed 0 0 '[' ']' ys
                        then reverse ys : splitList c (Just xs) []
                        else splitList c (Just xs) (x : ys)
                | otherwise = splitList c (Just xs) (x : ys)

splitAcc :: Char -> Maybe String -> String -> [String]
splitAcc _ Nothing [] = []
splitAcc _ Nothing (_:_) = []
splitAcc _ (Just []) [] = []
splitAcc _ (Just []) ys = [reverse ys]
splitAcc c (Just(x:xs)) ys
                | x == c = if allListsClosed 0 0 '[' ']' ys && allListsClosed 0 0 '{' '}' ys && allQuotesClosed 0 ys
                        then reverse ys : splitAcc c (Just xs) []
                        else splitAcc c (Just xs) (x:ys)
                | otherwise = splitAcc c (Just xs) (x : ys)

splitObject :: Char -> Maybe String -> String -> [String]
splitObject _ Nothing [] = []
splitObject _ Nothing (_:_) = []
splitObject _ (Just []) [] = []
splitObject _ (Just []) ys = [reverse ys]
splitObject c (Just(x:xs)) ys
                | x == c = if allListsClosed 0 0 '{' '}' ys && allListsClosed 0 0 '[' ']' ys
                        then reverse ys : splitObject c (Just xs) []
                        else splitAcc c (Just xs) (x:ys)
                | otherwise = splitObject c (Just xs) (x : ys)

extractObject :: Maybe String -> Char -> Maybe String
extractObject Nothing _ = Nothing
extractObject (Just xs) i = Just (removeLast (removeFirst i (trim xs)))

removeFirst :: Char -> String -> String
removeFirst _ [] = []
removeFirst c (x:xs) = if x == c then xs else x:xs

removeLast :: String -> String
removeLast [] = []
removeLast [_] = []
removeLast (h:t) = h : removeLast t

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
