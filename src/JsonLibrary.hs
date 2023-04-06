module JsonLibrary(toJsonString, toJsonObject) where

import JsonObject (JsonValue(JString, JObject, JList, JNumber))
import JsonBuilder (writeJson, parseJson)

stringVal :: String
stringVal = "{\"test1\": \"test\",\"object1\": {\"list\":[\"abc\",[1,2,3],\"abc:[d]\"], \"object2\": {\"name\": \"value\", \"test3\": 2}}}"

jsonValue :: JsonValue
jsonValue =  (JObject [("test1",Just (JString "test")),
                            ("object1",Just (JObject [("list",Just (JList [Just (JString "abc"),
                            Just (JList [Just (JNumber 1),Just (JNumber 2),Just (JNumber 3)]),
                            Just (JString "abc:[d]")])),("object2",
                            Just (JObject [("name",Just (JString "value")),
                            ("test3",Just (JNumber 2))]))]))])

toJsonString :: IO ()
toJsonString = putStrLn (writeJson (Just jsonValue))

toJsonObject :: IO ()
toJsonObject = print (parseJson stringVal)