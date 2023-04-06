module JsonLibrary(toJsonString, toJsonObject) where

import JsonObject (JsonValue(JString, JObject, JList, JNumber))
import JsonBuilder (writeJson, parseJson)

stringVal :: String
stringVal = "{\"name\":\"Desiree\",\"favoriteColors\":[\"orange\",\"yellow\",\"red\"], \"lastName\":\"Acosta\",\"age\":20}"

jsonValue :: JsonValue
jsonValue = JObject[("daysOfWeek", Just (JList [Just (JString "M"), Just (JString "T"), Just (JString "T")])),
                    ("name", Just (JString "John")), ("lastName", Just (JString "Doe")), ("number", Just (JNumber 1))]

toJsonString :: IO ()
toJsonString = putStrLn (writeJson (Just jsonValue))

toJsonObject :: IO ()
toJsonObject = print (parseJson stringVal)