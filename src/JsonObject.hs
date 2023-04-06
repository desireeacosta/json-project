module JsonObject(JsonValue(..)) where

data JsonValue = JString String 
            | JBool Bool 
            | JObject [(String, Maybe JsonValue)]
            | JNumber Int 
            | JList [Maybe JsonValue]
            | JNil
            deriving Show