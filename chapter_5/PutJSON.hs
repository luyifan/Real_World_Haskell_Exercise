-- file: PutJSON.hs
module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ (getPairs o) ++ "}"
	where getPairs xlist 
       		| null xlist = ""
		| otherwise  = intercalate "," (map pairOne xlist)
	      pairOne (k,v) = k ++ ":" ++ renderJValue v
renderJValue (JArray o) = "[" ++ ( intercalate "," (map renderJValue o) ) ++ "]"

putJValue :: JValue -> IO()
putJValue v = putStrLn( renderJValue v )
