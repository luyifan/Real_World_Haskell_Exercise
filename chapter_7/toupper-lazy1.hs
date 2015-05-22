-- file: toupper-lazy1.hs
import System.IO
import Data.Char(toUpper)

main::IO()
main = do
	inh <- openFile "input.txt" ReadMode
	outh <- openFile "output.txt" WriteMode
	content <- hGetContents inh
	let changed_content = processData content 
        hPutStr outh changed_content 
	hClose inh 
	hClose outh

processData :: String -> String
processData = map toUpper 
