-- funcrecs2.hs
data FuncRec =
	FuncRec { name :: String , 
	 	  calc :: Int -> Int ,
		  namedCalc :: Int -> (String,Int)}

mkFunnRec :: String -> (Int -> Int) -> FuncRec 
mkFunnRec name calcfunc = 
	FuncRec { name = name ,
	          calc = calcfunc ,
		  namedCalc = \x -> ( name , calcfunc x) }
plus5 = mkFunnRec "plus5" (+ 5)
always0 = mkFunnRec "always0" ( \_ -> 0 ) 
