-- interact 
main = interact wordCount
	where wordCount input = show ( length (lines input) ) 
-- lines change input to list split by \n
-- length get the length of list
-- show change integer to string



