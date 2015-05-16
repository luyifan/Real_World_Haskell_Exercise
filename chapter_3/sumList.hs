-- file: sumList.hs
sumList [] = 0
sumList (x:xlist)= x + sumList xlist
