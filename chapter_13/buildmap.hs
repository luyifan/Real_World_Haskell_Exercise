import qualified Data.Map as Map 
-- Function to generate a Map that represents an association list as map 

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")] 
-- Create a map representation of 'al' by Map.fromList
mapFromAl = Map.fromList al 

-- Create a map representation of 'al' by fold
mapFold = foldl ( \map (k,v) -> Map.insert k v map ) Map.empty al

