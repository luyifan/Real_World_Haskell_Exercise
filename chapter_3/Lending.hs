-- file: Lending.hs
lend :: Int -> Int -> Maybe Int
lend amount balance = let reverse = 100 
			  newbalance = balance - amount 
		      in if balance < reverse
		   	then Nothing 
			else Just newbalance

lend2 amount balance = if balance < reverse 
			       then Nothing
			       else Just newbalance
		where reverse = 100
		      newbalance = balance - amount

lend3 amount balance 
	| amount <= 0   = Nothing
	| amount > reverse * 0.5  = Nothing
	| otherwise = Just newbalance
	where reverse = 100
       	      newbalance = balance - amount
