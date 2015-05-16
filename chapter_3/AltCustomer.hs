-- file: AltCustomer.hs
type CustomerID = Int
type Address = [String]
type Name = String 
data Customer = Customer  CustomerID Name Address
	      deriving (Show)
customerID :: Customer -> CustomerID
customerID (Customer id _ _ ) = id

customerName :: Customer -> Name 
customerName (Customer _ name _ ) = name 

customerAddress :: Customer -> Address
customerAddress (Customer _ _ address ) = address
