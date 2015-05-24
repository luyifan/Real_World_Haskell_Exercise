-- file: ControlledVisit.hs
data Info = {
	infoFile :: Maybe  FilePath ,
	infoPerms :: Maybe Permissions ,
	infoSize :: Maybe  Integer ,
	infoTime :: Maybe  UTCTime ,
} deriving ( Eq , Ord , Show ) 

