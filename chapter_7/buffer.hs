-- file: buffer.hs
import GHC.IO.Handle
import GHC.IO.Handle.FD
main = do 
     hGetBuffering stdin
     --runghc buffer.hs
     --LineBuffering
     hSetBuffering stdin (BlockBuffering Nothing)
     hGetBuffering stdin
     hSetBuffering stdin (BlockBuffering $ Just 4096)
     hGetBuffering stdin 
