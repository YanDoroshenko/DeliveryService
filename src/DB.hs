module DB where

import Database.CQL.IO as Client
import qualified System.Logger as Logger
import Database.CQL.Protocol

connect :: IO ClientState
connect = do
    g <- Logger.new Logger.defSettings
    c <- Client.init defSettings
    return c

cqlParams :: Tuple a => a -> QueryParams a
cqlParams p = QueryParams One False p Nothing Nothing Nothing (Just True)

cqlQuery :: (Tuple a, Tuple b, RunQ q) => q R a b -> a -> ClientState -> IO [b]
cqlQuery queryString p client  =
    runClient client $ query queryString $ cqlParams p

cqlInsert queryString p client =
  runClient client $ write queryString $ cqlParams p

close :: ClientState -> IO ()
close c = shutdown c
