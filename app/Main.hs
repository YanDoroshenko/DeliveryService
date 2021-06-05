module Main where

import Lib

import Data.Int
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text
import Data.Text.Lazy
import Data.Functor.Identity
import Database.CQL.IO as Client
import qualified System.Logger as Logger
import Database.CQL.Protocol

connect:: IO ClientState
connect = do
    g <- Logger.new Logger.defSettings
    c <- Client.init defSettings
    return c

cqlParams :: Tuple a => a -> QueryParams a
cqlParams p = QueryParams One False p Nothing Nothing Nothing (Just True)

cqlQuery queryString p client  =
    runClient client $ query queryString $ cqlParams p

getCustomerIds db =
    cqlQuery ((QueryString $ fromStrict $ Data.Text.pack "SELECT id, name from user.users;") :: QueryString R() (Int32, Data.Text.Text)) () db

main :: IO ()
main =
  do
    c <- liftIO connect
    ids <- getCustomerIds c
    putStrLn $ Prelude.foldr (\x y -> (show $ fst x) ++ " " ++ (Data.Text.unpack (snd x)) ++ "\n" ++ y) "" ids
