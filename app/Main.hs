module Main (main) where

import Statements
import Hasql.Connection (Connection, ConnectionError, acquire)

connection :: IO (Either ConnectionError Connection)
connection = acquire "postgresql://scott@localhost/Rel8Demo"

main :: IO ()
main = do
  conn <- connection
  case conn of
    Left err -> print err 
    Right c  -> do
      putStrLn "Original DB state"
      runQueries c
      putStrLn "Inserting author"
      runInsert c
      putStrLn "New DB state"
      runQueries c
      putStrLn "Updating author"
      runUpdate c
      putStrLn "New DB state"
      runQueries c
      putStrLn "Deleting author"
      runDelete c
      putStrLn "Final DB state"
      runAllQueries c
