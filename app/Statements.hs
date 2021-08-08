module Statements 
  ( printErr
  , runQueries
  , runAllQueries
  , runInsert
  , runDelete
  , runUpdate
  ) where


import Reltest 
import Rel8
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement)

runQueries :: Connection -> IO()
runQueries conn = do
  runqry "Authors:" qryAuthor conn
  putStrLn "Finished"

runAllQueries :: Connection -> IO()
runAllQueries conn = do
  runqry "Projects:" qryProject conn
  runqry "Authors:" qryAuthor conn
  runqry "Authors with projects" qryAuthorProject conn
  runqry "Authors with project list" qryAuthorProjectList conn
  putStrLn "Finished"

runqry :: (Serializable exprs (FromExprs exprs), Show (FromExprs exprs)) => String -> Query exprs -> Connection -> IO ()
runqry cap qry conn = do
  putStrLn cap
  res <- runStatement () (select qry) conn
  either printErr (mapM_ print) res

runStatement :: params -> Statement params result -> Connection -> IO (Either QueryError result)
runStatement params stmnt conn = run (statement params stmnt) conn

runInsert :: Connection -> IO()
runInsert conn = do
  let ins = Insert { into = authorSchema
                   , rows = values [ lit Author { authorId = AuthorId 4, authorName = "Scott Sedgwick", authorUrl = Nothing } ]
                   , onConflict = Abort 
                   , returning = NumberOfRowsAffected
                   }
  res <- runStatement () (insert ins) conn
  either printErr (printRes "Records inserted: ") res
  putStrLn "Finished"

runUpdate :: Connection -> IO()
runUpdate conn = do
  let upd = Update { target = authorSchema
                   , from = qryAuthor
                   , set = \_ row -> row { authorUrl = litExpr $ Just "https://scott.sedgwick.name" }
                   , updateWhere = \_ row -> authorId row ==. litExpr (AuthorId 4)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (update upd)) conn
  either printErr (printRes "Records updated: ") res
  putStrLn "Finished"

runDelete :: Connection -> IO()
runDelete conn = do
  let del = Delete { from = authorSchema 
                   , using = qryAuthor
                   , deleteWhere = \_ row -> authorId row ==. litExpr (AuthorId 4)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (delete del)) conn
  either printErr (printRes "Records deleted: ") res
  putStrLn "Finished"

printErr :: Show a => a -> IO()
printErr = printRes "Error: "

printRes :: Show a => String -> a -> IO()
printRes cap val = putStrLn $ cap <> show val