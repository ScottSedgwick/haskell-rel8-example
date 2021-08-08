module Reltest.Queries
  ( qryAuthor
  , qryProject
  , qryAuthorProject
  , qryAuthorProjectMaybe
  , qryAuthorProjectList
  ) where

import Data.Text (Text)
import Rel8 (Expr, ListTable, MaybeTable, Query, (==.), each, filter, many, optional)
import Reltest.Schemas

-- Various examples of queries.

-- Return every record in the author table
qryAuthor :: Query (Author Expr)
qryAuthor = each authorSchema

-- Returns every record in the project table
qryProject :: Query (Project Expr)
qryProject = each projectSchema

-- Returns author names, with their associated projects. Authors without projects are left out.
qryAuthorProject :: Query (Expr Text, Expr Text)
qryAuthorProject = do
  a <- each authorSchema
  p <- projectsForAuthor a
  return (authorName a, projectName p)

-- Returns every author name, with their associated projects, or Nothing if they have none.
qryAuthorProjectMaybe :: Query (Expr Text, MaybeTable Expr (Expr Text))
qryAuthorProjectMaybe = do
  a <- each authorSchema
  p <- optional $ projectName <$> projectsForAuthor a
  return (authorName a, p)

-- Returns every author name, with a list of their associated projects
qryAuthorProjectList :: Query (Expr Text, ListTable Expr (Expr Text))
qryAuthorProjectList = do
  a <- each authorSchema
  p <- many $ projectName <$> projectsForAuthor a
  return (authorName a, p)

-- Filter helper for collating projects with authors
projectsForAuthor :: Author Expr -> Query (Project Expr)
projectsForAuthor a = each projectSchema >>= Rel8.filter (\p -> authorId a ==. projectAuthorId p)