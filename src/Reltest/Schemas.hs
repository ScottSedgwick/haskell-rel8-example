module Reltest.Schemas
  ( AuthorId(..)
  , Author(..)
  , Project(..)
  , authorSchema
  , projectSchema
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (Column, DBEq, DBType, Name, Rel8able, Result, TableSchema(..))

-- Newtype to wrap an author_id so we cannot accidentally mix it up with a different Int64 value.
newtype AuthorId = AuthorId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show)

-- Schema for a Table with the following structure:
--
-- CREATE TABLE IF NOT EXISTS public.author
-- (
--     author_id integer NOT NULL,
--     name text COLLATE pg_catalog."default" NOT NULL,
--     url text COLLATE pg_catalog."default"
-- )

data Author f = Author
  { authorId   :: Column f AuthorId
  , authorName :: Column f Text
  , authorUrl  :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance f ~ Result => Show (Author f)

authorSchema :: TableSchema (Author Name)
authorSchema = TableSchema
  { name = "author"
  , schema = Nothing
  , columns = Author
      { authorId = "author_id"
      , authorName = "name"
      , authorUrl = "url"
      }
  }

-- Schema for a Table with the following structure:
--
-- CREATE TABLE IF NOT EXISTS public.project
-- (
--     author_id integer NOT NULL,
--     name text COLLATE pg_catalog."default" NOT NULL
-- )

data Project f = Project
  { projectAuthorId :: Column f AuthorId
  , projectName     :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance f ~ Result => Show (Project f)

projectSchema :: TableSchema (Project Name)
projectSchema = TableSchema
  { name = "project"
  , schema = Nothing
  , columns = Project
      { projectAuthorId = "author_id"
      , projectName = "name"
      }
  }