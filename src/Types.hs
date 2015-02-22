module Types
    ( Option(..)
    ) where

data Option = Option
             { source         :: FilePath
             , showLineNumber :: Bool
             , debug          :: Bool
             } deriving (Eq,Ord,Show)
