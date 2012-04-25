module Paths_haskeme where

getDataFileName :: FilePath -> IO FilePath
getDataFileName str = return $ "../lib/" ++ str