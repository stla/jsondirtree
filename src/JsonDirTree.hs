{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module JsonDirTree
  where
import           Data.Aeson                    (FromJSON, ToJSON, encode,
                                                toEncoding, toJSON)
import           Data.Aeson.Types              (defaultOptions,
                                                genericToEncoding,
                                                genericToJSON,
                                                omitNothingFields)
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Tree                     (Tree, drawTree, unfoldTree)
import           Data.Tree.Pretty              (drawVerticalTree)
import           GHC.Generics
import           System.Directory
import           System.FilePath               (splitPath, takeFileName)

data DirTree = DirTree {
                name     :: FilePath,
                _type    :: Text,
                size     :: Maybe Integer,
                children :: Maybe [DirTree]
              }
              deriving (Show, Generic)

-- toEncoding useless; or more efficient ?
instance ToJSON DirTree where
  toEncoding = genericToEncoding defaultOptions {omitNothingFields = True}
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

dirtreeExample :: DirTree
dirtreeExample = DirTree {
  name = "x",
  _type = "folder",
  size = Nothing,
  children = Just [
  DirTree {
    name = "y1.zzz",
    _type = "folder",
    size = Nothing,
    children = Just [
    DirTree {
      name = "z.txt",
      _type = "file",
      size = Just 100,
      children = Nothing
    }
    ]
  },
  DirTree {
    name = "y2.txt",
    _type = "file",
    size = Just 99,
    children = Nothing
  }
  ]
}

-- getPermissions ? Optional ? errors to handle...
dirToDirTree :: Maybe Int -> FilePath -> IO DirTree
dirToDirTree depth dir = do
  isDir <- doesDirectoryExist dir
  isSymbolic <- pathIsSymbolicLink dir
  let isReallyDir = isDir && (not isSymbolic)
  if isReallyDir
    then
      do
        contents <- listDirectory dir
        if contents /= []
          then
            do
              let newdepth = maybe Nothing
                               (\x -> if x>0 then Just (x-1) else Nothing) depth
              dirtree <- mapM (dirToDirTree newdepth)
                           $ map (\x -> dir ++ '/':x) contents
              return DirTree {
                name = last $ splitPath dir,
                _type = "folder",
                size = Nothing,
                children = if depth == Just 0 then Nothing else (Just dirtree)
              }
          else
            return DirTree {
              name = last $ splitPath dir,
              _type = "folder",
              size = Nothing,
              children = Nothing
            }
    else if isSymbolic
      then
        return DirTree {
          name = takeFileName dir,
          _type = "link",
          size = Nothing,
          children = Nothing
        }
      else
        do
          size <- getFileSize dir -- what happens if symbolic ?
          return DirTree {
            name = takeFileName dir,
            _type = "file",
            size = Just size,
            children = Nothing
          }

dirToJSONtree :: Maybe Int -> FilePath -> IO ByteString
dirToJSONtree depth dir = do
  dirtree <- dirToDirTree depth dir
  return $ encode dirtree

-- ** ASCII tree ** --
dirTreeToTree :: DirTree -> Tree String
dirTreeToTree dirtree = unfoldTree f dirtree
  where f dt = (name dt, fromMaybe [] (children dt))

drawDirTree :: DirTree -> String
drawDirTree = drawTree . dirTreeToTree

drawVerticalDirTree :: DirTree -> String
drawVerticalDirTree = drawVerticalTree . dirTreeToTree

drawDir :: Maybe Int -> FilePath -> Bool -> IO String
drawDir depth dir vertical = do
  dirtree <- dirToDirTree depth dir
  if vertical
    then
      return $ drawVerticalDirTree dirtree
    else
      return $ drawDirTree dirtree
