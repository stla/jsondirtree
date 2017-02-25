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
import qualified Data.MessagePack              as MP
-- import Data.MessagePack.Derive
import           Data.Text                     (Text)
import qualified Data.Text                     as T
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

-- toEncoding useless; more efficient ?
instance ToJSON DirTree where
  toEncoding = genericToEncoding defaultOptions {omitNothingFields = True}
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- -- for MessagePack
-- $(deriveObject True ''DirTree)
-- $(derivePack True ''DirTree)
-- -- $(deriveUnpack True ''Document)


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
  isdir <- doesDirectoryExist dir
  isSymbolic <- pathIsSymbolicLink dir
  if isdir
    then
      do
        let dirtype = if isSymbolic then "symbolic link" else "folder"
        contents <- listDirectory dir
        if contents /= []
          then
            do
              let newdepth = maybe Nothing (\x -> if x>0 then Just (x-1) else Nothing) depth
              dirtree <- mapM (dirToDirTree newdepth)
                           $ map (\x -> dir ++ '/':x) contents
              return DirTree {
                name = last $ splitPath dir,
                _type = dirtype,
                size = Nothing,
                children = if depth == Just 0 then Nothing else (Just dirtree)
              }
          else
            return DirTree {
              name = last $ splitPath dir,
              _type = dirtype,
              size = Nothing,
              children = Nothing
            }
    else
      do
        let dirtype = if isSymbolic then "symbolic link" else "file"
        size <- getFileSize dir -- what happens if symbolic ?
        return DirTree {
          name = takeFileName dir,
          _type = dirtype,
          size = Just size,
          children = Nothing
        }

dirToJSONtree :: Maybe Int -> FilePath -> IO ByteString
dirToJSONtree depth dir = do
  dirtree <- dirToDirTree depth dir
  return $ encode dirtree

-- string ou bytestring ça ne pack rien du tout...
dirToMPtree :: Maybe Int -> FilePath -> IO ByteString
dirToMPtree depth dir = do
  jsontree <- dirToJSONtree depth dir
  return $ MP.pack jsontree
