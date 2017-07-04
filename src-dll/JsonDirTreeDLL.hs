-- Windows:
-- 64 bit:
-- stack build --arch x86_64
-- stack exec --arch x86_64 -- ghc -shared -fPIC src-dll/JsonDirTreeDLL.hs StartEnd.c -o JsonDirTree.dll
-- 32 bit:
-- stack clean
-- stack setup --arch i386
-- stack build --arch i386
-- stack exec --arch i386 -- ghc -shared -fPIC src-dll/JsonDirTreeDLL.hs StartEnd.c -o JsonDirTree.dll
-- Linux:
-- stack build
-- stack exec -- ghc -shared -fPIC -dynamic -lHSrts-ghc8.0.2 src-dll/JsonDirTreeDLL.hs StartEnd.c -o JsonDirTree.so

{-# LANGUAGE ForeignFunctionInterface #-}

module JsonDirTreeDLL
  where
import           Data.ByteString.Lazy.Internal (unpackChars)
import           Foreign
import           Foreign.C
import           JsonDirTree                   (dirToJSONtree, drawDir)

foreign export ccall dirToJSON :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSON :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSON depth dir result = do
  depth <- peek depth
  let d = fromIntegral depth :: Int
  dir <- (>>=) (peek dir) peekCString
  json <- dirToJSONtree
            (if d<0 then Nothing else Just d)
              dir
  jsonC <- newCString $ unpackChars json
  poke result jsonC

foreign export ccall dirToTree :: Ptr CInt -> Ptr CString -> Ptr CInt ->
                                                            Ptr CString -> IO ()
dirToTree :: Ptr CInt -> Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
dirToTree depth dir vertical result = do
  depth <- peek depth
  let d = fromIntegral depth :: Int
  dir <- (>>=) (peek dir) peekCString
  vertical <- peek vertical
  let v = vertical > 0
  tree <- drawDir
            (if d<0 then Nothing else Just d)
              dir v
  treeC <- newCString tree
  poke result treeC
