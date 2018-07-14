module HashStore
       ( hashStore
       ) where

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Data.ByteString.Char8 (ByteString)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BS

-- QUESTION: does it make sense to create `newtype Hash = Hash ByteString`?
-- | Perform 'ByteString' hashing.
hashContent :: ByteString -> ByteString
hashContent = hash 64 mempty
{-# INLINE hashContent #-}

-- | Build the new 'FilePath' adding the hash of the provided content.
hashFile :: ByteString -> FilePath -> FilePath
hashFile content file = BS.unpack (hashContent content) ++ "-" ++ file
{-# INLINE hashFile #-}

{- | @'hashStore' storePath action (name, content)@ computes @hash@ of @content@
and performs @action@ if file @storePath '</>' (hash ++ "-" ++ name)@ doesn't
exist, writing new content in this file.

The property is that for any item @(name, hash)@, we get a valid filepath by
@storePath '</>' (hash ++ "-" ++ name)@.
-}
hashStore :: FilePath
             -- ^ Directory to store file contents with new hash
          -> (ByteString -> IO ByteString)
             -- ^ Action to be performed if hash is different
          -> (String, ByteString)
             -- ^ File name
          -> IO (String, ByteString)
             -- ^ Hash of given 'ByteString' and new 'ByteString' if old hash and new one are different
hashStore storePath action (name, content) = do
    createDirectoryIfMissing True storePath
    let hashName = hashFile content name
    let storeName = storePath </> hashName

    isUpToDate <- doesFileExist storeName
    if isUpToDate then
        return (hashName, content)
    else do
        newContent <- action content
        BS.writeFile storeName newContent
        return (hashName, newContent)
