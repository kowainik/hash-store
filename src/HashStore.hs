module HashStore
       ( hashStore
       , hashStoreWithContent
       ) where

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BS

-- | Blake2b hash encoded in base16.
newtype Hash = Hash { unHash :: ByteString }
    deriving (Eq)

-- | Perform 'ByteString' hashing.
hashContent :: ByteString -> Hash
hashContent = Hash . encode . hash 64 mempty
{-# INLINE hashContent #-}

-- | Build the new 'FilePath' adding the hash of the provided content.
hashFile :: ByteString -> FilePath -> FilePath
hashFile content file = BS.unpack (unHash $ hashContent content) ++ "-" ++ file
{-# INLINE hashFile #-}

{- | @'hashStore' storePath action (name, content)@ computes @hash@ of @content@
and performs @action@ if file @storePath/hash-name@ doesn't exist, writing new
content in this file.

The property is that for any item @(name, hash)@, we get a valid filepath by
@storePath/hash-name@.

This functions solves the following problem. The caller doesn't need the
content. Then it is better not to return the bytestring (we avoid the overhrad
of reading the file in the cache hit case).
-}
hashStore :: FilePath
             -- ^ Directory to store file contents with new hash
          -> (ByteString -> IO ByteString)
             -- ^ Action to be performed if hash is different
          -> (String, ByteString)
             -- ^ File name
          -> IO String
             -- ^ Hash of given 'ByteString'
hashStore storePath action (name, actionInput) = do
    createDirectoryIfMissing True storePath
    let hashName  = hashFile actionInput name
    let storeName = storePath </> hashName

    isUpToDate <- doesFileExist storeName
    if isUpToDate then
        pure hashName
    else do
        actionOutput <- action actionInput
        BS.writeFile storeName actionOutput
        pure hashName

{- | Like 'hashStore' but also returns file content of hashed file.

This functions solves the following problem. The caller needs the file content.
Then it's better to return the bytestring (we avoid the overhead of reading the
file in the cache miss case).
-}
hashStoreWithContent :: FilePath
                        -- ^ Directory to store file contents with new hash
                     -> (ByteString -> IO ByteString)
                        -- ^ Action to be performed if hash is different
                     -> (String, ByteString)
                        -- ^ File name
                     -> IO (String, ByteString)
                        -- ^ Hash of given 'ByteString' and content of file
hashStoreWithContent storePath action (name, actionInput) = do
    -- code duplication with `hashStore` could be avoided, but it doesn't
    -- actually make code shorter or simpler
    createDirectoryIfMissing True storePath
    let hashName  = hashFile actionInput name
    let storeName = storePath </> hashName

    isUpToDate <- doesFileExist storeName
    if isUpToDate then do
        fileContent <- BS.readFile storeName
        pure (hashName, fileContent)
    else do
        newFileContent <- action actionInput
        BS.writeFile storeName newFileContent
        pure (hashName, newFileContent)
