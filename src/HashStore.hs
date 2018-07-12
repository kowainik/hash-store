module HashStore
       ( hashContent
       , hashFile
       ) where

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Data.ByteString.Char8 (ByteString, unpack)

-- | Perform 'ByteString' hashing.
hashContent :: ByteString -> ByteString
hashContent = hash 64 mempty

-- | Build the new 'FilePath' adding the hash of the provided content.
hashFile :: ByteString -> FilePath -> FilePath
hashFile content file = unpack (hashContent content) ++ "-" ++ file
