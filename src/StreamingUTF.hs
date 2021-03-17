module StreamingUTF where

import           Control.Monad                       (when)
import           Data.Char                           (toLower)
import qualified Data.HashMap.Strict                 as M
import qualified Data.HashTable                      as H
import           Data.HashTable.Internal             (atomicallyChangeLoad,
                                                      deleteFirstKey,
                                                      genericModify)
import qualified Data.List                           as L
import           Data.Traversable
import           GHC.Conc                            (numCapabilities)
import           GHC.Conc.Sync                       (readTVar, writeTVar)
import           GHC.Word                            (Word8)
import qualified Streamly                            as S
import qualified Streamly.Data.Fold                  as FL
import qualified Streamly.Data.String                as S
import qualified Streamly.FileSystem.Handle          as FH
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.Array      as A
import qualified Streamly.Prelude                    as S
import           System.IO                           (IOMode (..), openFile)
import           Types

streamingUTF :: [FilePath] -> IO [(FilePath, Counts)]
streamingUTF paths = for paths $ \fp -> do
    src <- openFile fp ReadMode
    count <-
          S.foldl' mappend mempty
        $ S.aheadly
        $ S.maxThreads numCapabilities
        $ S.mapM countBytes
        $ FH.toStreamArraysOf 1024000 src
    return (fp, count)
    where
    countBytes =
          S.foldl' (\acc c -> acc <> countByteUTF8 c) mempty
        . S.decodeChar8
        . A.toStream

{-# INLINE streamingUTF #-}

hm :: FilePath -> IO (M.HashMap String Int)
hm fp = openFile fp ReadMode >>= go
  where
    go src =
        S.foldl' (M.unionWith (+)) mempty
            $ S.aheadly
            $ S.maxThreads numCapabilities
            $ S.mapM count
            $ FH.toStreamArraysOf 1024000 src
    count :: A.Array Word8 -> IO (M.HashMap String Int)
    count =
        S.foldl' (flip (M.alter incf)) mempty
            . S.map (map toLower)
            . S.splitOn (`elem` [' ', '\t', '\n']) FL.toList
            . S.decodeUtf8Lenient
            . A.toStream

ht :: FilePath -> IO (H.HashTable String Int)
ht fp = openFile fp ReadMode >>= go
  where
    go src = do
        t <- H.newWithDefaults 503003 :: IO (H.HashTable String Int)
        let count =
                S.mapM_ (\w -> upsert t w 1 (+ 1))
               -- S.mapM_ (\w -> H.add t w 0 >> H.modify t w (+1)) -- almost no difference from upset
                    . S.map (map toLower)
                    . S.splitOn (`elem` [' ', '\t', '\n']) FL.toList
                    . S.decodeUtf8Lenient
                    . A.toStream
        S.drain
            $ S.aheadly
            $ S.maxThreads numCapabilities
            $ S.mapM count
            $ FH.toStreamArraysOf 1024000 src
        return t

{-# INLINABLE upsert #-}
upsert :: (Eq k)
       => H.HashTable k v
       -> k            -- ^ key `k`
       -> v            -- ^ add `k` if didn't exist
       -> (v -> v)     -- ^ update-function
       -> IO () -- ^ returns the old value for key `k` if it existed
upsert htable k i f = do
    result <- genericModify htable k $ \tvar -> do
                list <- readTVar tvar
                case L.lookup k list of
                    Nothing -> do
                        writeTVar tvar ((k,i) : deleteFirstKey k list)
                        return True
                    Just v -> do -- entry was already there, so we overwrite it
                        writeTVar tvar ((k,f v) : deleteFirstKey k list)
                        return False
    when result $
        atomicallyChangeLoad htable 1

incf :: Maybe Int -> Maybe Int
incf Nothing  = Just 1
incf (Just x) = Just (x + 1)
{-# INLINE incf #-}
