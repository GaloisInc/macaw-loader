{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Macaw.BinaryLoader.PPC.ELF
  ( parseTOC
  )
where

import           Control.Monad ( replicateM, unless )
import qualified Control.Monad.Catch as X
import qualified Data.ByteString.Char8 as C8
import qualified Data.ElfEdit as E
import qualified Data.Macaw.BinaryLoader.PPC.TOC as TOC
import qualified Data.Macaw.Memory as MM
import qualified Data.Map.Strict as M
import           Data.Proxy ( Proxy(..) )
import qualified Data.Serialize.Get as G
import qualified Data.Word.Indexed as W
import           GHC.TypeLits ( KnownNat, natVal )


-- | Given an ELF file, extract a mapping from function entry points to the
-- value of the TOC pointer (which is to be stored in r2) for that function.
--
-- The generated function is called from 'mkInitialAbsState' to set up the value
-- of r2 at the entry point to each function, which will allow macaw to discover
-- the values of loaded function pointers.
--
-- In the v1 PowerPC ABI (at least as generated by gcc), the entries are all
-- stored in the @.opd@ section.  Each entry is three pointers, where the first
-- entry is the function address and the second is the value of the TOC.  The
-- third entry is unused in C programs (it is meant for Pascal).
parseTOC :: forall w m
          . (KnownNat w,
             MM.MemWidth w,
             X.MonadThrow m)
         => E.Elf w
         -> m (TOC.TOC w)
parseTOC e =
  case E.findSectionByName (C8.pack ".opd") e of
    [sec] ->
      case G.runGet (parseFunctionDescriptors @w (fromIntegral ptrSize)) (E.elfSectionData sec) of
        Left msg -> X.throwM ((TOC.TOCParseError msg) :: TOC.TOCException w)
        Right t -> return (TOC.toc t)
    _ -> X.throwM ((TOC.MissingTOCSection ".opd") :: TOC.TOCException w)
  where
    ptrSize = natVal (Proxy @w)

parseFunctionDescriptors :: (KnownNat w, MM.MemWidth w)
                         => Int
                         -> G.Get (M.Map (MM.MemAddr w) (W.W w))
parseFunctionDescriptors ptrSize = do
  let recordBytes = (3 * ptrSize) `div` 8
  let recordParser =
        case ptrSize of
          32 -> getFunctionDescriptor G.getWord32be
          64 -> getFunctionDescriptor G.getWord64be
          _ -> error ("Invalid pointer size: " ++ show ptrSize)
  totalBytes <- G.remaining
  unless (totalBytes `mod` recordBytes == 0) $ do
    fail "The .opd section is not divisible by the record size"
  funcDescs <- replicateM (totalBytes `div` recordBytes) recordParser
  return (M.fromList funcDescs)

getFunctionDescriptor :: (KnownNat w, Integral a, MM.MemWidth w)
                      => G.Get a
                      -> G.Get (MM.MemAddr w, W.W w)
getFunctionDescriptor ptrParser = do
  entryAddr <- ptrParser
  tocAddr <- ptrParser
  _ <- ptrParser
  let mso = MM.absoluteAddr (fromIntegral entryAddr)
  return (mso, fromIntegral tocAddr)
