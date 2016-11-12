module XV6.Buf where

import qualified BetaCpu.Types as BT 
import BetaCpu.Instructions
import qualified BetaCpu.ToBeta as TB 
import qualified BetaCpu.Util as BCU

import qualified Data.Word as W

-- struct buf {
--   int flags;
--   uint dev;
--   uint sector;
--   struct buf *prev; // LRU cache list
--   struct buf *next;
--   struct buf *qnext; // disk queue
--   uchar data[512];
-- };
-- #define B_BUSY  0x1  // buffer is locked by some process
-- #define B_VALID 0x2  // buffer has been read from disk
-- #define B_DIRTY 0x4  // buffer needs to be written to disk

data Pointer a = Pointer a

data Buf = Buf { bufFlags :: W.Word32
               , bufDev :: W.Word32
               , bufSector :: W.Word32
               , bufPrev :: Pointer Buf
               , bufNext :: Pointer Buf
               , bufQNext :: Pointer Buf
               , bufData :: [W.Word8] -- assert that this is 512
               } 

-- struct_buf = do
--   int flags;
--   uint dev;
--   uint sector;
--   struct buf *prev; // LRU cache list
--   struct buf *next;
--   struct buf *qnext; // disk queue
--   uchar data[512];
-- };

  

