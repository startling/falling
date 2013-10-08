module Main where
-- base
import System.Environment
-- lens
import Control.Lens
-- falling
import Falling

-- Example data.
corpus :: Integer -> [Particle Float]
corpus c = map (particle . fromInteger) [1..fromInteger c]

main :: IO ()
main = getArgs >>= \as -> let
  iterations = maybe 200 read $ preview (ix 0) as
  particles = maybe 20 read $ preview (ix 1) as in
  print . take iterations . iterate update $ corpus particles

-- Lazy:
--   6,970,238,640 bytes allocated in the heap
--   1,024,415,952 bytes copied during GC
--         221,744 bytes maximum residency (720 sample(s))
--          61,624 bytes maximum slop
--               2 MB total memory in use (0 MB lost due to fragmentation)
--
--                                    Tot time (elapsed)  Avg pause  Max pause
--  Gen  0     12726 colls,     0 par    2.29s    2.36s     0.0002s    0.0024s
--  Gen  1       720 colls,     0 par    0.33s    0.33s     0.0005s    0.0012s
--
--  INIT    time    0.00s  (  0.00s elapsed)
--  MUT     time    5.49s  (  7.14s elapsed)
--  GC      time    2.62s  (  2.69s elapsed)
--  EXIT    time    0.00s  (  0.00s elapsed)
--  Total   time    8.11s  (  9.83s elapsed)
--
--  %GC     time      32.3%  (27.4% elapsed)
--
--  Alloc rate    1,269,391,918 bytes per MUT second
--
--  Productivity  67.7% of total user, 55.9% of total elapsed

