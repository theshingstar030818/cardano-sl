module Statistics.Throughput
    ( throughput
    ) where

import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as M
import           Data.Time.Units                           (Microsecond)
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import           Graphics.Rendering.Chart.Grid

import           Pos.Txp.MemState.Types                    (MemPoolModifyReason (..))
import           Pos.Util.JsonLog                          (JLMemPool (..))
import           Types
import           Universum

throughput :: FilePath 
           -> Double 
           -> Int
           -> [(NodeIndex, Timestamp, Int)] 
           -> [(NodeIndex, Timestamp, JLMemPool)] 
           -> IO ()
throughput f w cnt xs ys =
    let xs'    = [(t, c) | (_, t, c) <- xs]
        ys'    = mapMaybe wait ys
        times  = map fst xs' ++ map fst ys'
        tmin   = minimum times
        tmax   = maximum times
        step   = (tmax - tmin) `div` fromIntegral cnt
        times' = [tmin, tmin + step .. tmax]
        xs''   = scaleShift tmin $ sliding w times' (\zs -> fromIntegral (sum zs) / w) xs'
        ys''   = scaleShift tmin $ sliding w times' average                            ys'
    in  grid f xs'' ys''
  where
    wait :: (NodeIndex, Timestamp, JLMemPool) -> Maybe (Timestamp, Integer)
    wait (_, t, JLMemPool{..}) = case jlmReason of
        ProcessTransaction _ -> Just (t, jlmWait)
        _                    -> Nothing

grid :: FilePath -> [(Double, Double)] -> [(Double, Double)] -> IO ()
grid f xs ys = void $ renderableToFile def f $ fillBackground def $ gridToRenderable $ chart1 `above` chart2
  where
    g = layoutToGrid . execEC
    
    chart1 = g $ do
        setColors [opaque blue]
        layout_x_axis . laxis_title .= "time (s)"
        layout_y_axis . laxis_title .= "tx/s"
        plot $ line "throughput" [xs]

    chart2 = g $ do
        setColors [opaque red]
        layout_x_axis . laxis_title .= "time (s)"
        layout_y_axis . laxis_title .= "wait (mcs)"
        plot $ line "wait" [ys]

sliding :: Double -> [Timestamp] -> ([a] -> b) -> [(Timestamp, a)] -> [(Timestamp, b)]
sliding _      _     _ [] = []
sliding window times f xs =
    let m = M.fromList xs
    in  [(t, f $ g t m) | t <- times]
  where

    w2 :: Microsecond
    w2 = round $ window * 500000

    g :: Timestamp -> Map Timestamp a -> [a]
    g t m =
        let tmin = t - w2
            tmax = t + w2
            m'   = mapBetween (tmin, tmax) m
        in  map snd $ M.toList m'

scaleShift :: Microsecond -> [(Microsecond, a)] -> [(Double, a)]
scaleShift t = map $ \(t', x) -> (fromIntegral (t' - t) / 1000000, x)

mapGE :: Ord a => a -> Map a b -> Map a b
mapGE a m = case M.lookupLT a m of
    Nothing      -> m
    Just (a', _) -> snd $ M.split a' m

mapLE :: Ord a => a -> Map a b -> Map a b
mapLE a m = case M.lookupGT a m of
    Nothing      -> m
    Just (a', _) -> fst $ M.split a' m

mapBetween :: Ord a => (a, a) -> Map a b -> Map a b
mapBetween (l, u) = mapGE l . mapLE u

average :: [Integer] -> Double
average = f . foldl' g (0, 0)
  where
    f (s, c)
        | c == 0    = 0
        | otherwise = fromIntegral s / fromIntegral c

    g :: (Integer, Int) -> Integer -> (Integer, Int)
    g (s, c) x = (s + x, c + 1)
