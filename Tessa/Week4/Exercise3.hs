import Data.List
import System.Random
import Test.QuickCheck
import LTS


getTrans :: IOLTS -> [LabeledTransition]
getTrans (_,_,_,ts,_) = ts

getStart :: IOLTS -> State
getStart (_,_,_,_,s) = s

-- tr = [Label]
-- trc = [ [Label], [Label], ... ]

traces :: [LabeledTransition] -> State -> Trace -> [Trace]
traces [] s tr = tr -- tr = [Label]
traces ((x,l,y):ts) s tr | x == s     = [l] : [(traces ts s tr)] : [(traces ts y (tr ++ [l]))] -- add single trace, look for other single traces from startstate s, and look for following longer traces
                         | otherwise  = tr

-- TODO: remove empty lists? Except for first?

straces :: [Trace]
straces = traces getTrans getStart []