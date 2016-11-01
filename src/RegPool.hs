module RegPool where

import Types


data RegPool = RegPool { rpoolInUse :: [Reg]
                       , rpoolFree :: [Reg]
                       } deriving (Show, Eq)

