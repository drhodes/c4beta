module RegPool where

import Types
import BetaCpu.Types

new = RegPool [] [R0 .. R26]



--data RegAlloc = RegAlloc { raRegMap :: DM.Map Reg 

-- these should be encapsulated
spill = undefined
fill = undefined

request = undefined
release = undefined
