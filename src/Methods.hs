module Methods where
  
import Entities
printEntity :: Entity -> String
printEntity e = sPosition ++ show (vector e) ++ sSize
  where
    sPosition = show $ position e
    sSize = show $ size e