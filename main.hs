 --main.hs
 {-# LANGUAGE MultiParamTypeClasses #-}
import OOP

data I1 = I1
getI1::I1->String
getI1 i1 = "Interface 1"

data I2 = I2
getI2::I2->String
getI2 i2 = "Interface 2"


data C = C


instance Implements C I1 where
    implement C o = I1

instance Implements C I2 where
    implement C o = I2

data O = O
instance Instance C O where
    classOf o = C

main = do
    putStrLn (O #> getI1)
    putStrLn (O #> getI2)