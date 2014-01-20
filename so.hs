data Pixel = Pixel Float Float Int

instance Eq Pixel where 
    Pixel _ _ g1 == Pixel _ _ g2 = g1 == g2

instance Show Pixel where
    show ( Pixel  x y g ) = "Pixel " ++ show g ++ " at ("  ++ show x ++ "," ++ show y ++ ")"

main = do
    print $ Pixel 2 3 100