
instance Num () where
    negate () = ()
    () + () = ()
    () * () = ()
    fromInteger x = ()
    abs () = ()
    signum () = ()

main = do
    print $ 1 == 0 `asTypeOf`()
