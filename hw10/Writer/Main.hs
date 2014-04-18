-- (0.5 балла)
import Control.Monad.Writer
import Test.HUnit

fac :: Int -> Writer String Int
fac = undefined

main = fmap (const ()) $ runTestTT $ test
    [ runWriter (fac 0) ~?= (1,"1")
    , runWriter (fac 1) ~?= (1,"1")
    , runWriter (fac 2) ~?= (2,"(1) * 2")
    , runWriter (fac 3) ~?= (6,"((1) * 2) * 3")
    , runWriter (fac 4) ~?= (24,"(((1) * 2) * 3) * 4")
    , runWriter (fac 5) ~?= (120,"((((1) * 2) * 3) * 4) * 5")
    ]
