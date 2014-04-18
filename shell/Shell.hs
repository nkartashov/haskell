module Shell
    ( Shell, runShell
    -- Добавьте другие функции для работы с Shell по вкусу
    , getCurrentDirectory, putCurrentDirectory
    ) where

import Control.Monad.Trans

data Shell a = Undefined

instance Functor Shell where
    fmap = undefined

instance Monad Shell where
    return = undefined
    (>>=) = undefined

-- В Shell можно будет выполнять произвольные IO действия, используя liftIO.
instance MonadIO Shell where
    liftIO = undefined

runShell :: Shell a -> IO a
runShell = undefined

getCurrentDirectory :: Shell String
getCurrentDirectory = undefined

putCurrentDirectory :: String -> Shell ()
putCurrentDirectory = undefined
