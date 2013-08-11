-- | All sorts of functions that have no clear place where else they could be.
--

module Dfterm3.Util
    ( whenJust )
    where

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing _ = return ()
whenJust (Just x) action = action x

