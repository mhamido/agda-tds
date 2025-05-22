{-# LANGUAGE LambdaCase #-}

module Doogle.Utils where

import Control.Monad.IO.Class (MonadIO)
import Data.List (isSuffixOf)

catMaybes' :: (MonadIO m) => (a -> m (Maybe b)) -> (a -> m ()) -> [a] -> m [b]
catMaybes' _f _onNothing [] = pure []
catMaybes' f onNothing (a : as) =
    f a >>= \case
        Nothing -> onNothing a >> catMaybes' f onNothing as
        Just x -> (x :) <$> catMaybes' f onNothing as

hasExtension :: String -> FilePath -> Bool
hasExtension ext = isSuffixOf ext . dropWhile (/= '.')