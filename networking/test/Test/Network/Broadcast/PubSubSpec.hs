{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Network.Broadcast.PubSubSpec
       ( spec
       ) where

import           Universum
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import           Data.List ((!!))
import           Network.Broadcast.PubSub (dequeueTChan)
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (Arbitrary(..), Gen, Property, choose, forAll, ioProperty, ioProperty, property, sized, vectorOf, (===))

spec :: Spec
spec = describe "PubSubQ" $ do
    describe "dequeueTChan" $ do
        it "should not block when empty" $ property prop_doNotBlock
        it "should return Nothing when not found" $ property prop_notFound
        it "should pop first found element" $ property prop_pop

prop_doNotBlock :: Property
prop_doNotBlock = forAll arbitrary $ \(as :: [Int]) ->
    ioProperty $ do
        chan <- newTChanIO
        fillTChan chan as
        as' <- replicateM (length as + 1) $ dequeueTChan chan (const True)
        return $ as' === (Just <$> as) ++ [Nothing]

prop_notFound :: Property
prop_notFound = forAll arbitrary $ \(as :: [Int]) ->
    ioProperty $ do
        chan <- newTChanIO
        fillTChan chan as
        ma <- dequeueTChan chan (const False)
        return $ ma === Nothing

prop_pop :: Property
prop_pop = forAll (indexedList arbitrary) $ \((i, as) :: (Int, [Int])) ->
    ioProperty $ do
        chan <- newTChanIO
        fillTChan chan as
        let x = as !! i
        _ <- dequeueTChan chan (== x)
        as' <- asList chan
        return $ as' === dropFirst (== x) as

    where 
    indexedList :: Gen a -> Gen (Int, [a])
    indexedList gen = do
        n <- sized pure
        i <- choose (0,n-1)
        as <- vectorOf n gen
        return (i, as)

    asList :: TChan a -> IO [a]
    asList c = atomically $ go []
        where
        go as = do
            ma <- tryReadTChan c
            case ma of
                Nothing -> return $ reverse as
                Just a  -> go (a : as)

    dropFirst :: (a -> Bool) -> [a] -> [a]
    dropFirst _ [] = []
    dropFirst p (a : as) = if p a then as else a : dropFirst p as

fillTChan :: TChan a -> [a] -> IO ()
fillTChan chan as = traverse_ (atomically . writeTChan chan) as
