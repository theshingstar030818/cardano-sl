module Test.Pos.Wallet.Web.AddressSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString          as BS
import           Data.Default             (def)
import           Formatting               (build, sformat, (%))
import           Test.Hspec               (Spec)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck.Monadic  (forAllM)

import           Pos.Arbitrary.Crypto     (arbitraryMostlyUnencryptedKeys)
import           Pos.Binary               (serialize')
import           Pos.Client.Txp.Addresses (getFakeChangeAddress)
import           Pos.Core.Address         (IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Crypto               (ShouldCheckPassphrase (..))
import           Pos.Launcher             (HasConfigurations)
import           Pos.Util.CompileInfo     (withCompileInfo)

import           Test.Pos.Util            (assertProperty, maybeStopProperty,
                                           withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
    prop fakeAddressHasMaxSize fakeAddressHasMaxSizeTest
  where
    fakeAddressHasMaxSize = "Fake address has maximal possible size"

fakeAddressHasMaxSizeTest
    :: HasConfigurations
    => Word32 -> Word32 -> WalletProperty ()
fakeAddressHasMaxSizeTest accIdx addrIdx =
    forAllM arbitraryMostlyUnencryptedKeys $
        \(rootSK, pp) ->
    do
    (addr, _) <-
        maybeStopProperty "bad passphrase" $
        deriveLvl2KeyPair
            (IsBootstrapEraAddr True)
            (ShouldCheckPassphrase False)
            pp rootSK accIdx addrIdx
    maxAddr <- lift getFakeChangeAddress
    assertProperty
        (countSize addr <= countSize maxAddr)
        (sformat (build%" > "%build) (countSize addr) (countSize maxAddr))
  where
    countSize = BS.length . serialize'

