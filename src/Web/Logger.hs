module Web.Logger where
import qualified Data.ByteString as BS
import qualified Data.Vault.Lazy as V

type VaultLogger = BS.ByteString -> IO ()
type VaultLoggerKey = V.Key VaultLogger
