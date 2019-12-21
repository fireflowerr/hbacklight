{-# LANGUAGE ScopedTypeVariables #-}
module Hbacklight where
import Prelude hiding (max, readFile)
import Control.Monad (when, unless, foldM)
import Control.Applicative (optional, (<**>))
import Options.Applicative (Parser(..), execParser, info, helper, fullDesc, progDesc, header, metavar, long, short, strOption, help, switch)
import Data.Semigroup ((<>))
import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesFileExist, doesDirectoryExist)
import Text.Read (readMaybe)
import Text.PrettyPrint.Boxes (Box(..), text, hsep, vcat, left, right, printBox, (<+>))
import System.IO.Strict (readFile)
import System.Posix.IO (openFd, fdWrite, OpenMode(..), defaultFileFlags)

devicePath :: String
devicePath = "/sys/class/backlight/"

deviceSub :: [(String, Device)]
deviceSub =
    [ ("power"      , "bl_power")
    , ("brightness" , "brightness")
    , ("actual"     , "actual_brightness")
    , ("max"        , "max_brightness")
    , ("type"       , "type") ]


lookupJust :: (Eq a) => a -> [(a, b)] -> b
lookupJust x xs = case x `lookup` xs of
    Nothing  -> error "key not present"
    (Just r) -> r


data Interface = Backlight
    { power      :: Integer
    , brightness :: Integer
    , actual     :: Integer
    , max        :: Integer
    , type'      :: String
    , name       :: String
    , mode       :: Mode }
    deriving (Show)

type Device = String


data Opts = Opts
    { id'     :: String
    , verbose :: Bool
    , delta   :: Maybe String }

data Mode = Plus Integer | Minus Integer | Percent Integer | Set Integer | NoOp deriving (Show)

toMode :: String -> Mode
toMode ('+':xs) = Plus    $ read xs
toMode ('-':xs) = Minus   $ read xs
toMode ('%':xs) = Percent $ read xs
toMode ('~':xs) = Set     $ read xs
toMode xs       = Set     $ read xs


opts :: Parser Opts
opts = Opts
    <$> strOption
        ( long "id"
        <> short 'i'
        <> metavar "TARGET"
        <> help "Identifier of backlight device" )
    <*> switch
        (long "verbose"
        <> short 'v'
        <> help "informative of backlight device state"
        )
    <*> (optional . strOption)
        ( long "delta"
        <> short 'd'
        <> metavar "[+,-,%,~]AMOUNT"
        <> help "Modify the backlight value, ~ sets the value to AMOUNT, else shift is relative. Defaults to ~" )


readArg :: forall a b. (Read a) => String -> (a -> b) -> Maybe a
readArg s f = readMaybe s


parseDevicePath :: String -> String -> String -> String -> String -> String -> Mode -> Maybe Interface
parseDevicePath a1 a2 a3 a4 a5 a6 m = Backlight
    <$> readMaybe a1
    <*> readMaybe a2
    <*> readMaybe a3
    <*> readMaybe a4
    <*> pure a5
    <*> pure a6
    <*> pure m


parseDevice :: Device -> Mode -> IO (Maybe Interface)
parseDevice d m = do
    let parent   = devicePath <> d <> "/"
    let paths = (parent <>) . snd <$> deviceSub
    valid <- (&&) <$> doesDirectoryExist parent <*> foldM
        (\acc x -> (&&) <$> doesFileExist x <*> pure acc)
        True
        paths
    if valid
        then do
            (a1:a2:a3:a4:a5:_) <- traverse (fmap (filter (/= '\n')) . readFile) paths
            return $ parseDevicePath a1 a2 a3 a4 a5 d m
        else return Nothing


dim :: Interface -> IO ()
dim (Backlight _ _ _ _ _ _ NoOp) = return ()
dim i = case mode i of
    (Plus x)    -> setV $ lvl + x
    (Minus x)   -> setV $ lvl - x
    (Percent x) -> let
        p    = (fromIntegral x / 100)
        m    = fromIntegral . max $ i
        in setV . round $ p * m
    (Set x) -> setV x
    where
        path = devicePath <> lookupJust "brightness" deviceSub
        fd   = openFd
            "/sys/class/backlight/intel_backlight/brightness"
            WriteOnly
            Nothing
            defaultFileFlags
        lvl     = brightness i
        setV v  = fd >>= \handle -> handle `fdWrite` show v >> return ()


idL :: Int
idL = 16


table :: Interface -> Box
table i = lv <+> rv where
    lv = vcat left $ text <$> "device" : map snd deviceSub
    rv = vcat right $ text <$>
        [ take idL $ name i
        , show . power $ i
        , show . brightness $ i
        , show . actual $ i
        , show . max $ i
        , type' i
        ]


run :: Opts -> IO ()
run o = do
    let m = maybe NoOp toMode $ delta o
    interface <- parseDevice (id' o) m
    case interface of
        Nothing  -> putStrLn "err: cannot stat backlight device"
        (Just i) -> do
            when (verbose o) $ printBox . table $ i
            dim i


main :: IO ()
main = run =<< execParser cmd where
    cmd = info (helper <*> opts )
        ( fullDesc
        <> progDesc "Aadjust backlight device"
        <> header "hbacklight - backlight manager" )
