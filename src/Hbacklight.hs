{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hbacklight (main) where

import Control.Applicative (optional, (<|>))
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Catch (throwM)

import Data.Char (isDigit)
import Data.Either (either)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Options.Applicative (Parser, customExecParser, prefs, showHelpOnEmpty
    , flag', info, helper, fullDesc, progDesc, header, metavar, long, short
    , strOption, help, switch)

import System.Directory (listDirectory)
import System.Posix.IO (openFd, closeFd, fdWrite, OpenMode(WriteOnly), defaultFileFlags)

import Text.PrettyPrint.Boxes (Box, text, vcat, left, right, printBox, (<+>)
    , emptyBox)
import Text.Read (readMaybe)

import qualified Data.Text as T (Text, pack, unpack, uncons)
import qualified System.IO.Strict as Strict (readFile)

--------------------------------------------------------------------------------
-- types & defaults

data Device = Backlight
    { power        :: Int
    , brightness   :: Int
    , actual       :: Int
    , maxB         :: Int
    , typeB        :: T.Text
    , name         :: T.Text }
    | Led
    { brightness   :: Int
    , maxB         :: Int
    , name         :: T.Text }
    deriving (Show)

data Config = Dim
    { led     :: Bool
    , dId     :: T.Text
    , verbose :: Bool
    , delta   :: Maybe T.Text }
    | Enumerate Bool

data OpT = Plus | Minus | Percent | Set deriving (Show)
data Operation = Op OpT Int deriving (Show)

type TFilePath = T.Text

type DeviceProps = [(T.Text, TFilePath)]

type DeviceMap = (TFilePath, DeviceProps)

data Env = Env
    { blMap  :: DeviceMap
    , ledMap :: DeviceMap }

defaultEnv :: Env
defaultEnv = Env
    { blMap  = (blPath, blProps)
    , ledMap = (ldPath, ldProps) }
    where
        blPath = "/sys/class/backlight/"
        ldPath = "/sys/class/leds/"
        blProps =
            [ ("power"      , "bl_power")
            , ("brightness" , "brightness")
            , ("actual"     , "actual_brightness")
            , ("max"        , "max_brightness")
            , ("type"       , "type") ]
        ldProps =
            [ ("brightness", "brightness")
            , ("max", "max_brightness") ]

data Ex = ParseFailure String

instance Exception Ex

instance Show Ex where
    show (ParseFailure s) = "*** Exception: parseFailure: " <> s

--------------------------------------------------------------------------------
-- main logic

readEither :: Read a => b -> String -> Either b a
readEither err s = case readMaybe s of
    Just x -> Right x
    _      -> Left err

readErr :: Read a => String -> Either Ex a
readErr s = readEither (ParseFailure s) s

liftEIO :: Exception e => Either e a -> IO a -- may throw IOError
liftEIO = either throwM return

-- DeviceProps and Device are known to have same argument structure
table :: DeviceProps -> DeviceProps -> Device -> Box
table bl ld = \case
    d@Backlight{} -> lv "backlight" (fmap T.unpack <$> bl)
        <+> rv
        [ T.unpack $ name d
        , show $ power d
        , show $ brightness d
        , show $ actual d
        , show $ maxB d
        , T.unpack $ typeB d
        ]
    d@Led{} -> lv "led" (fmap T.unpack <$> ld)
        <+> rv
        [ T.unpack $ name d
        , show $ brightness d
        , show $ maxB d ]
    where
        lv itype = vcat left . (text itype :) . fmap (text . snd)
        rv = vcat right . fmap text

readOpT :: Char -> Either Ex OpT
readOpT = \case
    '+' -> Right Plus
    '-' -> Right Minus
    '%' -> Right Percent
    '~' -> Right Set
    x   -> Left . ParseFailure $ "unsupported mode \"" <> (x:"\"")

readOp :: T.Text -> Either Ex Operation
readOp s = do
    (x, xs) <- uncons' s
    opType  <- readOpT x
    op      <- readErr $ T.unpack xs
    return $ Op opType op
    where
        uncons' x = case T.uncons x of
            Nothing -> Left . ParseFailure $ "cannot parse empty input"
            Just u@(h,_)  -> Right $ if isDigit h then ('~', x) else u --deflt ~

-- read DeviceMap attributes from filesystem
readDProps :: T.Text -> DeviceMap -> IO [String]
readDProps dname (dloc, dmap) = mapM
    (Strict.readFile . toPath . ('/' :) . T.unpack . snd)
    dmap
    where
        dpath = T.unpack dloc <> T.unpack dname
        toPath = (dpath <>)

parseBl :: T.Text -> DeviceMap -> IO Device -- may throw Ex or IOError
parseBl dname dmap = do
    -- read each property from DeviceMap and apply to Device cons
    (a1:a2:a3:a4:a5:_) <- readDProps dname dmap
    liftEIO $ Backlight
        <$> readErr a1
        <*> readErr a2
        <*> readErr a3
        <*> readErr a4
        <*> Right (T.pack a5)
        <*> Right dname

parseLed :: T.Text -> DeviceMap -> IO Device -- may throw Ex or IOError
parseLed dname dmap = do
    (a1:a2:_) <- readDProps dname dmap
    liftEIO $ Led
        <$> readErr a1
        <*> readErr a2
        <*> Right dname

-- dims and returns updated device
dim :: DeviceMap -> Device -> Operation -> IO Device
dim (dpath, dsub) i op = do
        device <- runOp op
        () <- closeFd =<< fd
        return device
        where
            path = T.unpack $ dpath <> name i <> "/"
                <> (fromJust $ lookup "brightness" dsub)
            lvl = brightness i
            fd = openFd
                path
                WriteOnly
                Nothing
                defaultFileFlags
            nonNeg n = if n > 0 then n else 0
            writeV v = fd >>= \handle -> handle `fdWrite` show v
                >> return (i { brightness = v } )
            setV = writeV . nonNeg
            runOp = \case
                Op Plus x    -> setV $ lvl + x
                Op Minus x   -> setV $ lvl - x
                Op Percent x -> setV $ x * maxB i `div` 100
                Op Set x     -> setV x


 -- list all available led and backlight devices
enumDevices :: TFilePath -> TFilePath -> IO Box
enumDevices blPath ledPath = do
   let blHeader  = text "backlight"
   let ledHeader = text "led"
   blDir  <- listDirectory $ T.unpack blPath
   ledDir <- listDirectory $ T.unpack ledPath
   let blBody = mkBody  blDir
   let ledBody = mkBody ledDir
   return $ vcat left
        [ blHeader
        , blBody
        , ledHeader
        , ledBody ]
    where
        fname = text . last . splitOn "/"
        mkBody = (<+>) (emptyBox 1 3) . vcat left . fmap fname

run :: Env -> Config -> IO ()
run Env{blMap=bM, ledMap=lM} = \case
    c@Dim{} -> do
        tmp <- if led c
            then (lM, ) <$> parseLed (dId c) lM
            else (bM, ) <$> parseBl  (dId c) bM
        device <- uncurry (dimUpdate c) tmp -- update device if dimmed
        when (verbose c) $ printBox $ table (snd bM) (snd lM) device
    Enumerate{} -> printBox =<< enumDevices (fst bM) (fst lM)
    where
        dimUpdate :: Config -> DeviceMap -> Device -> IO Device
        dimUpdate Dim{delta=del} dmap device = case liftEIO . readOp <$> del of
            Just op -> dim dmap device =<< op
            Nothing -> return device

main :: IO ()
main = run defaultEnv =<< customExecParser (prefs showHelpOnEmpty) cmd where
    cmd = info ( helper <*> enumParser <|> dimParser )
        ( fullDesc
        <> progDesc "Adjust backlight backlight"
        <> header "hbacklight - backlight manager" )

--------------------------------------------------------------------------------
-- app parser

dimParser :: Parser Config
dimParser = Dim
    <$> switch
        (long "led"
        <> short 'l'
        <> help "Target led device"
        )
    <*> strOption
        ( long "id"
        <> short 'i'
        <> metavar "TARGET"
        <> help "Identifier of backlight backlight" )
    <*> switch
        (long "verbose"
        <> short 'v'
        <> help "Informative summary backlight backlight state"
        )
    <*> (optional . strOption)
        ( long "delta"
        <> short 'd'
        <> metavar "[+,-,%,~]AMOUNT"
        <> help ( "Modify the backlight value, ~ sets the value to AMOUNT, else"
            <> "shift is relative. Defaults to ~" ) )

enumParser :: Parser Config
enumParser = Enumerate
    <$> flag'
        True
        (long "enum"
        <> short 'e'
        <> help "List available devices"
        )
