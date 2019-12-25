module Hbacklight (main) where
import Control.Applicative (optional, (<|>))
import Control.Monad (when, foldM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, except)
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative (Parser, customExecParser, prefs, showHelpOnEmpty, flag', info,
       helper, fullDesc, progDesc, header, metavar, long, short, strOption, help, switch)
import Prelude hiding (max, readFile)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.IO (hPutStrLn, stderr)
import System.IO.Strict (readFile)
import System.Posix.IO (openFd, fdWrite, OpenMode(..), defaultFileFlags)
import Text.PrettyPrint.Boxes (Box(..), text, vcat, left, right, printBox, (<+>), emptyBox)
import Text.Read (readMaybe)

backlightPath :: String
backlightPath = "/sys/class/backlight/"


backlightSub :: [(String, FilePath)]
backlightSub =
    [ ("power"      , "bl_power")
    , ("brightness" , "brightness")
    , ("actual"     , "actual_brightness")
    , ("max"        , "max_brightness")
    , ("type"       , "type") ]


ledPath :: String
ledPath = "/sys/class/leds/"


ledSub :: [(String, FilePath)]
ledSub =
    [ ("brightness", "brightness")
    , ("max", "max_brightness") ]


lookupJust :: (Eq a) => a -> [(a, b)] -> b
lookupJust x xs = case x `lookup` xs of
    Nothing  -> error "key not present"
    Just r   -> r


data Interface = Backlight
    { power        :: Int
    , brightnessBl :: Int
    , actual       :: Int
    , maxBl        :: Int
    , type'        :: String
    , nameBl       :: String
    , modeBl       :: Mode }
    | Led
    { brightnessLd :: Int
    , maxLd        :: Int
    , nameLd       :: String
    , modeLd       :: Mode }
    deriving (Show)


data Opts = Opts
    { led     :: Bool
    , id'     :: String
    , verbose :: Bool
    , delta   :: Maybe String }
    | Enumerate
    { enum    :: Bool }


data Mode = Plus Int | Minus Int | Percent Int | Set Int | NoOp deriving (Show)


data InMode = Bl | Ld deriving (Show)


deviceType :: InMode -> String
deviceType Bl = "backlight"
deviceType Ld = "led"


max :: Interface -> Int
max i@Backlight{} = maxBl i
max i@Led{}       = maxLd i


name :: Interface -> String
name i@Backlight{} = nameBl i
name i@Led{}       = nameLd i


mode :: Interface -> Mode
mode i@Backlight{} = modeBl i
mode i@Led{}       = modeLd i


toMode :: String -> Either String Mode
toMode s = case f s of
    Nothing -> Left $ "could not parse delta: " <> s
    Just m  -> Right m
    where
        f ('+':xs) = Plus    <$> readMaybe xs
        f ('-':xs) = Minus   <$> readMaybe xs
        f ('%':xs) = Percent <$> readMaybe xs
        f ('~':xs) = Set     <$> readMaybe xs
        f xs       = Set     <$> readMaybe xs


opts :: Parser Opts
opts = Opts
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
        <> help "Modify the backlight value, ~ sets the value to AMOUNT, else shift is relative. Defaults to ~" )


enumOpts :: Parser Opts
enumOpts = Enumerate
    <$> flag'
        True
        (long "enum"
        <> short 'e'
        <> help "List available devices"
        )


parseBacklightPaths :: InMode -> [String] -> String -> Mode -> Either String Interface
parseBacklightPaths u xs n m = case f of
      Nothing -> Left $ "err: cannot stat " <> deviceType u <> " properties"
      Just x  -> Right x
      where
      f = case (u, xs) of
        (Bl, (a1:a2:a3:a4:a5:_)) -> Backlight
            <$> readMaybe a1
            <*> readMaybe a2
            <*> readMaybe a3
            <*> readMaybe a4
            <*> pure a5
            <*> pure n
            <*> pure m
        (Ld, (a1:a2:_)) -> Led
            <$> readMaybe a1
            <*> readMaybe a2
            <*> pure n
            <*> pure m
        _ -> Nothing


parseFilePath :: InMode -> String -> Mode -> IO (Either String Interface)
parseFilePath u d m = do
    let
     (devicePath, deviceSub) = case u of
        Bl -> (backlightPath, backlightSub)
        Ld -> (ledPath, ledSub)
    let parent = devicePath <> d <> "/"
    let paths = (parent <>) . snd <$> deviceSub
    valid <- (&&) <$> doesDirectoryExist parent <*> foldM
        (\acc x -> (&&) <$> doesFileExist x <*> pure acc)
        True
        paths
    if valid
        then do
            xs <- traverse (fmap (filter (/= '\n')) . readFile) paths
            return $ parseBacklightPaths u xs d m
        else return . Left $ "err: cannot locate " <> deviceType u <> ": " <> d


dim :: Interface -> IO ()
dim i = case mode i of
    NoOp      -> return ()
    Plus x    -> setV $ lvl + x
    Minus x   -> setV $ lvl - x
    Percent x -> setV $ x * (max i) `div` 100
    Set x     -> setV x
    where
        (dPath, dSub) = case i of
            Backlight{} -> (backlightPath, backlightSub)
            Led{}       -> (ledPath, ledSub)
        path = dPath <> name i <> "/" <> lookupJust "brightness" dSub
        fd   = openFd
            path
            WriteOnly
            Nothing
            defaultFileFlags
        lvl     = brightnessBl i
        setV v  = fd >>= \handle -> handle `fdWrite` show v >> return ()


idL :: Int
idL = 24


table :: Interface -> Box
table i = case i of
    Backlight{} -> lv "backlight" backlightSub
        <+> rv
        [ take idL $ nameBl i
        , show . power $ i
        , show . brightnessBl $ i
        , show . actual $ i
        , show . maxBl $ i
        , type' i
        ]
    Led{} -> lv "led" ledSub
        <+> rv
        [ take idL $ nameLd i
        , show . brightnessLd $ i
        , show . maxLd $ i ]
    where
        lv itype = vcat left . (text itype :) . fmap (text . snd)
        rv = vcat right . fmap text


run :: Opts -> IO ()
run o@Opts{} = do
    let m = maybe (Right NoOp)  toMode  $ delta o
    let t = if led o
        then Ld
        else Bl
    interface <- runExceptT $ ExceptT . parseFilePath t (id' o) =<< except m
    case interface of
        Left e  -> hPutStrLn stderr e
        Right i -> do
            when (verbose o) $ printBox . table $ i
            dim i
run Enumerate{} = do
    let mkBody  = fmap ( (<+>) (emptyBox 1 3) . vcat left . fmap (text . last . splitOn "/") ) . listDirectory
    let blHeadr = text "backlight"
    let ldHeadr = text "led"
    blBody <- mkBody backlightPath
    ldBody <- mkBody ledPath
    printBox $ vcat left [blHeadr, blBody, ldHeadr, ldBody]


main :: IO ()
main = run =<< customExecParser (prefs showHelpOnEmpty) cmd where
    cmd = info ( helper <*> enumOpts <|> opts )
        ( fullDesc
        <> progDesc "Adjust backlight backlight"
        <> header "hbacklight - backlight manager" )
