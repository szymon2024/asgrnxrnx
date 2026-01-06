-- 2026-06-06
{-
   asgrnxrnx to narzędzie dla plików obserwacyjnych RINEX 3.04 z
   ASG‑EUPOS tworzące na podstawie pliku RINEX ASG-EUPOS nowy plik
   RINEX bez nadmiarowych typów obserwacji.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Builder as B
    
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.IntSet as IntSet
import           Data.List              (groupBy, findIndex, (\\))
import           Data.Char              (isSpace)
import           Data.Int               (Int64)
import           System.FilePath        (splitFileName, (</>))
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO              (hFlush, stdout, stderr)
import           System.Directory       (doesFileExist)
import           Control.Monad          (when, unless, forM_)
import           Text.Printf            (printf, hPrintf)
import           Control.Monad.ST
import           Data.Array.ST
    
import           Data.Time.Clock     (getCurrentTime, diffUTCTime)



type ObsType       = L8.ByteString
type SysObsTypes   = (Sys, [ObsType])
type ObsTypesMap   = Map Sys [ObsType]

type Sys           = Char
type PRN           = L8.ByteString
type EpochRecord   = L8.ByteString
type ObsRecord     = (Sys, PRN, L8.ByteString)

type ObsRnxHeaderRecord = (L8.ByteString, L8.ByteString)
type ObsRnxDataRecord   = (EpochRecord, [ObsRecord])

programVersion :: String
programVersion = "1.0.0"

fieldLen :: Int
fieldLen = 16

-- Entry to program
main :: IO ()
main = do
    args <- getArgs

    let verbose = "--gadatliwie" `elem` args || "-g" `elem` args
        dryRun  = "--na-sucho"   `elem` args || "-n" `elem` args
        args'   = filter (`notElem` ["--gadatliwie", "-g"
                                    ,"--na-sucho"  , "-n"]) args
            
    case args' of
      ["--pomoc"]  -> showHelp
      ["-p"]       -> showHelp
      ["--wersja"] -> showVersion
      ["-w"]       -> showVersion

      [input] -> do
          runWithOptions input Nothing verbose dryRun

      [input, "-o", output] ->
          runWithOptions input (Just output) verbose dryRun

      _ -> do
          hPrintf stderr "Użycie   asgrnxrnx [--gadatliwie] WEJŚCIE [-o WYJŚCIE]\n\
                         \Spróbuj  asgrnxrnx --pomoc\n\n"
          exitFailure

-- | Version printing function
showVersion :: IO ()
showVersion = do
  printf "cleanasgrnx wersja %s\n" programVersion
  exitSuccess

-- | Help printing function
showHelp :: IO ()
showHelp = do
  printf "asgrnxrnx - program tworzy z pliku obserwacyjnego RINEX 3.04 ASG-EUPOS\n\
         \            plik RINEX bez nadmiarowych typów obserwacji.\n\
         \            Nadmiarowe typy obserwacji zawierają tylko dane obserwacyjne\n\
         \            wypełnione spacjami.\n\
         \            Program nie modyfikuje pliku wejściowego.\n\
         \\n\
         \Użycie: asgrnxrnx [--gadatliwie] [--na-sucho] WEJŚCIE [-o WYJŚCIE]\n\
         \Opcje:\n\
         \  -o WYJŚCIE       zapisuje wynik do WYJŚCIE,\n\
         \  -g, --gadatliwie wypisuje usunięte typy obserwacji,\n\
         \  -n, --na sucho   tryb testowy - nie zapisuje pliku,\n\
         \  -p, --pomoc      pokazuje tę pomoc,\n\
         \  -w, --wersja     pokazuje wersję programu.\
         \\n\
         \Argumenty:\n\
         \ WEJŚCIE  plik wejściowy RINEX\n\
         \ WYJŚCIE  plik wyjściowy RINEX (opcjonalny)\n\
         \\n\
         \Jeśli WYJŚCIE nie zostanie podane, program utworzy plik:\n\
         \ cleaned_WEJŚCIE\n\
         \\n\
         \Program blokuje nadpisanie pliku wejściowego i ostrzega,\n\
         \jeśli plik wyjściowy już istnieje.\n\
         \\n"
  exitSuccess

validate :: FilePath -> Maybe FilePath -> Bool -> IO FilePath
validate input mOutput dryRun = do
    inExists <- doesFileExist input
    unless inExists $ do
        hPrintf stderr "Błąd: plik wejściowy \"%s\" nie istnieje.\n" input
        exitFailure

    -- Set the output file name
    let output = case mOutput of
            Just o  -> o
            Nothing -> addPrefix "cleaned_" input    

    when (input == output && not dryRun) $ do
        hPrintf stderr "Błąd: plik wejściowy i wyjściowy nie mogą mieć tej samej nazwy.\n"
        exitFailure

    outExists <- doesFileExist output
    when (outExists && not dryRun) $ do
        printf "Plik wyjściowy \"%s\" już istnieje.\n" output
        putStr "Nadpisać? [t/n]: "
        hFlush stdout
        ans <- getLine
        when (ans /= "t" && ans /= "T") $ do
            hPrintf stderr "Przerwano - plik nie został nadpisany.\n"
            exitFailure

    return output


runWithOptions :: FilePath -> Maybe FilePath -> Bool -> Bool -> IO ()
runWithOptions input mOutput verbose dryRun = do
  
    output <- validate input mOutput dryRun
                   
    -- If everything is OK, start the proper processing
    t0 <- getCurrentTime
          
    printf "Czytam   %s\n" input      
    bs <- L8.readFile input
          
    let (diffTMap, bs') = asgrnxrnx bs

    -- Verbose                 
    when verbose $ do
      printf "Usunięte typy obserwacji:\n"
      if Map.null diffTMap
      then printf "  (nic nie usunięto)\n"
      else
          forM_ (Map.toList diffTMap) $ \(sys, obs) -> do
            printf "  System %c: " sys
            L8.putStrLn (L8.intercalate " " obs)

    -- Dry run
    when dryRun $ do
      printf "Typy obserwacji do usunięcia:\n"
      if Map.null diffTMap
      then printf "  (nie ma nic do usunięcia)\n"
      else
          forM_ (Map.toList diffTMap) $ \(sys, obs) -> do
            printf "  System %c: " sys
            L8.putStrLn (L8.intercalate " " obs)
      exitSuccess

    -- Saving a file
    printf "Zapisuję %s\n" output
    L8.writeFile output bs'
    printf "Gotowe\n"

    t <- getCurrentTime
    let diffT = diffUTCTime t t0
    when (diffT >= 0.001) $
         printf "Czas przetwarzania: %.3f s.\n" (realToFrac diffT::Double)

-- | Processing function
asgrnxrnx :: L8.ByteString -> (ObsTypesMap, L8.ByteString)
asgrnxrnx bs =
    let (d@(hdrRs , ts , _), rest) = obsRnxRead bs
        (hdrRs', ts', dataRs')     = clean d

        bs' = B.toLazyByteString (buildHeaderBS hdrRs' <> buildBodyBS dataRs')
        diffTMap =
            Map.differenceWith
                   (\old new ->
                        let gone = old \\ new
                        in if null gone then Nothing else Just gone)
                   (Map.fromList ts)
                   (Map.fromList ts')
    in -- Check ASG-EUPOS
      if not (obsRnxAsgEupos hdrRs)
      then
        errorWithoutStackTrace
          "Błąd\n\
          \Nie znaleziono pliku ASG-EPOS.\n\
          \Plik ASG-EPOS powinien zawierać w nagłówku:\n\
          \dla OBSERVER / AGENCY   :ASG-EUPOS           GUGiK\n\
          \dla PGM / RUN BY / DATE :TPP"
        -- Check unrecognized tail
      else if not (L8.null rest)
        then errorWithoutStackTrace $
               "Błąd\n\
               \Program nie rozpoznaje całego pliku. \
               \Nierozpoznany tekst zaczyna się od: \
               \\" ++ L8.unpack (L.8 take 50 rest) ++ \""
        -- Everything OK
        else (diffTMap, bs')
        
-- | Removing excess data.
clean
    :: ([ObsRnxHeaderRecord], [SysObsTypes], [ObsRnxDataRecord])
    -> ([ObsRnxHeaderRecord], [SysObsTypes], [ObsRnxDataRecord])
clean (hdrRs, ts, dataRs) =
  let 
      iMap    = positionsWithoutObsData dataRs
      -- Removal of observation types that do not have observation data.
      ts'     = deleteObsTypesWithMap iMap ts
      -- Removal of observation data containing only spaces.
      dataRs' = deleteObsDataWithMap iMap dataRs
      -- Updating header records.
      hdrRs'  = obsRnxReplaceObsTypes ts' hdrRs
                
  in  (hdrRs', ts', dataRs')

----------------------------------------------------------------------

-- | Add prefix to file name
addPrefix :: String -> FilePath -> FilePath
addPrefix prefix path =
    let (dir, file) = splitFileName path
    in dir </> (prefix ++ file)

----------------------------------------------------------------------
-- ByteString Builders
----------------------------------------------------------------------
  
-- | Build RINEX header content from list of header records
buildHeaderBS :: [ObsRnxHeaderRecord] -> B.Builder
buildHeaderBS =
    mconcat . map (\(a,b) -> B.lazyByteString a <> B.lazyByteString b <> B.char8 '\n')

-- | Build RINEX body content from list od data recores            
buildBodyBS :: [ObsRnxDataRecord] -> B.Builder
buildBodyBS =
    mconcat . concatMap buildEpochBS
  where
    buildEpochBS (epoch, recs) =
        B.lazyByteString epoch <> B.char8 '\n'
        : map buildObsRecBS recs

    buildObsRecBS (sys, prn, bs) =
        B.char8 sys
        <> B.lazyByteString prn
        <> B.lazyByteString bs
        <> B.char8 '\n'

                         
-- | Replace observation types in header records. 
obsRnxReplaceObsTypes :: [(Sys, [L8.ByteString])] -> [ObsRnxHeaderRecord] -> [ObsRnxHeaderRecord]
obsRnxReplaceObsTypes ts hdrRs =
    let
        hdrRs' = filter (\(_, fld2) -> trim fld2 /= "SYS / # / OBS TYPES") hdrRs
        hdroTs = map (\t -> (t, "SYS / # / OBS TYPES ")) (makeRnxObsTypes ts)
       -- Insert other header records at position in current header record list.
    in insertAt (idx hdrRs) hdroTs hdrRs'
    where
        idx xs = case findIndex (\(_,fld2) -> trim fld2 == "SYS / # / OBS TYPES") xs of
                   Nothing ->
                       errorWithoutStackTrace
                         "Błąd\n\
                         \Nie można w nagłówku odnaleźć\
                         \ SYS / # / OBS TYPES."
                   Just i  -> i

-- | Insert a list of header records into another list of header records.
insertAt :: Int -> [ObsRnxHeaderRecord] -> [ObsRnxHeaderRecord] -> [ObsRnxHeaderRecord]
insertAt i ys xs =
    let (ls1, ls2) = splitAt (fromIntegral i) xs
    in if null ls2 then xs else ls1 ++ ys ++ ls2
    
-- | Make rinex content for header records
makeRnxObsTypes :: [(Sys, [L8.ByteString])] -> [L8.ByteString]
makeRnxObsTypes = concatMap makeRnxSysObsTypes

-- | Format "SYS / # / OBS TYPES" records according to RINEX 3.04
--   specification Appendix A7 p. 61
formatObsLine :: Maybe Sys -> Maybe Int -> [L8.ByteString] -> L8.ByteString
formatObsLine mSys mN ts =
    let x2Num = case mN of
                    Just n  -> L8.pack (printf "%5d" n) -- first line
                    Nothing -> L8.replicate 5 ' '       -- continuation line
        sysFld  = case mSys of
                    Just sys -> sys
                    Nothing  -> ' '
        fields = L8.cons sysFld ( x2Num
                                <> L8.concat (prependSpace ts)
                                )

    in fields <> L8.replicate (60 - L8.length fields) ' '
    where prependSpace = map (" " <>)

-- | Make single constellation rinex content
makeRnxSysObsTypes :: (Sys, [L8.ByteString]) -> [L8.ByteString]
makeRnxSysObsTypes (sys, ts) =
    let n   = length ts
        chs = chunk 13 ts                         -- max obs types in line is 13
    in case chs of
         [] -> []
         (first:rest) ->
             formatObsLine (Just sys) (Just n) first :
                               [ formatObsLine Nothing Nothing ch | ch <- rest ]

-- | Splitting the list into n pieces
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
    let (a,b) = splitAt n xs
    in a : chunk n b
         

----------------------------------------------------------------------

deleteObsDataWithMap
    :: Map Sys [Int]
    -> [ObsRnxDataRecord]
    -> [ObsRnxDataRecord]
deleteObsDataWithMap iMap =
    map $ \(epoch, obsRs) ->
        (epoch, map (deleteObsRecordData iMap) obsRs)

deleteObsRecordData :: Map Sys [Int] -> ObsRecord -> ObsRecord
deleteObsRecordData iMap (sys, prn, bs) =
    case Map.lookup sys iMap of
        Nothing -> (sys, prn, bs)
        Just  i -> (sys, prn, deleteFields i bs)        

deleteFields :: [Int] -> L8.ByteString -> L8.ByteString
deleteFields is bs =
    let iSet = IntSet.fromList is
        nFields = fromIntegral (L8.length bs) `div` fieldLen :: Int
        keep i  = not (i `IntSet.member` iSet)
        extract i =
            let off = fromIntegral (i * fieldLen)
            in L8.take (fromIntegral fieldLen) (L8.drop off bs)
    in L8.concat [ extract i | i <- [0 .. nFields - 1], keep i ]


----------------------------------------------------------------------
         
deleteObsTypesWithMap
    :: Map.Map Sys [Int]
    -> [SysObsTypes]
    -> [SysObsTypes]
deleteObsTypesWithMap iMap ts =
    [ (sys, filterByIdx xs (Map.lookup sys iMap))
    | (sys, xs) <- ts
    ]
  where
    -- removes elements with indices given in Maybe [Int]
    filterByIdx :: [L8.ByteString] -> Maybe [Int] -> [L8.ByteString]
    filterByIdx xs Nothing      = xs
    filterByIdx xs (Just is) =
        let iSet = IntSet.fromList is
        in  [ x | (i, x) <- zip [0..] xs, not (i `IntSet.member` iSet) ]

----------------------------------------------------------------------

-- Checks if the i-th field (16 characters) is all spaces
isSpacesField :: L8.ByteString -> Int -> Bool
isSpacesField bs i =
    let off = fromIntegral (i * fieldLen) :: Int64
    in L8.all (== ' ')
       (L8.take (fromIntegral fieldLen) (L8.drop off bs))

positionsWithoutObsData
    :: [ObsRnxDataRecord]
    -> Map Sys [Int]                    -- ^ Map of space-only observation data indexes
positionsWithoutObsData dataRs =
    let
        -- We group records by Sys, but only keep ByteString fields
        grouped :: Map Sys [L8.ByteString]
        grouped =
            Map.fromListWith (++)
                [ (sys, [obsBs])
                | (_, obsRs) <- dataRs
                , (sys, _, obsBs) <- obsRs
                ]

        -- Analysis of one system using STUArray
        analyze :: [L8.ByteString] -> [Int]
        analyze [] = []
        analyze (r:rs) =
            let rows = r : rs
                bsLen = fromIntegral (L8.length r) :: Int
                n = bsLen `div` fieldLen
            in runST $ do
                 -- We create a mutable mask: True = empty field
                 mask <- newArray (0, n - 1) True
                         :: ST s (STUArray s Int Bool)

                 -- For each record
                 forM_ rows $ \row -> do
                   -- For each field
                   forM_ [0 .. n - 1] $ \i -> do
                     v <- readArray mask i
                     when v $ do
                       -- If the field is not a space -> set to False
                         unless (isSpacesField row i) $
                                writeArray mask i False
                 -- Replacing a mask with an index list
                 final <- getAssocs mask
                 return [ i | (i, True) <- final ]
    in
        Map.map analyze grouped

----------------------------------------------------------------------
  
-- | Checks whether it is the header of the ASG-EUPOS observator, the
--   GUGiK agency, generated by the TPP program.
obsRnxAsgEupos
    :: [ObsRnxHeaderRecord]                             -- ^ observation rinex header records
    -> Bool
obsRnxAsgEupos = (==2) . length .
    filter (\(fld1, fld2) ->
                (   observer fld1 == "ASG-EUPOS"
                 && agency   fld1 == "GUGiK"
                 && label    fld2 == "OBSERVER / AGENCY"
                ) ||
                (   pgm      fld1 == "TPP"
                 && label    fld2 == "PGM / RUN BY / DATE"
                )
           )
    where
      label    = trim
      pgm      = trim . L8.take 20
      observer = trim . L8.take 20
      agency   = trim . L8.take 40 . L8.drop 20

----------------------------------------------------------------------
-- READ RINEX
----------------------------------------------------------------------

obsRnxRead
    :: L8.ByteString
    -> (([ObsRnxHeaderRecord], [SysObsTypes], [ObsRnxDataRecord]), L8.ByteString)
obsRnxRead bs =
    let
      (hdrRs, body)  = obsRnxReadHeaderRecords bs
      ts             = obsRnxGetObsTypes hdrRs
      tMap           = Map.fromList ts
      nMap           = Map.map length tMap
      (dataRs, rest) = obsRnxReadDataRecords nMap body
    in ((hdrRs, ts, dataRs), rest)

----------------------------------------------------------------------
-- READ HEADER
----------------------------------------------------------------------

-- | Read all header records until label == "END OF HEADER" from RINEX
--   3.04 observation file.  Each record consists of two fields. Label
--   is in the second field.
obsRnxReadHeaderRecords
    :: L8.ByteString                              -- ^ rinex content
    -> ([ObsRnxHeaderRecord], L8.ByteString)      -- ^ (list of header records, rest of content)
obsRnxReadHeaderRecords bs
    | L8.null     bs           = errorWithoutStackTrace
                                   "Błąd\n\
                                   \Nie można odnaleźć END OF HEADER.\
                                   \Puste wejście."
    | rnxVer      bs /= "3.04" = errorWithoutStackTrace
                                   "Błąd\n\
                                   \Plik wejściowy nie jest w wersji 3.04."
    | rnxFileType bs /= "O"    = errorWithoutStackTrace
                                   "Błąd\n\
                                   \Plik wejściowy nie jest \
                                   \plikiem obserwacyjnym."
    | otherwise                = collect [] bs
    where
      rnxVer      = trim . takeField  0 9
      rnxFileType = trim . takeField 20 1
                    
      collect acc xs
          | label == "END OF HEADER"  =
              (reverse (r:acc), rest)
          | otherwise =
              collect (r:acc) rest
          where
            (r, rest) = obsRnxReadHeaderRecord xs
            (_, fld2) = r
            label = trim fld2

-- | Read a single header record from RINEX 3.04 observation file. The
--   header record consists of:
--   - data field   0-59,
--   - label field 60-EOL.
--   It cannot use the RINEX 3.04 specification knowledge that the
--   width of a line should be 80 characters, because last line sometimes
--   breaks this rule.
obsRnxReadHeaderRecord
    :: L8.ByteString                              -- ^ rinex content
    -> (ObsRnxHeaderRecord, L8.ByteString)        -- ^ ((data field, label field), rest of content)
obsRnxReadHeaderRecord bs
    | L8.null bs = errorWithoutStackTrace
                     "Błąd\n\
                     \Nie można odczytać rekordu nagłówka.\
                     \ Puste wejście."
    | otherwise  = 
        let
            (fld1, bs1) = L8.splitAt 60 bs
            (fld2, bs2) = L8.break (`L8.elem` "\n\r") bs1
            rest  = L8.dropWhile (`L8.elem` "\n\r") bs2
        in ((fld1, fld2), rest)

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace              

takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

-- | Get observation types list grouped by satellite system.
obsRnxGetObsTypes :: [ObsRnxHeaderRecord] -> [(Sys, [L8.ByteString])]
obsRnxGetObsTypes hdrRs =
    let ds = map fst $ filter (\(_, fld2) -> trim fld2 == "SYS / # / OBS TYPES") hdrRs
        sysGroup = groupBy (\_ b -> (L8.take 1 b) == " ") ds
    in map obsRnxGetSysObsTypes sysGroup

-- | Get satelite system observation types based on RINEX 3.04
--   observation file specification Appendix A7, p. 61
obsRnxGetSysObsTypes
    :: [L8.ByteString]                         -- ^ list of header "SYS / # / OBS TYPES" data
                                               --   for satellite system
    -> (Sys, [L8.ByteString])                  -- ^ (satelite system, [observation types])
obsRnxGetSysObsTypes []     = errorWithoutStackTrace
                                "Błąd\n\
                                \Brak rekordów nagłówka \
                                \z typami obserwacji."
obsRnxGetSysObsTypes (d:ds) =
    let sys = case L8.uncons d of
                Nothing -> errorWithoutStackTrace
                             "Błąd\n\
                             \Pusta linia nagłówka -\
                             \brak oznaczenia systemu satelitarnego."
                Just (c, _)
                  | c `L8.elem` "GREJCIS" -> c
                  | otherwise             ->
                      errorWithoutStackTrace $
                        "Błąd\n\
                        \Nieoczekiwany system satelitarny '" ++ [c]
                        ++ "' w nagłówku pliku wejściowego.\n\
                           \Dozwolone: G, R, E, J, C, I, S."
        num = let field = takeField 3 3 d
              in case L8.readInt (trim field) of
                   Just (n, _) -> n
                   Nothing -> errorWithoutStackTrace $
                                "Błąd\n\
                                \Nie udało się odczytać \
                                \liczby typów obserwacji \
                                \ dla systemu " ++ [sys]
                                ++ "."
        ts  = collect num (d:ds)
    in (sys, ts)
  where
    collect :: Int -> [L8.ByteString] -> [L8.ByteString]
    collect 0 []     = []
    collect 0 _      = errorWithoutStackTrace
                         "Błąd\n\
                         \Więcej typów obserwacji niż \
                         \zadeklarowano w nagłówku."
    collect _ []     = errorWithoutStackTrace
                         "Błąd\n\
                         \Mniej typów obserwacji niż \
                         \zadeklarowano w nagłówku."
    collect n (x:xs) =
      let ts = L8.words (L8.drop 6 x)
          l  = length ts
      in ts ++ collect (n - l) xs

----------------------------------------------------------------------
-- READ BODY
----------------------------------------------------------------------
  
-- | Read data records of RINEX 3.04 observation file. The beginning
--   of a record is recognized by reading the '>' character.
obsRnxReadDataRecords
    :: Map Sys Int                                -- ^ Number of observation types per sat system
    -> L8.ByteString                              -- ^ Observation RINEX body
    -> ([ObsRnxDataRecord], L8.ByteString)
obsRnxReadDataRecords m bs
    | L8.null bs           = ([], L8.empty)
    | L8.head bs == '>'    =
        let (r,  bs1) = obsRnxReadDataRecord  m bs
            (rs, bs2) = obsRnxReadDataRecords m bs1
        in (r:rs, bs2)
      -- Last line without data
    | isSpace (L8.head bs) =
        let bs' = L8.dropWhile isSpace bs
        in  ([], bs')
    | otherwise            =
         errorWithoutStackTrace $
           "Błąd\n\
           \Nieoczekiwany znak '" ++ [L8.head bs]
           ++ "' na początku linii."

-- | Read RINEX observation DATA record based on RINEX ver. 3.04
--   appendix A13 p. 67
obsRnxReadDataRecord
    :: Map Sys Int
    -> L8.ByteString
    -> (ObsRnxDataRecord, L8.ByteString)
obsRnxReadDataRecord m bs0 = 
    let (r, bs1) = obsRnxReadEpochRecord bs0
        (rs, bs2)       =  collectObsRecords [] bs1
    in ((r, reverse rs), bs2)
    where 
      collectObsRecords rs bs
          | L8.null bs         = (rs, bs)
          | L8.head bs == '>'  = (rs, bs)
          | otherwise  =
              let (r  , bs1) = obsRnxReadObsRecord n bs
                  (rs', bs2) = collectObsRecords (r:rs) bs1 
              in (rs', bs2)
          where
            sys  = L8.head bs
            n    = case Map.lookup sys m of
                     Just num -> num
                     Nothing  -> errorWithoutStackTrace $
                                   "Znaleziono literę '"
                                    ++ [sys] ++ "' \
                                   \w miejscu na literę \
                                   \systemu satelitarnego \
                                   \w rekordzie obserwacyjnym."

-- | Read EPOCH record as a 56-character single ByteString based on
--   RINEX ver. 3.04 appendix A13 p. 67.
obsRnxReadEpochRecord :: L8.ByteString -> (L8.ByteString, L8.ByteString)
obsRnxReadEpochRecord bs =
        let (r, bs1) = L8.splitAt 56 bs
            bs' = dropEOL . dropGarbage $ bs1
        in (r, bs')
    where
       dropGarbage = L8.dropWhile (not . (`L8.elem` "\r\n"))
       dropEOL     = L8.dropWhile        (`L8.elem` "\r\n")

-- | Read OBSERVATION record based on RINEX ver. 3.04
--   appendix A13 p. 67.       
obsRnxReadObsRecord
  :: Int                                -- ^ Number of fields observation data
  -> L8.ByteString
  -> (ObsRecord, L8.ByteString)
obsRnxReadObsRecord n bs =
    let
        (sys  , bs1) = (L8.head bs, L8.drop 1 bs)
        (prn  , bs2) = L8.splitAt 2 bs1
        (obsBS, bs3)  = L8.splitAt (fromIntegral (n * fieldLen)) bs2
        rest  = L8.dropWhile (`L8.elem` "\r\n") bs3
    in ((sys, prn, obsBS), rest)

----------------------------------------------------------------------

         
