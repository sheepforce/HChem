import           Data.Attoparsec.Text.Lazy
import qualified Data.Chemistry.BasisOptimisation as BasisOptimisation
import qualified Data.Chemistry.Molden            as Molden
import qualified Data.Chemistry.Parser            as Parser
import qualified Data.Chemistry.Types             as Types
import qualified Data.Chemistry.Wavefunction      as Wavefunction
import qualified Data.Chemistry.Writer            as Writer
import qualified Data.Chemistry.XYZ               as XYZ
import           Data.Either.Unwrap
import           Data.Maybe
import qualified Data.Text.IO                     as T
import qualified Data.Text as T
import           System.Environment
import           System.IO
import           Text.Read

-- find out what user wants to do and print info text if no or non valid choice
-- calling all other subroutines
main :: IO()
main = do
  -- read command line for task to execute
  program <- getArgs
  if (program == [])
    then hchem_help
    else case (head program) of
      "align"       -> align
      "align-many"  -> align_many
      "interpolate" -> interpolate
      "basconv"     -> basconv
      "bascont"     -> bascont
      "momix"       -> momix
      "trajfilter"  -> trajfilter
      _             -> hchem_help

-- align a input structure at given atoms
align :: IO()
align = do
  -- get filename and the nuber of the atoms to align from the command line
  opts <- getArgs
  let xyzFile = opts !! 1
      maybeAtoms = map (readMaybe :: String -> Maybe Int) . (drop 2) $ opts
  -- if input is OK proceed, else print help message
  if (elem Nothing maybeAtoms || length maybeAtoms /= 3)
    then do
      align_help
    else do
      let atoms = map fromJust maybeAtoms
      xyzFile_content <- T.readFile xyzFile
      let molecule_try = parseOnly XYZ.xyzParser xyzFile_content
      -- if content of the file is no valid XYZ content, print error
      if (isLeft molecule_try)
        then putStrLn "  not a valid XYZ file"
        else do
          let molecule = fromRight molecule_try
              molecule_aligned = XYZ.align (atoms !! 0, atoms !! 1, atoms !! 2) molecule
          hPutStr stdout $ Writer.write_XYZ molecule_aligned


-- same as align but for multiple frames in a XYZ trajectory
align_many :: IO()
align_many = do
  opts <- getArgs
  let xyzTrajFile = opts !! 1
      maybeAtoms = map (readMaybe :: String -> Maybe Int) . (drop 2) $ opts
  if (elem Nothing maybeAtoms || length maybeAtoms /= 3)
    then do
      align_many_help
    else do
      let atoms = map fromJust maybeAtoms
      xyzTrajFile_content <- T.readFile xyzTrajFile
      let traj_try = parseOnly XYZ.xyzTrajParser xyzTrajFile_content
      if (isLeft traj_try)
        then putStrLn "  not a valid XYZ file"
        else do
          let traj = fromRight traj_try
              traj_aligned = map (XYZ.align (atoms !! 0, atoms !! 1, atoms !! 2)) traj
          putStr $ concat . map Writer.write_XYZ $ traj_aligned


-- interpolate geometries in cartesian coordinates
-- geometries from two XYZ files and number of images from command lin
interpolate :: IO()
interpolate = do
  -- read two file names and number of images from command line
  opts <- getArgs
  let xyz1File = opts !! 1
      xyz2File = opts !! 2
      maybeNImages = (readMaybe :: String -> Maybe Int) $ opts !! 3
  -- check if input is OK, if not print help, if yes continue
  if (length opts /= 4 || isNothing maybeNImages)
    then do
      interpolate_help
    else do
      let nImages = fromJust maybeNImages
      xyz1_content <- T.readFile xyz1File
      xyz2_content <- T.readFile xyz2File
      let xyz1_try = parseOnly XYZ.xyzParser xyz1_content
          xyz2_try = parseOnly XYZ.xyzParser xyz2_content
      -- are both XYZ files readable? If yes continue
      if (isLeft xyz1_try || isLeft xyz2_try)
        then putStrLn "  no valid XYZ files"
        else do
          let xyz1 = fromRight xyz1_try
              xyz2 = fromRight xyz2_try
              xyzInterImages = XYZ.interpolate nImages xyz1 xyz2
          hPutStrLn stdout $ concat . map Writer.write_XYZ $ xyzInterImages

-- convert basis set from one format in a different one
basconv :: IO()
basconv = do
  opts <- getArgs
  let basfile = opts !! 1
      informat_raw = opts !! 2
      outformat_raw = opts !! 3
  if (length opts /= 4)
    then do
      basconv_help
    else do
      basInContent <- T.readFile basfile
      case informat_raw of
        "nwchem" -> do
          case outformat_raw of
            "bagel" -> do
               let basparse_try = parseOnly Parser.nwBasisParser basInContent
               if (isRight basparse_try)
                 then do
                   let nwbasis = fromRight basparse_try
                   Writer.write_BagelBasis stdout nwbasis
                 else do
                   putStrLn "  not a valid NWChem basis set file"
            _ -> basconv_help
        "gamess" -> do
          case outformat_raw of
            "bagel" -> do
               let basparse_try = parseOnly Parser.gmsBasisParser basInContent
               if (isRight basparse_try)
                 then do
                    let gmsbasis = fromRight basparse_try
                    Writer.write_BagelBasis stdout gmsbasis
                  else do
                    putStrLn "  not a valid GAMESS-US basis set file"
            _ -> basconv_help
        _ -> basconv_help


bascont :: IO()
bascont = do
  -- collect the options from the command line
  opts <- getArgs
  let setOfBFs = opts !! 1
      setOfMOs = opts !! 2
      renorm = opts !! 3
      molden_path = opts !! 4

      maysetOfBFs = (readMaybe :: String -> Maybe [Int]) setOfBFs
      maysetOfMOs = (readMaybe :: String -> Maybe [Int]) setOfMOs
      mayRenorm = (readMaybe :: String -> Maybe Bool) renorm

  -- check if input is complete
  if (length opts /= 5 || isNothing maysetOfBFs == True || isNothing maysetOfMOs == True || isNothing mayRenorm == True)
    then do
      bascont_help
    else do
      -- read the content of the molden file
      molden_file <- T.readFile molden_path

      -- try to parse the molden file
      let moldenparse_try = parseOnly Parser.moldenParser molden_file
          issetOfBFs = fromJust maysetOfBFs
          issetOfMOs = fromJust maysetOfMOs
          isrenorm = fromJust mayRenorm
      -- if it works, give the output
      if (isRight moldenparse_try)
        then do
          let molden = fromRight moldenparse_try
          BasisOptimisation.getContrCoeff_print stdout issetOfBFs issetOfMOs isrenorm molden
        else do
          putStrLn "  not a valid Molden file"

momix :: IO()
momix = do
  opts <- getArgs
  let moldenfile = opts !! 1
      linCombList_raw = opts !! 2
      linCombList = parseOnly parseLinCombList $ T.pack linCombList_raw
  if (length opts /= 3 || isLeft linCombList)
    then do
      momix_help
    else do
      molden_raw <- T.readFile moldenfile
      let molden = parseOnly Parser.moldenParser molden_raw
      case molden of
        Right m -> do
          let moldenNewMO = Molden.mixMMOs m (fromRight linCombList)
          putStr $ Writer.write_Molden moldenNewMO
        Left _ -> do
          putStrLn "Can not read your molden file"

trajfilter :: IO()
trajfilter = do
  opts <- getArgs
  let mayberefAtomNumber = (readMaybe :: String -> Maybe Int) $ opts !! 2
      types = opts !! 3
      maybedistance = (readMaybe :: String -> Maybe Double) $ opts !! 4
      trajFile = opts !! 1

  if (length opts /= 5 || isNothing mayberefAtomNumber || isNothing maybedistance)
    then do
      trajfilter_help
    else do
      traj_raw <- T.readFile trajFile
      let traj = parseOnly Parser.xyzTrajParser traj_raw
      case traj of
        Right t -> do
          let filteredTraj = map (XYZ.findNeighboursOfType_Distance
                (fromJust mayberefAtomNumber) types (fromJust maybedistance)) t
              emptyXYZ = Types.XYZ
                { XYZ._xyz_nAtoms = 0
                , XYZ._xyz_comment = "Empty frame, either due to no present neighbours or wrong index of reference atom"
                , XYZ._xyz_xyzcontent = []
                }
          putStr $ concat . map Writer.write_XYZ . map (fromMaybe emptyXYZ) $ filteredTraj
        Left _ -> do
          putStrLn "Could not read your trajectory"


hchem_help :: IO()
hchem_help = do
  putStrLn "HChem version 0.3"
  putStrLn "  hchem [command] [options]"
  putStrLn ""
  putStrLn "  command can be"
  putStrLn "      align        -- aligning a molecule in a specific orientation"
  putStrLn "      align-many   -- align a trajectory, each frame in specific orientation"
  putStrLn "      interpolate  -- interpolate two structures in cartesian coordinates"
  putStrLn "      basconv      -- convert a basis set to a different format"
  putStrLn "      bascont      -- recontraction and optimization of basis sets"
  putStrLn "      momix        -- arbitrary mixing of molecular orbitals from Molden files"
  putStrLn "      trajfilter   -- filter frames of a trajectory for elements within a distance of a reference atom"


align_help :: IO()
align_help = do
  putStrLn "  Usage: hchem align $filename $atom1 $atom2 $atom3"
  putStrLn "  counting starts at 0"

align_many_help :: IO()
align_many_help = do
  putStrLn "  Usage: hchem align-many $filename $atom1 $atom2 $atom3"
  putStrLn "  counting starts at 0"

interpolate_help :: IO()
interpolate_help = do
  putStrLn "  Usage: hchem interpolate $filename1 $filename2 $nImages"

basconv_help :: IO()
basconv_help = do
  putStrLn "  Usage hchem basconv $filename $inputformat $outputformat"
  putStrLn "  $inputformat can be"
  putStrLn "    nwchem"
  putStrLn "    gamess"
  putStrLn "  $outformat can be"
  putStrLn "    bagel"

bascont_help :: IO()
bascont_help = do
  putStrLn "  Usage hchem bascont $SetOfBasisFunctions $SetOfMolecularOrbitals $Renormalize $filename"
  putStrLn "  $SetOfBasisFunctions is a Haskell style list of the basis functions"
  putStrLn "  $SetOfMolecularOrbitals is a Haskell style list of molecular orbitals"
  putStrLn "  $Renormalize is either True or False"
  putStrLn "  $filename is the path to a molden file"

momix_help :: IO()
momix_help = do
  putStrLn "  Usage hchem momix $MoldenFile $WeightOrbList"
  putStrLn "  $MoldenFile is the path to a proper molden file (if in doubt use Janpas molden2molden)"
  putStrLn "  $WeightOrbList is a list of weights and orbital indices in the style of"
  putStrLn "    weight|orbital -> \"1.0|10 1.0|11\" would mix the orbitals 10 and 11 50% each"

trajfilter_help :: IO()
trajfilter_help = do
  putStrLn "  Usage hchem trajfilter $TrajectoryFile $RefAtomNumber $ElementToFind $MaximumDistance "
  putStrLn "  $TrajectoryFile is a XYZ trajectory file"
  putStrLn "  $RefAtomNumber is the index of the atom to which the distance is calculated"
  putStrLn "  $ElementToFind is the element symbol, that shall be filtered out (X for CP2K Wannier centres)"
  putStrLn "  $MaximumDistance is the maximal distance in angstrom"


--------------------------------------------------------------------------------
-- Parsers for input
--------------------------------------------------------------------------------
parseLinCombList :: Parser [(Double, Int)]
parseLinCombList = do
  linCombList <- many1 pairParser
  return linCombList
  where
    pairParser :: Parser (Double, Int)
    pairParser = do
      skipSpace
      weight <- double
      _ <- char '|'
      orb <- decimal
      return (weight, orb)
