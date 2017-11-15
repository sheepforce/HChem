import System.IO
import System.Environment
import Text.Read
import Data.Maybe
import Data.Either.Unwrap
import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import qualified Data.Chemistry.XYZ as XYZ
import qualified Data.Chemistry.BasisSet as BasisSet
import qualified Data.Chemistry.Molden as Molden
import qualified Data.Chemistry.BasisOptimisation as BasisOptimisation

-- find out what user wants to do and print info text if no or non valid choice
-- calling all other subroutines
main :: IO()
main = do
    -- read command line for task to execute
    program <- getArgs
    if (program == [])
       then hchem_help
       else case (head program) of
                 "align" -> align
                 "align-many" -> align_many
                 "interpolate" -> interpolate
                 "basconv" -> basconv
                 "bascont" -> bascont
                 _ -> hchem_help
         
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
           xyzFile_content <- TIO.readFile xyzFile
           let molecule_try = parseOnly XYZ.xyzParser xyzFile_content
           -- if content of the file is no valid XYZ content, print error
           if (isLeft molecule_try)
              then putStrLn "  not a valid XYZ file"
              else do
                  let molecule = fromRight molecule_try
                      molecule_aligned = XYZ.align (atoms !! 0, atoms !! 1, atoms !! 2) molecule
                  XYZ.printXYZ stdout molecule_aligned


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
           xyzTrajFile_content <- TIO.readFile xyzTrajFile
           let traj_try = parseOnly XYZ.xyzTrajParser xyzTrajFile_content
           if (isLeft traj_try)
              then putStrLn "  not a valid XYZ file"
              else do
                  let traj = fromRight traj_try
                      traj_aligned = map (XYZ.align (atoms !! 0, atoms !! 1, atoms !! 2)) traj
                  mapM_ (XYZ.printXYZ stdout) traj_aligned


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
           xyz1_content <- TIO.readFile xyz1File
           xyz2_content <- TIO.readFile xyz2File
           let xyz1_try = parseOnly XYZ.xyzParser xyz1_content
               xyz2_try = parseOnly XYZ.xyzParser xyz2_content
           -- are both XYZ files readable? If yes continue
           if (isLeft xyz1_try || isLeft xyz2_try)
              then putStrLn "  no valid XYZ files"
              else do
                  let xyz1 = fromRight xyz1_try
                      xyz2 = fromRight xyz2_try
                      xyzInterImages = XYZ.interpolate nImages xyz1 xyz2
                  mapM_ (XYZ.printXYZ stdout) xyzInterImages


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
           basInContent <- TIO.readFile basfile
           case informat_raw of
                "nwchem" -> do
                    case outformat_raw of
                         "bagel" -> do
                             let basparse_try = parseOnly BasisSet.nwBasisParser basInContent
                             if (isRight basparse_try)
                                then do
                                    let nwbasis = fromRight basparse_try
                                    BasisSet.printBagelBasisList stdout nwbasis
                                else do
                                    putStrLn "  not a valid NWChem basis set file"
                         _ -> basconv_help
                "gamess" -> do
                    case outformat_raw of
                         "bagel" -> do
                             let basparse_try = parseOnly BasisSet.gmsBasisParser basInContent
                             if (isRight basparse_try)
                                then do
                                    let gmsbasis = fromRight basparse_try
                                    BasisSet.printBagelBasisList stdout gmsbasis
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
           molden_file <- TIO.readFile molden_path
           
           -- try to parse the molden file
           let moldenparse_try = parseOnly Molden.moldenParser molden_file
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

    
hchem_help :: IO()
hchem_help = do
    putStrLn "HChem version 0.1"
    putStrLn "  hchem [command] [options]"
    putStrLn ""
    putStrLn "  command can be"
    putStrLn "      align        -- aligning a molecule in a specific orientation"
    putStrLn "      align-many   -- align a trajectory, each frame in specific orientation"
    putStrLn "      interpolate  -- interpolate two structures in cartesian coordinates"
    putStrLn "      basconv      -- convert a basis set to a different format"
    putStrLn "      bascont      -- recontraction and optimization of basis sets"
    

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
