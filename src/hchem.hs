import System.IO
import System.Environment
import Text.Read
import Data.Maybe
import Data.Either.Unwrap
--import Control.Exception
import Data.Attoparsec.Text
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Chemistry.XYZ as XYZ

main :: IO()
main = do
    program <- getArgs
    if (program == [])
       then hchem_help
       else case (head program) of
                 "align" -> align
                 "align-many" -> align_many
                 _ -> hchem_help
         

align :: IO()
align = do
    opts <- getArgs
    let xyzFile = opts !! 1
        maybeAtoms = map (readMaybe :: String -> Maybe Int) . (drop 2) $ opts
    if (elem Nothing maybeAtoms || length maybeAtoms /= 3)
       then do
           align_help
       else do
           let atoms = map fromJust maybeAtoms
           xyzFile_content <- TIO.readFile xyzFile
           let molecule_try = parseOnly XYZ.xyzParser xyzFile_content
           if (isLeft molecule_try)
              then putStrLn "  not a valid XYZ file"
              else do
                  let molecule = fromRight molecule_try
                      molecule_aligned = XYZ.align (atoms !! 0, atoms !! 1, atoms !! 2) molecule
                  XYZ.printXYZ stdout molecule_aligned


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

hchem_help :: IO()
hchem_help = do
    putStrLn "HChem version 0.1"
    putStrLn "  hchem [command] [options]"
    putStrLn ""
    putStrLn "  command can be"
    putStrLn "      align -- aligning a molecule in a specific orientation"
    

align_help :: IO()
align_help = do
    putStrLn "  Usage: hchem align $filename $atom1 $atom2 $atom3"
    putStrLn "  counting starts at 0"

align_many_help :: IO()
align_many_help = do
    putStrLn "  Usage: hchem align-many $filename $atom1 $atom2 $atom3"
    putStrLn "  counting starts at 0"    
