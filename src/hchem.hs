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
    case (head program) of
         "align" -> align
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
    

hchem_help :: IO()
hchem_help = do
    putStrLn "HChem Version 0.1"
    

align_help :: IO()
align_help = do
    putStrLn "  Usage: hchem align $filename $atom1 $atom2 $atom3"
    putStrLn "  counting starts at 0"
