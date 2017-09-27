# HChem -- Tools for working on chemical data
--------------
## Installation
Install the Glasgow Haskell compiler, BLAS and cabal as system dependencies, for example on Debian:

    aptitude install ghc cabal libopenblas-dev

Use cabal to install haskell-packages:

    cabal install hmatrix either-unwrap attoparsec text
   
   and then install the [chemical data for Haskell](https://github.com/sheepforce/Haskell_Data.Chemistry) and the HChem package with cabal

    # install HChem-libs
    git clone https://github.com/sheepforce/Haskell_Data.Chemistry.git
    cd Haskell_Data.Chemistry/
    cabal configure && cabal build
    cabal install
    
    # install HChem
    cd $HOME
    git clone https://github.com/sheepforce/Haskell_Data.Chemistry.git
    cd HChem
    cabal configure && cabal build
    cabal install

---------------
## Usage
HChem can currently only handle XYZ files. Following operations are supported

 - align structure in a specific orientation
 - align trajectory
 - interpolate structures in cartesian coordinates
 - convert basis set formats

Calling is easy

    hchem [command] [option]
If you enter help or non existing commands it prints info text. You get more specific help if you just enter the command. For example to align a structure in `Molecule.xyz` so that atom 2 has x, y, z = 0, atom 4 has x, y = 0 and atom 5 has z = 0 enter

    hchem align Molecule.xyz 2 4 5
