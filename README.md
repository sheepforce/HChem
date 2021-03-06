# HChem -- Tools for working on chemical data


## Installation
Install the Glasgow Haskell compiler, BLAS and cabal and/or stack as system dependencies, for example on Debian:

    aptitude install ghc cabal haskell-stack libopenblas-dev

You can use cabal to install the packages. First install the dependencies

    cabal install hmatrix either-unwrap attoparsec text

and then install the [chemical data for Haskell](https://github.com/sheepforce/Haskell_Data.Chemistry) and the HChem package with cabal

    # install HChem-libs
    git clone https://github.com/sheepforce/Haskell_Data.Chemistry.git
    cd Haskell_Data.Chemistry/
    cabal configure && cabal build
    cabal install

    # install HChem
    cd $HOME
    git clone https://github.com/sheepforce/HChem.git
    cd HChem
    cabal configure && cabal build
    cabal install

Or (to avoid version conflicts) use stack

    git clone https://github.com/sheepforce/HChem.git
    cd HChem
    stack setup
    stack install


## Usage
HChem is able to

 - align structure in a specific orientation
 - align trajectory
 - interpolate structures in cartesian coordinates
 - convert basis set formats (NWChem & GAMESS-US -> BAGELs JSON)
 - recontract a basis set from a atomic calculation

Calling is easy

    hchem [command] [option]
If you enter help or non existing commands it prints info text. You get more specific help if you just enter the command. For example
 - to align a structure in `Molecule.xyz` so that atom 2 has x, y, z = 0, atom 4 has x, y = 0 and atom 5 has z = 0 enter

    hchem align Molecule.xyz 2 4 5

 - to find contraction coefficients from am molden file `Molecule.molden`, where the molecular orbitals 0,1,2 are used to find contraction coefficients for the basis functions 4,5 without renormalisation

    hchem bascont [0,1,2] [4,5] False Molecule.molden
