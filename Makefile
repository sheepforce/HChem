# the installation prefix where it will be installed
LIB_PREFIX	= ~/.local/lib/haskell
EXE_PREFIX	= ~/.local/bin

# the Haskell compiler
HC	= ghc

# the Haskell compiler flags
HFLAGS		= -O2 -Wall
HINCLUDE	= $(INCLUDE)

# building routines
.PHONY: all
all: hchem

hchem:
	cd src && $(HC) -i$(HINCLUDE) $(HFLAGS) hchem.hs


clean: cabal_clean hchem_clean

cabal_clean:
	rm -rf dist

hchem_clean:
	cd src && rm -f *.o *.hi hchem
