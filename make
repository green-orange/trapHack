#~/bin/bash
cd src
ghc -Wall -fno-warn-orphans -O3 Main
cd -
rm Main
ln src/Main Main
