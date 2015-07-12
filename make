#~/bin/bash
cd src
ghc -Wall -O3 Main
cd -
rm Main
ln src/Main Main
