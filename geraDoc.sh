#! /bin/sh
haddock -o doc/ -h  *.hs  -p header.txt --use-package=NewBinary --use-package=base --use-package=mtl --use-package=readline -t DiskCataloguer
