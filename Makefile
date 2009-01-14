all:
	ghc --make Main.hs -o cd
clean:
	rm -fr *~ file *.o *.*~ *.hi *.s
