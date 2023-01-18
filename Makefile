
pipemath: pipemath.hs Makefile
	ghc -O2 -dynamic -split-sections pipemath
	strip -s pipemath
	upx --best ./pipemath

clean:
	rm -rf pipemath.{o,hi}
