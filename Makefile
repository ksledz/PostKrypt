
all: postkrypt_converter
	
TestTransform: src/Lib.hs test/TestTransform.hs test/Mon.hs
	ghc -isrc:test --make test/TestTransform.hs -o TestTransform

postkrypt_converter: src/Lib.hs src/Main.hs test/Mon.hs
	ghc -isrc:test --make src/Main.hs -o postkrypt_converter


clean:
	-rm -f src/*.bak src/*.hi src/*.o test/*.hi test/*.o postkrypt_converter TestTransform *tar.gz

dist: 
	tar -czf ks386105.tar.gz src/Main.hs src/Lib.hs
