all: build

build: src/*.idr
	idris --build vindinium.ipkg

clean:
	idris --clean vindinium.ipkg
