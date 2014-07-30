all: install

install: build
	idris --install vindinium.ipkg

build: src/*.idr
	idris --build vindinium.ipkg

clean:
	idris --clean vindinium.ipkg
