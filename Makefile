## Build binary and docker images
build: 
	stack build
	BINARY_PATH=".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/notion-ocr-plugin/notion-ocr-plugin-exe" docker-compose build
