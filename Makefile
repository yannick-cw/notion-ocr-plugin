all: build

build: client-build server-build

client-build:
	stack build && stack exec elm-gen-exe

server-build:
	stack build

run: 
	stack build && stack exec notion-ocr-service-exe
