VERSION := $(shell cat package.yaml | grep version | cut -d ' ' -f 2)

## Build binary and docker images
build: 
	BINARY_PATH=".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/notion-ocr-plugin-exe/notion-ocr-plugin-exe" VERSION=${VERSION} docker-compose build

release:
	echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin
	docker push rincewind373/notion-ocr-plugin:${VERSION}
	VERSION=${VERSION} ./deploy.sh
