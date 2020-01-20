## Build binary and docker images
build: 
	BINARY_PATH=".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/notion-ocr-plugin-exe/notion-ocr-plugin-exe" docker-compose build

release:
	echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin
	docker push rincewind373/notion-ocr-plugin 
