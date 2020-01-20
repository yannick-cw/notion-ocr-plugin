#!/bin/sh

ssh -o StrictHostKeyChecking=no travis@$DIGITAL_OCEAN_IP_ADDRESS << ENDSSH
  docker stop notion-ocr-plugin
  docker rm notion-ocr-plugin
  echo "Tag to be deployed:"
  echo $VERSION
  docker run --name notion-ocr-plugin -p 8081:8081 -d rincewind373/notion-ocr-plugin:$VERSION
  docker image prune -a -f
ENDSSH
