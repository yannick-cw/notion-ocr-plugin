FROM ubuntu:16.04
RUN mkdir -p /opt/notion-ocr-plugin/
ARG BINARY_PATH
WORKDIR /opt/notion-ocr-plugin
RUN apt-get update && apt-get install -y \
  ca-certificates \
  tesseract-ocr \
  tesseract-ocr-eng \
  libgmp-dev
COPY "$BINARY_PATH" /opt/notion-ocr-plugin
CMD ["/opt/notion-ocr-plugin/notion-ocr-plugin-exe"]
