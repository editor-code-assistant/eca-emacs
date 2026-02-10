FROM ubuntu:22.04

WORKDIR /tmp
RUN apt update && \
    apt install --yes wget unzip git && \
    # Install eca to path
    wget https://github.com/editor-code-assistant/eca/releases/download/0.100.1/eca-native-static-linux-amd64.zip && \
    unzip eca-native-static-linux-amd64.zip && \
    install eca /usr/local/bin/eca  && \
    # Clone a sample repo for demo
    mkdir /code && \
    git clone --depth 1 https://github.com/editor-code-assistant/eca-emacs.git /code/eca-emacs

WORKDIR /code/eca-emacs
ENTRYPOINT ["/usr/bin/bash", "--login"]
