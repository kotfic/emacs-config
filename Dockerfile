FROM ubuntu:14.04
MAINTAINER Chris Kotfila
RUN apt-get update && apt-get install -y git emacs24-nox

RUN useradd -m -p letmein kotfic
VOLUME /home/kotfic/remote


USER kotfic
WORKDIR /home/kotfic

CMD git clone remote .emacs.d && emacs --debug-init