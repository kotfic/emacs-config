FROM ubuntu:14.04
MAINTAINER Chris Kotfila

RUN apt-get update && apt-get install -y git emacs24-nox

RUN useradd -m -p letmein kotfic

COPY . /home/kotfic/.emacs.d

RUN chown -R kotfic:kotfic /home/kotfic/.emacs.d

USER kotfic
