#!/bin/bash


# Build the container
# /usr/bin/docker build -t kotfic/emacs .

# Mount .emacs.d  at /home/kotfic/remote on the docker
# container.  Inside the container,  docker will clone
# the repo to /home/kotfic/.emacs.d and then run emacs
# See:  Dockerfile
/usr/bin/docker \
    run \
    -v /home/kotfic/.emacs.d:/home/kotfic/remote \
    -it kotfic/emacs
