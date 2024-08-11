ARG OS_VERSION=14.04.2
FROM ubuntu:$OS_VERSION

RUN apt update && apt upgrade -y
RUN apt install make git opam libgmp-dev -y
RUN opam init --disable-sandboxing || opam init -y
WORKDIR /usr/apps/main

ARG OCAML_VERSION=4.05.0

COPY chapar chapar
RUN cd chapar && eval $(opam config env) && dune exec mex client