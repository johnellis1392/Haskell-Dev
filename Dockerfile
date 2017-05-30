FROM haskell:latest
ENV dir /project

RUN mkdir ${dir}
VOLUME ${dir}
WORKDIR ${dir}

RUN stack setup
RUN /bin/bash

