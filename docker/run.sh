
TAG=haskell_dev
PROJECT_DIR=/project
VOLUME=$(pwd)
COMMAND=/bin/bash
echo ${VOLUME}
docker run -v ${VOLUME}:${PROJECT_DIR} -it ${TAG} ${COMMAND}

