#!/bin/bash

docker image build -t ghcr.io/nathansam/predicct .

docker container run -it \
  --mount type=bind,source="/Volumes/igmm/cvallejo-predicct/predicct",target="/analysis/data" \
  --mount type=bind,source="$(pwd)/docs",target="/analysis/docs" \
  ghcr.io/nathansam/predicct

docker container run  \
  --mount type=bind,source="/Volumes/igmm/cvallejo-predicct/predicct/",target="/analysis/data" \
  --mount type=bind,source="$(pwd)/docs",target="/analysis/docs" \
  --mount type=bind,source="$(pwd)/src",target="/analysis/src" \
  ghcr.io/nathansam/predicct
