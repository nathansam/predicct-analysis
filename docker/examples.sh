#!/bin/bash

docker image build -t ghcr.io/nathansam/predicct .

docker buildx build . -t ghcr.io/nathansam/predicct:latest --platform linux/arm64,linux/amd64 --cpuset-cpus 0-9 --push

docker container run  \
  --mount type=bind,source="/Volumes/igmm/cvallejo-predicct/predicct/",target="/analysis/data" \
  --mount type=bind,source="$(pwd)/docs",target="/analysis/docs" \
  --mount type=bind,source="$(pwd)/src",target="/analysis/src" \
  ghcr.io/nathansam/predicct
