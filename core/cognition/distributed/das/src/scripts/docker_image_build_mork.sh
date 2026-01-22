#!/bin/bash -x
VERSION="${VERSION:-0.10.2}"
docker buildx build -t trueagi/das:mork-server-${VERSION} --load -f src/docker/mork/Dockerfile.server .
docker buildx build -t trueagi/das:mork-loader-${VERSION} --load -f src/docker/mork/Dockerfile.loader .