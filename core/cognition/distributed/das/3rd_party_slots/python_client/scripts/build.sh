#!/bin/bash
DIR="/app"
PROTO_DIR="/proto_files"
bash "$DIR"/scripts/build_grpc_files.sh "$PROTO_DIR"
poetry build --format wheel