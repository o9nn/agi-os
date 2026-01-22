#! /bin/bash
cd docker-base
docker build --tag="linkgrammar/lgbase:latest" .
cd ../docker-parser
docker build --tag="linkgrammar/lgparser:latest" .
cd ../docker-server
docker build --tag="linkgrammar/lgserver:latest" .
cd ../docker-python
docker build --tag="linkgrammar/lgpython:latest" .