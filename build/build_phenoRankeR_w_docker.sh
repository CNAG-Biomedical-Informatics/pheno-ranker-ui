#!/bin/bash

# IMAGE_NAME="leistivo/pheno-ranker-ui-r-package-builder:latest"
IMAGE_NAME="phenorankerui_package_builder:latest"

HOST_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )" # directory of the script
CONTAINER_PATH="/opt/build"

cd $HOST_PATH
echo "Current directory: $HOST_PATH"

echo "Go to the root directory of the project"
cd ../

echo get the pheno-rankerui-package builder docker image...
#docker build -f Dockerfile_package_builder -t phenorankerui_package_builder:latest .
docker pull $IMAGE_NAME

echo "Starting container..."
container_id=$(docker run -d $IMAGE_NAME)

new_dir=$(date +%Y%m%d_%H%M%S)
mkdir $HOST_PATH/$new_dir

echo "Copying build artifacts from container to host..."
docker cp "${container_id}:${CONTAINER_PATH}/." $HOST_PATH/$new_dir
docker wait $container_id

echo "Stopping and removing container..."
docker stop $container_id
docker rm $container_id

echo "Copying build package to phenoRankeR/deploy..."
cp $HOST_PATH/$new_dir/*.tar.gz phenoRankeR/deploy/.

# build the newest docker image
phenoRankeR_VERSION=$(ls $HOST_PATH/$new_dir/*.tar.gz | sed 's/.*_\(.*\)\.tar\.gz/\1/')
echo "Building docker image with version: $phenoRankeR_VERSION"
cd phenoRankeR/deploy && docker build -f Dockerfile -t phenorankerui:${phenoRankeR_VERSION} .

echo "Process completed."
