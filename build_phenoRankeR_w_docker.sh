#!/bin/bash

# Configuration
IMAGE_NAME="your-image-name"
CONTAINER_NAME="your-container-name"
HOST_PATH="/path_on_host"
CONTAINER_PATH="/path_to_package_in_container"

# Step 1: Build the Docker Image
echo "Building Docker image..."
docker build -t $IMAGE_NAME .

# Step 2: Run the Docker Container with a Mounted Volume
echo "Running Docker container with volume mounted..."
docker run -d --name $CONTAINER_NAME -v $HOST_PATH:$CONTAINER_PATH $IMAGE_NAME

# Optional: Wait for the container to finish its task
# echo "Waiting for the container to complete its task..."
# docker wait $CONTAINER_NAME

# Optional: Copying files to the host (if not automatically done by the container)
# docker cp $CONTAINER_NAME:$CONTAINER_PATH/* $HOST_PATH/

# Optional: Stop and Remove the Container
echo "Stopping and removing container..."
docker stop $CONTAINER_NAME
docker rm $CONTAINER_NAME

echo "Process completed."
