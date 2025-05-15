#! /bin/bash

set -euxo pipefail

BASE_PATH="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$BASE_PATH"

# @see https://docs.docker.com/reference/cli/docker/container/run/
docker run \
	--hostname docker \
	--interactive \
	--rm \
	--user root \
	--tty \
	--volume "${BASE_PATH}:/home/ubuntu/Flex-Bison-Compiler" \
	--workdir=/home/ubuntu/Flex-Bison-Compiler \
	flex-bison-compiler:latest

echo "All done."
