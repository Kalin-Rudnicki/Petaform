
if [[ -z "$BUILD__DOCKER_APP_NAME" ]]; then
  echo "Error: Missing 'BUILD__DOCKER_APP_NAME' env var." >&2
  exit 1
fi
if [[ -z "$BUILD__DOCKER_TAG" ]]; then
  echo "Error: Missing 'BUILD__DOCKER_TAG' env var." >&2
  exit 1
fi

echo $BUILD_CFG

BUILD__BASH_SCRIPT="""
#!/bin/bash

echo \"Starting bash script\"

if [[ -z \"\$APP_VERSION\" ]]; then
  echo \"Error: Missing 'APP_VERSION' env var.\" >&2
  exit 1
fi

echo \"APP_VERSION=\$APP_VERSION\"
"""

BUILD__BASH_TEMPFILE=$(mktemp --tmpdir=.)

# echo "BUILD__BASH_TEMPFILE=$BUILD__BASH_TEMPFILE}"

echo "$BUILD__BASH_SCRIPT" >> $BUILD__BASH_TEMPFILE
chmod +x "$BUILD__BASH_TEMPFILE"

BUILD__DOCKERFILE_SCRIPT="""
FROM ubuntu:22.04

WORKDIR /app

COPY $BUILD__BASH_TEMPFILE script.sh

CMD [\"bash\", \"./script.sh\"]
"""

BUILD__DOCKERFILE_TEMPFILE=$(mktemp --tmpdir=.)

echo "$BUILD__DOCKERFILE_SCRIPT" >> $BUILD__DOCKERFILE_TEMPFILE

# docker build --file "$BUILD__DOCKERFILE_TEMPFILE" -t "$BUILD__DOCKER_APP_NAME:$BUILD__DOCKER_TAG" .

rm $BUILD__BASH_TEMPFILE
rm $BUILD__DOCKERFILE_TEMPFILE
