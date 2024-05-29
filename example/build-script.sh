
if [[ -z "$BUILD__DOCKER_APP_NAME" ]]; then
  echo "Error: Missing 'BUILD__DOCKER_APP_NAME' env var." >&2
  exit 1
fi
if [[ -z "$BUILD__DOCKER_TAG" ]]; then
  echo "Error: Missing 'BUILD__DOCKER_TAG' env var." >&2
  exit 1
fi

BUILD__DOCKERFILE_SCRIPT="""
FROM ubuntu:22.04

RUN apt-get update && \
    apt-get install -y openjdk-11-jdk && \
    apt-get clean

WORKDIR /app

COPY example/jars/petaform-example--main--$BUILD__DOCKER_TAG.jar petaform-example.jar

CMD [\"java\", \"-jar\", \"petaform-example.jar\", \"-C=env:HARNESS_CFG\", \"--\", \"log-entry\"]
"""

BUILD__DOCKERFILE_TEMPFILE=$(mktemp --tmpdir=.)
echo "$BUILD__DOCKERFILE_SCRIPT" >> $BUILD__DOCKERFILE_TEMPFILE

export WEB_SERVER_VERSION="$BUILD__DOCKER_TAG"

sbt \
  petaform-example-project/assembly

docker build --file "$BUILD__DOCKERFILE_TEMPFILE" -t "$BUILD__DOCKER_APP_NAME:$BUILD__DOCKER_TAG" .

rm $BUILD__DOCKERFILE_TEMPFILE
