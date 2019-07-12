# Base image for Google Cloud Build build steps.
#
# Includes the following tools
#
# - ghc v8.6.3
# - stack v1.9.1
# - gcloud
# - gsutil
# - docker client v18.09.2
# - docker-compose v1.23.2

FROM haskell:8.4.4

ENTRYPOINT []

# Install `gcloud` and `gsutil`
ADD https://packages.cloud.google.com/apt/doc/apt-key.gpg google-apt-key.gpg
RUN \
  echo "deb http://packages.cloud.google.com/apt cloud-sdk-stretch main" \
    > /etc/apt/sources.list.d/google-cloud-sdk.list && \
  apt-key add google-apt-key.gpg && \
  apt-get update -yqq && \
  apt-get -yq --no-install-recommends install \
    build-essential zip unzip rpm \
    libleveldb-dev leveldb-doc openssh-client expat libexpat1-dev libpq-dev liblzma-dev xz-utils \
    google-cloud-sdk \
    python \
    postgresql && \
  rm -rf /var/lib/apt/lists/*

RUN stack upgrade

# Install `docker`
ADD https://download.docker.com/linux/static/stable/x86_64/docker-18.09.2.tgz docker.tgz
RUN \
  tar --extract --file docker.tgz --strip-components 1 --directory /usr/local/bin/ && \
  rm docker.tgz

# Install go set GOPATH etc
ADD https://storage.googleapis.com/golang/go1.8.linux-amd64.tar.gz go.tar.gz
RUN \
  tar -xvf go.tar.gz && \
  mv go /usr/local/ && \
  chmod +x /usr/local/go/bin/*

ENV GOPATH /go
ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

# Install ghr
RUN go get github.com/tcnksm/ghr
