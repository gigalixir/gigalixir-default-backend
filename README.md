# gigalixir-default-backend

# development
docker run -it --rm -p 4001:8080 -v /etc/passwd:/etc/passwd -u `id -u`:`id -g` -v `pwd`:/app -w /app -e HOME=/app haskell /bin/bash
stack build --fast --pedantic
stack exec gigalixir-default-backend-exe

# build
docker build . -t us.gcr.io/gigalixir-152404/default-backend

# test
docker run --rm -P -e APIKEY=REDACTED us.gcr.io/gigalixir-152404/default-backend

# deploy
gcloud docker -- push us.gcr.io/gigalixir-152404/default-backend
