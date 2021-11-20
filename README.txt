# gigalixir-default-backend

# development
docker run -it --rm -p 4001:8080 -v /etc/passwd:/etc/passwd -u `id -u`:`id -g` -v `pwd`:/app -w /app -e HOME=/app haskell /bin/bash
stack build --fast --pedantic
stack test
APIKEY=REDACTED stack exec gigalixir-default-backend-exe
curl -i localhost:4001 -H 'Host: bar.gigalixirapp.com' -H 'X-Code: 504'

# build
# doesn't work anymore?: docker build . -t us.gcr.io/gigalixir-152404/default-backend-build-env
# run the development commands above to "build" the executable
# cp .stack-work/install/x86_64-linux/lts-12.7/8.4.3/bin/gigalixir-default-backend-exe gigalixir-default-backend-exe
# docker build . -t us.gcr.io/gigalixir-152404/default-backend -f Dockerfile.run
docker build . -t us.gcr.io/gigalixir-152404/default-backend

# test
docker run --rm -p 4001:8080 -e APIKEY=REDACTED us.gcr.io/gigalixir-152404/default-backend
curl -i localhost:4001 -H 'Host: bar.gigalixirapp.com' -H 'X-Code: 504'

# deploy
gcloud docker -- push us.gcr.io/gigalixir-152404/default-backend
