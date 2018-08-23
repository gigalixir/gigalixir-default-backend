# gigalixir-default-backend

# development
docker run -it --rm -p 4001:8080 -v /etc/passwd:/etc/passwd -u `id -u`:`id -g` -v `pwd`:/app -w /app -e HOME=/app haskell /bin/bash
stack build --fast --pedantic
stack exec gigalixir-default-backend-exe
