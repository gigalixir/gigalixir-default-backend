# gigalixir-default-backend

# development
docker run -it --rm -v /etc/passwd:/etc/passwd -u `id -u`:`id -g` -v `pwd`:/app -w /app -e HOME=/app haskell /bin/bash
