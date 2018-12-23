FROM haskell
EXPOSE 8080
COPY ./ /app
WORKDIR /app
RUN stack build --fast --pedantic
ENTRYPOINT ["stack", "exec", "gigalixir-default-backend-exe"]

# static binary? segfaults..
# WORKDIR /usr/lib/gcc/x86_64-linux-gnu/6
# RUN cp crtbeginT.o crtbeginT.o.orig
# RUN cp crtbeginS.o crtbeginT.o
# RUN stack --local-bin-path /sbin install --test --ghc-options '-optl-static -fPIC'


