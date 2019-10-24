FROM ubuntu:eoan

RUN apt-get update && apt-get -y install yamllint python3-pkg-resources

WORKDIR /app

COPY support/yamllint/run-yamllint /app/   
COPY milo.yaml /app/   

ENTRYPOINT ["/app/run-yamllint"]
