FROM alpine:3.17.0
WORKDIR /opt/bpa
COPY . .
# RUN yum -y install gcc gcc-gfortran motif motif-devel make cmake
RUN apk update && apk add --no-cache build-base cmake gcc gfortran motif
# libgcc libtool linux-headers musl-dev ninja tar unzip wget bash bash-completion gdb
WORKDIR /opt/bpa/build
RUN cmake ..
RUN make
RUN make install

WORKDIR /opt/bpa/data
CMD ["bpf"]
