FROM centos:7.9.2009 as build-stage
WORKDIR /app
COPY . .
RUN yum -y install gcc gcc-gfortran motif motif-devel make cmake
# RUN apk update && apk add --no-cache build-base cmake gcc gfortran
# libgcc libtool linux-headers musl-dev ninja tar unzip wget bash bash-completion gdb
WORKDIR /app/build
RUN cmake ..
RUN make
RUN make install

WORKDIR /app/data
CMD ["bpf"]