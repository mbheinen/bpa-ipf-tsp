************
Installation
************
The majority of this codebase is Fortran with some C. Both Fortran and C compiler are needed in order to compile it. Also, note that to this point it has only been test compiled on CentOS/RHEL 7 (gcc 4.8.5) and Ubuntu 20.04 (gcc 9.4.0).

Docker
======
You can use docker to build and run this project. Just build the container:

.. code::

    $ git clone https://github.com/mbheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ docker build -t bpa-ipf-tsp .

Then run it, the container will run ``bpf`` by default. So once the container is started, you can just type the name of the file in the ``data`` directory that you'd like to run (e.g. ``bench.pfc``):

.. code::

    $ docker run -it --rm bpa-ipf-tsp
     BPA POWER FLOW PROGRAM VERSION:IPF  327
     Enter Power Flow Control (PFC) file name > bench.pfc

Otherwise start an interactive tty with ``bash`` to run other processes.

.. code::

    $ docker run -it --rm bpa-ipf-tsp bash
    [root@e9d28ac4f070 data]# bpf bench.pfc

Standard
========

CentOS/RHEL
-----------
To get the compilers:

.. code::

    $ yum install gcc
    $ yum install gcc-gfortran

If you will be building the GUI as well, you will also need to install Motif X Window

.. code::

    $ yum install motif
    $ yum install motif-devel

Ubuntu
------
To get the compilers:

.. code::

    $ apt-get install gcc
    $ apt-get install gfortran

If you will be building the GUI as well, you will also need to install Motif X Window

.. code::

    $ apt-get install libmotif-dev
    $ apt-get install libxmu-dev

Compiling
---------
This project uses `CMake`_. CMake is a multi-platform build tool that can generate build files for many different target platforms. CMake recommends doing "out of source" builds, that is, the build files and artifacts are separated from the source files. This is convenient when doing development because there is no need to clean out compiled stuff (e.g. object files, libraries, executables, etc.) from the source tree. To do this, you create a ``build/`` directory at the top level of the project and everything gets built there. This allows you to just delete the ``build/`` directory when you're done. Doing a checkout and compile of this repository is done as follows:

.. code::

    $ git clone https://github.com/mbheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install
    
After building, you will see the library binaries in ``lib/`` and the executables in ``bin/``. To build with debug symbols, do `cmake -DCMAKE_BUILD_TYPE=Debug ..` instead.

Testing
-------
You can run test suite with `ctest -C <Build Type>`:

.. code::

    $ ctest -C Release

.. _CMake: http://www.cmake.org