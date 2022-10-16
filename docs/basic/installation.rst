************
Installation
************
IPF is installed by compiling from source. This is probably easiest using Docker approach 
outlined below, but compiling manually from source is also an option.

Docker
======
You can use docker to build and run this project. Just build the container:

.. code::

    $ git clone https://github.com/mbheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ docker build -t bpa-ipf-tsp .

Then run it, the container will run ``bpf`` by default. So once the container is started, 
you can just type the name of the file in the ``data`` directory that you'd like to run 
(e.g. ``bench.pfc``):

.. code::

    $ docker run -it --rm bpa-ipf-tsp
     BPA POWER FLOW PROGRAM VERSION:IPF  327
     Enter Power Flow Control (PFC) file name > bench.pfc

Otherwise start an interactive tty with ``bash`` to run other processes.

.. code::

    $ docker run -it --rm bpa-ipf-tsp bash
    [root@e9d28ac4f070 data]# bpf bench.pfc
    [root@e9d28ac4f070 data]# tsp bench.fil

Compiling Manually
==================
If Docker isn't an option, compile manually. The majority of this codebase is written in
Fortran with some C so both Fortran and C compiler are needed in order to compile it. 
Also, note that to this point it has only been test compiled on CentOS/RHEL 7 (gcc 4.8.5) 
and Ubuntu 20.04 (gcc 9.4.0).

Install the build tool (`CMake`_), compilers, and dependencies (`Motif X Window`_):

**CentOS/RHEL**

.. code::

    $ yum install cmake gcc gcc-gfortran motif motif-devel

**Ubuntu**

.. code::

    $ apt-get install cmake gcc gfortran libmotif-dev libxmu-dev

Then you're ready to compile. This project uses `CMake`_. CMake is a multi-platform build tool that 
can generate build files for many different target platforms. CMake recommends doing "out of 
source" builds, that is, the build files and artifacts are separated from the source files. This is 
convenient when doing development because there is no need to clean out compiled stuff (e.g. object
files, libraries, executables, etc.) from the source tree. To do this, you create a ``build/`` 
directory at the top level of the project and everything gets built there. This allows you to just 
delete the ``build/`` directory when you're done. 

Doing a checkout and compile of this repository is done as follows:

.. code::

    $ git clone https://github.com/mbheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install
    
After running these commands, you will see the library binaries in a ``lib/`` directory and 
the executables in a ``bin/`` directory. The ``sudo make install`` command will also place 
the binaries in ``/usr/local/bin``, which should be in your path.

Debug
-----
To build with debug symbols, do ``cmake -DCMAKE_BUILD_TYPE=Debug ..`` instead of ``cmake ..``.

Testing
-------
You can run the test suite with ``ctest -C <Build Type>```:

.. code::

    $ ctest -C Release

.. _CMake: http://www.cmake.org
.. _Motif X Window: https://motif.ics.com/motif