************
Installation
************
The majority of this codebase is Fortran with some C. Both Fortran and C compiler are needed in order to compile it. Also, note that to this point it has only been test compiled on CentOS and Ubuntu. 

CentOS/RHEL
===========
To get the compilers:

.. code::

    $ yum install gcc
    $ yum install gcc-gfortran

If you will be building the GUI as well, you will also need to install Motif X Window

.. code::

    $ yum install motif
    $ yum install motif-devel

Ubuntu
======
To get the compilers:

.. code::

    $ apt-get install gcc
    $ apt-get install gfortran

If you will be building the GUI as well, you will also need to install Motif X Window

.. code::

    $ apt-get install libmotif-dev
    $ apt-get install libxmu-dev

This project uses `CMake`_. CMake is a multi-platform build tool that can generate build files for many different target platforms. CMake recommends doing "out of source" builds, that is, the build files are separated from your sources. This is convenient when doing development because there is no need to clean out compiled stuff (e.g. object files and executables) from the source tree. To do this, you create a ``build/`` directory at the top level of the project and everything gets built there. This allows you to just delete the ``build/`` directory when you're done. Doing a checkout and compile of this repository is done as follows:


.. code::

    $ git clone https://github.com/mbheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install
    
After building, you will see the library binaries in ``lib/`` and the executables in ``bin/``.

.. _CMake: http://www.cmake.org