# Interactive Power Flow & Transient Stability Program
This is a fork of the Bonneville Power Administration's Interactive Power Flow (IPF) and Transient Stability Program (TSP). Both of these are public domain and were originally obtained from ftp://ftp.bpa.gov (which no longer seems to be available). The goal is to get this codebase to a point where it can be compiled and run in hopes to use it for benchmarking future projects related to power flow or transient stability analysis.

Note that the original programs had a GUI component, but given how dated it is, it is very unlikely it will ever run without significant effort. The initial goal is to get these ipf and tsp programs operational enough to be able to be used from the command line.

# Documentation
The best place for a thorough description of the original BPA IPF and TSP applications is the [manuals](https://github.com/mbheinen/bpa-ipf-tsp/tree/master/manuals) in this repo. Additionally, check out John Schaad's (one of the original creators) [website](http://members.efn.org/~jschaad/ipf-1.html).

# Dependencies
The majority of this codebase is Fortran with some links to C. Both Fortran and C compiler are needed in order to compile it.

    $ yum install gcc
    $ yum install gcc-gfortran

# Building
This project uses CMake. CMake is a multi-platform build tool that can generate build files for many different target platforms. See more info at http://www.cmake.org. CMake allows/recommends doing "out of source" builds, that is, the build files are separated from your sources, so there is no need to clean out compiled stuff (e.g. object files and executables) from the source tree. To do this, you create a `build` directory at the top level of the project and everything gets built there. This allows you to just delete the `build` directory when you're done.

    $ git clone https://github.com/mheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install
    
Libraries are created in `lib/`.  The executables are created in `bin/`.  

# Running
Currently, only the Batch Power Flow (bpf) executable compiles (and only on linux). After building as shown above, test it out using the bench test case found in  `data` directory by running.

    $ bpf bench.pfc
