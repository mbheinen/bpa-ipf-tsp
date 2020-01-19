# Interactive Power Flow & Transient Stability Program
This is a fork of the Bonneville Power Administration's Interactive Power Flow (IPF) and Transient Stability Program (TSP). Both of these are public domain and were originally obtained from ftp://ftp.bpa.gov (which no longer seems to be available). The goal is to get this codebase to a point where it can be compiled and run in hopes to use it for benchmarking future projects related to power flow or transient stability analysis.

Note that the original programs had a GUI component, but given how dated it is, it is very unlikely it will ever run without significant effort. The initial goal is to get the power flow and transient programs operational enough to be able to be used from the command line and later see if original GUI components can be made to work again.

# Documentation
The best place for a thorough description of the original BPA IPF and TSP applications is the [manuals](https://github.com/mbheinen/bpa-ipf-tsp/tree/master/manuals) in this repo. Additionally, check out John Schaad's (one of the original creators) [website](http://members.efn.org/~jschaad/ipf-1.html).

# Dependencies
The majority of this codebase is Fortran with some links to C. Both Fortran and C compiler are needed in order to compile it. Also note that to this point it has only been test compiled on CentOS Linux.

    $ yum install gcc
    $ yum install gcc-gfortran

# Building
This project uses CMake. CMake is a multi-platform build tool that can generate build files for many different target platforms. See more info at http://www.cmake.org. CMake recommends doing "out of source" builds, that is, the build files are separated from your sources. This is convenient when doing development because there is no need to clean out compiled stuff (e.g. object files and executables) from the source tree. To do this, you create a `build/` directory at the top level of the project and everything gets built there. This allows you to just delete the `build/` directory when you're done. Doing a checkout and compile of this repository is done as follows:

    $ git clone https://github.com/mheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install
    
Libraries are created in `lib/`. The executables are created in `bin/`.  

# Running
After building as shown above, test it out using the bench test case found in the `data/` directory by running. To run powerflow on the bench case just run:

    $ bpf bench.pfc

# Test Cases
There are a variety of test cases in the `data/` directory. Some of them came from original IPF code base others came from public/synthetic cases like Texas A&M's set of databases found [here](https://electricgrids.engr.tamu.edu/electric-grid-test-cases/). None of the data is from live/real power system networks since such information is generally considered confidential by Transmission Owners.
