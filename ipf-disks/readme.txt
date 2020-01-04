IPF README.txt FILE:

IMPORTANT NOTE:
IPF is in the public domain and certain conditions apply.
The terms in which it is released are described in the section
"Notice to Non-BPA Users" in each of the IPF manuals.


For more information on IPF, go to these web sites:  
http://www.bcity.com/bpaipf
http://www.interactivepowerflow.com/

---------------------------------
Building the IPF suite of programs:

First, have installed Motif 1.24/2.01 and set up Motif Window Manager:

1. Use mkdir to create the directory /shr.  Download the file
ipf-3.24.tar.gz into directory /shr.  If you download into some other
directory, you will have to change several *.c, *.bash, .bash_profile,
and XGUI files.

   gunzip ipf-3.24.tar.gz
   tar -xvf ipf-3.24.tar

This will create the subdirectory /shr/ipf-3.24 and its subdirectories.
(If this directory already  exists, you should first rename the older
directory to something like ipf-3.24.old).

2. Create all libraries.

   cd /ipf-3.24
   cd /ipf
   ./build_libipf.bash
   cd ../ipc
   ./build_libipc.bash
   cd ../gui
   ../build_libgui.bash

3. Create all executables:  (NOTE: The make files are configured for
the Redhat 5.2 linux distribution.  A set of make files configured for
the Redhat 5.1 linux distribution is included.  Other distributions may
have different versions of f2c or may use g2c.  Edit the make files to
correspond with your version of f2c or g2c).

Compile entire IPF Package:
   Type the following:

   cd /shr/ipf-3.24
   make

    -- OR --

Compile individual parts of the IPF Package:
   Type the following:
   
   cd /shr/ipf-3.24/exe
   make -f ipfsrv.make      #! This makes the server ipf
   make -f ipfipc.make      #! This makes the interprocess communications
   make -f gui.make         #! This makes the client GUI

   rm -f ipfinc
   ln -s /shr/ipf-3.24/ipf

 The other executables you could make, but are not necessary to a basic gui,
 are:

   make -f bpf.make               #! This makes the batch ipf, useful to those
                                     who prefer launching powerflow studies from
                                     scripts
   make -f netdat.make            #! This makes the network extraction program
                                     (exports to PTI's PSS/E versions 23/24 and
				     to GE's PSLF, versions 10/11
   make -f ipf_reports.make       #! A very popular interactive program
                                     for examining a base case

   etc....

4. Do the following into the root directories of all users who will execute 
the GUI: 

   cd ~
   cp /shr/ipf-3.24/exe/XGUI .
   
Edit the .bash_profile file for each user to include the definitions for
UIDPATH, RUN_IPFSRV, IPF_SOCK_ PATH and IPFDIRS, as indicated below.  

Insert this text into the default .bash_profile file for each user.  

# ----------------------------------

mesg n

UIDPATH="/shr/ipf-3.24/exe/%U:/usr/lib/X11/uid/%U"
export UIDPATH
# path to locate help and sample data files
IPFDIRS="/shr/ipf-3.24/dat/"
# optional alternative IPFDIRS=<IPF base case directory>
IPFSRV="/shr/ipf-3.24/exe/ipfsrv"
IPFSRV_CF="shr/ipf-3.24/exe/ipfsrv"
IPF_SOCK_PATH=<IPF temporary directory to hold socket locking files>
export IPFDIRS IPFSRV IPFSRV_CF IPF_SOCK_PATH
RUN_IPFSRV="/shr/ipf-3.24/exe/ipfsrv"
export RUN_IPFSRV
rm -f IPF_EXE
ln -s /shr/ipf-3.24/exe IPF_EXE

/usr/games/fortune

# ---------------------------------


You will have to logout/login to make these changes effective on all 
terminal windows.

5. To execute the GUI,  

   cd to your user directory, and type this command:

   /shr/ipf-3.24/exe/gui

You can use the pico editor to create a small file, xgui, with this
command, use CHMOD +x to make it executable, and run it like this: ./xgui 

   To execute the batch ipf using the example test case TESTDC3.PFC,

   cd /shr/ipf-3.24/dat
   ../exe/bpf TESTDC3.PFC


IMPORTANT NOTE:
IPF is in the public domain and certain conditions apply.  
The terms in which it is released are described in the section 
"Notice to Non-BPA Users" in each of the IPF manuals.  


Last Modified:  5/17/99  11:00am by JGS


