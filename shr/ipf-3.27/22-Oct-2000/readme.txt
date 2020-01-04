These instructions have three parts.  Part I updates and rebuilds ipf. Part II is optional.
It modifies some pvcurve data to make it execute more efficiently. Part III can be ignored
completely.  It describes in some detail the changes made to the ipf modules and pvcurve 
data.

--------------------------------------------------------------------------------------
Part I.


The ipf rebuild must be done as su root. The ftp retrieval may be performed under any user
name. 

1. Change to su root, cd to the ipf source directory, create a unique subdirectory, 
   cd to that directory, and FTP the latest ipf changes.  Note that ftp must be in binary 
   mode. ASCII mode will corrupt the file such that it cannot be un-tarred. 

   cd /shr/ipf-3.27/ipf
   mkdir 22-Oct-2000
   cd 22-Oct-2000 
   ftp                              # you can use other ftp programs
     > open <brick>
     > login:
     > password:
     > cd /web/ext/ftp/outgoing/gen-team
     > binary
     > get pf_changes.tar
     > close
     > quit
   tar -xvf ipf_changes.tar

2. Return to the IPF source directory and create backup versions of all source files which 
   will be modified.

   cd ..                        # You are now in /shr/ipf-3.27/ipf
   cp comdrp.f comdrp.f.0
   cp gendrp.inc gendrp.inc.0
   cp getdrp.f getdrp.f.0
   cp p_gtdata.f p_gtdata.f.0
   cp srv_fcmdprs.f srv_fcmdprs.f.0

3. Copy the new modules from the new subdirectory into this directory.

   cp 22-Oct-2000/comdrp.f .
   cp 22-Oct-2000/gendrp.inc .
   cp 22-Oct-2000/getdrp.f .
   cp 22-Oct-2000/p_gtdata.f .
   cp 22-Oct-2000/srv_fcmdprs.f .

3. Rebuild the entire ipf-3.27 suite.

   cd ..               # You are now in /shr/ipf-3.27
   make clean
   make

4. Copy some of the new executables to the /usr/bin directory.

   cp exe/bpf /usr/bin
   cp exe/ipfsrv /usr/bin
   cp ipf/22-Oct-2000/tkdiff /usr/bin  # this is optional

5. This step is optional. CD to the new subdirectory, copy tkdiff, which is a nifty 
   text difference program to /usr/bin, and examine the data changes I made to n1a.lis.

   cd ipf/22-Oct-2000     # you are in subdirectory /shr/ipf-3.27/ipf/22-Oct-2000
   cp tkdiff /usr/bin
   tkdiff n1a.lis n1a.lis.0

--------------------------------------------------------------------------------------
Part II.
Instructions for modifying the pvcurce data.  

This is optional.  The changes made in the ipf rebuild above will correct the problem 
of excessive *.pfo and *.pfd file size.  The changes suggested here will further 
reduce those files and improve the performance slightly. 

1. CD to the directory containing the pvcurve data.

2. Modify the COMMON_MODE_DATA file referenced in each *.trc file. For James case
   pv-b1001-n1a.trc, the file is n1a.lis.  In this file, n1a.lis, modify each instance 
   of /CHANGE_BUS_TYPES and /GEN_DROP as show.

   /GEN_DROP,AI_CONTROL=OFF,SUMMARY=OFF,INITIAL_DROP=50,TOL=10
   /CHANGE_BUS_TYPES, BQ=B,BG=BQ,BX=B,LTC=OFF,LIST=OFF,FILE=CHANGEBUS.TXT

This file contains 35 instances of each command. 

Caution.  If you converted all your pv data using the set of commands

   dos-linux.bash '*.TRC *.trc'
   dos-linux.bash '*.LIS *.lis'
   dos-linux.bash '*.TXT *.txt'
   dos-linux.bash '*.DAT *.dat'

then all your file names are lower case.  The file name in the example 

   /CHANGE_BUS_TYPES, ...,FILE=CHANGEBUS.TXT

should list that file as "changebus.txt".

Before these changes, James pv-b1001-n1a.trc file generated two huge ipfsrv output files
 -- root.pfo and root.pfd.  These files were 900,000,000 and 400,000,000 bytes 
respectively.  After the changes, they never exceeded 500,000 bytes.  

The VAX version suffers the same problem.

--------------------------------------------------------------------------------------
Part III.
Description of changes made to ipf modules and pvcurve data.

1. In the COMMON_MODE_FILE, added a new option (SUMMARY=OFF) was added to all
   instances of the /GEN_DROP to supress the GEN_DROP summary listing. 

   The name of the COMMON_MODE_FILE is given in  pv-b1001-n1a.trc.

   The change affected ipf modules comdrp.f, gendrp.inc, and getdrp.f.

   To implement this change, modify all instances of /GEN_DROP COMMON_MODE_FILE 
   (n1a.lis). The original and revised commands are listed below.

   /GEN_DROP,AI_CONTROL=OFF,INITIAL_DROP=50,TOL=10
   /GEN_DROP,AI_CONTROL=OFF,SUMMARY=OFF,INITIAL_DROP=50,TOL=10

2. In the same COMMON_MODE_FILE mentioned above, invoked an existing option to supress 
   the summary of the /CHANGE_BUS_TYPES. The original and revised commands are listed 
   below.

   /CHANGE_BUS_TYPES, BQ=B,BG=BQ,BX=B,LTC=OFF,FILE=CHANGEBUS.TXT
   /CHANGE_BUS_TYPES, BQ=B,BG=BQ,BX=B,LTC=OFF,LIST=OFF,FILE=CHANGEBUS.TXT

3. Added code to rewind the *.pfo, *.pfd, and *.pff file in IPFSRV following each loading 
   of an OLD_BASE file.  Before this change, the root.pfo and root.pfd files contained 
   the entire summary of 5385 solved cases (4,000 and 1500 lines respectively in each 
   case).  The file sizes for root.pfo and root.pfd before these changes were 850,000,000
   bytes and 450,000,000 bytes respectively.  After the changes (module srv_fcmdprs.f),
   the file sizes were 450,000 and 450,000 bytes each. 
   
   With this revision, the *.pfo, *.pfd, and *.pff files reflect only the processing 
   following loading the last OLD_BASE  file.

4. Remove some debug printout in p_gtdata which accompanies USER_ANALYSIS processing.


wlp 21-Oct-2000
