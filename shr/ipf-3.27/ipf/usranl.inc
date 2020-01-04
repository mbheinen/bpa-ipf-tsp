C
      integer MAXUSRANL
      parameter (MAXUSRANL = 1500)

      common /usranl/  numusr, lunusr, numdef(20), numtxt(20),
     &                 usrdbg(20), numupx(20), cmnflg(20)
      integer usrdbg
      logical cmnflg

      common /usranl_c/  usrtxt(MAXUSRANL), usrdef(MAXUSRANL), 
     &                   usrfil(20)
      character usrtxt * 132, usrdef * 132, usrfil * 60
C
C     Definitions:
C
C     LUNUSR = logical unit for scratch file.
C     NUMUSR = counter for number of /USER_ANALYSIS files.
C     NUMDEF = counter for USRDEF, the symbol definition file.
C     NUMTXT = counter for USRTXT, the user-supplied, symbol ladden
C              text to be printed.
C     USRDBG = debug switch: 0/1 - debug off/on/.
C     USRTXT = character array of user-defined output report.
C     USRDEF = character array containing symbol definitions.
C     USRFIL = character string of auxiliary output file.
C     NUMUPX = integer array of count of contents of output files.
C     COMFLG = logical array: append case comments to output?
C
