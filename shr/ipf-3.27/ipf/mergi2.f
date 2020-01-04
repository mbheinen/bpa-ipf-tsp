C    @(#)mergi2.f	20.3 2/13/96
      subroutine mergi2
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/oldsrt.inc'
      include 'ipfinc/prt.inc'
 
      character*10 tempc
C
C     CONTENTS OF "COMVAR"
C
C     COMVAR(1) - NUMBER OF SUBSYSTEMS
C           (2) - NAME OF SUBSYSTEM 1
C           (3) - NAME OF SUBSYSTEM 2
C           (4) - NUMBER OF BUSES IN SUBSYSTEM 1
C           (5) - NUMBER OF BUSES IN SUBSYSTEM 2
C           (6) - COUNT OF ENCODED BUS TEXT
C           (7) - COUNT OF ENCODED BRANCH TEXT
C           (8) - COUNT OF ENCODED INTERFACE TEXT
C           (9) - INPUT SWITCH TO SAVE AREA INTERCHANGE SYSTEM
C          (10) - RESERVED FOR DEBUG
C
      call dbgprt (1)
C
C     INITIALIZE PARAMETERS
C
      kexit = 0
      tempc = comcha(2)
      itemp = comvar(4)
      do 100 i = 1,10
      comcha(i) = ' '
  100 comvar(i) = 0
      comvar(1) = 2
      comcha(2) = tempc
      comcha(3) = cspare(38)
      comvar(4) = itemp
      comvar(5) = ntot
C
C     IDENTIFY SUBSYSTEM
C
      kerrsw = 0
      call sys100
      if (kerrsw.ne.0) go to 900
C
C     PROCESS INTERFACE
C
      call inf100
      if (kerrsw.ne.0) go to 900
C
C     STORE INTERFACE DATA AND EXIT
C
      kspare(34) = 0
      kspare(35) = comvar(7)
      kspare(36) = 0
      kspare(37) = 0
C
C     RESET "KBSORT" TO IDENTIDY TRANSFORMATION (NO NEW
C     EXTERNAL ORDER)
C
      do 110 i = 1,ntot+1
  110 kbsort(i) = i
      go to 930
C
C     ERROR EXIT
C
  900 write (errbuf(1),910)
  910 format ('   MERGE RUN ABORTED BY DATA ERRORS ')
      call prterx ('W',1)
      call erexit
  930 call dbgprt (0)
      return
      end
