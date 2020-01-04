C    @(#)newptibs.f	20.2 3/29/99
C****************************************************************  
C  
C     File: newptibs.f  
C  
C     Purpose: Routine to create a mew, unique bus name  
C  
c     Return code:  n = 0 : Success  
c                   n = 1 : Error - cannot create a unique name  
c                   n = 2 : Error - more thatn MAXBUS entities  
c  
C     Author: Walt Powell  Date: 14 Jan 1999  
C     Called by: ldptibus.f  
C  
C****************************************************************  
      integer function newptibs (busname, basekv, kx, indx)  
      integer kx, indx  
      character *(*) busname  
      real basekv  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/alt_case.inc'  
      include 'ipfinc/alpha.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/cbus.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/wsccbase.inc'  
  
      logical finished  
      integer add_bus  
  
      newptibs = 0  
  
      finished = .false.  
      itag = 48                      ! Begin with "0,1,2, ..."  
      do while (.not. finished)  
        write (busname, 10040) busname(1:6), '#', char(itag)  
10040   format (a, a, a)  
        indx = add_bus (busname, basekv, kx)  
        if (indx .gt. 0) then  
          finished = .true.  
        else if (indx .eq. 0) then  
          finished = .true.  
          newptibs = 2  
        else if (itag .eq. 57) then  
          tag = 65                   ! Change to "A,B,C ..."  
        else if (itag .ge. 90) then  
          finished = .true.  
          newptibs = 1  
        else   
          itag = itag + 1  
        endif  
      enddo  
  
      return  
      end  
