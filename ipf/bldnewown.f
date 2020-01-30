C    @(#)bldnewown.f	20.2 2/28/00
C****************************************************************
C
C     File: bldnewown.f
C
C     Purpose: Routine to rebuild new ownership hash tables
C
C     Author: Walt Powell  Date: 18 Jan 2000
C     Called by: saveptit.f
C
C****************************************************************
        integer function bldnewown ()

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/owner_cm.inc'
 
        integer MAXPTIRECORDS
        parameter (MAXPTIRECORDS = 16000)
        common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                   nextptr_2(MAXBUS), count_newbus, 
     &                   newbusno(MAXBUS), count_newzone, 
     &                   newzoneno(MAXCZN), count_newown, 
     &                   newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                   sort_tempc(MAXPTIRECORDS)
        integer array, count, htable_2, count_newbus, newbusno, 
     &          count_newzone, newzoneno, count_newown, newownno, 
     &          sort_tempc
        character tempc*80

        integer add_ptio
c
c       Hash owner data
C
        num_onam = 0
        do i = 1, numown
          nptio = add_ptio (i, owner_o(i))
          count_newown = count_newown + 1
          newownno(count_newown) = nptio
        enddo

        bldnewown = 0
        return
        end
