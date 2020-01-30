C    %W% %G%
        subroutine dcaux (dname,descr,icount,t,work)
c       SUBROUTINE DCAUX (DESCR,ICOUNT,T,WORK)
          character dname*(*),descr*(*)                                 !dem
          character*20 atem
          dimension t(*), work(*)
C * * *
C * * * THIS SUBROUTINE WRITES REQUESTED DC DATA TO THE AUXILIARY
C * * * OUTPUT FILE (FOR011).  IT IS CALLED BY NDCOUT AND CALLS
C * * * VARFMT.
C Revs:
c  Oct/05/92 - DEM: Expanded field sizes for alternate format; 6 dec
c                     places for time and 15 chars for parameters.
c  May/07/92 - DEM: Added DNAME argument so that every t-parm list could
c                   have branch name in its header for the new aux
c                   format.
c       DNAME . . Name of DC branch
C       DESCR . . Parm description
C       ICOUNT  . No of time steps
C       T . . . . Time step array
C       WORK  . . Array containing parm values
C       L11 . . . AUXILIARY OUTPUT FILE
      include 'tspinc/params.inc'                                              !dem
      include 'tspinc/blkcom1.inc'
      include 'tspinc/link56.inc'                                              !dem
c       -     begin     begin     begin     begin     begin     begin
      if (auxfmt .eq. 'STD') then                                       !dem
        write(l11,100) descr
100       format(a)
      else                                                              !dem
        write (l11,'(A)') '/CURVE'                                      !dem
        write (l11,'(2A)') '  LEGEND  ',dname                           !dem
        write (l11,'(2A)') '  ! ',descr                                 !dem
        write (l11,'(2A)') '  DATA'                                     !dem
      endif                                                             !dem
      do 105 j = 1, icount
c       -  New format has wider fields
        if (auxfmt .eq. 'STD') then                                     !dem
          call varfmt(work(j),atem,10)                                  !dem
          write(l11,'(F9.2,1X,A10)') t(j), atem                         !dem
        else                                                            !dem
          call varfmt(work(j),atem,15)                                  !dem
          write(l11,'(F13.6,1X,A15)') t(j), atem                        !dem
        endif                                                           !dem
c       CALL VARFMT(WORK(J),ATEM,10)
c       WRITE(L11,110) T(J), ATEM
c110      FORMAT(F9.2,1X,A10)
105   continue
      if (auxfmt .eq. 'STD') then                                       !dem
        write(l11,120)
120       format('-100.0')
      else                                                              !dem
        write (l11,'(A)') '/END'                                        !dem
      endif                                                             !dem
c 
      return
      end
