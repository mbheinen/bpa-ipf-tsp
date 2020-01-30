C    %W% %G%
        subroutine auxout (gname,ind,iad)                               !dem
c       SUBROUTINE AUXOUT(IND,IAD)
          character gname*40                                            !dem
C * * *
C       Routine for outputting generator values to auxiliary file
c     -  Writes item description but not gen name.
c Revisions:
c   Oct/05/92 - DEM
c     Expanded time field to 6 decimal places and parameter field to
c       15 chars.
c   Sep/16/92 - DEM: Renamed signal exciter supply to PSS output.
c   May/07/92 - DEM: Added GNAME parameter so that every parm list
c                    could have gen name attached.
C * * *
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/room.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
c     -  Local variables
        dimension descr(15)
        character descr*40
        character atem*15                                               !dem
        data (descr(i),i=1,15)/
     1  'ANGLE',
     1  'FREQUENCY DEVIATION',
     1  'FIELD VOLTAGE',
     1  'FLUX LINKAGE EPQ',
     1  'MAIN FIELD SATURATION',
     1  'MECHANICAL POWER',
     1  'ELECTRICAL POWER',
     1  'EXCITER SATURATION',
     1  'REGULATOR OUTPUT',
     1  'ACCELERATING POWER',
     1  'GENERATOR MVAR',
     1  'PSS OUTPUT',
     1  'DAMPING TORQUE',
     1  'FIELD CURRENT',
     1  'AREA ACCELERATING POWER'/
c
c     -     begin     begin     begin     begin     begin     begin
c     -  Write heading
      if (auxfmt .eq. 'STD') then                                       !dem
        write(l11,100) descr(ind)                                       !dem
100       format(a40)                                                   !dem
      else                                                              !dem
c       -  New format for postscript plots
        write (l11,'(A)') '/CURVE'                                      !dem
        write (l11,'(A,A30)') '  LEGEND  ',gname                        !dem
        write (l11,'(A,A40)') '  ! ',descr(ind)                         !dem
        write (l11,'(A)') '  DATA'                                      !dem
      endif                                                             !dem
c       -  Make t & parm list
        do 115 j = 1,icount
c         -  New format has wider fields
          if (auxfmt .eq. 'STD') then                                   !dem
            call varfmt (work(iad+j),atem,10)                           !dem
            write (l11,'(F9.2,1X,A10)') t(j),atem                       !dem
          else                                                          !dem
            call varfmt (work(iad+j),atem,15)                           !dem
            write (l11,'(F13.6,1X,A15)') t(j),atem                      !dem
          endif                                                         !dem
c         CALL VARFMT (WORK(IAD+J),ATEM,10)
c         WRITE (L11,110) T(J),ATEM
c110        FORMAT (F9.2,1X,A10)
115     continue
c     -  Write curve end marker
      if (auxfmt .eq. 'STD') then                                       !dem
        write (l11,'(a)') '  -100.00'                                   !dem
c       WRITE (L11,120)
c120      FORMAT ('  -100.00')
      else                                                              !dem
        write (l11,'(A)') '/END'                                        !dem
      endif                                                             !dem
      return
      end
