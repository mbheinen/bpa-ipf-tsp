C    %W% %G%
      subroutine sptout
c 
c     This subroutine outputs special points quantities
c 
c     Revisions:

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/prtmax.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/spare2.inc'

      character*20 atem

      character crvend*12                                               !dem

c     begin     begin     begin     begin     begin     begin

      call mpost('SPTOUT')
c
      if (auxfmt .eq. 'STD') then                                       !DEM
        crvend = '  -100.00'                                            !DEM
      else                                                              !DEM
        crvend = '/END'                                                 !dem
      endif                                                             !DEM
c
      do ix = 1, ispknt
C
C       Check for user-specified minimum and maximum limits
C
        jx = ispcde(ix)
        if (spmax(jx) .eq. 0.0) then
           spmax(jx) = -1.0e10
           do i = 1, icount
              spmax(jx) = amax1 (spmax(jx), spdata(i,jx))
           enddo
        endif
        if (spmin(jx) .eq. 0.0) then
           spmin(jx) = +1.0e10
           do i = 1, icount
              spmin(jx) = amin1 (spmin(jx), spdata(i,jx))
           enddo
        endif
c
c       Generate spare point output
c
        if (auxfmt .eq. 'STD') then                                    !DEM
           write (l11, 100) spaxis(1,jx), spaxis(2,jx)
  100      format('B  *SP ', 2a, 10x)
           write (l11, 110) spaxis(1,jx), spaxis(2,jx)
  110      format(2a)
        else
           write (l11,'(a)') '/CURVE'                                   !dem
           write (l11, 120) spaxis(1,jx), spaxis(2,jx)
  120      format ('  LEGEND  *SP ', 2a)
           write (l11, 130) spaxis(1,jx), spaxis(2,jx)
  130      format ('  ! ', 2a)
           write (l11,'(a)') '  data'                                   !DEM
        endif
        do i = 1, icount
           yvalue = amin1 (amax1 (spdata(i,jx), spmin(jx)), spmax(jx))

c          New format has wider fields

           if (auxfmt .eq. 'STD')  then                                 !dem
             call varfmt(yvalue, atem, 10) 
             write(l11,'(f9.2,1x,a10)') t(i), atem                      !DEM
           else                                                         !dem
             call varfmt(yvalue, atem, 15)
             write(l11,'(f13.6,1x,a15)') t(i), atem                     !DEM
           endif                                                        !dem
        enddo
        if (auxfmt .eq. 'STD') then                                       !DEM
          crvend = '  -100.00'                                            !DEM
        else                                                              !DEM
          crvend = '/END'                                                 !dem
        endif                                                             !DEM
        write (l11,'(a)') crvend                                         !DEM
c
      enddo
      return
      end
