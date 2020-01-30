C    %W% %G%
      subroutine rdprtbse
      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/in1n.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/brnch.inc'
      include 'tspinc/rddtai_mv.inc'

      integer keybrd
      integer i
      integer is
      integer brcnt
      dimension bsbr(200), jbsbr(200)
      integer jbsbr
      real bsbr
      equivalence(bsbr,jbsbr)
      character brid

c     real brvctr
c     integer kk,ktempn,ixn,indo2n,indn2o,indo2x,indx2o
c     real vold1, vold2, capold
c     dimension indn2o(MAXBUS),indo2x(MAXBUS),indx2o(MAXBUS),
c    1 indo2n(MAXBUS), ktempn(3,MAXBUS), ixn(MAXBUS),
c    2 vold1(MAXBUS), vold2(MAXBUS), capold(2,MAXBUS), kk(2,MAXBUS)
c     dimension brvctr(54*MAXBUS)
c     equivalence (brvctr,jbrnch)
c     equivalence (indo2n,brvctr(1)), (indn2o,brvctr(MAXBUS+1)),
c    1  (indo2x,brvctr(2*MAXBUS+1)), (indx2o,brvctr(3*MAXBUS+1)),
c    2  (vold1,brvctr(4*MAXBUS+1)), (vold2,brvctr(5*MAXBUS+1)),
c    3  (capold,brvctr(6*MAXBUS+1)), (kk,brvctr(8*MAXBUS+1)),
c    4  (ktempn,brvctr(10*MAXBUS+1)), (ixn,brvctr(13*MAXBUS+1))

      integer dbg
       
      dbg = keybrd(0)

      if(dbg .eq. 1) then
        write(*,'(1x,a)') 'prtdump.f: dump the ecs tables'
        write(*,'(1x,a)') 'No Length Pnetu Qnetu GKKu BKKu ntypu nspar'
        write(*,'(1x,a)') ' vlimn vlimx ploadu qloadu inetr ineti'
        write(*,'(1x,a)') '   ikmu - gkmu - bkmu'
        do i = 1, ntot
          call redecs(bsbr, kk(1,i) + kecst, kk(2,i))
          write(*,'(1x,a,i4,2f8.3, 2f14.4, 2i4, 2f14.4, 4f8.3)')
     1      exnamc(indo2x(i)),i, kk(1,i), kk(2,i), bsbr(1), bsbr(2), 
     2      bsbr(3), bsbr(4),
     3      jbsbr(5), jbsbr(6), bsbr(7), bsbr(8), bsbr(9),
     4      bsbr(10), bsbr(11), bsbr(12)
            do brcnt = 13, kk(2,i) - 1, 3
              write(*, '(1x,a10, i4, 2f12.4)')
     1        exnamc(indo2x(jbsbr(brcnt))), jbsbr(brcnt), 
     2        bsbr(brcnt+1), bsbr(brcnt+2)
            end do
        end do

c      else if (dbg .eq. 1) then
        write(*,'(1x,a)') 'prtdump.f: new dump the pf voltage tables'
        write(*,'(1x,a)') ' bus: exnamc, exbase, eyr, eyi'
        write(*,'(1x,a)') ' branch: name1, name2, brtype, brid, brsect'
        write(*,'(1x,a)') '   jbrnch 2, 12, 1, 13, 14'
        do i = 1, ntot
          write(*, '(1x,a10, f6.1, f10.4, f10.4)' )
     1      exnamc(i), exbase(i), eyr(i), eyi(i)
        end do
        do i = 1, ltot
          call getchr(1,brid,jbrnch(13,i))
          write(*, '(2a10,i4,a3,i4)' )
     1      exnamc(jbrnch(2,i)),exnamc(jbrnch(12,i)),
     2      jbrnch(1,i), brid, jbrnch(14,i)
        end do
      endif
c
c        write(*,'(1x,a)') 'prtdump.f: dump the pf voltage tables'
c        write(*,'(1x,a)') ' xname, xbase, vold1, vold2'
c        do i = 1, ntot
c          write(*, '(1x,a10, f6.1, f10.4, f10.4)' )
c     1      exnamc(i), exbase(i), vold1(i), vold2(i)
c        end do
c      endif
        
      return
      end
