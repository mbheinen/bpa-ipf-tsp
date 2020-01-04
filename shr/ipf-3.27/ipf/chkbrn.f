C    @(#)chkbrn.f	20.4 11/11/97
      subroutine chkbrn
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/brchk.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/outpt2.inc'
      include 'ipfinc/prt.inc'
C
C     BLKDTA-CHKBRN
C
      common /itot/ itot,ioerr
      dimension ibchk(3,MAXBRN)
      equivalence (ibchk,bchk)
      save
 
C     STORE BRANCH QUANTITIES
 
      if (k1 .lt. k2) then
        kpack = 200000*k1 + 10*k2 + mod(ichar(id),10)
        itot=itot+1
        ibchk(1,itot)=kpack
        bchk(2,itot)=ploss
        bchk(3,itot)=qloss
      else
        il1 = 1
        il2 = itot
        kpack = 200000*k2 + 10*k1 + mod(ichar(id),10)
 
        do while (il1 .le. il2)
          il = (il1+il2) / 2
          komp = kpack - ibchk(1,il)
          if (komp .lt. 0) then
            il2 = il - 1
          else if (kpack .gt. 0) then
            il1 = il + 1
          else
            go to 160
          endif
        enddo 

        write (errbuf(1),150) bus(k1), base(k1), bus(k2), base(k2),
     &    id
  150   format (' Output error -- non-symmetrical ',
     1        'branch ', a8, f7.1, ' TO ', a8, f7.1, ' PAR ', a1)
        call prterx ('W',1)
        ioerr=ioerr+1
        go to 190
 
  160   aloss=bchk(2,il)
        bloss=bchk(3,il)
        if (sqrt((ploss-aloss)**2+(qloss-bloss)**2) .gt. 0.05) then
          aloss=ploss-aloss
          bloss=qloss-bloss
          write (errbuf(1),170) bus(k1),base(k1),bus(k2),base(k2),
     &      id,aloss,bloss
  170     format (' Output error --- branch loss quantities',
     1        'are non-symmetrical ',a8,f7.1,' TO ',a8,f7.1,
     2        ' par ',a1,' DP =',f7.2,' DQ =',f7.2)
          call prterx ('W',1)
          ioerr=ioerr+1
        endif 
      endif 
  190 continue
      return
 
C     CHECK LOSS QUANTITIES
 
      entry fibrck
 
      if (itot.eq.0) go to 220
      pinsum=0.0
      qinsum=0.0
 
      do i=1,itot
        pinsum=pinsum+bchk(2,i)
        qinsum=qinsum+bchk(3,i)
      enddo
 
      write (outbuf,210) ioerr,pinsum,qinsum
  210 format('0 LOSS CHECK: TOTAL ERRORS ',i4,' PINSUM ',f10.3,
     1  ' QINSUN ',f10.3)
      call prtout(1)
 
  220 continue
 
      return
      end
