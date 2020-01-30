C    %W% %G%
      subroutine rvread
C * * *
C * * * This subroutine reads the RV undervoltage load dropping relay
C * * * data cards and forms data tables for the model.  It is called
C * * * by INPUT1.
C * * *
      include 'tspinc/params.inc'
      include 'tspinc/ffcard.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/rvcom.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/relays.inc'
c     -  Functions
      logical dbghere                                                   !dem
c     -  Local variables 
      character*8 name
      logical debug                                                     !dem
c     -     begin      begin      begin      begin      begin      begin 
      debug = dbghere ('RVREAD  ')                                      !dem
      kntrv = kntrv + 1
      if(kntrv .gt. MAXRV)then
         write(errbuf(1),800)MAXRV
 800     format(5x,' NUMBER OF RV CARDS EXCEEDS ',i4,' JOB WILL ABORT.')
         call prterr('E',1)
         iabort = 1
         return
      endif
      read (buffer,1000) name,base,rvshnt(kntrv),rvdrp1(kntrv),
     1                  rvtim1(kntrv),rvpdp1(kntrv),rvqdp1(kntrv),
     2                  rvbdy1(kntrv)
 1000   format (bz,3x,a8,f4.0,1x,a1,1x,f4.3,1x,f4.2,1x,f6.2,1x,f6.2,
     +    1x,f4.2)
      kb=nambas(base)
      nbus=inam(name,kb)
      if (nbus.eq.0) then
         write (outbuf,1200) buffer
         call prtout (1)
 1200    format('0',a)
         write(errbuf(1),1400)
 1400    format(1x,' THE BUS NAME ON THE ABOVE CARD IS INCORRECT.',
     1         ' THIS RV CARD WILL BE IGNORED.')
         call prterr('E',1)
         kntrv = kntrv - 1
         iabort = 1
         return
      endif
      if (noprnt .ne. 0)then
         if (kntrv .eq. 1)then
            write(outbuf,1410)
 1410       format ('0',24x,'****************************************',
     1           '**********')
            call prtout(1)
            write(outbuf,1415)
 1415       format('0',24x,'VOLTAGE DIFFERENCE LOAD DROPPING RELAYS')
            call prtout(1)
         endif
         write(outbuf,1420)buffer
 1420    format('0',a)
         call prtout(1)
      endif
      irectp = 47                                                       !dem
      idesc = 256 + 8                                                   !dem
      irecln = 20                                                       !dem
      if (debug) then                                                   !dem
        call dbgeko ('RVREAD - writing RV input card to history file.') !dem
        call dbgwri ('  IRECTP /record type/ = ',irectp)                !dem
        call dbgwri ('  IDESC /rec descrip/  = ',idesc)                 !dem
        call dbgwri ('  IRECLN /rec length/  = ',irecln)                !dem
        call dbgwrc ('  Partial card written = ',buffer(1:40))          !dem
      endif                                                             !dem
      call puthisrc (irectp,idesc,irecln,buffer)                        !dem
C     WRITE(L8) 2,10, BUFFER
      irvbno(kntrv) = nbus
      irvsw(kntrv) = 0
      rvclk1(kntrv) = 0.0
      rvclk2(kntrv) = 0.0
      return
      end
