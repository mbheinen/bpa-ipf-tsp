C    %W% %G%
      subroutine rminp
C     
C     This subroutne decodes the RM card and builds tables.
C     It is called by INPUT2
C     
      include 'tspinc/params.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/cf1.inc'
      include 'tspinc/param.inc'
      include 'tspinc/rmcom.inc'
      include 'tspinc/relays.inc'
      character*8 namec
      character*1 id

      data pi/3.14159265/

C     -     begin     begin     begin     begin     begin     begin

      do lnt = 1, ngenf
        if (lnt .gt. MAXRM) goto 100
        read (rmt80(lnt), 10000) namec, base, id, rmfq1(lnt), rmtrp1
     &   (lnt), rmfq2(lnt), rmtrp2(lnt)
10000   format (bz, 3x, a8, f4.0, a1, 4x, 5f5.2)
        kb = nambas(base)
        ngen = inam(namec, kb)
        if (ngen .eq. 0) then
          write (errbuf(1), 10010) (rmtab(ir, lnt), ir = 1, 8)
10010     format (1x, 8a10)
          call prterr('E', 1)
          write (errbuf(1), 10020)
10020     format ('0 BUS NAME ON THE ABOVE CARD DOES NOT EXIST.  THIS '
     &     , 'RM CARD WILL BE IGNORED.')
          call prterr('E', 1)
          iabort = 1
        else
          irmtcd(lnt) = 1
          irmbno(lnt) = ngen
          rmgid(lnt) = id
C         
C         CONVERT TRIPPING FREQUENCIES TO FREQUENCY DIFFERENCES IN
C         RADIANS PER CYCLE (INTERNAL FREQUENCY)
C         
          rmfq1(lnt) = (rmfq1(lnt)-frqbse)*2.*pi/frqbse
          rmfq2(lnt) = (rmfq2(lnt)-frqbse)*2.*pi/frqbse
        endif
      enddo
      goto 110
  100 write (errbuf(1), 10030) MAXRM
10030 format (5x, ' MORE THAN ', i4, ' RM CARDS ENTERED.  JOB WILL ',
     & 'ABORT.')
      call prterr('E', 1)
      iabort = 1
  110 return
      end
