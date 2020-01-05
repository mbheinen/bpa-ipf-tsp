C    %W% %G%
      subroutine rmint
C     
C     This subroutine initializes data tables for the under
C     frequency generator dropping model.  It is called by
C     INITL2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/rmcom.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/pointr.inc'
      character*8 name

      do k = 1, ngenf
        ngen = irmbno(k)
        ngen = indx2n(ngen)
        irmbno(k) = ngen
        rmeyr(k) = eyr(ngen)
        rmeyi(k) = eyi(ngen)
        do i = 1, isgg
          if (igentn(1, i) .eq. ngen .and. igentc(i) .eq. rmgid(k))
     &     goto 100
        enddo
        goto 110
  100   irmgno(k) = i
        rmrtim(k) = 0.0
        rmbtim(k) = 0.0
        rmfreq(k) = 0.0
      enddo
      goto 120
  110 name = exnamc(ngen)
      kv = ixnamn(ngen)
      bkv = basekv(kv)
      write (errbuf(1), 10000) name, bkv, rmgid(k)
10000 format ('Underfrequency generator is not in system ',
     & a8, 1x, f5.1, 1x, a1)
      call prterr('E', 1)
      iabort = 1
  120 return
      end
