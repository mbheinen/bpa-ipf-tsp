C    %W% %G%
      subroutine svsss1

C     THIS SUBROUTINE DECODES AND ERROR CHECKS TYPE W SUPPLEMENTARY
C     SIGNAL DATA CARD.  IT IS CALLED BY INPUT3.

      include 'tspinc/param.inc'
      include 'tspinc/svs.inc'
      include 'tspinc/inp3.inc'
      include 'tspinc/inp3a.inc'
      include 'tspinc/tzro.inc'
      include 'tspinc/prt.inc'
      dimension temp(16)
      character*8 name1, name2
      character*1 id1, id2

C     -     begin     begin     begin     begin     begin     begin
      read (work80(icard), 10000) (temp(i),i = 1, 4),(temp(i),i=7,10) 
     &                    ,ierr, name1, bkv1, id1, name2, bkv2, id2
10000 format (bz, 16x, f4.0, 3f4.4, f6.2, 2f4.2, f4.3, i1, 2x, 2(a8, 
     & f4.0, a1))


      svserr(ksv) = 0.0
      if (ierr .ne. 0) svserr(ksv) = 1.

      if (temp(4) .le. 0.0) then
        write (errbuf(1), 10040) nbname, bkv, nid
10040   format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &   'TS9 MUST BE GREATER THAN ZERO')
        call prterr('E', 1)
        imchn = 1
      endif

      if (temp(7) .le. 0.0) then
        write (errbuf(1), 10050) nbname, bkv, nid
10050   format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &   'TS13 MUST BE GREATER THAN ZERO')
        call prterr('E', 1)
        imchn = 1
      endif

      if (temp(8) .le. 0.0) then
        write (errbuf(1), 10060) nbname, bkv, nid
10060   format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &   'TS14 MUST BE GREATER THAN ZERO')
        call prterr('E', 1)
        imchn = 1
      endif

      if (temp(9) .eq. 0.0) then
        write (errbuf(1), 10070) nbname, bkv, nid
10070   format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &   'KS3 MUST NOT BE ZERO')
        call prterr('E', 1)
        imchn = 1
      endif

      if (temp(10) .le. 0.0) then
        write (errbuf(1), 10080) nbname, bkv, nid
10080   format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &   'VSCSMAX MUST BE GREATER THAN ZERO')
        call prterr('E', 1)
        imchn = 1
      endif

      if (imchn .ne. 1) then
        if (subtyp .eq. 'A') then
          isupt(ksv) = 1
          if (name1 .eq. ' ') then
            jbus = 0
          else
            kb = nambas(bkv1)
            jbus = inam(name1, kb)
          endif
          irbus(ksv) = jbus
          irmote(ksv) = jbus
          igenno(ksv) = jbus
          iparsv(ksv) = id1
        elseif (subtyp .eq. 'B') then
          isupt(ksv) = 2
          kb1 = nambas(bkv1)
          kb2 = nambas(bkv2)
          irmote(ksv) = inam(name1, kb1)
          jrmote(ksv) = inam(name2, kb2)
          iparsv(ksv) = id2
        else
          isupt(ksv) = 3
          kb1 = nambas(bkv1)
          irbus(ksv) = inam(name1, kb1)
          irmote(ksv) = irbus(ksv)
        endif

        cks1(ksv) = temp(1)
        cks3(ksv) = temp(9)
        vscsmx(ksv) = temp(10)
        xcon = 2.0*frqbse/dtsvs
        as7(ksv) = xcon*temp(2) + 1.
        as8(ksv) = xcon*temp(3) + 1.
        as9(ksv) = xcon*temp(4) + 1.
        as13(ksv) = xcon*temp(7)
        as14(ksv) = xcon*temp(8) + 1.
        ispcde(ksv) = 1
      endif
      return
      end
