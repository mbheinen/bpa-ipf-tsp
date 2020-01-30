C    %W% %G%
      subroutine ruint
C     
C     THIS SUBROUTINE CONVERTS THE BUS NUMBERS TO THE SWING
C     INTERNAL ORDER FOR THE LINE CONTAINING THE UNDERFREQUENCY LI
C     TRIPPING RELAY AND INITIALIZES THE VOLTAGE TABLES.
C     IT ALSO SEARCHES THE REMOTE RELAY TABLES TO FIND THE INDICES
C     OF THE REMOTE RELAYS FOR THIS RELAY. IT IS CALLED BY INITL2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/rrcom.inc'
      include 'tspinc/rucom.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/busvolt.inc'
      character*8 name1c, name2c
      character*1 id
C     
C     REORDER BUS NUMBERS IN UNDERFREQUENCY LINE RELAY TABLE
C     
      do itrr = 1, kntru
        ibusru(itrr) = indx2n(ibusru(itrr))
        jbusru(itrr) = indx2n(jbusru(itrr))
        ibusn = ibusru(itrr)
        jbusn = jbusru(itrr)
        eiru(itrr) = eyr(ibusn)
        firu(itrr) = eyi(ibusn)
C       
C       SEARCH REMOTE RELAY TABLES TO FIND REMOTE RELAYS
C       ATTACHED TO THIS RELAY
C       
        id = iparru(itrr)
        if (kntrr .ne. 0) then
          knt = 0
          do ktrr = 1, kntrr
            if (ibusn .eq. ibs1rr(ktrr)) then
              if (jbusn .eq. jbs1rr(ktrr)) then
                if (kompr(id, ipr1rr(ktrr), kdum) .eq. 0) then
                  if (rltprr(ktrr) .eq. 8) then
                    knt = knt + 1
                    if (knt .gt. 5) goto 100
                    rltprr(ktrr) =  - 1.0
                    idrrru(knt, itrr) = ktrr
                  endif
                endif
              endif
            endif
          enddo
          icd4ru(itrr) = knt
          goto 110
  100     name1c = exnamc(ibusn)
          kv1 = ixnamn(ibusn)
          bkv1 = basekv(kv1)
          name2c = exnamc(jbusn)
          kv2 = ixnamn(jbusn)
          bkv2 = basekv(kv2)
          write (errbuf(1), 10000) name1c, bkv1, name2c, bkv2, id
10000     format (5x, ' UNDERFREQUENCY LINE RELAY ', 2(1x, a8, 1x,
     &     f5.1), 1x, a1, ' HAS MORE THAN 5 REMOTE RELAYS.')
          iabort = 1
        endif
  110   continue
      enddo
      return
      end
