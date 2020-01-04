C    %W% %G%
	subroutine bus_frpt (ib, ia)
	integer ib, ia

c       PRINT-FINAL-REPORT-BUS.

	include 'tspinc/vfhistory.inc'
	include 'tspinc/params.inc'
        include 'tspinc/newtab.inc' 
        include 'tspinc/filter.inc'
        include 'tspinc/link56.inc'
        include 'tspinc/prt.inc'
        include 'tspinc/wcom.inc'

        integer ibus, ic, n0, n1
        logical finished, debug, nochng, test1, test2
        character buf*132

        data debug / .false. /

        save

        ibus = ibuss(1,ib)
        if (ia .eq. 1 .or. ia .eq. 4) then 
          rtemp1 = 100.0 * (1.0 + rbuss(ia*2,ib))
          rtemp2 = rbuss(1,ib) * (1.0 + rbuss(ia*2,ib))            
          write (outbuf, 10010) busowner(ibus), newtbc(ibus), 
     &      basekv(inwtb(ibus)), 
     &      rtemp1, rtemp2, rbuss(ia*2+1,ib), iarea(ibus), 
     &      arcnam(iarea(ibus))
10010     format(8x, a3, 2x, '[', a, f6.1, ']', 1x, f7.2, 2x, f7.4, 2x,
     &      f7.2, 2x, i2, 1x, a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
        else if (ia .eq. 2) then
          write (outbuf, 10020) busowner(ibus), newtbc(ibus), 
     &      basekv(inwtb(ibus)), 
     &      rbuss(ia*2,ib), rbuss(ia*2+1,ib),
     &      iarea(ibus), arcnam(iarea(ibus))
10020     format(8x, a3, 2x, '[', a, f6.1, ']', 1x, f7.3, 2x, f7.2, 2x,
     &      i2, 1x, a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
        else if (ia .eq. 3) then
          write (outbuf, 10030) busowner(ibus), newtbc(ibus), 
     &      basekv(inwtb(ibus)), 
     &      rbuss(ia*2,ib), rbuss(ia*2+1,ib), iarea(ibus), 
     &      arcnam(iarea(ibus))
10030     format(8x, a3, 2x, '[', a, f6.1, ']', 1x, f7.4, 2x, f7.2, 2x,
     &      i2, 1x, a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
        endif

        return
        end
