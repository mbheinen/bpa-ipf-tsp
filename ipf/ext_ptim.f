C    @(#)ext_ptim.f	20.12 5/3/00
C****************************************************************  
C  
C     File: ext_ptim.f  
C  
C     Purpose: Routine to extract multi-terminal d-c data in PTI format  
C  
C     Input parameters:  
C  
C             savfil   - the logical unit opened  
C             version  - "23" or "24"  
C  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: saveptid.f  
C  
C****************************************************************  
      integer function ext_ptim (savfil, version, option)  
      integer savfil, version  
      character *(*) option(10)  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/arcntl.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/filnam.inc'  
      include 'ipfinc/lfiles.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/alpha.inc'  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/dcmt.inc'  
      include 'ipfinc/tran.inc'  
      include 'ipfinc/branch.inc'  
      include 'ipfinc/ordsta.inc'  
   
      integer gtptinum, total1, bvconv, circuit(10), node(20),   
     &        ptr, status, write_ge_file
      character code*10, xbuf*132, bas1ch*4, bas2ch*4, contyp  
      logical found, ior, prt_names  
  
      parameter (RD2DG = 180.0 / 3.1415926536)  
        
      prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  
  
      total1 = 0  
c  
c     Determine number of d-c circuits  
c  
      numdckts = 0  
      do i = 1, mtdcbs  
	 ickt = dcmtbs(21,i)  
	 found = .false.  
	 do j = 1, numdckts  
	   if (circuit(j) .eq. ickt) found = .true.  
	 enddo  
	 if (.not. found) then  
	   numdckts = numdckts + 1  
	   circuit(numdckts) = ickt  
	 endif  
      enddo  
      do jt = 1, numdckts  
	 numbuses = 0  
	 numconvs = 0  
	 numlines = 0  
	 nvconv = 0  
	 do i = 1, mtdcbs  
	   call getchr (1, contyp, dcmtbs(4,i))  
	   vsch = dcmtbs(6,i)  
	   ickt = dcmtbs(21,i)  
	   if (ickt .eq. circuit(jt)) then  
	     numbuses = numbuses + 1  
	     node(numbuses) = dcmtbs(1,i)  
	     if (dcmtbs(3,i) .gt. 0.0) numconvs = numconvs + 1  
	   endif  
	   if (vsch .gt. 0.0)   nvconv = dcmtbs(1,i)  
	 enddo  
	 do i = 1, mtdcln  
	   ickt = dcmtln(7,i)  
	   if (ickt .eq. circuit(jt)) then  
	     numlines = numlines + 1  
	   endif  
	 enddo  
c  
c        Obtain PTI bus number(s) for voltage-controlling inverter(s)  
c  
	 if (nvconv .gt. 0) then  
	   iostat = gtptinum (nvconv, iptia, iptiz, iptib)  
	   bvconv = pti_num(iptib)          
	 else  
	   bvconv = 0  
	   write (errbuf(1), 9900)  jt  
9900       format ('MTDC link # ', i2, ', no ',  
     &             'positive-pole inverter regulates voltage.')  
	   call prterx ('E', 1)
           write (xbuf, 10000) jt, numbuses, numconvs, numlines, 1,  
     &                         bvconv, 0.0, 0  
	   last = lastch (xbuf)      
           status = write_ge_file (0, xbuf(1:last))
	   call erexit()
	 endif  
c  
c        D-c record Set No. 1 (one only)  
c  
	 write (xbuf, 10000) jt, numbuses, numconvs, numlines, 1,   
     &                       bvconv, 0.0, 0  
10000    format (i2, 4i3, i6, f4.0, i3)  
	 last = lastch (xbuf)  
         status = write_ge_file (0, xbuf(1:last))
	 total1 = total1 + 1  
c  
c        D-c record Set No. 2 (converter buses follow)  
c  
	 do i = 1, mtdcbs  
	   k1     = dcmtbs(1,i)  
	   k1c    = dcmtbs(3,i)  
	   call getchr (1, contyp, dcmtbs(4,i))  
	   psch   = dcmtbs(5,i)  
	   vsch   = dcmtbs(6,i)  
	   pinj   = dcmtbs(19,i)  
	   ickt   = dcmtbs(21,i)  
	   if (ickt .eq. circuit(jt) .and. k1c .gt. 0) then  
	     if (orddc .eq. 2) then  
	       k1  = opt2inp(k1)  
	       k1c = opt2inp(k1c)  
	     endif  
	     iostat = gtptinum (k1, iptia, iptiz, iptib)  
	     bas1ch = code (base(k1), 4, 0)  
	     numbr = dcmtbs(7,i)  
	     ltc = dcmtbs(15,i)  
	     if (ltc .gt. 0) then  
	       tr_nom = base(k1) / base(k1c)  
	       tapact = 1.0 / tap(ltc)  
	       tapmin = 1.0 / tran(7,ltc)  
	       tapmax = 1.0 / tran(8,ltc)  
	       steps = tran(11,ltc)    
	       if (steps .gt. 1.0) then   
		 tdisc = (tapmax - tapmin) / (steps - 1.0)   
	       else   
		 tdisc = 0.0005  
	       endif  
	     else  
	       tr_nom = 1.0  
	       tapact = 1.0  
	       tapmin = 1.0  
	       tapmax = 1.0  
	       tdisc = 0.0005  
	     endif  
	     if (vsch .ne. 0.0) then  
	       setvl = vsch  
	     else if (psch .ne. 0.0) then  
	       setvl = psch  
	     else  
	       setvl = pinj      ! Use actual MW's  
	     endif  
  
	     alfmin = dcmtbs(10,i) * RD2DG  
	     alfstp = dcmtbs(11,i) * RD2DG  
	     algmnm = dcmtbs(12,i) * RD2DG  
	     alfgam = dcmtbs(13,i) * RD2DG  
	     gamma0 = dcmtbs(29,i) * RD2DG  
  
	     errang = abs (alfgam - algmnm)  
	     if (contyp .eq. 'I' .or. contyp .eq. 'M')  then  
	       angmx = amin1 (alfgam, algmnm) + errang  
	       angmn = gamma0  
	     else if (contyp .eq. 'R') then  
c**               angmx = amin1 (alfgam + errang, alfstp)  
	       angmx = amin1 (2*alfgam - alfmin, alfstp)  
	       angmn = amin1 (alfgam - errang, alfmin)  
	     endif  
  
c            The following two values are each "total ohms" -- secondary (DC)  
c            side transformer impedances multiplied by number of bridges,  
c            whereas PSS/E requires values in ohms per bridge (at one  
c            transformer per bridge).  The user must provide equivalent  
c            (parallel) transformer impedance values in IPF data as required.  
  
	     rc = dcmtbs(17,i)  
	     xc = dcmtbs(18,i)  
  
             write (xbuf, 10010)  pti_num(iptib), numbr,  
     &         angmx, angmn, rc, xc, base(k1c), tr_nom, tapact,  
     &         tapmax, tapmin, tdisc, setvl, 1., 0., 1  
10010        format (i5, i3, 2f5.1, 2f7.3, f7.1, 5f8.5, f9.2,   
     &         2f4.0, i3)   
	     if (prt_names)  then  
               last = lastch (xbuf)
	       write (xbuf(last+1:), 10020)  bus(k1), bas1ch  
10020          format (' / ', a, 1x, a)  
	     endif
  
	     last = lastch (xbuf)  
             status = write_ge_file (0, xbuf(1:last))
	     total1 = total1 + 1  
	   endif  
	 enddo  
c  
c        D-c record Set No. 3 (all d-c buses follow)  
c  
	 do i = 1, mtdcbs  
	   ickt = dcmtbs(21,i)  
	   if (ickt .eq. circuit(jt)) then  
	     k1 = dcmtbs(1,i)  
	     k1c = dcmtbs(3,i)  
	     if (orddc .eq. 2) then  
	       k1 = opt2inp(k1)  
	       if (k1c .gt. 0) k1c = opt2inp(k1c)  
	     endif  
	     iostat = gtptinum (k1, iptia, iptiz, iptib)  
	     bas1ch = code (base(k1), 4, 0)  
	     if (k1c .gt. 0) then  
	       iostat = gtptinum (k1c, iptiac, iptizc, iptibc)  
	       ib = pti_num(iptib)  
	     else  
	       ib = 0  
	     endif  
	     numdcsq = 0  
	     do j = 1, numbuses  
	       if (k1 .eq. node(j))  numdcsq = j  
	     enddo  
  
             write (xbuf, 10120) numdcsq, ib,     
     &         pti_anum(iptia), pti_znum(iptiz),  
     &         bus(k1), 0, 0.0  
10120        format (i3, i6, 2i4, 1x, '''', a, '''', 2x, i5, f5.1)  
	     if (prt_names)  then    
               last = lastch (xbuf)  
	       write (xbuf(last+1:), 10130) bus(k1), bas1ch  
10130          format (' / ', a, 1x, a)  
	     endif
  
	     last = lastch (xbuf)  
             status = write_ge_file (0, xbuf(1:last))
	     total1 = total1 + 1  
	   endif  
	 enddo  
c  
c        D-c record Set No. 4 (all d-c lines follow)  
c  
	 do i = 1, mtdcln  
	   ickt = dcmtln(7,i)  
	   if (ickt .eq. circuit(jt)) then  
	     k1 = dcmtln(1,i)  
	     k2 = dcmtln(2,i)  
	     if (orddc .eq. 2) then  
	       k1 = opt2inp(k1)  
	       k2 = opt2inp(k2)  
	     endif  
	     bas1ch = code (base(k1), 4, 0)  
	     bas2ch = code (base(k2), 4, 0)  
	     numdc1 = 0  
	     numdc2 = 0  
	     do j = 1, numbuses  
	       if (k1 .eq. node(j)) numdc1 = j  
	       if (k2 .eq. node(j)) numdc2 = j  
	     enddo  
	     ptr = numbrn (k1, k2, '*', 0)  
	     nbr = iabs(brnch_ptr(ptr))  
	     ior = ((brnch_ptr(ptr) .gt. 0 .and.   
     &               kbrnch(15, nbr) .eq. 2) .or.  
     &              (brnch_ptr(ptr) .lt. 0 .and.   
     &               kbrnch(15, nbr) .eq. 1) .or.
     &              (brnch_ptr(ptr) .gt. 0 .and.
     &               kbrnch(15, nbr) .eq. 0))

	     if (ior) numdc2 = -numdc2  
  
             write (xbuf, 10230) numdc1, numdc2, '1', dcmtln(4,i),  
     &         dcmtln(5,i)  
10230        format (i2, i4, 1x, a, 2f9.3)  
	     if (prt_names)  then  
               last = lastch (xbuf)  
	       write (xbuf(last+1:), 10240) bus(k1), bas1ch, 
     &           bus(k2), bas2ch  
10240          format (' / ', a, a, 1x, a, 1x, a)  
	     endif
  
	     last = lastch (xbuf)  
             status = write_ge_file (0, xbuf(1:last))
	     total1 = total1 + 1  
	   endif  
	 enddo  
      enddo  
  
      xbuf = ' 0 / Begin Line Section Data'
      last = lastch (xbuf)  
      status = write_ge_file (0, xbuf(1:last))
  
      write ( errbuf(1), 10100) total1  
10100 format (' Total Multi-terminal d-c records extracted:', i5)  
   
      ext_ptim = total1  
  
      return  
      end  
