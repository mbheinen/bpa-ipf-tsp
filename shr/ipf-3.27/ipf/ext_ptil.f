C    @(#)ext_ptil.f	20.17 8/30/00
C****************************************************************  
C  
C        File: ext_ptil.f  
C  
C        Purpose: Routine to extract branch data in PTI format  
C  
C        Input parameters:  
C  
C             savfil   - the logical unit opened  
C             version  - "23" or "24"  
C  
C        Author: Walt Powell  Date: 21 May 1996  
C        Called by: saveptid.f  
C  
C****************************************************************  
	 integer function ext_ptil (savfil, version, option)  
	 integer savfil, version  
	 character *(*) option(10)  
   
	 include 'ipfinc/parametr.inc'  
  
	 include 'ipfinc/blank.inc'  
	 include 'ipfinc/area.inc'  
	 include 'ipfinc/arcntl.inc'  
	 include 'ipfinc/bus.inc'  
	 include 'ipfinc/branch.inc'  
	 include 'ipfinc/cbus.inc'  
	 include 'ipfinc/com007.inc'  
	 include 'ipfinc/prt.inc'  
	 include 'ipfinc/pseudo_b.inc'  
	 include 'ipfinc/tran.inc'  
	 include 'ipfinc/alpha.inc'  
	 include 'ipfinc/pti_data.inc'  
	 include 'ipfinc/ordsta.inc'  
	 include 'ipfinc/lfiles.inc'  
  
         integer max_out_of_service
         parameter (MAX_OUT_OF_SERVICE = 200)
         common /out_of_service/ numoossh, shunt_status(MAXCBS),
     &                           shunt_value(16,MAX_OUT_OF_SERVICE), 
     &                           branch_status(MAXBRN)
         integer numoossh, shunt_status, branch_status
         real shunt_value

	 character xbuf*240, lntype(9)*2, id*1, idpl*1, code*4,   
     &             base1c*4, base2c*4, vmaxc*8, vminc*8, lown*3
	 integer total1, total2, total3, ptr, qptr, oldptr, orienttx,   
     &           fndpsptr, ktemp(22), fnd_ptib, ftn_atoi, q, add_ptiq,
     &           write_ge_file

	 real temp(22), miles  
	 equivalence (temp, ktemp)  
	 logical print_flag, ior, prt_names  
  
	 data lntype / 'L*', 'LM', 'L ', 'R ', 'T ', 'TP', 'LD', 'E ',  
     &                 'RZ' /  
  
	 izero = ichar ('0')  
   
	 ext_ptil = 0  
	 total1 = 0       ! Count of branches extracted  
	 total2 = 0       ! Count of pseudo-branches extracted  
	 total3 = 0       ! Count of LTC branches extracted  
  
	 prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  
C                                                                   
C        PTI criteria for selection:  (low-hi/hi-low):  
C                                                                    
C        Type           Criteria                              
C         
C        L,E,T,TP,LM,RZ Original submittal                  
C        R              Adjustable tap side or hi-low       
C        LD             Rectifier-inverter                  
C                                                                   
	 call bcdopt ( 'LOWX',0.0000 )  
c  
c        First pass - Generate PTI branch data  
c  
	 do ib = 1, ntot_alf  
	   nb = alf2inp(ib)  
	   print_flag = .false.  
	   ptr = kbsdta(16,nb)  
	   if (option(2) .eq. 'Y') then  
c  
c            Rename all parallels '1', '2' if first parallel is blank  
c  
	     lastky = 0  
	     do while (ptr .gt. 0)  
	       if (brid(ptr) .eq. ' ' .and. brtype(ptr) .ne. 4) then  
		 oldptr = ptr  
		 lastky = ky(ptr)  
		 numpr = 0  
		 id = brid(ptr)  
		 do while (ptr .gt. 0 .and. ky(ptr) .eq. lastky)     
		   if (brtype(ptr) .eq. 1) then  
		     do while (ptr .gt. 0 .and.   
     &                         ky(ptr) .eq. lastky .and.  
     &                         brid(ptr) .eq. id)  
		       ptr = brnch_nxt(ptr)  
		     enddo  
		     id = brid(ptr)  
		     numpr = numpr + 1  
		   else  
		     ptr = brnch_nxt(ptr)  
		     id = brid(ptr)  
		     numpr = numpr + 1  
		   endif  
		 enddo  
		 if (numpr .gt. 1) then  
		   ptr = oldptr  
		   numpr = 0  
		   lastky = ky(ptr)  
		   id = brid(ptr)  
		   do while (ptr .gt. 0 .and. ky(ptr) .eq. lastky)     
		     numpr = numpr + 1  
		     numid = ftn_atoi(brid(ptr))  
		     if (numid .lt. numpr) then  
		       ich = numpr + ichar('0')  
		       if (ich .gt. ichar('9')) then  
			 ich = numpr + ichar('A') - 9  
		       endif  
		       brid(ptr) = char(ich)  
		       if (brtype(ptr) .eq. 1) then  
			 ptr = brnch_nxt(ptr)  
			 do while (ptr .gt. 0 .and.   
     &                             ky(ptr) .eq. lastky .and.  
     &                             brid(ptr) .eq. id)  
			   brid(ptr) = char(ich)  
			   ptr = brnch_nxt(ptr)  
			 enddo  
			 id = brid(ptr)  
		       else  
			 ptr = brnch_nxt(ptr)  
			 id = brid(ptr)  
		       endif  
		     else  
		       numpr = numid  
		       ptr = brnch_nxt(ptr)  
		       id = brid(ptr)  
		     endif  
		   enddo  
		 endif  
	       else  
		 ptr = brnch_nxt(ptr)  
	       endif  
	     enddo  
	   endif  
  
	   ptr = kbsdta(16,nb)  
	   do while (ptr .gt. 0)  
	     qptr = brnch_ptr(ptr)  
	     nbr = iabs (qptr)  
  
	     if (brtype(ptr) .eq. 1 .and. qptr .gt. 0) then  
c  
c              Process section PSEUDOBUSES option  
c  
	       oldptr = ptr  
	       ptr = brnch_nxt(ptr)  
	       do while (ptr .gt. 0 .and.   
     &                   ky(ptr) .eq. ky(oldptr) .and.  
     &                   brid(ptr) .eq. brid(oldptr))  
		 nsec = fndpsptr (ptr)  
		 if (nsec .le. 0) then  
		   write ( errbuf(1), 10000) lntype(brtype(ptr)),   
     &               bus(kx(ptr)), base(kx(ptr)), bus(ky(ptr)),   
     &               base(kx(ptr)), brid(ptr), brsect(ptr)  
10000              format (' Failed to process branch [', a, 1x,  
     &               a8, f6.1, 1x, a8, f6.1, 1x, a, i2,   
     &               '] using pseudo-sections')  
		   call prterx ('W', 1)  
		 else   
		   k1xx = pseudo(3,nsec)  
		   iptib1 = fnd_ptib (bus(k1xx), base(k1xx),   
     &                                arcnam(jarzn(k1xx)))  
		   k2xx = pseudo(4,nsec)  
		   iptib2 = fnd_ptib (bus(k2xx), base(k2xx),   
     &                                arcnam(jarzn(k2xx)))  
		   if (iptib1 .le. 0 .or. iptib2 .le. 0) then  
		   else  
		     k1x = pti_num(iptib1)  
		     k2x = pti_num(iptib2)  
                     q = brnch_ptr(ptr) 
                     nbr = iabs (q)
                     intovr = kbrnch(15, nbr)
                     if (intovr .gt. 0 .and. q .lt. 0) 
     &                 intovr = 3 - intovr
                     if (brtype(ptr) .eq. 2) then
                       pdc = brnch(8,nbr)
                       if (q .lt. 0) pdc = -pdc
                       if (pdc .lt. 0.0) then
                         intovr = 1
                       else
                         intovr = 2
                       endif
                     else if (intovr .eq. 0) then
                       call getchr (3, lown, kbrnch(3,nbr))
                       if (owner(k1) .eq. owner(k2)) then
                         intovr = 1
                         if (inp2alf(k1) .gt. inp2alf(k2))
     &                     intovr = 2
                       else
                         if (lown .eq. owner(k1) .and.
     &                       lown .ne. owner(k2)) then
                           intovr = 2
                         else if (lown .eq. owner(k2) .and.
     &                            lown .ne. owner(k1)) then
                           intovr = 1
                         else
                           intovr = 1
                           if (inp2alf(k1) .gt. inp2alf(k2))
     &                       intovr = 2
                         endif
                       endif
                       if (intovr .gt. 0 .and. q .lt. 0) 
     &                   intovr = 3 - intovr
                     endif
		     ior = ((q .gt. 0 .and. intovr .eq. 2) .or.  
     &                      (q .lt. 0 .and. intovr .eq. 1) .or.
     &                      (q .gt. 0 .and. intovr .eq. 0))

		     if (ior) k2x = -k2x  
		     base1c = code (base(pseudo(3,nsec)), 4, 0)  
		     base2c = code (base(pseudo(4,nsec)), 4, 0)  
		     call gtptibrv (ptr, temp)  
		     id = brid(ptr)  
		     if (id .eq. ' ' .and. option(2) .eq. 'Y') then  
		       id = '1'  
		     else if(id .eq. ' ') then  
		       id = '0'  
		     endif  
		     if (brtype(ptr) .eq. 3 .or.   
     &                   brtype(ptr) .eq. 8) then  
  
                       write (xbuf, 10030) k1x, k2x, id,    
     &                   (temp(i), i=1,2), (temp(i), i=4,7),   
     &                   0., 0., (temp(i), i=11,14), branch_status(nbr)
10030                  format (i5, i7, 1x, a, 3f9.5, 3f7.1,   
     &                   f9.0, f7.0, 4f10.5, i3)  

c                      Append line length (in miles) and ownership

                       if (version .ge. 24)  then
                         if (brtype(ptr) .eq. 3)  then  
                           miles = temp(15)  
                         else  
                           miles = 0.0  
                         endif  
                         iptio = add_ptiq (lown)
                         last = lastch (xbuf)  
                         write (xbuf(last+1:), 10032) miles,
     &                     pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &                     0, 0.0  
10032                    format (f7.1, 4(1x, i4, 1x, f4.2))
                       endif  

                       if (prt_names)  then  
		         last = lastch (xbuf)
                         write (xbuf(last+1:), 10034) 
     &                     bus(pseudo(3,nsec)), base1c, 
     &                     bus(pseudo(4,nsec)), base2c  
10034                    format (' / ', a8, a4, 1x, a8, 1x, a4)  
                       endif
  
		       total2 = total2 + 1  
		       last = lastch (xbuf)  
                       status = write_ge_file (0, xbuf(1:last))
  
		     else if (brtype(ptr) .eq. 5 .or.   
     &                        brtype(ptr) .eq. 6) then  
                         
                       tr_gsh = temp(13)/2.0  
                       tr_bsh = temp(14)/2.0  
                       write (xbuf, 10040) k1x, k2x, id,  
     &                   (temp(i), i=1,2), (temp(i), i=4,7),  
     &                   (temp(i), i=9,10), tr_gsh, tr_bsh,   
     &                    tr_gsh, tr_bsh, branch_status(nbr)
10040                  format (i5, i7, 1x, a, 3f9.5, 3f7.1,  
     &                    f9.5, f7.1, 4f10.5, i3)  
                       if (version .ge. 24)  then  
                         last = lastch (xbuf)  

c                        Append line length (in miles) and ownership

                         miles = 0.0  
		         iptio = add_ptiq (lown)
                         last = lastch (xbuf)  
			 write (xbuf(last+1:), 10032) miles,
     &                     pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &                     0, 0.0  
                       endif       
		       if (prt_names)  then  
                         last = lastch (xbuf)
			 write (xbuf(last+1:), 10042) 
     &                     bus(pseudo(3,nsec)), base1c, 
     &                     bus(pseudo(4,nsec)), base2c  
10042                    format (' / ', a8, a4, 1x, a8, a4)  
		       endif
  
		       total2 = total2 + 1  
		       last = lastch (xbuf)  
                       status = write_ge_file (0, xbuf(1:last))
  
		     else  
  
		       write ( errbuf(1), 10050)   
     &                   lntype(brtype(ptr)), bus(kx(ptr)),   
     &                   base(kx(ptr)), bus(ky(ptr)),   
     &                   base(kx(ptr)), brid(ptr), brsect(ptr)  
10050                  format (' Failed to process branch [', a,   
     &                   1x, a8, f6.1, 1x, a8, f6.1, 1x, a, i2,   
     &                   '] -- invalid type within sections')  
		       call prterx ('W', 1)  
		     endif  
		   endif  
		 endif  
		 ptr = brnch_nxt(ptr)  
	       enddo  
	      
	     else if (brtype(ptr) .eq. 1) then  
  
	       oldptr = ptr  
	       ptr = brnch_nxt(ptr)  
  
	       do while (ptr .gt. 0 .and.   
     &                  (ky(ptr) .eq. ky(oldptr) .and.  
     &                   brid(ptr) .eq. brid(oldptr)))  
  
		 ptr = brnch_nxt(ptr)  
	       enddo  
	      
	     else if ((brtype(ptr) .eq. 3 .or.   
     &                 brtype(ptr) .eq. 5 .or.   
     &                 brtype(ptr) .eq. 6. or.   
     &                 brtype(ptr) .eq. 8) .and. qptr .gt. 0) then  
  
C              L,E,T,TP,LM: Original submittal                  
  
	       iptib1 = fnd_ptib (bus(kx(ptr)), base(kx(ptr)),   
     &                            arcnam(jarzn(kx(ptr))))  
	       iptib2 = fnd_ptib (bus(ky(ptr)), base(ky(ptr)),   
     &                            arcnam(jarzn(ky(ptr))))  
	       if (iptib1 .le. 0 .or. iptib2 .le. 0) then  
	       else  
		 k1 = kx(ptr)  
		 k2 = ky(ptr)  
		 base1c = code (base(k1), 4, 0)  
		 base2c = code (base(k2), 4, 0)  
		 k1x = pti_num(iptib1)  
		 k2x = pti_num(iptib2)  
                 q = brnch_ptr(ptr) 
                 nbr = iabs (q)
                 intovr = kbrnch(15, nbr)
                 if (intovr .gt. 0 .and. q .lt. 0) 
     &             intovr = 3 - intovr
                 if (brtype(ptr) .eq. 2) then
                   pdc = brnch(8,nbr)
                   if (q .lt. 0) pdc = -pdc
                   if (pdc .lt. 0.0) then
                     intovr = 1
                   else
                     intovr = 2
                   endif
                 else if (intovr .eq. 0) then
                   call getchr (3, lown, kbrnch(3,nbr))
                   if (owner(k1) .eq. owner(k2)) then
                     intovr = 1
                     if (inp2alf(k1) .gt. inp2alf(k2))
     &                 intovr = 2
                   else
                     if (lown .eq. owner(k1) .and.
     &                   lown .ne. owner(k2)) then
                       intovr = 2
                     else if (lown .eq. owner(k2) .and.
     &                        lown .ne. owner(k1)) then
                       intovr = 1
                     else
                       intovr = 1
                       if (inp2alf(k1) .gt. inp2alf(k2))
     &                   intovr = 2
                     endif
                   endif
                   if (intovr .gt. 0 .and. q .lt. 0) 
     &                   intovr = 3 - intovr
                 endif
		 ior = ((q .gt. 0 .and. intovr .eq. 2) .or.  
     &                  (q .lt. 0 .and. intovr .eq. 1) .or.
     &                  (q .gt. 0 .and. intovr .eq. 0))
		 if (ior) k2x = -k2x  
		 call gtptibrv (ptr, temp)  
		 id = brid(ptr)  
		 if (id .eq. ' ' .and. option(2) .eq. 'Y') then  
		   id = '1'  
		 else if (id .eq. ' ') then  
		   id = '0'  
		 endif  
		 if (brtype(ptr) .eq. 3 .or.   
     &               brtype(ptr) .eq. 8) then  
  
                   write (xbuf, 10030) k1x, k2x, id,  
     &               (temp(i), i=1,2), (temp(i), i=4,7),  
     &               0.0, 0.0, (temp(i), i=11,14), branch_status(nbr)
		   if (version .ge. 24)  then  
  
c                    Append line length (in miles) and ownership

                     miles = temp(15)
                     iptio = add_ptiq (lown)
                     last = lastch (xbuf)
                     write (xbuf(last+1:), 10032) miles,
     &                 pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0  
		   endif  
		   if (prt_names)  then  
		     last = lastch (xbuf)
                     write (xbuf(last+1:), 10034) bus(k1), base1c,   
     &                 bus(k2), base2c  
		   endif
  
		   total1 = total1 + 1  
                   last = lastch (xbuf)  
                   status = write_ge_file (0, xbuf(1:last))
  
		 else if (brtype(ptr) .eq. 5 .or.   
     &                    brtype(ptr) .eq. 6) then  
  
		   insvc = 1  
  
		   ibtyp1 = kbsdta(1,k1)  
		   ibtyp2 = kbsdta(1,k2)  
		   if (brtype(ptr) .eq. 5 .and.   
     &                (ibtyp1 .eq. 5 .or. ibtyp1 .eq. 12 .or.  
     &                 ibtyp2 .eq. 5 .or. ibtyp2 .eq. 12))  then  
  
c                    Disable any commutator transformer for HVDC  
c                    by setting in-service status to 0 -- commutator  
c                    transformers are represented in PTI DC data groups.  
  
		     insvc = 0  

                   endif
                   tr_gsh = temp(13)/2.0    
                   tr_bsh = temp(14)/2.0  
                   write (xbuf, 10040) k1x, k2x, id,  
     &               (temp(i), i=1,2), (temp(i), i=4,7),  
     &               (temp(i), i=9,10), tr_gsh, tr_bsh,  
     &               tr_gsh, tr_bsh, insvc  
                   if (version .ge. 24)  then  

c                    Append line length (in miles) and ownership

                     miles = 0.0  
                     iptio = add_ptiq (lown)
                     last = lastch (xbuf)  
                     write (xbuf(last+1:), 10032) miles,
     &                 pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 0, 0.0  
		   endif    
		   if (prt_names)  then  
                     last = lastch (xbuf)
		     write (xbuf(last+1:), 10034) bus(k1), base1c, 
     &                 bus(k2), base2c  
                   endif
    
		   total1 = total1 + 1  
                   last = lastch (xbuf)  
                   status = write_ge_file (0, xbuf(1:last))
  
		 endif  
	       endif  
	       ptr = brnch_nxt(ptr)  
  
	     else if ((brtype(ptr) .eq. 2 .or.   
     &                 brtype(ptr) .eq. 7) .and. qptr .gt. 0) then  
  
C              LD, LM: Original submittal                  
  
	       ptr = brnch_nxt(ptr)  
    
	     else if (brtype(ptr) .eq. 2 .or.   
     &                brtype(ptr) .eq. 3 .or.   
     &                brtype(ptr) .eq. 5. or.   
     &                brtype(ptr) .eq. 6. or.   
     &                brtype(ptr) .eq. 7 .or.   
     &                brtype(ptr) .eq. 8 .or.  
     &                brtype(ptr) .eq. 9) then  
	       ptr = brnch_nxt(ptr)  
  
	     else if (brtype(ptr) .eq. 4) then  
  
C              R: Adjustable tap side or hi-low       
  
	       krmap = orienttx (ptr, brnch_nxt(ptr), k1, k2,   
     &                           tap1, tap2)  
c  
c              krmap = 1 if kx(ptr) = fixed-tap  
c                           ky(ptr) = variable-tap side  
c  
c                      2 if kx(ptr) = variable-tap side  
c                           ky(ptr) = fixed-tap  
c  
	       if (krmap .eq. 2) then  
		 k1 = kx(ptr)  
		 k2 = ky(ptr)  
		 base1c = code (base(k1), 4, 0)  
		 base2c = code (base(k2), 4, 0)  
		 iptib1 = fnd_ptib (bus(k1), base(k1),  
     &                              arcnam(jarzn(k1)))  
		 iptib2 = fnd_ptib (bus(k2), base(k2),  
     &                              arcnam(jarzn(k2)))  
		 oldptr = ptr  
		 ptr = brnch_nxt(ptr)  
		 do while (ptr .gt. 0 .and.   
     &                    (ky(ptr) .eq. k2))  
  
		   if (iptib1 .le. 0 .or. iptib2 .le. 0) then  
		   else  
		     k1x = pti_num(iptib1)  
		     k2x = pti_num(iptib2)  
                     q = brnch_ptr(ptr) 
                     nbr = iabs(q)
                     intovr = kbrnch(15, nbr)
                     if (intovr .gt. 0 .and. q .lt. 0) 
     &                 intovr = 3 - intovr
                     if (brtype(ptr) .eq. 2) then
                       pdc = brnch(8,nbr)
                       if (q .lt. 0) pdc = -pdc
                       if (pdc .lt. 0.0) then
                         intovr = 1
                       else
                         intovr = 2
                       endif
                     else if (intovr .eq. 0) then
                       call getchr (3, lown, kbrnch(3,nbr))
                       if (owner(k1) .eq. owner(k2)) then
                         intovr = 1
                         if (inp2alf(k1) .gt. inp2alf(k2))
     &                     intovr = 2
                       else
                         if (lown .eq. owner(k1) .and.
     &                       lown .ne. owner(k2)) then
                           intovr = 2
                         else if (lown .eq. owner(k2) .and.
     &                            lown .ne. owner(k1)) then
                           intovr = 1
                         else
                           intovr = 1
                           if (inp2alf(k1) .gt. inp2alf(k2))
     &                       intovr = 2
                         endif
                       endif
                       if (intovr .gt. 0 .and. q .lt. 0) 
     &                   intovr = 3 - intovr
                     endif
		     ior = ((q .gt. 0 .and. intovr .eq. 2) .or.  
     &                      (q .lt. 0 .and. intovr .eq. 1) .or.
     &                      (q .gt. 0 .and. intovr .eq. 0))
		     if (ior) k2x = -k2x  
		     call gtptibrv (ptr, temp)  
		     id = brid(ptr)  
		     if (id .eq. ' ' .and. option(2) .eq. 'Y') then  
		       id = '1'  
		     else if (id .eq. ' ') then  
		       id = '0'  
		     endif  
  
		     if (brtype(ptr) .eq. 5 .or.   
     &                   brtype(ptr) .eq. 6) then  
  
		       insvc = 1  
  
		       ibtyp1 = kbsdta(1,k1)  
		       ibtyp2 = kbsdta(1,k2)  
		       if (brtype(ptr) .eq. 5 .and.  
     &                    (ibtyp1 .eq. 5 .or. ibtyp1 .eq. 12 .or.  
     &                     ibtyp2 .eq. 5 .or. ibtyp2 .eq. 12))  then  
  
c                        Disable any commutator transformer for HVDC  
c                        by setting in-service status to 
c                        0 -- commutator  
c
c                        Transformers are represented in PTI DC data 
c                        groups.  Also, connect a low-impedance line 
c                        in parallel.  
  
			 idpl = char (ichar(id) + 1)  
			 write (xbuf, 10030)  k1x, k2x, idpl,  
     &                          0.0, 0.0003, 0.0, (temp(i), i=5,7),   
     &                          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1  
			 if (version .ge. 24)  then  

c                          Append line length (in miles) and ownership

                           miles = 0.0  
                           iptio = add_ptiq (lown)
			   last = lastch (xbuf)  
			   write (xbuf(last+1:), 10032) miles,
     &                       pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &                       0, 0.0  
			 endif  
                         if (prt_names)  then  
                           last = lastch (xbuf)
                           write (xbuf(last+1:), 10034) bus(k1), 
     &                       base1c, bus(k2), base2c  
                         endif
                         last = lastch (xbuf)  
                         status = write_ge_file (0, xbuf(1:last))
  
			 insvc = 0  
		       endif  
  
                       tr_gsh = temp(13)/2.0    
                       tr_bsh = temp(14)/2.0  
                       write (xbuf, 10040) k1x, k2x, id,  
     &                   (temp(i), i=1,2), (temp(i), i=4,7),  
     &                   (temp(i), i=9,10), tr_gsh, tr_bsh,  
     &                    tr_gsh, tr_bsh, insvc  
                       if (version .ge. 24)  then  

c                        Append line length (in miles) and ownership

                         miles = 0.0  
		         iptio = add_ptiq (lown)
                         last = lastch (xbuf)  
			 write (xbuf(last+1:), 10032) miles,
     &                     pti_onum(iptio), 1.0, 0, 0.0, 0, 0.0, 
     &                     0, 0.0  
		       endif  
  
		       if (prt_names)  then  
                         last = lastch (xbuf)
		         write (xbuf(last+1:), 10034) bus(k1), base1c, 
     &                     bus(k2), base2c  
                       endif

		       total1 = total1 + 1  
                       last = lastch (xbuf)  
                       status = write_ge_file (0, xbuf(1:last))
		     endif  
		   endif  
		   ptr = brnch_nxt(ptr)  
		 enddo  
	       else  
		 oldptr = ptr  
		 ptr = brnch_nxt(ptr)  
  
		 do while (ptr .gt. 0 .and.   
     &                     ky(ptr) .eq. ky(oldptr))  
       
		   ptr = brnch_nxt(ptr)  
		 enddo  
	       endif  
	     else  
	       write ( errbuf(1), 10060) lntype(brtype(ptr)),   
     &           bus(kx(ptr)), base(kx(ptr)), bus(ky(ptr)),   
     &           base(kx(ptr)), brid(ptr), brsect(ptr)  
10060          format (' Failed to process branch [', a, 1x,  
     &           a8, f6.1, 1x, a8, f6.1, 1x, a, i2,   
     &           '] -- invalid type??')  
	       call prterx ('W', 1)  
	       ptr = brnch_nxt(ptr)  
	     endif  
	   enddo  
  
	 enddo  
  
         xbuf = ' 0 / Begin Transformer Adjustment Data'
	 last = lastch (xbuf)  
         status = write_ge_file (0, xbuf(1:last))
c  
c        Second pass - Generate PTI Transformer Adjustment Data  
c  
	 do jt = 1, ntota  
	    ienctl = 1  
	    ityp = mod(ltran(10,jt),100)    
  
	    if (ityp .gt. 20)  then  
c             Control was permanently disabled by LTC option throughout IPF   
c             solution.  Write the controller record with control inactive.  
c  
	      ienctl = 0  
	      ityp = ityp - 20  
	    endif  
	    if (ityp .gt. 5)  then  
c             Control was temporarily disabled by IPF to facilitate  
c             solution.  Write the controller record with control inactive.  
c  
	      ienctl = 0  
	      ityp = ityp - 5  
	    endif  
  
	    if (ityp .eq. 1 .or. ityp .eq. 2)  then  
	      k = ltran(1,jt)  
	      m = ltran(9,jt)  
	      kc = ltran(2,jt)   
	      if (kc .eq. -1) then   
		kc = k   
	      else if (kc .eq. -2) then  
		kc = m  
	      else if (kc .eq. 0) then  
		kc = k  
	      endif  
              if (ordltc .eq. 2) then  
                if (kc .gt. 0) kc = opt2inp(kc)  
                k = opt2inp(k)  
                m = opt2inp(m)  
              endif  
	    else if (ityp .eq. 3)  then     
	      k = ltran(9,jt)  
	      m = ltran(1,jt)  
	      kc = 0  
              if (ordltc .eq. 2) then  
                if (kc .gt. 0) kc = opt2inp(kc)  
                k = opt2inp(k)  
                m = opt2inp(m)  
              endif  
	    else    
	      k = ltran(1,jt)  
	      m = ltran(9,jt)  
	      kc = 0  
              if (ordltc .eq. 2) then  
                if (kc .gt. 0) kc = opt2inp(kc)  
                k = opt2inp(k)  
                m = opt2inp(m)  
              endif  
	      base1c = code (base(k), 4, 0)  
	      base2c = code (base(m), 4, 0)  
	      write (errbuf(1), 10068) ityp, bus(k), base1c, 
     &           bus(m), base2c  
10068         format ('Unsupported IPF type ', i2, ' LTC ', 
     &           a8, 1x, a4, 1x, a8, 1x, a4)
	      call prterx ('E', 1)
	    endif  
	    if (kbsdta(1,k) .eq. 5 .or. kbsdta(1,k) .eq. 12 .or.
     &          kbsdta(1,m) .eq. 5 .or. kbsdta(1,m) .eq. 12)
     &        go to 100
            lt = ltran(10,jt) / 100   
c  
c           Obtain tmax, tmin  
c  
	    if (ityp .eq. 1 .or. ityp .eq. 2) then    
	      rma = 1.0 / tran(8,jt)  
	      rmi = 1.0 / tran(7,jt)  
	      steps = tran(11,jt)    
	      if (steps .gt. 1.0) then   
		tdisc = (rma - rmi) / (steps - 1.0)   
	      else   
		tdisc = 0.0005  
	      endif  
	    else if (ityp .eq. 3)  then  
	      rma = -57.295779 * tran(8,jt)  
	      rmi = -57.295779 * tran(7,jt)   
	      steps = tran(11,jt)    
	      if (steps .gt. 1.0) then   
		tdisc = (rma - rmi) / (steps - 1.0)   
	      else   
		tdisc = 60.0 / 2000.0  
	      endif  
	    endif  
C  
C           Obtain Pmax, Pmin  
C  
	    if (ityp .eq. 1) then    
	      kt = inp2opt(kc)  
	      vma = vlimx(kt)  
	      vmi = vlimn(kt)  
	      if (vma-vmi .lt. tdisc) then  
		vma = vma + tdisc  
		vmi = vmi - tdisc  
	      endif  
              write (vmaxc, fmt='(f8.5)') vma
              write (vminc, fmt='(f8.5)') vmi
	    else if (ityp .eq. 2) then  
	      vma = -tran(5,jt) * bmva  
	      vmi = -tran(4,jt) * bmva  
	      if (vma - vmi .lt. 5.0) then  
		vmavg = 0.5 * (vma + vmi)  
		vma = vmavg + 2.5  
		vmi = vmavg - 2.5  
	      endif  
              write (vmaxc, fmt='(f8.2)') vma
              write (vminc, fmt='(f8.2)') vmi
	    else if (ityp .eq. 3)  then  
	      vma = -tran(5,jt) * bmva  
	      vmi = -tran(4,jt) * bmva  
	      if (vma - vmi .lt. 5.0) then  
		vmavg = 0.5 * (vma + vmi)  
		vma = vmavg + 2.5  
		vmi = vmavg - 2.5  
	      endif  
              write (vmaxc, fmt='(f8.2)') vma
              write (vminc, fmt='(f8.2)') vmi
	    endif  
  
	    base1c = code (base(k), 4, 0)  
	    base2c = code (base(m), 4, 0)  
	    iptib1 = fnd_ptib (bus(k), base(k), arcnam(jarzn(k)))  
	    iptib2 = fnd_ptib (bus(m), base(m), arcnam(jarzn(m)))  
	    if (ityp .eq. 1) then  
	      iptibc = fnd_ptib (bus(kc), base(kc), arcnam(jarzn(kc)))  
	      icont = pti_num(iptibc)  
	    else  
	      icont = 0  
	    endif  
  
	    ptr = numbrn (k, m, '*', 0)  
	    oldptr = ptr  
	    if (brtype(ptr) .eq. 4) ptr = brnch_nxt(ptr)  
	    do while (ptr .gt. 0 .and.   
     &                kx(ptr) .eq. kx(oldptr) .and.  
     &                ky(ptr) .eq. ky(oldptr))  
	      id = brid(ptr)  
	      if (id .eq. ' ' .and. option(2) .eq. 'Y') then  
		id = '1'  
	      else if (id .eq. ' ') then  
		id = '0'  
	      endif  
  
	      if (ityp .eq. 1) then  
  
                write (xbuf, 10070) pti_num(iptib2), pti_num(iptib1),  
     &            id, icont, rma, rmi, vmaxc, vminc, tdisc,  
     &            0, ienctl, 0.0, 0.0  
10070           format (i5, i7, 1x, a, i6, 2f9.5, 1x, a8, 1x, a8,
     &            f9.5, 2i3, 2f5.1)  
		if (prt_names)  then  
		  last = lastch (xbuf)
                  write (xbuf(last+1:), 10072) bus(m), base2c, 
     &              bus(k), base1c  
10072             format (' / ', a8, 1x, a4, 1x, a8, 1x, a4)  
		endif
  
	      else if (ityp .eq. 2) then  
		  
		write (xbuf, 10080) pti_num(iptib2), pti_num(iptib1),  
     &            id, icont, rma, rmi, vmaxc, vminc, tdisc,  
     &            0, ienctl, 0.0, 0.0  
10080           format (i5, i7, 1x, a, i6, 2f9.5, 1x, a8, 1x, a8,
     &            f9.5, 2i3, 2f5.1)  
		if (prt_names)  then  
		  last = lastch (xbuf)
		  write (xbuf(last+1:), 10072) bus(m), base2c, 
     &              bus(k), base1c  
		endif
  
	      else if (ityp .eq. 3)  then  
  
                write (xbuf, 10090) pti_num(iptib1), pti_num(iptib2),  
     &            id, icont, rma, rmi, vmaxc, vminc, tdisc,  
     &            0, ienctl, 0.0, 0.0  
10090           format (i5, i7, 1x, a, i6, 2f9.1, 1x, a8, 1x, a8, 
     &            f9.3, 2i3, 2f5.1)  
		if (prt_names)  then  
		  last = lastch (xbuf)
		  write (xbuf(last+1:), 10072) bus(k), base1c, 
     &              bus(m), base2c  
		endif
#  
	      endif  
  
	      total3 = total3 + 1  
              last = lastch (xbuf)  
              status = write_ge_file (0, xbuf(1:last))
	      ptr = brnch_nxt(ptr)  
	    enddo  
  100       continue
	 enddo  
  
         xbuf = ' 0 / Begin Area Interchange Data'
	 last = lastch (xbuf)  
         status = write_ge_file (0, xbuf(1:last))
  
	 write ( errbuf(1), 10100) total1  
10100    format (' Total branch records extracted:', t45, i5)  
	 write ( errbuf(2), 10110) total2  
10110    format (' Total pseudo branch records extracted:', t45, i5)  
	 write ( errbuf(3), 10120) total3  
10120    format (' Total LTC records extracted:', t45, i5)  
	 call prterx ('I', 3)  
   
	 ext_brn = total1 + total2 + total3  
  
	 return  
	 end  
  
