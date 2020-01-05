C    @(#)ext_ptis.f	20.10 5/3/00
C****************************************************************  
C  
C        File: ext_ptis.f  
C  
C        Purpose: Routine to extract branch section data in PTI format  
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
         integer function ext_ptis (savfil, version, option)  
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
  
         character xbuf*132, code*4, base1c*4, base2c*4, lntype(9)*2,  
     &             id*1
         integer total1, ptr, fnd_ptib, status, section(10), 
     &           write_ge_file
         logical ior, prt_names  
  
         data lntype / 'L*', 'LM', 'L ', 'R ', 'T ', 'TP', 'E ', 'LM',  
     &                 'RZ' /  
  
         prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  
  
         ext_ptis = 0  
         total1 = 0       ! Count of pseudo-branches extracted  
  
         status = 0       ! Begin new branch  
         ix = 1  
	 do while (ix .le. num_pseudo)  
           if (pseudo(2,ix) .eq. 0) then                           
             ix = ix + 1  
           else if (pseudo(2,ix) .eq. 1) then  
             k1 = pseudo(3,ix)  
             nsec = 1  
             section(nsec) = pseudo(4,ix)  
             k2 = 0  
             ptr = pseudo(1,ix)  
             nbr = iabs(brnch_ptr(ptr))  
             ior = ((brnch_ptr(ptr) .gt. 0 .and.   
     &               kbrnch(15, nbr) .eq. 2) .or.  
     &              (brnch_ptr(ptr) .lt. 0 .and.   
     &               kbrnch(15, nbr) .eq. 1) .or.
     &              (brnch_ptr(ptr) .gt. 0 .and.
     &               kbrnch(15, nbr) .eq. 0))

             ix = ix + 1  
             do while (ix .le. num_pseudo .and. k2 .eq. 0)  
               if (pseudo(2,ix) .eq. 2) then   
                 nsec = nsec + 1  
                 section(nsec) = pseudo(4,ix)  
                 ix = ix + 1  
               else if (pseudo(2,ix) .eq. 3) then  
                 k2 = pseudo(4,ix)  
                 ix = ix + 1  
               else   
                 k2 = -1  
               endif  
             enddo  
             if (k2 .gt. 0) then  
               base1c = code (base(k1), 4, 0)  
               base2c = code (base(k2), 4, 0)  
               iptib1 = fnd_ptib (bus(k1), base(k1), arcnam(jarzn(k1)))  
               iptib2 = fnd_ptib (bus(k2), base(k2), arcnam(jarzn(k2)))  
               numpti1 = pti_num(iptib1)  
               numpti2 = pti_num(iptib2)  
               if (ior) numpti2 = -numpti2  
               id = brid(ptr)  
               if (id .eq. ' ' .and. option(2) .eq. 'Y') then  
                 id = '1'  
               else if (id .eq. ' ') then  
                 id = '0'  
               endif  
               write (xbuf, 10000) numpti1, numpti2, '&' // id, id  
10000          format (i5, i7, 1x, a, 1x, a)  
               do i = 1, nsec  
                 nb = section(i)  
                 iptib2 = fnd_ptib (bus(nb), base(nb),   
     &                              arcnam(jarzn(nb)))  
                 last = lastch (xbuf)  
                 write (xbuf(last+1:), 10010) pti_num(iptib2), id  
10010            format (i6, 1x, a)  
               enddo  
  
               last = max0 (100, lastch (xbuf))  
  
               if (prt_names)  then  
                 write (xbuf(last+1:), 10020) bus(k1), base1c,   
     &                  bus(k2), base2c  
10020            format (' / ', a, 1x, a, 1x, a, 1x, a)  
               endif  
  
               last = lastch (xbuf)  
               total1 = total1 + 1  
               status = write_ge_file (0, xbuf(1:last))
             else  
               write ( errbuf(1), 10030) lntype(brtype(ptr)),   
     &           bus(kx(ptr)), base(kx(ptr)), bus(ky(ptr)),   
     &           base(kx(ptr)), brid(ptr), brsect(ptr)  
10030          format (' Failed to process branch [', a, 1x,  
     &           a8, f6.1, 1x, a8, f6.1, 1x, a, i2,   
     &           '] using pseudo-sections')  
               call prterx ('W', 1)  
             endif  
           else  
             ptr = pseudo(1,ix)  
             write ( errbuf(1), 10030) lntype(brtype(ptr)),   
     &         bus(kx(ptr)), base(kx(ptr)), bus(ky(ptr)),   
     &         base(kx(ptr)), brid(ptr), brsect(ptr)  
               call prterx ('W', 1)  
             ix = ix + 1  
           endif  
         enddo  
  
         xbuf = ' 0 / Begin Zone Data'
         last = lastch (xbuf)  
         status = write_ge_file (0, xbuf(1:last))
  
         write ( errbuf(1), 10100) total1  
10100    format (' Total section records extracted:', t45, i5)  
         call prterx ('I', 1)  
   
         ext_ptis = total1  
  
         return  
         end  
  
