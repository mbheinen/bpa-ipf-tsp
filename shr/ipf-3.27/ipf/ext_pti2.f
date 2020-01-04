C    @(#)ext_pti2.f	20.13 8/30/00
C****************************************************************  
C  
C     File: ext_pti2.f  
C  
C     Purpose: Routine to extract 2-terminal d-c data in PTI format  
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
      integer function ext_pti2 (savfil, version, option)  
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
      include 'ipfinc/dc2t.inc'  
      include 'ipfinc/tran.inc'  
      include 'ipfinc/ordsta.inc'  
   
      integer fnd_ptib, total1, status, write_ge_file
      character code*10, xbuf*132, base1ch*4, base2ch*4
      logical prt_names

      parameter (RD2DG = 180.0 / 3.1415926536)

      prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  

      total1 = 0
      do i = 1, kdtot
	 k1  = dc2t(1,i)
	 k2  = dc2t(3,i)
	 k1c = dc2t(33,i)
	 k2c = dc2t(34,i)
	 if (orddc .eq. 2) then
	   k1  = opt2inp(k1)
	   k2  = opt2inp(k2)
	   k1c = opt2inp(k1c)
	   k2c = opt2inp(k2c)
	 endif
	 base1ch = code (base(k1), 4, 0)
	 base2ch = code (base(k2), 4, 0)
	 iptib1 =  fnd_ptib (bus(k1), base(k1), arcnam(jarzn(k1)))
	 iptib2 =  fnd_ptib (bus(k2), base(k2), arcnam(jarzn(k2)))
	 ipr = pti_num(iptib1)
	 ipi = pti_num(iptib2)
	 setvl = dc2t(5,i)
	 if (dc2t(7,i) .eq. 2)  setvl = -setvl
	 vreg = dc2t(6,i)
	 rdc = dc2t(8,i)
c
c        D-c record No. 1
c
	 write (xbuf, 10000) i, 1, rdc, setvl, vreg,
     &     0.80*vreg, rdc, 0.10, 'I', 0.98*vreg
10000    format (i3, i2, f8.2, f7.1, 2f8.1, f7.2, f4.1, 1x, a, f8.1)
	 last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))
	 total1 = total1 + 1
c
c        D-c record No. 2
c
	 numbr = dc2t(12,i)
	 ltc = dc2t(37,i)
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

	 alfmin = dc2t(18,i) * RD2DG
	 alfstp = dc2t(19,i) * RD2DG 
	 alfnrm = dc2t(21,i) * RD2DG
	 alpha  = dc2t(22,i) * RD2DG
	 gamma0 = dc2t(25,i) * RD2DG
	 gamma  = dc2t(26,i) * RD2DG

c        The following four values are each "total ohms" -- secondary (DC)
c        side transformer impedances multiplied by number of bridges, 
c        whereas PSS/E requires values in ohms per bridge (at one
c        transformer per bridge).  The user must provide equivalent 
c        (parallel) transformer impedance values in IPF data as required.
  
	 rcr = dc2t(29,i)
	 xcr = dc2t(30,i)
	 rci = dc2t(31,i)
	 xci = dc2t(32,i)

	 erralf = abs (alfnrm - alpha)
	 errgam = abs (gamma0 - gamma)
	 alfmn = amin1 (alpha - erralf, alfmin)
	 alfmx = amin1 (2*alpha - alfmin, alfstp)
	 gammn = amax1 (gamma - errgam, gamma0)
	 gammx = amin1 (gamma, gamma0) + errgam

         if (k1c .gt. 0) then
           iptib1c =  fnd_ptib (bus(k1c), base(k1c), 
     &                          arcnam(jarzn(k1c)))
           iprc = pti_num(iptib1c)
         else
           iprc = 0
         endif
         write (xbuf, 10010) ipr, numbr, alfmx, alfmn, rcr, xcr, 
     &     base(k1c), tr_nom, tapact, tapmax, tapmin, tdisc, 0, 0,
     &     0, '1'
10010    format (i5, i3, 2f7.1, 2f7.3, f7.1, 5f8.5, 3i2, 1x, a)
	 if (prt_names)  then
           last = lastch (xbuf)
	   write (xbuf(last+1:), 10020) bus(k1), base1ch
10020      format (' / ', a, 1x, a)
	 endif

	 last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))
	 total1 = total1 + 1
c
c        D-c record No. 3
c
	 numbr = dc2t(15,i)
	 ltc = dc2t(38,i)

	 if (ltc .gt. 0) then
	   tr_nom = base(k2) / base(k2c)
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

         if (k2c .gt. 0) then
           iptib2c =  fnd_ptib (bus(k2c), base(k2c), 
     &                          arcnam(jarzn(k2c)))
           ipic = pti_num(iptib2c)
         else
           ipic = 0
         endif

	 write (xbuf, 10010) ipi, numbr, gammn, gammn, rci, xci, 
     &     base(k2c), tr_nom, tapact, tapmax, tapmin, tdisc, 0, 0,
     &     0, '1'
	 if (prt_names)  then
           last = lastch (xbuf)
	   write (xbuf(last+1:), 10020) bus(k2), base2ch
	 endif

	 last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))
	 total1 = total1 + 1

      enddo

      xbuf = ' 0 / Begin Swithced Reactance Data'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write ( errbuf(1), 10100) total1
10100 format (' Total 2-terminal d-c records extracted:', i5)
 
      ext_pti2 = total1

      return
      end
