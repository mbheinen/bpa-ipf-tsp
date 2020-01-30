C    @(#)ext_ptia.f	20.8 5/3/00
C****************************************************************  
C  
C     File: ext_ptia.f  
C  
C     Purpose: Routine to extract area interchange data in PTI format  
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
      integer function ext_ptia (savfil, version, option)  
      integer savfil, version  
      character *(*) option(10)  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/filnam.inc'  
      include 'ipfinc/lfiles.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/alpha.inc'  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/arcntl.inc'  
      include 'ipfinc/ordsta.inc'  
   
      integer gtptinum, status, total1, write_ge_file
      character code*10, xbuf*132, base1c*4  
      logical prt_names  
  
      prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  
  
      total1 = 0  
      do ic = 1, ntotc  
         nb = karea(1,ic)  
         if (ordtie .eq. 2) nb = opt2inp(nb)  
         base1c = code (base(nb), 4, 0)  
         status = gtptinum (nb, iptia, iptiz, iptib)  
  
         write (xbuf, 10000)  pti_anum(iptia), pti_num(iptib),  
     &     area(2,ic) * bmva, 3.0, arcnam(ic)(1:8)  
10000    format (i3, i6, 1x, f8.1, f5.1, 2x, '''', a, '''')  
         if (prt_names)  then  
           last = lastch (xbuf)
           write (xbuf(last+1:), 10010) bus(nb), base1c   
10010      format (' / slack bus : ', a, 1x, a)  
         endif
  
         last = lastch (xbuf)  
         status = write_ge_file (0, xbuf(1:last))
         total1 = total1 + 1  
      enddo  
  
      xbuf = ' 0 / Begin 2-terminal D-C Data'
      last = lastch (xbuf)  
      status = write_ge_file (0, xbuf(1:last))
  
      write ( errbuf(1), 10100) total1  
10100 format (' Total area interchange records extracted:', i5)  
   
      ext_ptia = total1  
  
      return  
      end  
