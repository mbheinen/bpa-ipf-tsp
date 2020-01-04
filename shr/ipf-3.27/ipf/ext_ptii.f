C    @(#)ext_ptii.f	20.8 5/3/00
C****************************************************************  
C  
C     File: ext_ptii.f  
C  
C     Purpose: Routine to extract area intertie "I" data in PTI format  
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
      integer function ext_ptii (savfil, version, option)  
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
   
      integer fnd_ptia, total1, status, write_ge_file
      character xbuf*132, tempc*10  
      logical prt_names  
  
      prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  
  
      total1 = 0  
      do nn = 1, ntotic  
        if ( kompr (arcint(1,nn), arcint(2,nn), junk) .lt. 0) then  
          tempc = arcint(1,nn)(1:8)  
          inda1 = fnd_ptia (tempc)  
          tempc = arcint(2,nn)(1:8)  
          inda2 = fnd_ptia (tempc)  
  
          write (xbuf, 10010) pti_anum(inda1), pti_anum(inda2),  
     &      '1', arcinp(nn)  
10010     format (i3, i4, 1x, a, f8.1)  
          if (prt_names)  then  
            last = lastch (xbuf)
            write (xbuf(last+1:), 10020) arcint(1,nn), arcint(2,nn)  
10020       format (' / ', a, 1x, a)  
          endif
  
          last = lastch (xbuf)  
          status = write_ge_file (0, xbuf(1:last))
          total1 = total1 + 1  
        endif  
      enddo  
  
      xbuf = ' 0 / Begin Ownership Data'
      last = lastch (xbuf)  
      status = write_ge_file (0, xbuf(1:last))
  
      write ( errbuf(1), 10100) total1  
10100 format (' Total area interchange "I" records extracted:', i5)  
   
      ext_ptii = total1  
  
      return  
      end  
