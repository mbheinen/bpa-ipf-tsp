C    @(#)ext_gezdt.f	20.3 2/28/00
C****************************************************************
C
C     File: ext_gezdt.f
C
C     Purpose: Routine to extract z data in GE format
C
C     Input parameters:
C
C             savfil   - the logical unit opened
C             version  - "23" or "24"
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: save_ged.f
C
C****************************************************************
      integer function ext_gezdt (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tx_misc.inc'
 
      integer status, error, gtge_num, write_ge_file
      character xbuf*256, zdatac(22)*8

      ext_gezdt = 0       ! set default return status = successful
      error = 0           ! initialize error count

      write (*, 10000) num_zdata
10000 format (' * Writing ', i5, ' z table data records to NETDAT file')

      write (xbuf, 10010) num_zdata
10010 format ('z table data  [', i2, ']')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do jt = 1, num_zdata
        k1 = tx_zdata(1,jt)
c
c       Get GE area, zone, owner, and bus number
c
        status = gtge_num (k1c, iptia, iptiz, iptib)
        if (status .ne. 0) then
          error = 5
          go to 900
        endif
        itype = tx_zdata(2,jt)
        do i = 3, 22
          write (zdatac(i), fmt='(f8.3)') tx_zdata(i,jt)
        enddo
        write (xbuf, 10040) pti_num(iptib), itype, 
     &    (zdatac(j),j=3,22)
10040   format (2x, i5, 1x, i1, 12(1x, a, 1x, a))
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))
  900   continue
      enddo

      write ( errbuf(1), 10110) num_zdata
10110 format (' Total z table records extracted:', i5)
      call prterx ('I', 1)
 
      ext_gezdt = num_zdata

      return
      end
