C    @(#)ext_ptio.f	20.5 5/3/00
C****************************************************************
C
C     File: ext_ptio.f
C
C     Purpose: Routine to extract ownership data in PTI format
C
C     Input parameters:
C
C             savfil   - the logical unit opened
C             version  - "23" or "24"
C
C     Author: Walt Powell  Date: 24 May 2000
C     Called by: saveptid.f
C
C****************************************************************
      integer function ext_ptio (savfil, version, option)
      integer savfil, version
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc' 
 
      common /scratch/ count, array(4,MAXBUS)
      integer count, array

      integer total1, status, write_ge_file
      external kmp_ptiz, swp_ptiz
      character xbuf*132
c
c     Sort owner names
c
      do i = 1, num_onam
         array(1,i) = i
         array(2,i) = 0
         array(3,i) = 0
         array(4,i) = 0
      enddo
      key = 7

      call qiksrt (1, num_onam, kmp_ptiz, swp_ptiz)

      total1 = 0
      do i = 1, num_onam
         indo = array(1,i)
         write (xbuf, 10000) pti_onum(indo), pti_onam(indo)
10000    format (i3, 1x, '''', a, '''')
         last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))
         total1 = total1 + 1
      enddo

      xbuf = ' 0'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write ( errbuf(1), 10100) total1
10100 format (' Total owners extracted:', i5)
 
      ext_ptio = total1

      return
      end
