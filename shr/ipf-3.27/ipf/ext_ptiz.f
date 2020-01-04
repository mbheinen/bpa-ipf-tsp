C    @(#)ext_ptiz.f	20.7 5/3/00
C****************************************************************
C
C     File: ext_ptiz.f
C
C     Purpose: Routine to extract zone data in PTI format
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
      integer function ext_ptiz (savfil, version, option)
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
c     Sort zone names
c
      do i = 1, num_znam
         array(1,i) = i
         array(2,i) = 0
         array(3,i) = 0
         array(4,i) = 0
      enddo
      key = 4

      call qiksrt (1, num_znam, kmp_ptiz, swp_ptiz)

      total1 = 0
      do i = 1, num_znam
         indz = array(1,i)
         write (xbuf, 10000) pti_znum(indz), pti_znam(indz)
10000    format (i3, 1x, '''', a, '''')
         last = lastch (xbuf)
         status = write_ge_file (0, xbuf(1:last))
         total1 = total1 + 1
      enddo

      xbuf = ' 0 / Begin Area Interchange "I" Data'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write ( errbuf(1), 10100) total1
10100 format (' Total zones extracted:', i5)
 
      ext_ptiz = total1

      return
      end
