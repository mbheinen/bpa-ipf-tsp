C    @(#)saveptid.f	20.10 5/3/00
C****************************************************************
C
C     File: saveptid.f
C
C     Purpose: Routine to save a network data file in PTI format
C
C     / LOAD_PTI, FILE = <filename>, -
C                 TRNFILE = <filename>, -
C                 VERSION = <number>
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ctlpow.f
C
C****************************************************************
      integer function saveptid (scrfil, filename, version, option, 
     &                           error)
      integer scrfil, version, error
      character *(*) filename, option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
 
      common /verify_pti/ num_verify, hash_verify(PTI_HASHSIZE), 
     &                    next_verify(PTI_MAXBUS), 
     &                    ptinum_verify(PTI_MAXBUS),
     &                    bpanum_verify(PTI_MAXBUS)
      integer num_verify, hash_verify, next_verify, ptinum_verify,
     &                    bpanum_verify

      integer ext_ptib, ext_ptil, ext_ptia, ext_pti2, ext_ptix, 
     &        ext_ptim, ext_ptis, ext_ptiz, ext_ptii, status, 
     &        write_ge_file
      character month(12)*3, xbuf*132

      data  month / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
     &              'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /

      saveptid = 0        ! set default return status = successful
      error = 0           ! initialize error count
c
c     Initialize hash arrays to capture duplicate numbers OF buses
c     actually used
c
      num_verify = 0
      do i = 1, PTI_MAXBUS
        ptinum_verify(i) = 0
        bpanum_verify(i) = 0
        next_verify(i) = 0
      enddo
      do i = 1, PTI_HASHSIZE
        hash_verify(i) = 0
      enddo
c
c     set up comment lines
c     
      if (index (cspare(30), char(0)) .eq. 1 .or.
     &    cspare(30) .eq. ' ') cspare(30) = 'data'
      call n_date (imon, idate, iyear)
      if (iyear .lt. 50) then
        iyear = 2000 + iyear
      else
        iyear = 1900 + iyear
      endif
      call n_time (ihour, imin, isec)

      write (xbuf, 10000) 0, bmva, idate, month(imon), iyear, ihour, 
     &                      imin, isec
10000 format (i1, f6.1, ' / ', i2, '-', a, '-', i4, 1x, i2.2, ':',
     &        i2.2, ':', i2.2)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
      write (xbuf, '(a)') cspare(30)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
      write (xbuf, '(a)') cspare(30)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
 
c     Extract bus data in PTI format
      numb = ext_ptib ( scrfil, version, option)

c     Extract branch data in PTI format
      numl = ext_ptil ( scrfil, version, option)

c     Extract area interchange data in PTI format
      numa = ext_ptia ( scrfil, version, option)

c     Extract two-terminal d-c data in PTI format
      num2 = ext_pti2 ( scrfil, version, option)

c     Extract switched reactance data in PTI format
      numx = ext_ptix ( scrfil, version, option)
c
c     No transformer impedance data available
c
      xbuf = ' 0 / Begin Multi-terminal D-C Data'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
      
c     Extract multi-terminal d-c data in PTI format
      numm = ext_ptim ( scrfil, version, option)

c     Extract line section data in PTI format
      nums = ext_ptis ( scrfil, version, option)

c     Extract zone data in PTI format
      numz = ext_ptiz ( scrfil, version, option)

c     Extract area interchange "I" data in PTI format
      numi = ext_ptii ( scrfil, version, option)

c     Extract ownership data in PTI format
      if (version .ge. 24) then
        numo = ext_ptio (scrfil, version, option)
      endif

  900 continue
      return
      end
