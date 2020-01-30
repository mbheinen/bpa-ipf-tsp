C    @(#)ext_gezon.f	20.8 8/30/00
C****************************************************************
C
C     File: ext_gezon.f
C
C     Purpose: Routine to extract zone interchange data in GE format
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
      integer function ext_gezon (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

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
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/qksrt.inc'
 
      integer fnd_ptiy, status, error, write_ge_file, add_ptiz
      character xbuf*256, zonename*32
      external kmp_ptib, swp_ptib
      logical duplicate

      ext_gezon = 0       ! set default return status = successful
      error = 0           ! initialize error count

      write (*, 10000) nztot
10000 format (' * Writing ', i5, ' zone records to NETDAT file')

      write (xbuf, 10010) nztot
10010 format ('zone data  [', i3, ']                            pnet    
     & qnet')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Sort pti_znum()
c
      do i = 1, num_znam
        sort(i) = i
      enddo

      key = 6
      if (num_znam .gt. 0) then
        call qiksrt (1, num_znam, kmp_ptib, swp_ptib)
      endif
c
c     Add any missing zones from arcnam() to pti_znum()/pti_znam()
c
      lastzone = pti_znum(sort(num_znam))
      do jt = 1, nztot
        iptiz = fnd_ptiy(acznam(jt))
        if (iptiz .le. 0) then
          write ( errbuf(1), 10020) lastzone+1, acznam(jt)
10020     format (' Adding missing zone ', i4, ' (', a2, 
     &      ' to zone data segment')
          call prterx ('W', 1)
          lastzone = lastzone + 1
          num = add_ptiz (lastzone, acznam(jt)) 
          zone_name(num) = 'ZONE ' // acznam(jt)
        endif
      enddo

      do i = 1, num_znam
        if (i .gt. 1) then
          duplicate = (pti_znum(sort(i-1)) .eq. pti_znum(sort(i)) .and.
     &                 zone_name(sort(i-1)) .eq. zone_name(sort(i)))
        else
          duplicate = .false.
        endif
        if (.not. duplicate) then
          write (xbuf, 10040) pti_znum(sort(i)), zone_name(sort(i)),
     &      0.0, 0.0
10040     format (2x, i3, 1x, '"', a, '"', 2(1x, f9.3))
          last = lastch (xbuf)
          status = write_ge_file (0, xbuf(1:last))
        endif
      enddo

      write ( errbuf(1), 10110) num_znam
10110 format (' Total zone records extracted:', i5)
      call prterx ('I', 1)
 
      ext_gezon = nztot

      return
      end
