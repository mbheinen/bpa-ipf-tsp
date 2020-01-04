C    @(#)ext_geown.f	20.12 8/30/00
C****************************************************************
C
C     File: ext_geown.f
C
C     Purpose: Routine to extract owner data in GE format
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
      integer function ext_geown (scrfil, version, option, total)
      integer scrfil, version, total(4)
      character *(*) option(10)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/owner_cm.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
 
      integer MAXPTIRECORDS
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                 next_ptr2(MAXBUS), count_newbus, 
     &                 newbusno(MAXBUS), count_newzone, 
     &                 newzoneno(MAXCZN), count_newown, 
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone, newzoneno, count_newown, newownno,
     &        sort_tempc
      character tempc*80

      common /bpa_num / user_rule, num_area_rule, num_zone_rule, 
     &                  num_owner_rule, num_default_rule, 
     &                  area_rule(3,MAXCAR), zone_rule(3,MAXZON), 
     &                  owner_rule(3,MAXOWN), default_rule(2,100), 
     &                  owner_code_ge(MAXOWN)
      integer user_rule, num_area_rule, num_zone_rule, num_owner_rule,
     &        num_default_rule, area_rule, zone_rule, owner_rule, 
     &        default_rule
      character owner_code_ge*4

      integer   MAXOWNERS
      parameter (MAXOWNERS = 250)    !max # local symbols

      character owner_o(MAXOWNERS)*3

      integer status, error, write_ge_file, fnd_ptiq, add_ptio, 
     &        read_ge_file, open_ge_file
      logical finished
      character xbuf*256, last_owner_name*3
      external kmp_ptib, swp_ptib

      ext_geown = 0       ! set default return status = successful
      error = 0           ! initialize error count
c
c     Sort pti_onum()
c
      do i = 1, num_onam
        sort(i) = i
      enddo

      key = 7
      if (num_onam .gt. 0) then
        call qiksrt (1, num_onam, kmp_ptib, swp_ptib)
      endif
c
c     Because of common name duplication, retrieve these common
c     varibles through a specialized interface routine
c
      call gt_intcom ('NUMOWN', numown, 1)
      call gt_chrcom ('OWNER_O', owner_o, numown)
c
c     Add any missing owners from owner_o() to pti_onum()/pti_zonam()
c
      lastowner = pti_onum(sort(num_onam))
      do jt = 1, numown
        iptio = fnd_ptiq(owner_o(jt))
        if (iptio .le. 0) then
          write ( errbuf(1), 10020) lastowner+1, owner_o(jt)
10020     format (' Adding missing owner ', i4, ' (', a3,
     &      ') to owner data segment')
          call prterx ('W', 1)
          lastowner = lastowner + 1
          num = add_ptio (lastowner, owner_o(jt))
          owner_code_ge(num) = owner_o(jt)
        endif
      enddo
c
c     Sort pti_onum()
c
      do i = 1, num_onam
        if (owner_name(i)(1:1) .eq. char(0)) owner_name(i) = pti_onam(i)
        if (owner_code_ge(i)(1:1) .eq. char(0)) 
     &    owner_code_ge(i) = pti_onam(i)
      enddo

      do i = 1, num_onam
        sort(i) = i
      enddo

      key = 7
      if (num_onam .gt. 0) then
        call qiksrt (1, num_onam, kmp_ptib, swp_ptib)
      endif

      last_owner_num = 0
      last_owner_name = char(0)
      numrec = 0
c
c     "Rewind" temp file and write to netdata file
c
      status = close_ge_file (1)
      status = open_ge_file (1, 'scratch1.dat', 'w')

      do i = 1, num_onam
        is = sort(i)
        if (owner_name(is)(1:1) .eq. char(0)) 
     &    owner_name(is) = pti_onam(is)
        if (owner_name(is)(1:1) .eq. char(0)) 
     &    owner_name(is) = ' '
        if (owner_code_ge(is)(1:1) .eq. char(0)) 
     &    owner_code_ge(is) = pti_onam(is)
        if (owner_code_ge(is)(1:1) .eq. char(0)) 
     &    owner_code_ge(is) = ' '
        if (pti_onum(is) .ne. last_owner_num .or.
     &      owner_code_ge(is) .ne. last_owner_name) then
          write (xbuf, 10040) pti_onum(is), owner_name(is),
     &      owner_code_ge(is), 0.0, 0.0, 0.0, 0.0, 0
10040     format (3x, i5, 1x, '"', a, '"', 1x, '"', a, '"', 1x, ':',
     &      4(1x, f10.2), 1x, i2)
          last = lastch (xbuf)
          status = write_ge_file (1, xbuf(1:last))
          numrec = numrec + 1
        endif
        last_owner_num = pti_onum(is)
        last_owner_name = owner_code_ge(is)
      enddo

      write (*, 10000) numrec
10000 format (' * Writing ', i5, ' owner records to NETDAT file')

      write (xbuf, 10010) numrec
10010 format ('owner data  [', i5, ']                            pnet    
     & qnet')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     "Rewind" temp file and write to netdata file
c
      status = close_ge_file (1)
      status = open_ge_file (1, 'scratch1.dat', 'r')

      finished = .false.
      do while (.not. finished)
        last = read_ge_file (1, xbuf)
        if (last .eq. 0 .or. xbuf(1:5) .eq. '[EOF]') go to 110
        status = write_ge_file (0, xbuf(1:last))
      enddo

  110 continue

      write ( errbuf(1), 10110) numrec
10110 format (' Total owner records extracted:', i5)
      call prterx ('I', 1)

      ext_geown = num_onam

      return
      end
