C    @(#)save_ged.f	20.18 8/30/00

C****************************************************************
C
C     File: save_ged.f
C
C     Purpose: Routine to save a network data file in GE format
C
C     / LOAD_GE, FILE = <filename>, -
C                TRNFILE = <filename>, -
C                VERSION = <number>
C                NEWTRNFILE = <filename>
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ctlpow.f
C
C****************************************************************
      integer function save_ged (scrfil, filename, version, option,
     &                           brdatafile, datebr, extfil, 
     &                           extfilename, error)
      integer scrfil, error, version, extfil
      character *(*) filename, option(10), brdatafile, datebr,
     &               extfilename

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
      include 'ipfinc/coment.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/basval.inc'
 
      common /bpa_num / user_rule, num_area_rule, num_zone_rule, 
     &                  num_owner_rule, num_default_rule, 
     &                  area_rule(3,MAXCAR), zone_rule(3,MAXZON), 
     &                  owner_rule(3,MAXOWN), default_rule(2,100), 
     &                  owner_code_ge(MAXOWN)
      integer user_rule, num_area_rule, num_zone_rule, num_owner_rule,
     &        num_default_rule, area_rule, zone_rule, owner_rule, 
     &        default_rule
      character owner_code_ge*4

      common /verify_pti/ num_verify, hash_verify(PTI_HASHSIZE), 
     &                    next_verify(PTI_MAXBUS), 
     &                    ptinum_verify(PTI_MAXBUS),
     &                    bpanum_verify(PTI_MAXBUS)
      integer num_verify, hash_verify, next_verify, ptinum_verify,
     &                    bpanum_verify
      integer ext_geb, ext_gel, ext_geg, ext_geld, ext_geshn, 
     &        ext_gesvd, ext_gear, ext_gezon, ext_gedcb, ext_gedcl,
     &        ext_gedcc, ext_gezdt, ext_geown, total(4),
     &        write_ge_file, status, gtge_num, fnd_ptiy, add_ptiz,
     &        fnd_ptio, fnd_ptiq, add_ptio, ftn_atoi, winter_type,
     &        ext_getrx
      character month(12)*3, xbuf*512, weekday(7)*3, winter_code(3)*18,
     &          userid*10, season_code(4)*10
      logical found

      data month / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
     &             'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
      data weekday / 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' /

      data winter_code / 'Winter', 'Moderate Winter',
     &                   'Extra Heavy Winter' /

      data season_code / 'Summer', 'Winter', 'Spring', 'Fall' /

      save_ged = 0        ! set default return status = successful
      error = 0           ! initialize error count
      winter_type = index ('NME', option(7))
c
c     Initialize hash arrays to capture duplicate numbers of buses
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

      if (option(4) .eq. 'Y' .or. option(5) .eq. 'Y') then
c
c       Option(*)  Supersede 
c          (4)     GE bus-zones pairs with BPA bus-zones pairs
c          (5)     GE bus-owner pairs with BPA bus-owner pairs
c
        do ib = 1, ntot_alf  
          nb = alf2inp(ib)  
c  
c         Get GE area, zone, owner, and bus number  
c  
          status = gtge_num (nb, iptia, iptiz, iptib)  
          if (option(4) .eq. 'Y') then
            if (iptiz .eq. 0) then
              found = .false.
            else if (pti_znam(iptiz) .ne. zone(nb)) then
              found = .false.
            else
              found = .true.
            endif
            if (.not. found) then
              iptiz_new = fnd_ptiy (zone(nb))  
              if (iptiz_new .eq. 0) then
                last = 0
                do i = 1, num_znam
                  last = max0 (pti_znum(i), last)
                enddo
                iptiz_new = add_ptiz (zone(nb), last+1)
              endif
              if (iptiz_new .gt. 0) then
                pti_zone(iptib) = pti_znum(iptiz_new)
              endif
            endif
          endif
          if (option(5) .eq. 'Y') then
            iptio = fnd_ptio (pti_owner(iptib))  
            if (iptio .eq. 0) then
              found = .false.
            else if (pti_onam(iptio) .ne. owner(nb)) then
              found = .false.
            else
              found = .true.
            endif
            if (.not. found) then
              iptio_new = fnd_ptiq (owner(nb))  
              if (iptio_new .eq. 0) then
                last = 0
                do i = 1, num_onam
                  last = max0 (pti_onum(i), last)
                enddo
                iptio_new = add_ptio (last+1, owner(nb))
              endif
              if (iptio_new .gt. 0) then
                pti_owner(iptib) = pti_onum(iptio_new)
              endif
            endif
          endif
        enddo
      endif
c
c     Write "#" records
c     
      if (index (cspare(30), char(0)) .eq. 1 .or.
     &    cspare(30) .eq. ' ') cspare(30) = 'data'
      call nx_date (imon, idate, iyear, iwk_day)
      if (iyear .lt. 40) then
        iyear = iyear + 2000
      else
        iyear = iyear + 1900
      endif
      call n_time (ihour, imin, isec)

      write (xbuf, 10000) weekday(iwk_day), month(imon), idate, ihour, 
     &                    imin, isec, iyear
10000 format ('# history file date ', a, 1x, a, 1x, i2, 1x, i2.2, ':', 
     &   i2.2, ':', i2.2, 1x, i4)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write (xbuf, 10010) weekday(iwk_day), month(imon), idate, ihour, 
     &                    imin, isec, iyear
10010 format ('# present file date ', a, 1x, a, 1x, i2, 1x, i2.2, ':', 
     &   i2.2, ':', i2.2, 1x, i4)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write (xbuf, 10020) '11.1'
10020 format ('# Version ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Write title records
c     
      xbuf = 'title'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do i = 1, 2
        if (coment(i) .ne. ' ' .and. coment(i)(1:1) .ne. char(0)) then
          last = lastch (coment(i))
          status = write_ge_file (0, coment(i)(1:last))
        endif
      enddo

      xbuf = '!'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Write comments records
c     
      xbuf = 'comments'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do i = 1, ncom
        if (com(i) .ne. ' ') then
          last = lastch (com(i))
          status = write_ge_file (0, com(i)(1:last))
        endif
      enddo

      last1 = lastch(basval(1))
      last2 = lastch(basval(2))
      last3 = lastch(basval(3))

      write (xbuf, 10030) basval(2)(1:last2) // ':', basval(3)(1:last3),
     &                    basval(1)(1:last1)
10030 format ('  Source:          ', a, a, a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      last1 = lastch(basval(4))
      last2 = lastch(basval(5))
      last3 = lastch(basval(6))
      last4 = lastch(basval(7))

      write (xbuf, 10040) basval(4)(1:last1)
10040 format ('  Case:            ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write (xbuf, 10042) basval(7)(1:last4)
10042 format ('  Description:     ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write (xbuf, 10044) basval(5)(1:last2),
     &                    basval(6)(1:last3)
10044 format ('  Date:            ', a, 1x, a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      last1 = lastch(basval(9))
      last2 = lastch(basval(8))

      write (xbuf, 10050) basval(9)(1:last1)
10050 format ('  User:            ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write (xbuf, 10052) basval(8)(1:last2)
10052 format ('  PF Version:      ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      if (brdatafile .ne. ' ' .and. brdatafile(1:1) .ne. char(0)) then
        last = lastch (brdatafile)
        write (xbuf, 10054) brdatafile(1:last)
10054   format ('  Branch data:     ', a)
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        write (xbuf, 10056) datebr
10056   format ('  Extraction date: ', a, ' (MYYYY)')
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        write (xbuf, 10058) winter_code(winter_type)
10058   format ('  Winter ratings:  ', a)
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))
      endif

      iseason = ftn_atoi (option(6))
      write (xbuf, 10060) season_code(iseason)
10060 format ('  Default Ratings: ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = '!'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Write solution parameters
c     
      xbuf = 'solution parameters'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'tap    1    tcul  enabled/disabled'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'phas   1    ps    enabled/disabled'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'area   1    area  enabled/disabled'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'svd    0    svd   enabled/disabled'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'dctap  1    dc    enabled/disabled'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'ped    0    ped   enabled/disabled'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'jump  0.000250    jumper threshold'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'toler   0.2000    newton tolerance'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = 'sbase    100.0    system mva base'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      xbuf = '!'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      do i = 1, 4
        total(i) = 0
      enddo

c     Extract bus data in GE format
      numb = ext_geb ( scrfil, version, option, total)

c     Extract branch data in GE format
      numl = ext_gel ( scrfil, version, option, total, extfil,
     &                 extfilename)

c     Extract generator data in GE format
      numa = ext_geg ( scrfil, version, option, total)

c     Extract load data in GE format
      numa = ext_geld ( scrfil, version, option, total)

c     Extract shunt data in GE format
      numa = ext_geshn ( scrfil, version, option, total)

c     Extract svd data in GE format
      numa = ext_gesvd ( scrfil, version, option, total)

c     Extract area interchange data in GE format
      numa = ext_gear ( scrfil, version, option, total)

c     Extract zone interchange data in GE format
      numa = ext_gezon ( scrfil, version, option, total)
c
c     Skip interface data
c
      write (*, 10090) 0
10090 format (' * Skipping ', i4, ' interface records to NETDAT file')
      write (xbuf, 10100) 0
10100 format ('interface data  [', i4, ']                      pnet     
     &qnet   -rate1- -rate2- -rate3- -rate4-')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Skip interface branch data
c
      write (*, 10110) 0
10110 format (' * Skipping ', i4, ' interface branch records to NETDAT f
     &ile')
      write (xbuf, 10120) 0
10120 format ('interface branch data  [', i4, ']                       c
     &k      -i_no-  part_fac')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

c     Extract d-c bus data in GE format
      num2 = ext_gedcb ( scrfil, version, option, total)

c     Extract d-c line data in GE format
      num2 = ext_gedcl ( scrfil, version, option, total)

c     Extract d-c converter data in GE format
      num2 = ext_gedcc ( scrfil, version, option, total)

c     Extract transformer impedance data in GE format
      num2 = ext_gezdt ( scrfil, version, option, total)
c
c     Skip power electronic device data
c
      write (*, 10130) 0
10130 format (' * Skipping ', i4, ' ped records to NETDAT file')
      write (xbuf, 10140) 0
10140 format ('ped data  [', i4, ']')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

c     Extract area transaction data in GE format
      num2 = ext_getrx ( scrfil, version, option, total)

c     Extract owner data in GE format
      num2 = ext_geown ( scrfil, version, option, total)
c
c     Skip motor data
c
      write (*, 10170) 0
10170 format (' * Skipping ', i4, ' motor data records to NETDAT file')
      write (xbuf, 10180) 0
10180 format ('motor data  [', i5, ']')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Skip line data
c
      write (*, 10190) 0
10190 format (' * Skipping ', i4, ' line data records to NETDAT file')
      write (xbuf, 10200) 0
10200 format ('line  data  [', i5, ']                                 ck
     &  se  long_id_    st resist   react   charge   rate1  rate2  rate3
     &  rate4 aloss  lngth')
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))
c
c     Write "end"
c
      xbuf = 'end'
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      call get_user (userid)
      write (xbuf, 10210) userid
10210 format ('#  This data file written by: ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

      write (xbuf, 10220) prgvsn
10220 format ('#  Using PF Version:          ', a)
      last = lastch (xbuf)
      status = write_ge_file (0, xbuf(1:last))

  900 continue
      return
      end
