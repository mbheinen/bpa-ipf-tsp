C    @(#)net_data_sub.f	20.38 8/30/00
C****************************************************************
C
C   File: net_data_sub.f
C   Purpose: Standalone program to extract network data from a
C            base case file.
C
C   Author: EOHB staff        Date: circa 1980
C
C   Called by: 
C
C****************************************************************
C
	subroutine net_data_sub

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/miscfile.inc'

        integer max_out_of_service
        parameter (MAX_OUT_OF_SERVICE = 200)
        common /out_of_service/ numoossh, shunt_status(MAXCBS),
     &                          shunt_value(16,MAX_OUT_OF_SERVICE), 
     &                          branch_status(MAXBRN)
        integer numoossh, shunt_status, branch_status
        real shunt_value

        common /branch_ratings/ ratings(8,MAXBRN),
     &                          ext_ratings(15,MAXBRN),
     &                          ext_date(2,MAXBRN),
     &                          numrat
        real ratings, ext_ratings
        character ext_date*6
        integer numrat

        character capital*10, size*10, rating*10, dialect*10, 
     &            basefile*60, case*10, netfile*60, type*3,
     &            option(10)*1, filename*60, transfile*60, query*1,
     &            sections*12, type_e*12, ipsref_file*60,
     &            zone_rename_file*60, newtransfile*60,
     &            brdatafile*60, datebr*5, extbrdatafile*60,
     &            br_oos_file*60, sh_oos_file*60
        integer status, savenetd, scrfil, open_file, saveptid, ipsfil,
     &          zonerenfil, save_ged, open_ge_file, bldptitrn, 
     &          ldptitrn, saveptit, close_ge_file, ftn_atoi, ld_getrn,
     &          tran_file_type, save_get, ldbrndfil, winter_type,
     &          season, ldoosfile, ldshoosfil
        logical finished, baseok, bpa_netdat

        status = 0     ! default return SUCCESS state
c
c       Process /SAVE_FILES, TYPE = NETWORK_DATA, 
c                            FILE = <filename>,
c                            DIALECT = BPA,
c                                      WSCC,
c                                      WSCC1,
c                                      WSCC2,
c                            
c                            SIZE = 120,
C                                   80,
C                                   B80/L120
c                            RATING = EXTENDED,
c                            MINIMUM,
C                            NOMINAL,
c                            SECTIONS = <null>
c                                       PSEUDOBUSES,
c                            TYPE_E = NORMAL,
c                                     SYMMETRIC,
c                            WSCC_FILE = <filename>,
c                            ZONE_RENAME_FILE = <filename>
c
c       or      /SAVE_FILES, TYPE = PTI_DATA, -
c                            FILE = <filename>, -
c                            VERSION = <number>, -
c                            TRANFILE = <filename>, -
C                            NEWTRANS = <filename>
C
c       or      /SAVE_FILES, TYPE = GE_DATA, -
c                            FILE = <filename>, -
c                            VERSION = <number>, -
c                            TRANFILE = <filename>, -
C                            NEWTRANS = <filename>
c                            BRANCH_DATA = <filename>, -
c
c                                                                       
c       Initialization                                          
c                                                                       
   90   call pfinit
        call initlz(status)
        call set_batch
                                                                        
        basefile = ' '
        dialect = 'BPA'
        size = '120'
        rating = 'EXTENDED'
        sections = ' '
        type_e = ' '
        ipsref_file = ' '
        ipsfil = busfil
        zone_rename_file = ' '
        zonerenfil = chgdta
        baseok = .false.
        brdatafile = ' '
        netfile = ' '
        br_oos_file = ' '
        sh_oos_file = ' '

        do i = 1, 10
          option(i) = ' '
        enddo

        finished = .false.
        do while (.not. baseok .and. .not. finished)
           write (*, 10000) 
10000      format(1x, '> Enter OLD_BASE file name (or Q to quit): ',
     &$)                      
           read (*, fmt='(a)') basefile
           if (basefile .eq. 'q' .or. basefile .eq. 'Q') then
              finished = .true.              
           else

c             open oldbase and load case                                        
                                                                        
              case = ' '                                                      
              filename = basefile  ! Use temporary file name since
C                                  ! gtbase returns file in upper case
              call gtbase (filename, case, baseok)  
                                                                        
              if (.not. baseok) then                                           
                 write (*, 10020) 
10020            format (1x, '* Error opening/loading base case file')
              endif
           endif
        enddo
c
c       Initialize branch status
c
        numrat = 0
        do i = 1, ltot
          branch_status(i) = 1
        enddo
c
c       Initialize shunt status
c
        numoossh = 0
        do i = 1, ntot2
          shunt_status(i) = 1
        enddo

        type = 'net'
        filename = basefile
        last = lastch (filename)
        do while (last .gt. 1 .and. (filename(last:last) .ne. '.'))
           last = last - 1
        enddo
        if (last .lt. 1) last = 1
        if (filename(last:last) .eq. '.') then
           filename(last+1:last+3) = type
        else
           last = lastch (filename)
           filename(last+1:) = '.' // type
        endif

        finished = .false.
        do while (.not. finished)

          last = lastch(filename) 
          write (*, 10030) filename(1:last)
10030     format (1x, '> Enter saved network file (default is "', 
     &        a, '"): ', $)
          read (*, fmt='(a)') netfile
          if (netfile .eq. ' ') then 
            netfile = filename 
          endif
          scrfil = busbrn
          status = open_file (scrfil, netfile, 'F', 'W', iostat)
          if (status .ne. 0) then
            write (*, 10040) 
10040       format (1x, '* Error opening network data file')
          else
            finished = .true.
          endif
        enddo

        bpa_netdat = .true.
        finished = .false.
        do while (.not. finished)
           write (*, 10050) 
10050      format(1x,    '> Enter <return> to extract data as a NETWORK_
     &DATA file,', /, 
     &               1x, '  enter G to extract data as a GE_DATA file, '
     &, /,
     &               1x, '  enter P to extract data as a PTI_DATA file, 
     &or ', /,
     &               1x, '  enter Q to quit): ', $)
           read (*, fmt='(a)') query
           if (query .eq. 'q' .or. query .eq. 'Q') then
              go to 900
           else if (query .eq. 'g' .or. query .eq. 'G') then
              type = 'ge'
              bpa_netdat = .false.
              finished = .true.
           else if (query .eq. 'p' .or. query .eq. 'P') then
              type = 'pti'
              bpa_netdat = .false.
              finished = .true.
           else if (query .eq. char(0) .or. query .eq. ' ') then
              finished = .true.
           endif
        enddo

  142   continue

        if (bpa_netdat) then

c       -----------------------------------------------------------
c       WSCC/BPA conversion
c       -----------------------------------------------------------
c
          option(10) = 'B'

          finished = .false.
          do while (.not. finished)

            write (*, 10060) 
10060       format (1x, '> Enter dialect ( BPA, WSCC,', /,
     &    '                   WSCC1 :  +A INT, taps --> BPA taps,' ,/,
     &    '                   WSCC2 :  +A INT, taps --> WSCC steps,', /,
     &    '                or PTI ): ', $)
            read (*, fmt='(a)') dialect
            if (dialect .eq. ' ') then
              dialect = 'BPA'
              finished = .true.
            else
              dialect = capital(dialect)
              last = lastch(dialect)
              if ( index ('BPA#WSCC#WSCC1#WSCC2#PTI',dialect(1:last))
     &             .eq. 0) then
                write (*, 10070) 
10070           format (1x, '* Invalid response - press <RETURN> to cont
     &inue')
                read (*, fmt='(a)') option(1)
              else
                finished = .true.
              endif
            endif
          enddo

          if (dialect .eq. 'BPA') then
            finished = .false.
          else
            size = '80'
            finished = .true.
          endif

          finished = .false.
          do while (.not. finished)

            write (*, 10080) 
10080       format (1x, '> Enter record size (120, 80, or B80/L120): ', 
     &        $)
            read (*, fmt='(a)') size
            if (size .eq. ' ') then
              size = '120'
              finished = .true.
            else if (size(1:1) .eq. 'B' .or. size(1:1) .eq. 'b') then
              size = 'B80/L120'
              finished = .true.
            else if (size .eq. '80' .or. size .eq. '120') then
              finished = .true.
            else
              write (*, 10070) 
              read (*, fmt='(a)') option(1)
            endif
          enddo

          if (size .eq. '80') then
            finished = .false.
          else
            rating = 'EXTENDED'
            finished = .true.
          endif

          do while (.not. finished)

            write (*, 10090) 
10090       format (' > Nominal rating replacement code ', /
     &              '   T = Thermal E = Emergency B = Bottleneck ', /
     &              ' ', /
     &              '   T:  Transformers = T, Lines = T ', /
     &              '   E:  Transformers = E, Lines = T ', /
     &              '   B:  Transformers = B, Lines = B ', /
     &              '   ET: Transformers = E, Lines = T ', /
     &              '   EB: Transformers = E, Lines = B ', /
     &              '   M:  Transformers = min(TEB), Lines = min(TB)',/)
            write (*, 10100) 
10100       format (1x, '> Enter rating replacement code: ', $)
            read (*, fmt='(a)') rating
            last = lastch(rating)
            if (rating .eq. ' ') then
              rating = 'M'
              finished = .true.
            else if (index ('T#E#B#ET#EB#M', rating(1:last)) .gt. 0) 
     &        then
              finished = .true.
            else
              write (*, 10070) 
              read (*, fmt='(a)') option(1)
            endif
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10110) 
10110       format (1x, '> Section buses saved as Pseudo-buses (N or Y)?
     & :', $)
            read (*, fmt='(a)') query
            if (index ('Yy', query) .ne. 0) then
              sections = 'PSEUDOBUSES'
            else
              sections = ' '
            endif
            finished = .true.
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10120) 
10120       format (1x, '> Replace assymetric type E branches with type 
     &L branches (N or Y)? :', $)
            read (*, fmt='(a)') query
            if (index ('Yy', query) .ne. 0) then
              type_e = 'SYMMETRIC'
            else
              type_e = ' '
            endif
            finished = .true.
          enddo

          if (dialect(1:4) .eq. 'WSCC') then
            finished = .false.
            do while (.not. finished)

              last = lastch(filename) 
              write (*, 10130) 
10130         format (1x, '> Enter optional reference (*.IPS) file name 
     &for base kV''s: ', $)
              read (*, fmt='(a)') ipsref_file
              if (ipsref_file .ne. ' ' .and. 
     &            ipsref_file(1:1) .ne. char(0)) then
                status = open_file (ipsfil, ipsref_file, 'F', 'R', 
     &                              iostat)
                if (status .ne. 0) then
                  write (*, 10140) 
10140             format (1x, '* Error opening WSCC reference data file'
     &)
                else
                  finished = .true.
                endif
              else
                finished = .true.
              endif
            enddo
          endif

          if (dialect(1:4) .eq. 'WSCC' .or. dialect(1:3) .eq. 'PTI') 
     &      then
            finished = .false.
            do while (.not. finished)

              last = lastch(filename) 
              write (*, 10150) 
10150         format (1x, '> Enter optional zone rename change file name
     &: ', $)
              read (*, fmt='(a)') zone_rename_file
              if (zone_rename_file .ne. ' ' .and. 
     &            zone_rename_file(1:1) .ne. char(0)) then
                status = open_file (zonerenfil, zone_rename_file, 
     &                                  'F', 'R', iostat)
                if (status .ne. 0) then
                  write (*, 10160) 
10160             format (1x, '* Error opening zone rename change file')
                else
                  finished = .true.
                endif
              else
                finished = .true.
              endif
            enddo
          endif

          finished = .false.
          do while (.not. finished)

            write (*, 10172) 
10172       format (1x, '> Replace d-c system with equivalent bus inject
     &ions (N or Y): ', $)
            read (*, fmt='(a)') option(3)
            if (option(3) .eq. 'y' .or. option(3) .eq. 'Y') then
              option(3) = 'Y'
            else
              option(3) = 'N'
            endif
            finished = .true.
          enddo

          last1 = lastch (ipsref_file)
          if (last1 .eq. 0) last1 = 1
          last2 = lastch (zone_rename_file)
          if (last2 .eq. 0) last2 = 1
          write (*, 10170) dialect, size, rating,  sections, type_e,
     &      ipsref_file(1:last1), zone_rename_file(1:last2), option(3)
10170     format (' * Options selected - dialect  = ', a, /,
     &            '                      size     = ', a, /,
     &            '                      rating   = ', a, /,
     &            '                      sections = ', a, /,
     &            '                      type_e   = ', a, /,
     &            '                      ips_ref  = ', a, /,
     &            '                      zone_ren = ', a, /, 
     &            '                      net d-c  = ', a)

          write (*, 10180) 
10180     format (1x, '> Are above options correct (Y or N)? ', $)
          read (*, fmt='(a)') option(1)
          if (option(1) .eq. 'n' .or. option(1) .eq. 'N') go to 142

          status = savenetd (scrfil, netfile, dialect, size, rating,
     &                       sections, type_e, ipsfil, ipsref_file,
     &                       zonerenfil, zone_rename_file)
          call close_file (scrfil)

          write (*, 10190) 
10190     format (1x, '> Extract another case (Y or N)? ',$)
          read (*, fmt='(a)') option(1)
          if (option(1) .eq. 'y' .or. option(1) .eq. 'Y') go to 90
c
c       -----------------------------------------------------------
c       PTI conversion
c       -----------------------------------------------------------

        else if (type .eq. 'pti') then
c
c         Extract data in PTI format
c
          option(10) = 'P'
c
c         Close and reopen network data file.
c
          call close_file (scrfil)
          finished = .false.
          do while (.not. finished)
            last = lastch (netfile)
            status = open_ge_file (0, netfile(1:last), 'w')
            if (status .ne. 0) then
              write (*, 10040) 
              write (*, 10030) netfile(1:last)
              read (*, fmt='(a)') netfile
              if (netfile .eq. 'q' .or. netfile .eq. 'Q') then 
                go to 900
              endif
            else
              finished = .true.
            endif
          enddo

          call ptihinit()
          finished = .false.
          do while (.not. finished)
            write (*, 10200) 
10200       format(1x, '> Enter PTI translation file name (or <RETURN> i
     &f none, or Q to quit) > ', $)                      
            read (*, fmt='(a)') transfile
            if (transfile .eq. 'q' .or. transfile .eq. 'Q') then
              go to 900
            else if (transfile(1:1) .eq. char(0) .or. 
     &               transfile .eq. ' ') then
              if (bus_number(alf2inp(1)) .eq. 0) then
                 write (*, 10202) 
10202            format (1x, '* Warning -- a translation file is not spe
     &cified and no saved PTI bus number data is available')
                finished = .true.
                transfile = ' '
              else
                finished = .true.
                transfile = ' '
              endif
            else

c             open translation file
                                                                        
              call close_file (busfil)
              status = open_file (busfil, transfile, 'F', 'R', iost)
              if (status .ne. 0) then
                write (*, 10204) 
10204           format (1x, '* Error opening/loading PTI translation fil
     &e')
              else
                finished = .true.
              endif
            endif
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10206) 
10206       format (1x, '> Enter PTI version number (23 or 24): ', $)
            read (*, fmt='(a)') size
            if (size .eq. ' ' .or. size .eq. '23') then
              size = '23'
              finished = .true.
            else if (size .eq. '24') then
              finished = .true.
            else
              write (*, 10070) 
              read (*, fmt='(a)') option(1)
            endif
          enddo
          last = lastch (size)
          numver = ftn_atoi (size(1:last))

          tran_file_type = 0
          finished = .false.
          do while (.not. finished)
            write (*, 10208) 
10208       format(1x, '> Enter new PTI translation file name (or NULL i
     &f none) > ', $)                      
            read (*, fmt='(a)') newtransfile
            if (ichar(newtransfile(1:1)) .eq. 0) then
              newtransfile = ' '
              finished = .true.
            else

c             open translation file
                                                                        
              call close_file (chgdta)
              status = open_file (chgdta, newtransfile, 'F', 'W', iost)
              if (status .ne. 0) then
                write (*, 10210) 
10210           format (1x, '* Error creating new PTI translation file')
              else
                finished = .true.
              endif
            endif
          enddo

          finished = .false.
          do while (.not. finished)
            write (*, 10212) 
10212       format(1x, '> Enter optional Out-of-service Branch Data file
     & name (or <RETURN>  if none) > ', $)                      
            read (*, fmt='(a)') br_oos_file
            if (ichar(br_oos_file(1:1)) .eq. 0) then
              br_oos_file = ' '
              finished = .true.
            else if (br_oos_file .eq. ' ') then
              finished = .true.
            else

c             open Out-of-service Branch data file
                                                                        
              call close_file (svchfl)
              status = open_file (svchfl, br_oos_file, 'F', 'R', iost)
              if (status .ne. 0) then
                write (*, 10214) 
10214           format (1x, '* Error opening Out-of-Service Branch Data 
     &file')
                br_oos_file = ' '
              else
                finished = .true.
                inquire (unit=svchfl, name=br_oos_file)
              endif
            endif
          enddo

          finished = (br_oos_file .eq. ' ' .or. 
     &                br_oos_file(1:1) .eq. char(0))

          winter_type = 0
          do while (.not. finished)
            write (*, 10220) 
10220       format (' > Types of Winter Ratings          ', /,
     &              '   N:  Normal Winter (1 in 2)       ', /,
     &              '   M:  Moderate Winter (1 in 5)     ', /,
     &              '   E:  Extra Heavy Winter (1 in 20) ', /)
            write (*, 10221) 
10221       format (1x, '> Enter Winter Rating Code: ', $)
            read (*, fmt='(a)') query
            option(7) = capital (query)
            winter_type = index ('NME', option(7))
            if (winter_type .ne. 0) then
              finished = .true.
            else
              write (*, 10222) 
10222         format (1x, '* Illegal winter code')
            endif
          enddo

          season = 0
          finished = (br_oos_file .eq. ' ' .or. 
     &                br_oos_file(1:1) .eq. char(0))

          do while (.not. finished)
            write (*, 10223) 
10223       format (' > Season to deposit Out-of-service branch data rat
     &ings (those not in the branch data file)', /,
     &              '   1 : RATE1, RATE2 (Summer) ', /,
     &              '   2 : RATE3, RATE4 (Winter) ', /,
     &              '   3 : RATE5, RATE6 (Spring) ', /,
     &              '   4 : RATE7, RATE8 (Fall) ')
            write (*, 10224) 
10224       format (1x, '> Enter Season Code: ', $)
            read (*, fmt='(a)') query
            query = capital (query)
            season = index ('1234', query)
            if (season .ne. 0) then
              finished = .true.
              option(6) = query
            else
              write (*, 10225) 
10225         format (1x, '* Illegal season code')
            endif
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10226) 
10226       format (1x, '> Rename blank parallel ID''s to ''1'' (y or n)
     &: ', $)
            read (*, fmt='(a)') option(2)
            if (option(2) .eq. 'n' .or. option(2) .eq. 'N') then
              option(2) = 'N'
            else
              option(2) = 'Y'
            endif
            finished = .true.
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10227) 
10227       format (1x, '> Append bus name, basekv to end of record (Y o
     &r N): ', $)
            read (*, fmt='(a)') option(4)
            if (option(4) .eq. 'n' .or. option(4) .eq. 'N') then
              option(4) = 'N'
            else
              option(4) = 'Y'
            endif
            finished = .true.
          enddo

          if (transfile .eq. ' ') then
            status = bldptitrn (busfil, transfile, numver, error)
          else
            read (busfil, fmt='(a1)', end=144) query
            rewind busfil
            if (query .ne. '#') then
              status = ldptitrn (busfil, transfile, numver, error)
              tran_file_type = 1
            else
              status = ld_getrn (busfil, transfile, numver, error)
              tran_file_type = 2
            endif
            go to 146

  144       last = lastch (transfile)
            write (*, 10228) transfile(1:last)
10228       format (1x, '* Cannot read translation file ', a)
            go to 900

  146       continue

          endif
          if (br_oos_file .ne. ' ') then
            status = ldoosfile (svchfl, br_oos_file, option, error)
          endif
          if (sh_oos_file .ne. ' ') then
            status = ldshoosfil (lunscr2, sh_oos_file, option, error)
          endif
          status = saveptid (scrfil, netfile, numver, option, error)
          if (newtransfile .ne. ' ') then
            if (tran_file_type .eq. 0 .or. tran_file_type .eq. 1) then
               status = saveptit (busfil, transfile, chgdta, 
     &                            newtransfile)
            else
               status = save_get (busfil, transfile, chgdta, 
     &                            newtransfile)
            endif
          endif
          call close_file (busfil)
          call close_file (scrfil)
          call close_file (chgdta)
c
c         Close network data file
c
          status = close_ge_file (0)

        else
c
c       -----------------------------------------------------------
c       GE conversion
c       -----------------------------------------------------------

          option(10) = 'G'
c
c         Close and reopen network data file.
c
          call close_file (scrfil)
          finished = .false.
          do while (.not. finished)
            last = lastch (netfile)
            status = open_ge_file (0, netfile(1:last), 'w')
            if (status .ne. 0) then
              write (*, 10040) 
              write (*, 10030) netfile(1:last)
              read (*, fmt='(a)') netfile
              if (netfile .eq. 'q' .or. netfile .eq. 'Q') then 
                go to 900
              endif
            else
              finished = .true.
            endif
          enddo
c
c         Open three scratch files in GE format
c
          status = open_ge_file (1, 'scratch1.dat', 'w')
          status = open_ge_file (2, 'scratch2.dat', 'w')
          status = open_ge_file (3, 'scratch3.dat', 'w')

          call ptihinit()
          finished = .false.
          do while (.not. finished)
            write (*, 10230) 
10230       format(1x, '> Enter GE translation file name (or <RETURN> if
     & none, or Q to quit) > ', $)                      
            read (*, fmt='(a)') transfile
            if (transfile .eq. 'q' .or. transfile .eq. 'Q') then
              go to 900
            else if (transfile .eq. ' ' .or. 
     &               transfile(1:1) .eq. char(0)) then
              if (bus_number(alf2inp(1)) .eq. 0) then
                write (*, 10232) 
10232           format (1x, '* Warning -- a translation file is not spec
     &ified and no saved GE bus number data is available')
                transfile = ' '
                finished = .true.
              else
                transfile = ' '
                finished = .true.
              endif
            else

c             open translation file
                                                                        
              call close_file (busfil)
              status = open_file (busfil, transfile, 'F', 'R', iost)
              if (status .ne. 0) then
                write (*, 10240) 
10240           format (1x, '* Error opening/loading GE translation file
     &')
              else
                finished = .true.
              endif
            endif
          enddo

          finished = .false.
          do while (.not. finished)
            write (*, 10250) 
10250       format(1x, '> Enter new GE translation file name (or <RETURN
     &>  if none) > ', $)                      
            read (*, fmt='(a)') newtransfile
            if (ichar(newtransfile(1:1)) .eq. 0) then
              newtransfile = ' '
              finished = .true.
            else if (newtransfile .eq. ' ') then
              finished = .true.
            else

c             open translation file
                                                                        
              call close_file (chgdta)
              status = open_file (chgdta, newtransfile, 'F', 'W', iost)
              if (status .ne. 0) then
                write (*, 10260) 
10260           format (1x, '* Error creating new GE translation file')
              else
                finished = .true.
              endif
            endif
          enddo

          finished = .false.
          do while (.not. finished)
            write (*, 10270) 
10270       format(1x, '> Enter optional Branch Data file name (or <RETU
     &RN>  if none) > ', $)                      
            read (*, fmt='(a)') brdatafile
            if (ichar(brdatafile(1:1)) .eq. 0) then
              brdatafile = ' '
              finished = .true.
            else if (brdatafile .eq. ' ') then
              finished = .true.
            else

c             open Branch Data file
                                                                        
              call close_file (brndta)
              status = open_file (brndta, brdatafile, 'F', 'R', iost)
              if (status .ne. 0) then
                write (*, 10280) 
10280           format (1x, '* Error opening Branch Data file')
                brdatafile = ' '
              else
                finished = .true.
                inquire (unit=brndta, name=brdatafile)
              endif
            endif
          enddo

          finished = (brdatafile .eq. ' ' .or. 
     &                brdatafile(1:1) .eq. char(0))

          datebr = ' '
          do while (.not. finished)
            write (*, 10290) 
10290       format(1x, '> Enter date of study (MYYYY) or <RETURN> for de
     &fault date (12000) > ', $)                      
            read (*, fmt='(a)') datebr
            if (datebr .eq. ' ' .or. datebr(1:1) .eq. char(0)) 
     &        datebr = '12000'
            if (index (' 0123456789ONDond', datebr(1:1)) .ne. 0 .and.
     &          index (' 0123456789', datebr(2:2)) .ne. 0 .and.
     &          index (' 0123456789', datebr(3:3)) .ne. 0 .and.
     &          index (' 0123456789', datebr(4:4)) .ne. 0 .and.
     &          index (' 0123456789', datebr(5:5)) .ne. 0) then
              finished = .true.
            else
              write (*, 10300) 
10300         format (1x, '* Illegal date')
            endif
          enddo

          finished = (brdatafile .eq. ' ' .or. 
     &                brdatafile(1:1) .eq. char(0))

          winter_type = 0
          do while (.not. finished)
            write (*, 10310) 
10310       format (' > Types of Winter Ratings          ', /,
     &              '   N:  Normal Winter (1 in 2)       ', /,
     &              '   M:  Moderate Winter (1 in 5)     ', /,
     &              '   E:  Extra Heavy Winter (1 in 20) ', /)
            write (*, 10320) 
10320       format (1x, '> Enter Winter Rating Code: ', $)
            read (*, fmt='(a)') query
            option(7) = capital (query)
            winter_type = index ('NME', option(7))
            if (winter_type .ne. 0) then
              finished = .true.
            else
              write (*, 10330) 
10330         format (1x, '* Illegal winter code')
            endif
          enddo

          finished = (datebr(1:1) .ne. ' ' .and. 
     &                datebr(1:1) .ne. char(0))

          finished = .false.
          do while (.not. finished)
            write (*, 10340) 
10340       format(1x, '> Enter optional Extended Branch Data file name 
     &(or <RETURN>  if none) > ', $)                      
            read (*, fmt='(a)') extbrdatafile
            if (ichar(extbrdatafile(1:1)) .eq. 0) then
              extbrdatafile = ' '
              finished = .true.
            else if (extbrdatafile .eq. ' ') then
              finished = .true.
            else
c
c             Close and reopen new file for writing in GE-format
c                                                                        
              call close_file (wscfil)

              last = lastch (extbrdatafile)
              status = open_ge_file (4, extbrdatafile(1:last), 'w')
              if (status .ne. 0) then
                write (*, 10350) 
10350           format (1x, '* Error opening Extended Branch Data file')
                extbrdatafile = ' '
                read (*, fmt='(a)') extbrdatafile
                if (extbrdatafile .eq. 'q' .or. 
     &              extbrdatafile .eq. 'Q') go to 900
              else
                finished = .true.
              endif
            endif
          enddo

          finished = .false.
          do while (.not. finished)
            write (*, 10360) 
10360       format(1x, '> Enter optional Out-of-service Branch Data file
     & name (or <RETURN>  if none) > ', $)                      
            read (*, fmt='(a)') br_oos_file
            if (ichar(br_oos_file(1:1)) .eq. 0) then
              br_oos_file = ' '
              finished = .true.
            else if (br_oos_file .eq. ' ') then
              finished = .true.
            else

c             open Out-of-service Branch data file
                                                                        
              call close_file (svchfl)
              status = open_file (svchfl, br_oos_file, 'F', 'R', iost)
              if (status .ne. 0) then
                write (*, 10370) 
10370           format (1x, '* Error opening Out-of-Service Branch Data 
     &file')
                br_oos_file = ' '
              else
                finished = .true.
                inquire (unit=svchfl, name=br_oos_file)
              endif
            endif
          enddo

          finished = (br_oos_file .eq. ' ' .or. 
     &                br_oos_file(1:1) .eq. char(0) .or.
     &                winter_type .ne. 0)

          do while (.not. finished)
            write (*, 10400) 
10400       format (' > Types of Winter Ratings          ', /,
     &              '   N:  Normal Winter (1 in 2)       ', /,
     &              '   M:  Moderate Winter (1 in 5)     ', /,
     &              '   E:  Extra Heavy Winter (1 in 20) ', /)
            write (*, 10410) 
10410       format (1x, '> Enter Winter Rating Code: ', $)
            read (*, fmt='(a)') query
            option(7) = capital (query)
            winter_type = index ('NME', option(7))
            if (winter_type .ne. 0) then
              finished = .true.
            else
              write (*, 10420) 
10420         format (1x, '* Illegal winter code')
            endif
          enddo

          finished = .false.
          do while (.not. finished)
            write (*, 10422) 
10422       format(1x, '> Enter optional Out-of-service Shunt Data file 
     &name (or <RETURN>  if none) > ', $)                      
            read (*, fmt='(a)') sh_oos_file
            if (ichar(sh_oos_file(1:1)) .eq. 0) then
              sh_oos_file = ' '
              finished = .true.
            else if (sh_oos_file .eq. ' ') then
              finished = .true.
            else

c             open Out-of-service Shunt data file
                                                                        
              call close_file (lunscr2)
              status = open_file (lunscr2, sh_oos_file, 'F', 'R', iost)
              if (status .ne. 0) then
                write (*, 10424) 
10424           format (1x, '* Error opening Out-of-Service Shunt Data 
     &file')
                sh_oos_file = ' '
              else
                finished = .true.
                inquire (unit=lunscr2, name=sh_oos_file)
              endif
            endif
          enddo

  150     finished = .false.
          do while (.not. finished)

            write (*, 10430) 
10430       format (1x, '> Rename blank parallel ID''s to ''1'' (y or n)
     &: ', $)
            read (*, fmt='(a)') option(2)
            if (option(2) .eq. 'n' .or. option(2) .eq. 'N') then
              option(2) = 'N'
            else
              option(2) = 'Y'
            endif
            finished = .true.
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10440) 
10440       format (1x, '> Replace d-c system with equivalent bus inject
     &ions (N or Y): ', $)
            read (*, fmt='(a)') option(3)
            if (option(3) .eq. 'y' .or. option(3) .eq. 'Y') then
              option(3) = 'Y'
            else
              option(3) = 'N'
            endif
            finished = .true.
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10450) 
10450       format (1x, '> Preserve BPA bus zones (N or Y): ', $)
            read (*, fmt='(a)') option(4)
            if (option(4) .eq. 'y' .or. option(4) .eq. 'Y') then
              option(4) = 'Y'
            else
              option(4) = 'N'
            endif
            finished = .true.
          enddo

          finished = .false.
          do while (.not. finished)

            write (*, 10460) 
10460       format (1x, '> Preserve BPA bus ownerships (N or Y): ', $)
            read (*, fmt='(a)') option(5)
            if (option(5) .eq. 'y' .or. option(5) .eq. 'Y') then
              option(5) = 'Y'
            else
              option(5) = 'N'
            endif
            finished = .true.
          enddo

          season = 0
          finished = .false.
          do while (.not. finished)
            write (*, 10470) 
10470       format (' > Season to deposit IPF branch data ratings (those
     & not in the branch data file)', /,
     &              '   1 : RATE1, RATE2 (Summer) ', /,
     &              '   2 : RATE3, RATE4 (Winter) ', /,
     &              '   3 : RATE5, RATE6 (Spring) ', /,
     &              '   4 : RATE7, RATE8 (Fall) ')
            write (*, 10480) 
10480       format (1x, '> Enter Season Code: ', $)
            read (*, fmt='(a)') query
            query = capital (query)
            season = index ('1234', query)
            if (season .ne. 0) then
              finished = .true.
              option(6) = query
            else
              write (*, 10490) 
10490         format (1x, '* Illegal season code')
            endif
          enddo

          write (*, 10500) option(2), option(3), option(4), option(5),
     &      option(6)
10500     format (
     &   ' * Options selected - Rename blank id''s            = ', a, /,
     &   '                      Net d-c system               = ', a, /,
     &   '                      Preserve BPA bus zones       = ', a, /,
     &   '                      Preserve BPA bus ownerships  = ', a, /,
     &   '                      Season of IPF branch ratings = ', a, /)

          write (*, 10510) 
10510     format (1x, '> Are above options correct (Y or N)? ',$)
          read (*, fmt='(a)') option(1)
          if (option(1) .eq. 'n' .or. option(1) .eq. 'N') go to 150

          numver = 0
          if (transfile .eq. ' ') then
            status = bldptitrn (busfil, transfile, numver, error)
          else
            status = ld_getrn (busfil, transfile, numver, error)
          endif
          if (brdatafile .ne. ' ') then
            status = ldbrndfil (brndta, brdatafile, datebr, option, 
     &                          error)
          endif
          if (br_oos_file .ne. ' ') then
            status = ldoosfile (svchfl, br_oos_file, option, error)
          endif
          if (sh_oos_file .ne. ' ') then
            status = ldshoosfil (lunscr2, sh_oos_file, option, error)
          endif
          status = save_ged (scrfil, netfile, numver, option, 
     &                       brdatafile, datebr, wscfil, 
     &                       extbrdatafile, error)
          status = save_get (busfil, transfile, chgdta, newtransfile)

          call close_file (busfil)
          call close_file (scrfil)
          call close_file (chgdta)
          call close_file (wscfil)
c
c         Close network data file and the three scratch files.
c
          status = close_ge_file (0)
          status = close_ge_file (1)
          status = close_ge_file (2)
          status = close_ge_file (3)

        endif
  900   continue

        return
	end
