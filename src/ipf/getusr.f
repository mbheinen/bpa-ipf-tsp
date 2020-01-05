C    @(#)getusr.f	20.10 5/27/98
      subroutine getusr
C
C     This subroutine processes /USER_ANALYSIS commands.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/com003.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/zonlst.inc'
 
      character word(100) * 60, capital * 60, temfil * 60
      logical opened
      integer teminp, error, first, status, open_file, findstr
 
      incsw = 0
      error = 0
 
      if (findstr(inrcd,'USER_AN') .ne. 0 .or.
     1    findstr(inrcd,'USERAN') .ne. 0) then
C
C        /USER_ANALYSIS, APPEND_COMMENTS, FILE = <file_name>,
C                           DEBUG = ON, OUTPUT = <file_name>
C
         lunusr = 25
         inquire (unit=lunusr, opened=opened)
         if (.not. opened) then
C
C           Note: VAX/VMS limits sequential record size to 8191 words.
C           This restriction require the arrays to be written in
C           200-record segments.
C
            status = open_file(lunusr,'USER_ANA.SCR', 'U', 'W', ios)
            if (status .ne. 0) then
               write (errbuf(1), 104) ios, lunusr
  104          format (' Error No. ', i3, 
     1            ' opening scratch file on logical unit',i3)
               call prterx ('W',1)
c
c              Skip all /DEFINE commands
c
               do while (index ('(/', inrcd(1:1)) .eq. 0)
                  read (inp, 260, end=450) inrcd
                  write (outbuf, 108) inrcd(1:80)
  108             format (' Command record rejected : (', a, ')')
                  call prtout (1)
               enddo
               card = inrcd(1:1)
               go to 460
            endif
         endif
 
  110    if (numusr .ge. 20) then
            write (errbuf(1), 112) numusr
  112       format (' More than ', i3, ' /USER_ANALYSIS commands. ',
     1              'The following records will be rejected.')
            call prterx ('W',1)
  114       read (inp, 260, end=450) inrcd
            if (index ('(/', inrcd(1:1)) .eq. 0) then
               write (outbuf, 108) inrcd(1:80)
               call prtout (1)
               go to 114
            endif
            card = inrcd(1:1)
            go to 460
         endif
 
         numusr = numusr + 1
         numdef(numusr) = 0
         numtxt(numusr) = 0
         usrdbg(numusr) = 0
         usrfil(numusr) = ' '
         cmnflg(numusr) = .false.
C
C        Set analysis print switch on unconditionally.
C
         aplist(21) = 1
C
C        Set microfiche switch if at least one other has been set.
C
         j = 0
         do i = 1, 23
            j = max0 (aflist(i), j)
         enddo
         aflist(21) = j
 
         call scan(inrcd, word, nwrd)
c
c        Capitalize all word() except <filename>
c
         i = 1
         do while ( i .le. nwrd )
            word(i) = capital(word(i))
            if ( word(i)(1:4) .eq. 'FILE' ) i = i + 1
            if ( word(i)(1:6) .eq. 'OUTPUT' ) i = i + 1
            i = i + 1
         end do
 
         iwrd = 2
         do while (iwrd .le. nwrd)
c*          if (ickdic(word(iwrd), 'FILE*', 1) .eq. 1) then
            if (word(iwrd)(1:4) .eq. 'FILE') then
                temfil=inpnm
                teminp=inp
                inpnm = word(iwrd+1)
                inp = 30
                error = 0
                call opnfil (inp, inpnm, error)
                if (error .ne. 0) then
                   write (errbuf(1),1410) inpnm
 1410              format ('0 File ', a, 
     1                     ' cannot be opened. File ignored.')
                   call prterx ('W',1)
                   inpnm=temfil
                   inp=teminp
                else
                   incsw = 1
                endif
                iwrd = iwrd + 2 
            else if (word(iwrd)(1:5) .eq. 'DEBUG') then
               if (word(iwrd+1) .eq. 'ON') then
                  usrdbg(numusr) = 1
               else
                  usrdbg(numusr) = 0
               endif
               iwrd = iwrd + 2
c*          else if (ickdic(word(iwrd), 'OUTPUT*', 1) .eq. 1) then
            else if (word(iwrd)(1:6) .eq. 'OUTPUT') then
                usrfil(numusr) = word(iwrd+1)
                numupx(numusr) = 0
                iunit = 26
                close(unit=iunit,err=151)
  151           open (unit=iunit,
     1               file = usrfil(numusr),
     2               status = 'NEW',
     3               form = 'FORMATTED',
c **************************************************************
c *** this may cause a future fix to need to be designed, but the
c *** following line was commented out for F77 compatibility
c     4               RECORDTYPE = 'VARIABLE',
c **************************************************************
     5               iostat = ios,
     6               err = 152)
  152           close(unit=iunit,err=153)
  153           continue
                iwrd = iwrd + 2 
c*          else if (ickdic(word(iwrd), 'APPEND*', 1) .eq. 1) then
            else if (word(iwrd)(1:6) .eq. 'APPEND') then
                cmnflg(numusr) = .true.
                iwrd = iwrd + 1
            else
                iwrd = iwrd + 1
            endif
 
         enddo
C
C        Three categories of data follow / USER_ANALYSIS
C
C        1. Symbol definitions:
C
C           > DEFINE_TYPE OWNER_LOSS, o1 = <owner1>, o2 = <owner2>
C           > DEFINE_TYPE AREA_LOSS,  a1 = <area1>, a2 = <area2>
C           > DEFINE_TYPE ZONE_LOSS,  z1 = <zone1>, z2 = <zone2>
C           > DEFINE_TYPE SYSTEM_LOSS, s
C           > DEFINE_TYPE BRANCH_P
C             LET p1 = <Bus1,base1,bus2,base2>, -
C             LET p2 = <Bus3,base3,bus3,base4>, -
C             ...
C             LET pn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE BRANCH_Q
C             LET q1 = <Bus1,base1,bus2,base2>, -
C             LET q2 = <Bus3,base3,bus3,base4>, -
C             ...
C             LET qn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE BRANCH_P_METER
C             LET p1 = <Bus1,base1,bus2,base2>, -
C             LET p2 = <Bus3,base3,bus3,base4>, -
C             ...
C             LET pn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE BRANCH_Q_METER
C             LET q1 = <Bus1,base1,bus2,base2>, -
C             LET q2 = <Bus3,base3,bus3,base4>,
C             ...
C             LET qn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE INTERTIE_P
C             LET i1 = <Area_1,Area_2>, -
C             LET i2 = <area_3,area_4>, -
C             ...
C             LET in = <area_m,area_n>
C           > DEFINE_TYPE INTERTIE_Q
C             LET j1 = <Area_1,Area_2>, -
C             LET j2 = <area_3,area_4>, -
C             ...
C             LET jn = <area_m,area_n>
C           > DEFINE_TYPE INTERTIE_P_SCHEDULED
C             LET i1 = <Area_1,Area_2>, -
C             LET i2 = <area_3,area_4>, -
C             ..
C             LET in = <area_m,area_n>
C           > DEFINE_TYPE FUNCTION
C             LET i = h + g + k
C           > DEFINE_TYPE BUS_INDEX
C             LET b1 = <Bus1,base1>, -
C             LET b2 = <Bus2,base2>, -
C             ...
C             LET bn = <Busn,base2n, -
C           > DEFINE_TYPE LINE_INDEX or DEFINE_TYPE BRANCH_INDEX
C             LET l1 = <Bus1,base1,bus2,base2,id>, -
C             LET l2 = <Bus3,base3,bus4,base4,id>, -
C             ...
C             LET lp = <Busp,basep,busq,baseq,id>, -
C           > DEFINE_TYPE INTERTIE_INDEX
C             LET i1 = <area1,area2>, -
C             LET i2 = <area3,area4>, -
C             ...
C             LET ip = <areap,areaq>
C
C           > DEFINE_TYPE AREA_INDEX
C             LET a1 = <area1>, -
C             LET a2 = <area2>, -
C             ...
C             LET ip = <areap>
C
C           > DEFINE_TYPE TRANSFER_INDEX
C             LET i1 = <bus1, base1> <bus2, base2>
C             LET i2 = <bus3, base3> <bus4, base4>
C
C        2. Page Header/Subheader information:
C
C           H xxx (header text)
C           S xxx (subheader text)
C
C        3. Page text
C
C           C xxx (page text)
C
         numrcd = 0
  232    read (inp, 260, end=450) inrcd
  260    format (a)
         card = inrcd(1:1)
         numrcd = numrcd + 1
C
C        Skip "." comment text.
C
         if (card .eq. '.') go to 232
 
         if (index ('> ', card) .ne. 0) then
            if (numdef(numusr) .lt. MAXUSRANL) then
               numdef(numusr) = numdef(numusr) + 1
               usrdef(numdef(numusr)) = inrcd
            else
               write (errbuf(1),10237) MAXUSRANL
10237          format ('More than ',i3, 
     1                 '/ USER_ANALYSIS > definition records.')
               call prterx ('E', 1)
               error = 1
            endif
            go to 232
         else if (findstr ('HSC', card) .ne. 0) then
            if (numtxt(numusr) .lt. MAXUSRANL) then
               numtxt(numusr) = numtxt(numusr) + 1
               usrtxt(numtxt(numusr)) = inrcd
            else
               write (errbuf(1),10238) MAXUSRANL
10238          format ('More than ',i3, 
     1                 '/ USER_ANALYSIS text records .')
               call prterx ('E', 1)
               error = 1
            endif
            go to 232
         else
            go to 460
         endif
C
C        End-of-file. This is normal if alternate file is used.
C
  450    if (incsw .eq. 0) inrcd = '( END ) GETUSR'
C
C        Restore normal input file if alternate file invoked.
C
  460    if (incsw .eq. 1) then
            write (outbuf, 470) numrcd, inpnm
  470       format (1x, i4, 
     1              ' records read from /USER_ANALYSIS file ',a)
            call prtout (1)
            close( unit = inp )
            inpnm=temfil
            inp=teminp
            incsw = 0
            read (inp, 260, end=450) inrcd
            card = inrcd(1:1)
         endif
         if (error .eq. 0 .and. numusr .gt. 0) then
            max1 = numdef(numusr)
            do 472 first  = 1, max1, 200
               last = min0 (first+199,max1)
               write (lunusr) (usrdef(i),i=first,last)
  472       continue
            max2 = numtxt(numusr)
            do 474 first = 1, max2, 200
               last = min0 (first+199,max2)
               write (lunusr) (usrtxt(i),i=first,last)
  474       continue
         else
            numusr = 0
         endif
      else
         read (inp, '(a)', end=476) inrcd
         card = inrcd(1:1)
         go to 478

  476    inrcd = '( END ) getusr'
         card = inrcd(1:1)

  478    continue

      endif
 
  900 return
      end
