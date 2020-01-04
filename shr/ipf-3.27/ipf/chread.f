C    @(#)chread.f	20.8 3/29/99
      subroutine chread (numfil)
c
c     "numfil" returns source of changes:
c              0 = in input stream
c              n = oldchgfl(n)
c
c     Reads all change records
c     
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/chonly.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/delete.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/oldchg.inc'
      include 'ipfinc/pctger.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/zonlst.inc'
 
      common /skpdic/ skpdic(2)
      character*10 skpdic
 
      common /is_batch / is_batch

        character pcttyp*1, zmod(2,15)*2, azone(MAXCAZR)*2, zd*2, 
     &            aexc*8, aname*10, aname1*10, aname2*10, 
     &            word(40)*30, xbuf*120, bus1*8, bus2*8,
     &            base_c*4, getdif*120, oldtext*120, newtext*120,
     &            code*4
C
        integer ctyp, error, status, rename_bus, find_bus, find_ara,
     &          dif_flag
        character newdate * 10, userid * 10
        logical area_mod
c
        nbdel = 0
        numchg = 0
        ndel = 0
        nadd = 0
        nrename = 0
        ipctno = 0
        dif_flag = 0
        area_mod =.false.
C
C       Set up the file to read changes from
C
        if ( chgnam .eq. ' ') then
           inpchg = inp
           numfil = 0

           numchg = numchg + 1
           write (chgcrd(numchg), 90) numchg
   90      format ('/ changes, file = *', t122, i4)
           chgcrd(numchg)(126:126) = 'P'

           numchg = numchg + 1
           write (chgcrd(numchg), 92) numchg
   92      format ('.# ', t122, i4)
           chgcrd(numchg)(126:126) = 'P'

           numchg = numchg + 1
           call am_date (newdate)
           call get_user (userid)
           write (chgcrd(numchg), 100) newdate, userid, numchg
  100      format ('.# Changes added ', a, ' by user ', a, t122, i4)
           chgcrd(numchg)(126:126) = 'P'

           numchg = numchg + 1
           write (chgcrd(numchg), 92) numchg
           chgcrd(numchg)(126:126) = 'P'

        else
           inpchg = chgdta
           ierr=0
           call opnfil (inpchg, chgnam, ierr)
           if (ierr.ne.0) go to 1520
           if (numchgfl .lt. 20) then
              numchgfl = numchgfl + 1
              inquire (inpchg, name=chgfile(numchgfl))
              numfil = numchgfl
C
C             Store command in /oldchg/ 
C
              numchg = numchg + 1
              write (chgcrd(numchg), 90) numchg
              chgcrd(numchg)(126:126) = 'P'

              numchg = numchg + 1
              write (chgcrd(numchg), 92) numchg
              chgcrd(numchg)(126:126) = 'P'

              numchg = numchg + 1
              call am_date (newdate)
              write (chgcrd(numchg), 110) newdate, chgfile(numchgfl),
     1           numchg
  110         format ('.# Changes added ', a, ' from file ', a, t122, 
     &           i4)
              chgcrd(numchg)(126:126) = 'P'
 
              numchg = numchg + 1
              write (chgcrd(numchg), 92) numchg
              chgcrd(numchg)(126:126) = 'P'

           else
              write (errbuf(1), 120) 20
  120         format('More than ', i2, ' change files imported')
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              numfil = 20
           endif
        endif

        goto 150

  130   continue
        read (inpchg,140,end=240) buf
  140   format(a)
C
        if (index('DPZAB+XQLTERI',buf(1:1)).ne.0) then
c
c          Compute differences if change code = '?'
c
           if (buf(3:3) .eq. '?') then
              if (dif_flag .eq. 0) then
                 oldtext = buf
                 dif_flag = 1
                 go to 130
              else
                 newtext = buf
                 buf = getdif (oldtext, newtext)
                 dif_flag = 0
              endif
           endif
           write (outbuf,142) numchg+1, buf(1:92)
  142      format (' Change record No.', i5, ' (',a92,')')
           call prtout (1)
        else if (buf(1:1) .eq. '.') then
           write (outbuf, 142) numchg+1, buf(1:92)
           call prtout (1)
c
c          Store change comment text
c
           numchg = numchg + 1
           write (chgcrd(numchg), 171) buf, numchg
           chgcrd(numchg)(126:126) = 'P'
        else
           write (outbuf,144) buf(1:92)
  144      format (' COMMAND OR COMMENT     (',a92,')')
           call prtout (1)
        endif

  150   continue
C
        ctyp = index('./(', buf(1:1))
        if (ctyp .gt. 0) then
           if (ctyp .eq. 1) then
              goto 130
           else if (ctyp .eq. 2) then
              call scan(buf,word,nwrd)
              if (ickdic(word(1),skpdic,2) .gt. 0) then
                 goto 130
              else
                 goto 1530
              endif
           else
              goto 1530
           endif
        endif
C
        if (buf(1:1).eq. 'C') goto 260
C
        if (numchg .ge. MAXCHG) then
           write (errbuf(1),160) MAXCHG
  160      format('TOO MANY CHANGE CARDS  MAX=',i5)
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
           goto 130
        endif
C
C       Format of CHGCRD()
C
C         (1:120) - change record
C       (121:121) - origination code
C                   ' ' indicates original record
C                   'P' inidcates pseudo-change record
C                   'T' indicates transpose record 
C                   'U' indicates transpose psuedo-change record 
c
C       (122:125) - original change record numnber
C       (126:126) - process code
C                   ' ' indicates not processed
C                   'P' indicates processed without errors
C                   'E' indicates processed with errors
C
  170   continue
        numchg = numchg + 1
        write (chgcrd(numchg), 171) buf, numchg
  171   format (a120, 1x, i4)
C
C       Check for legal change types
C
        if (buf(3:3) .eq. ' ' .or. buf(3:3) .eq. 'R') then
           nadd = nadd + 1
        else if (buf(3:3) .eq. 'D') then
           ndel = ndel + 1
        endif
        if (index(' DRM',buf(3:3)) .eq. 0 .and. 
     &      (buf(1:2) .ne. 'AE' .and. buf(1:2) .ne. 'IE')) go to 200
        if (buf(1:1) .eq. 'B') then
C
C          BUS CARD
C
c ***        do not allow types JKL to be anything but deleted
           if ( index( '0 ESCDVQGOTXMFJKL', buf(2:2) ) .eq. 0  .or.
     &          ( index( 'JKL', buf(2:2) ) .ne. 0  .and.
     &            buf(3:3) .ne. 'D' )                        ) then
              write (errbuf(1),990) numchg
              errbuf(2) = ' '
              write (errbuf(3),392) buf(1:80)
              if (is_batch .eq. 0) then
                 call prterx ('E',3)
              else
                 call prterx ('F',3)
              endif
              chgcrd(numchg)(126:126) = 'E'
              goto 130
           else if (index('DM',buf(2:2)) .ne. 0) then
           else if (index('GX',buf(2:2)) .eq. 0) then
C
C             Blank out remote bus name if not type G or X.
C
              read (buf, 1130, err=220) bus1, base1, bus2, base2
 1130         format (bz, t7, a8, f4.0, t66, a8, f4.0)
              if (bus2 .ne. ' ' .or. base2 .ne. 0.0) then
                 write (errbuf(1),1132) bus2, base2, numchg
 1132            format(' Meaningless remote bus name (',a8,f6.1,
     &               ') on change card No.',i5,' is blanked out.')
                 errbuf(2) = ' '
                 write (errbuf(3),392) buf(1:80)
                 call prterx ('W',3)
                 buf(66:77) = ' '
                 chgcrd(numchg)(66:77) = ' '
              endif
           endif
           go to 130
        else if (index ('+XQRLET', buf(1:1)) .ne. 0) then
           go to 130
        else if (buf(1:1).eq.'A') then
           if (buf(2:2).eq.'C') then
c
c             New format of "AC" records is "A "
c
              buf(2:2) = ' '
              write (chgcrd(numchg),171) buf, numchg
           endif
           if (index (' 0123456789', buf(2:2)) .gt. 0) go to 290
           
        else if (buf(1:1).eq.'I') then
           go to 10550
        else if (buf(1:1).eq.'P') then
           goto 930
        else if (buf(1:1).eq.'Z'.and.buf(2:2).eq.' ') then
           goto 880
        else if (buf(1:1).eq.'Z'.and.buf(2:2).eq.'A') then
           goto 20880
        else if (buf(1:1).eq.'Z'.and.buf(2:2).eq.'B') then
           goto 10880
        else if (buf(1:1).eq.'D'.and.buf(2:2).eq.'Z') then
           goto 540
        else if (buf(1:1).eq.'D'.and.buf(2:2).eq.'A') then
           goto 580
        else
C
           write (errbuf(1),190) buf(1:1), numchg
  190      format('0 Illegal card type ',a,' Change card No.',i5)
           call prterx ('W',1)
           chgcrd(numchg)(126:126) = 'E'
           goto 130

        endif
C
  200   write (errbuf(1),190) buf(1:1),numchg
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        chgcrd(numchg)(126:126) = 'E'
        goto 130
C
  220   write (errbuf(1),230) buf(1:40), numchg
  230   format(' Illegal character in field ',a,' Change card No.',i5)
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        chgcrd(numchg)(126:126) = 'E'
        goto 130
C
  240   continue
C
        if ( inpchg .eq. inp ) then
           write (errbuf(1),241)
  241      format (' Unexpected End-of-file in input control',
     &             ' stream - (STOP) assumed.')
           call prterx ('W',1)
           buf = '( STOP ) END OF FILE READ IN CHANGE READ INP FILE'
           write (outbuf,144) buf(1:80)
           call prtout (1)
           goto 1530
        else
           inpchg = inp
           goto 130
        endif
C
C       COMMENTS
C
  260   if (ncom .lt. MAXCMT) goto 280
            write (errbuf(1),270) MAXCMT
  270       format(' TOO MANY COMMENTS ..MAX=',i5,
     &             '. FOLLOWING RECORD SKIPPED')
            write (errbuf(2),272) buf(1:80)
  272       format(' (',a,')')
            call prterx ('W',2)
        goto 130
C
  280   ncom=ncom+1
        com(ncom)=buf(2:80)
        goto 130
C
C       AREA INTERCHANGE      A
C
  290   read (buf,300,err=220) aname, bus1, base1, aex, aexc, azone
  300   format(bz, 3x, a10, a8, f4.0, 1x, f8.0, t27, a8, 10(1x, a2))
        if (buf(3:3) .eq. ' ' .or. buf(3:3) .eq. 'M' .or. 
     &      buf(3:3) .eq. 'D') go to 320
        if (buf(3:3) .ne. 'E') go to 200
        write (outbuf,310) numchg
  310   format(' All area interchange records are being deleted,',
     &         ' change card No. ', i5 )
        call prtout(1)
        area_mod =.false.
        ntotc=0
        ntotic=0
        jtie=0
        do i = 1, ntot
           jarzn(i) = 0
        enddo
        do i = 1, MAXCAR
           arcnam(i) = ' '
           arcbus(i) = ' '
           do j = 1, MAXCAZ
              arczns(j,i) = ' '
           enddo
        enddo
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        goto 130
C
  320   read (buf(2:2), '(i1)') isubtyp

        if ( isubtyp * MAXCAZR + 1 .gt. MAXCAZ ) then
           write (errbuf(1),325) buf(1:13), numchg
  325      format(' Illegal Area continuation, (', a,
     &            ') Change card No.', i5 )
           call prterx ('E',1)
           chgcrd(numchg)(126:126) = 'E'
           goto 130
        endif

        do 330 line = 1, ntotc
           if (aname .eq. arcnam(line)) go to 340
  330   continue
        go to 360
C
  340   if (buf(3:3) .eq. 'D') go to 400
        if (buf(3:3) .eq. 'M') go to 490
c
c       Process Area Addition
c
        if (isubtyp .eq. 0) then
           write (errbuf(1),350) buf(1:13), numchg
  350      format(' Area already exists, (', a, ') Change card No.',i5)
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
           chgcrd(numchg)(126:126) = 'E'
           goto 130
        else
           j = isubtyp * MAXCAZR + 1
           if (arczns(j,line) .ne. ' ') then
              write (errbuf(1),350) buf(1:13), numchg
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              chgcrd(numchg)(126:126) = 'E'
              go to 130
           endif
           goto 440
        endif
c
c  AREA NOT FOUND
c
  360   if (buf(3:3) .eq. 'D' .or. buf(3:3) .eq. 'M') then
           write (errbuf(1),370) numchg
  370      format(' Modify/Delete Area not found, Change card No.',i5)
           write (errbuf(2),392) buf(1:80)
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
           chgcrd(numchg)(126:126) = 'E'
           goto 130
        endif
c
c  Check for "add" errors
c
        if (isubtyp .eq. 0) then
           if (ntotc .ge. MAXCAR) then
              write (errbuf(1),390) MAXCAR, numchg
  390         format(' Area addition exceeds program limit of ',i2,
     &              ' areas, Change card No.',i5)
              errbuf(2) = ' '
              write (errbuf(3),392) buf(1:80)
  392         format(' (',a,')')
              if (is_batch .eq. 0) then
                 call prterx ('E',3)
              else
                 call prterx ('F',3)
              endif
              chgcrd(numchg)(126:126) = 'E'
              go to 130
           endif
        else
           write (errbuf(1),394) numchg
  394      format(' Area not found for zone addition,',
     &            ' Change card No.', i5 )
           write (errbuf(2),392) buf(1:80)
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
           chgcrd(numchg)(126:126) = 'E'
           goto 130
        endif
        go to 440
C
C       Delete area
C
  400   if (isubtyp .eq. 0) then
C
C          Delete entire area
C
           ntotc=ntotc-1
           call bcdarc(line,xbuf)
           write (outbuf, 410) xbuf(1:80)
  410      format (' Deleted area data: (',a,')')
           call prtout(1)
           j = 1
           do while ( j .lt. MAXCAZ/MAXCAZR   .and. 
     &              ( arczns(j*MAXCAZR+1,line) .ne. ' ' ) )
              call bcdarc2 (line, j, xbuf)
              write (outbuf, 410) xbuf(1:80)
              call prtout(1)
              j = j + 1
           enddo
           call space (1)
C
           do k=line,ntotc
              arcnam(k)=arcnam(k+1)
              arcnet(k)=arcnet(k+1)
              arcbus(k)=arcbus(k+1)
              arcbas(k)=arcbas(k+1)
              do jk = 1, MAXCAZ
                 arczns(jk,k) = arczns(jk,k+1)
              enddo
           enddo
c
c          Delete associated "I" records
c
           i = 1
           do while (i .le. ntotic)
              if (arcint(1,i) .eq. aname .or. arcint(2,i) .eq. aname) 
     &           then
                 ntotic = ntotic - 1
                 call bcdari(i,xbuf)
                 write (outbuf,10640) xbuf(1:80)
                 call prtout(1)
                 call space(1)
                 do k = i, ntotic
                    arcint(1,k) = arcint(1,k+1)
                    arcint(2,k) = arcint(2,k+1)
                    arcinp(k) = arcinp(k+1)
                 enddo
              else 
                 i = i + 1
              endif
           enddo
           go to 480
        else
C
C          Delete only area continuation record
C
           area_mod =.true.
           j1 = isubtyp * MAXCAZR + 1
           j2 = (isubtyp+1) * MAXCAZR
c#1           do while ( j2  .le.  MAXCAZ - MAXCAZR ) 
c#1              do jk = j1, j2
c#1                 arczns(jk,line) = arczns(jk+MAXCAZR,line)
c#1              enddo
c#1              j1 = j2 + 1
c#1              j2 = j1 + MAXCAZR - 1
c#1           enddo
           do jk = j1, j2
              arczns(jk,line) = ' '
           enddo
           go to 130 
        endif
C
c       ADD NEW AREA
C       Locate LINE (sort order to insert area), push list down,
C       and insert new area in LINE.
C
c       OR ADD NEW ZONES TO AN EXISTING   AREA
c
  440   if (isubtyp .eq. 0) then
           ntotc = ntotc + 1
           do line = 1, ntotc-1
              if (arcnam(line) .gt. buf(4:13)) go to 444
           enddo
           line = ntotc
  444      continue
           do k = ntotc-1, line, -1
 
              arcnam(k+1)=arcnam(k)
              arcnet(k+1)=arcnet(k)
              arcbus(k+1)=arcbus(k)
              arcbas(k+1)=arcbas(k)

              do jk=1,MAXCAZ
                 arczns(jk,k+1) = arczns(jk,k)
              enddo
           enddo
 
           do jk = 1, MAXCAZ
              arczns(jk,line) = ' '
           enddo

           read (buf,450,err=220) arcnam(line),arcbus(line),
     &                            arcbas(line),arcnet(line),
     &                            (arczns(i,line),i=1,MAXCAZR)
  450      format(bz, 3x, a10, a8, f4.0, 1x, f8.0, 10(1x, a2))
           arcnet(line)=arcnet(line)/bmva
           write (outbuf,470) arcnam(line),arcbus(line),
     &                        arcbas(line),arcnet(line)*bmva,
     &                        (arczns(i,line),i=1,MAXCAZR)
  470      format('0 New Area data ',1x,a10,1x,a8,f6.1,f10.2,10(1x,a2))
           call prtout(1)
           call space (1)
        else
           area_mod =.true.
           j1 = isubtyp * MAXCAZR + 1
           j2 = (isubtyp+1) * MAXCAZR
           read (buf,472,err=220) (arczns(i,line),i=j1,j2)
  472      format(bz, t35, 10(1x, a2))
           write (outbuf,474) arcnam(line),(arczns(i,line),i=j1,j2)
  474      format('0 New Area data ',1x,a10,25x,10(1x,a2))
           call prtout(1)
           call space (1)
           go to 130 
        endif
  480   continue
        knum=1
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        go to 130
C
C       Modify area
C
  490   area_mod =.true.
        if (isubtyp .eq. 0) then
           write (outbuf,510) arcnam(line),arcbus(line),
     &                        arcbas(line),arcnet(line)*bmva,
     &                        (arczns(i,line),i=1,MAXCAZR)
  510      format('0 Original data ',1x,a10,1x,a8,f6.1,f10.2,10(1x,a2))
           call prtout(1)
           if (bus1 .ne. ' ')    then
              arcbus(line) = bus1
              arcbas(line) = base1
           endif
           call chkmzr(aex,aexc,1)
           if (aex.gt.-9.0e9) arcnet(line)=aex/bmva
           do i = 1, MAXCAZR
c#              if (azone(i) .ne. ' ') arczns(i,line) = azone(i)
              arczns(i,line) = azone(i)
           enddo
           write (outbuf,532) arcnam(line),arcbus(line),
     &                        arcbas(line),arcnet(line)*bmva,
     &                        (arczns(i,line),i=1,MAXCAZR)
  532      format('  Modified data ',1x,a10,1x,a8,f6.1,f10.2,10(1x,a2))
           call prtout(1)
           call space (1)
        else
           j0 = isubtyp * MAXCAZR
           j1 = j0 + 1
           j2 = (isubtyp+1) * MAXCAZR
           do i = 1, MAXCAZR
c#              if (azone(i) .ne. ' ') arczns(i+j0,line) = azone(i)
              arczns(i+j0,line) = azone(i)
           enddo
           write (outbuf,536) arcnam(line),(arczns(i,line),i=j1,j2)
  536      format('0 Modified Area data ',1x,a10,25x,10(1x,a2))
           call prtout(1)
           call space (1)
        endif
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        goto 130
C
C       Area intertie 'I'
C
10550   read (buf,10560,err=220) aname1,aname2,aex
10560   format (bz, 3x, a10, 1x, a10, 2x, f8.0)
        if (index(' MD',buf(3:3)).ne.0) go to 10580
        if (buf(3:3).ne.'E') go to 200
        write (outbuf,10570)numchg
10570   format(' All area intertie "I" records are being deleted. ',
     &         'change card No.',i5)
        call prtout(1)
        ntotic = 0
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        go to 130
 
10580   do 10600 line=1,ntotic
        if (arcint(1,line) .eq. aname1 .and. 
     &      arcint(2,line) .eq. aname2) then
           if (buf(3:3).eq.'D') go to 10630
           if (buf(3:3).eq.'M') go to 10660
           write (errbuf(1), 10590) numchg
10590      format (' Add area intertie "I" already exists. Change card',
     &     ' No.',i5)
           call prterx ('W',1)
           chgcrd (numchg)(126:126) = 'E'
           go to 130
        endif
10600   continue
        if (buf(3:3) .eq. ' ') then
           if (ntotic .lt. 5*MAXCAR-1) then
              ntotic = ntotic+1
              arcint(1,ntotic) = aname1
              arcint(2,ntotic) = aname2
              arcinp(ntotic) = aex
              ntotic = ntotic+1
              arcint(1,ntotic) = aname2
              arcint(2,ntotic) = aname1
              arcinp(ntotic) = -aex
              chgcrd(numchg)(126:126) = 'P'  ! Set process flag
              go to 130
           else
              write (errbuf(1),10610) 5*MAXCAR, numchg
10610         format ('Area intertie "I" addition exceeds program limit 
     & of ',i3,' items, Change card No.', i5)
              errbuf(2) = ' '
              write(errbuf(3),392) buf(1:80)
              if (is_batch .eq. 0) then
                 call prterx ('E',3)
              else
                 call prterx ('F',3)
              endif
              chgcrd(numchg)(126:126) = 'E'
              go to 130
           endif
        else
           write (errbuf(1),10620) numchg
10620      format(' Modify/delete area intertie "I" not found ',
     &     'Change record No.',i5)
           errbuf(2) = ' '
           write (errbuf(3),392)buf(1:80)
           if (is_batch .eq. 0) then
              call prterx ('E',3)
           else
              call prterx ('F',3)
           endif
           chgcrd(numchg)(126:126) = 'E'
           go to 130
        endif
C
C       Delete area intertie "I"
C
10630   ntotic = ntotic-1
        call bcdari(line,xbuf)
        write (outbuf,10640)xbuf(1:80)
10640   format (' Deleted area intertie data: (',a,')')
        call prtout(1)
        call space(1)
        do 10650 k=line,ntotic
           arcint(1,k) = arcint(1,k+1)
           arcint(2,k) = arcint(2,k+1)
           arcinp(k) = arcinp(k+1)
10650   continue
        if (aname1.eq.buf(4:13).and.aname2.ne.buf(4:13)) then
           aname1 = buf(15:24)
           aname2 = buf(4:13)
           aex = -aex
           go to 10580
        else
           chgcrd(numchg)(126:126) = 'P'  ! Set process flag
           go to 130
        endif
C
C       Modify area intertie "I"
C
10660   call bcdari(line,xbuf)
        write (outbuf,10670)xbuf(1:80)
10670   format (' Area intertie data: (',a,')')
        call prtout(1)
        arcinp(line) = aex
        call bcdari (line,xbuf)
        write (outbuf,10680) xbuf(1:80)
10680   format (' Modified:           (',a,')')
        call prtout(1)
        call space(1)
        if (aname1 .eq. buf(4:13) .and. aname2 .ne. buf(4:13)) then
           aname1 = buf(15:24)
           aname2 = buf(4:13)
           aex = -aex
           go to 10580
        endif
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        go to 130
C
C       "DZ" - delete buses by zone
C
  540   zd = buf(4:5)
        n = 0
        do 560 nbx = 1,ntot_alf
           nb = alf2inp(nbx)
              if (zone(nb) .eq. zd) then
              n = n + 1
              if (numchg + n .ge. MAXCHG) then
                 write (errbuf(1),160) MAXCHG
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(numchg)(126:126) = 'E'  ! Set error flag
                 goto 130
              else
                 base_c = code(base(nb), 4, 0)
                 write (chgcrd(numchg+n), 552) bus(nb), base_c,  
     &               numchg
  552            format ('B D', t7, a8, a4, t121, 'P', i4)
              endif
           endif
  560   continue
C
        write (outbuf,570) n, zd, numchg
  570   format('0DELETED ',i5,' BUSES IN ZONE ',a2,
     &         '. Change card No.',i5)
        call prtout(1)
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        numchg = numchg + n
        goto 130
C
C       "DA" - delete buses by area
C
  580   read (buf,590) aname
  590   format(3x,a10)
        do 600 i=1,ntotc
           if (arcnam(i).eq.aname) goto 620
  600   continue
C
        write (errbuf(1),610) aname,numchg
  610   format(' Did not find area ',a10,
     &    ' to delete buses, Change card No.',i5)
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        chgcrd(numchg)(126:126) = 'E'
        goto 130
C
  620   n=0
        do 640 nbx = 1, ntot_alf
           nb = alf2inp(nbx)
           if (jarzn(nb) .eq. i) then
              n = n + 1
              if (numchg + n .ge. MAXCHG) then
                 write (errbuf(1),160) MAXCHG
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 chgcrd(numchg)(126:126) = 'E'  ! Set error flag
                 goto 130
              else
                 base_c = code(base(nb), 4, 0)
                 write (chgcrd(numchg+n), 552) bus(nb), base_c, 
     &              numchg
              endif
           endif
  640   continue

  650   write (outbuf,660) n, aname, numchg
  660   format('0DELETED',i5,' BUSES IN AREA ',a10,
     &         '. Change card No.',i5)
        call prtout(1)
        call space(1)
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        numchg = numchg + n
        goto 130
C
C       Process zone rename card "Z"
C
  880   continue
        read (buf,890) zmod
  890   format (3x,15(2a2,1x))
C
        i = 1
        do while (i .le. 15 .and. ((zmod(1,i) .ne. ' ' .or. i .eq. 1)))
           n=0
           m=0
           do l = 1, ntot
              if (zone(l) .eq. zmod(1,i)) then
                 zone(l) = zmod(2,i)
                 n = n + 1
              endif
           enddo
C
C          Rename zones within areas
C
           do j = 1,ntotc
              do k = 1,MAXCAZR
                 if (arczns(k,j) .ne. '  ' .or. k .eq. 1) then
                    if (zmod(1,i) .eq. arczns(k,j)) then
                       arczns(k,j) = zmod(2,i)
                       m = 1
                       go to 892
                    endif
                 endif
              enddo
           enddo
  892      continue
           i = i + 1
        enddo

        write (outbuf,910) n, m, zmod(1,i), zmod(2,i)
  910   format('0',i5,' Buses ',i2,' Areas changed from zone ',a,
     &  ' to zone ',a)
        call prtout(1)
        call space(1)
  920   continue
        chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        go to 130
C
C       Process area rename card "ZA"
C
20880   continue
        read (buf,20890) aname1, aname2
20890   format (t4, a10, 2x, a10)
        i1 = find_ara(aname1)
        i2 = find_ara(aname2)
        if (i1 .gt. 0 .and. i2 .le. 0) then
c
c          Temporarily store old name in ntotc + 1
c
           arcnam(ntotc+1) = arcnam(i1)
           arcnet(ntotc+1) = arcnet(i1)
           arcbus(ntotc+1) = arcbus(i1)
           arcbas(ntotc+1) = arcbas(i1)

           do jk = 1,MAXCAZ
              arczns(jk,ntotc+1) = arczns(jk,i1)
           enddo

           if (i1 .lt. i2) then

              do k = i1, i2-1
 
                 arcnam(k) = arcnam(k+1)
                 arcnet(k) = arcnet(k+1)
                 arcbus(k) = arcbus(k+1)
                 arcbas(k) = arcbas(k+1)

                 do jk = 1,MAXCAZ
                    arczns(jk,k) = arczns(jk,k+1)
                 enddo
              enddo

           else if (i1 .gt. i2) then

              do k = i2-1, i1, -1
 
                 arcnam(k+1) = arcnam(k)
                 arcnet(k+1) = arcnet(k)
                 arcbus(k+1) = arcbus(k)
                 arcbas(k+1) = arcbas(k)

                 do jk = 1,MAXCAZ
                    arczns(jk,k+1) = arczns(jk,k)
                 enddo
              enddo
           endif
c
c          Retrieve temporarily stored old name from ntotc + 1
c
           arcnam(i2) = arcnam(ntotc+1)
           arcnet(i2) = arcnet(ntotc+1)
           arcbus(i2) = arcbus(ntotc+1)
           arcbas(i2) = arcbas(ntotc+1) 

           do jk = 1,MAXCAZ
              arczns(jk,i2) = arczns(jk,ntotc+1)
           enddo

           chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        else if (i1 .le. 0) then
           write (errbuf(1), 20900) aname1
20900      format(' Rename area ', a10, ' is not in system ')
           call prterx ('W',1)
           chgcrd(numchg)(126:126) = 'E'  ! Set error flag
        else if (i2 .gt. 0) then
           write (errbuf(1), 20910) aname1, aname2
20910      format(' Renamed area ', a10, ' -> ', a10, 
     &         ' is already in system ')
           call prterx ('W',1)
           chgcrd(numchg)(126:126) = 'E'  ! Set error flag
        endif
        go to 130
C
C       Process bus rename card "ZB"
C
10880   nrename = nrename + 1
        read (buf,10890, err=10920) bus1, base1, bus2, base2
10890   format (t7, a8, f4.0, 2x, a8, f4.0)
        i1 = find_bus(bus1, base1)
        i2 = find_bus(bus2, base2)
        if (i1 .gt. 0 .and. i2 .le. 0) then
           status = rename_bus(i1, bus2, base2)
           chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        else if (i1 .le. 0) then
           write (errbuf(1), 10900) bus1, base1
10900      format(' Rename bus ', a8, f6.1, ' is not in system ')
           call prterx ('W',1)
           chgcrd(numchg)(126:126) = 'E'  ! Set error flag
        else if (i2 .gt. 0) then
           write (errbuf(1), 10910) bus1, base1, bus2, base2
10910      format(' Renamed bus ', a8, f6.1, ' -> ', a8, f6.1, 
     &         ' is already in system ')
           call prterx ('W',1)
           chgcrd(numchg)(126:126) = 'E'  ! Set error flag
        endif
        go to 130

10920   write (errbuf(1), 10930) buf(1:31)
10930   format(' Illegal data in field (', a, ')')
        call prterx ('W',1)
        chgcrd(numchg)(126:126) = 'E'  ! Set error flag
        go to 130
C
C       Percent change card P: PO, PZ, PN, PA, PB, PC, PD
C
  930   continue
        if (ipctno .ge. MAXPCT) then
           write (errbuf(1),950) MAXPCT
  950      format(' More than ',i3,' percentage change cards.')
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
           chgcrd(numchg)(126:126) = 'E'  ! Set error flag
           goto 130
        endif
        ipctno=ipctno+1
        read (buf,970,err=220) pctge(1,ipctno), pctzow(1,ipctno),
     &                         (pctge(i,ipctno),i=3,6)
  970   format (bz, 1x, a1, 1x, a2, 3x, 4(1x, f5.0) )
C
C       Percentage changes forces Rebuild = ON.
C
        if (kspare(1) .eq. 2) then
           write (errbuf(1), 972)
  972      format (' Case Rebuilding option forced by ',
     &             'percentage changes.')
           call prterx ('W', 1)
           kspare(1) = 1
        endif
C
        if ( buf(10:14) .eq. ' ' ) pctge(3,ipctno) = 1.0
        if ( buf(16:20) .eq. ' ' ) pctge(4,ipctno) = pctge(3,ipctno)
        if ( buf(22:26) .eq. ' ' ) pctge(5,ipctno) = 1.0
        if ( buf(28:32) .eq. ' ' ) pctge(6,ipctno) = pctge(5,ipctno)
C
        if (buf(2:2) .eq. 'O') go to 1000
        if (buf(2:2) .eq. 'Z') go to 1040
        if (buf(2:2) .eq. 'N') go to 1040
        if (buf(2:2) .eq. 'A') go to 1080
        if (buf(2:2) .eq. 'B') go to 1000
        if (buf(2:2) .eq. 'C') go to 1040
        if (buf(2:2) .eq. 'D') go to 1040
C
        ipctno = ipctno - 1
  980   write (errbuf(1),990) numchg
  990   format('0Illegal change subtype, Change card No.',i5)
        errbuf(2) = ' '
        write (errbuf(3),392) buf(1:80)
        if (is_batch .eq. 0) then
           call prterx ('E',3)
        else
           call prterx ('F',3)
        endif
        chgcrd(numchg)(126:126) = 'E'
        goto 130
C
C       Complete "PO", "PB" data
C
 1000   read (buf,1010,err=220) (pctzow(i,ipctno),i=1,12)
 1010   format (bz, 3x, a3, 27x, 11(1x, a2))
        n = 0
        do i = 2,12
           if (pctzow(i,ipctno) .eq. '  ') go to 1030
           if (pctzow(i,ipctno) .eq. '00') pctzow(i,ipctno)='  '
           n = n + 2
        enddo
 1030   continue
        pctge(2,ipctno) = n 
        goto 1080
C
C       Complete "PN", "PZ", "PC", and "PD" data.
C
 1040   read (buf,1050) (pctzow(i,ipctno),i=2,12)
 1050   format (bz, 33x, 11(1x, a3))
        n=0
        do i = 2,12
           if (pctzow(i,ipctno).eq.'   ') go to 1070
           if (pctzow(i,ipctno).eq.'000') pctzow(i,ipctno)='   '
           n = n + 1
        enddo
 1070   continue
        pctge(2,ipctno)= n
C
C       Flag different "P" subtypes.
C
 1080   error = 0
        do i = ipctno-1, 1, -1
c	
c	The third argument in this call is a real.  
c
           call getchr (1, pcttyp, pctge(1,i))
           if (pcttyp .eq. buf(2:2) .and. buf(2:2) .eq. 'A') then
              write (errbuf(1),1090) numchg
 1090         format(' Ignoring duplicate "PA" Change record No.',
     &           i5)
              errbuf(2) = ' '
              write (errbuf(3),392) buf(1:80)
              if (is_batch .eq. 0) then
                 call prterx ('E',3)
              else
                 call prterx ('F',3)
              endif
              chgcrd(numchg)(126:126) = 'E'
              ipctno = ipctno - 1
              error = 1
              go to 1112
           else if (pcttyp .ne. buf(2:2)) then
              write (errbuf(1),1100) buf(2:2), numchg
 1100         format(' Caution - different FACTOR subtypes "P', a,
     &          '" Change record No.', i5)
              call prterx ('W',1)
              go to 1112
           endif
        enddo
 1112   continue
        if (error .eq. 0) then
           chgcrd(numchg)(126:126) = 'P'  ! Set process flag
        else
           chgcrd(numchg)(126:126) = 'E'
        endif
        go to 130
C
C       "( S O" in buf(1:1) - all change cards read
C
C       File error handling return

 1520   continue
        chgnam = ' '
        inpchg = inp
        buf = '( STOP )'

 1530   continue
        if ( chgnam .ne. ' ' ) then
           close ( unit = chgdta )
           chgnam = ' '
        endif

        if ( .not. area_mod ) return
c
c  Compress any non trailing spaces out of arczns table lines
c
        do ia = 1, MAXCAR
           iz = 2
 1900      do while ( iz .lt. MAXCAZ  .and. arczns(iz,ia) .ne. ' ' )
              iz = iz + 1
           enddo
           if ( iz .eq. MAXCAZ ) goto 2000
           j = iz + 1 
           do while ( j .lt. MAXCAZ  .and. arczns(j,ia) .eq. ' ' )
              j = j + 1
           enddo
           if ( j .eq. MAXCAZ ) goto 2000
c             n = j - iz
c             k = MAXCAZ - n + 1
           k = MAXCAZ - j + iz + 1
           i = iz
           do while ( j .lt. MAXCAZ+1 )
              arczns(i,ia) = arczns(j,ia)
              i = i + 1
              j = j + 1
           enddo
           do i = k, MAXCAZ
              arczns(i,ia) = ' '
           enddo
           goto 1900
 2000      continue
        enddo
c
c Simpler version of above, but less efficient
c
c        do ia = 1, MAXCAR
c           do iz = 2, MAXCAZ
c              if ( arczns(iz,ia) .eq. ' ' ) then
c                 do k = iz, MAXCAZ - 1
c                    arczns(k,ia) = arczns(k+1,ia)
c                 enddo
c                 arczns(MAXCAZ,ia) = ' '
c              endif
c           enddo
c        enddo


        return
C
        end
