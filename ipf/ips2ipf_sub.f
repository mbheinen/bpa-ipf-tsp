C    @(#)ips2ipf_sub.f	20.6 11/11/97
C       Altered by M. George, 5/27/93
C       Altered by M. George, 9/21/93
C       Altered by M. George, 10/1/93
C       Altered by Walter Powell, 10/29/93
C       Altered by M. George, 11/5/93
C       Altered by M. George, 11/15/93 (missing BPA routines from JC)
C       Altered by M. George, 11/29/93 (bug fixes)
C       Altered by W. Powell, 12/27/94 (Recognize special LM record as commment)
C
C   This module must be linked with libipfnew.a
C
C  Program IPS2IPF:
c       Load WSCC network data file and perform the following
c       conversions:
c  Pass 1:
c       1. Convert non-zero steps to taps on reg. transformers.
c       2. Change 'C' in first column to '.', change 'LM' in first
c           two columns to '.LM'
c       3. Make system slack bus a BS bus (if named by user).
c   The two items below address conversion of WSCC std. study cases.
c       4. Convert HDG record to /HEADER, and title records to H
c             (header) records.
c       5. Convert BAS record to /NETWORK_DATA, and ZZ to (END).
c  Pass 2:
c       6. Rename buses as necessary. ** Altered to detect duplicate
c           busnames, and provide new names automatically.
c       7. Transfer remote bus name on X record to remote bus field
c           on BX record.
c  Pass 3:
c       8. Correct blank section id in multi-section lines.  If more
c             than 9 sections, combine the last two.
c  Pass 4:
c       9. Transfer B_shunt on BE and BQ records to B_shunt on +A record.
c      10. Transfer Vmin and Vmax from BX record to remote controlled
c              bus record.
c
        subroutine ips2ipf_sub
 
        include 'ipfinc/ips2ipf.inc'

        parameter (MAXDATA = 20000)    !number of text records

        common /text/ text, xdata, vdata
        character text(MAXDATA)*120, xdata(100)*80,vdata(100)*80

        character infile * 60, outfile * 60, code * 10, codec * 10,
     &            oldname * 12, newname * 12, swingbus * 12,
     &            temp(10) * 120, newtemp * 120
        logical loaded, eof, found, finished
        integer status, add_name, find_name, check_name,
     &          hashbskv, gtpieqiv, p, h

c       Input data file on unit 10
c       Log file on unit 11
c       Output .net file on unit 12

        loaded = .false.
        do while (.not.loaded)
           write(*, 100)
  100      format(' > Enter IPS input data file name: ',$)
           read (*, 110) infile
  110      format (a)
           last = lastch (infile)
           open (unit=10,file=infile(1:last),status='OLD',
     &           iostat=status)
           if (status .eq. 0) loaded = .true.
        enddo
        open (unit=11,file='ips2ipf.log',status='UNKNOWN')
        write (11,*) 'IPS Input Data File was ',infile

        write (*, 125)
  125   format(' > Enter name of swing bus: ',$)
        read (*,110) swingbus
        write (11,*) 'Swing Bus Name was ',swingbus
 
        loaded = .false.
        do while (.not.loaded)
           write(*, 130)
  130      format(' > Enter IPF network data output file name: ',$)
           read (*,110) outfile
           open (unit=12,file=outfile(1:last),status='UNKNOWN',
     &           iostat=status)
           last = lastch (outfile)
           if (status .eq. 0) loaded = .true.
        enddo
           write (11,*) 'IPF Network Data File is ',outfile

        eof = .false.

        rewind 12
c
c       Load WSCC network data file and perform conversions
c--------------------------------------------------------------------
c
c       First pass: Perform steps 1 - 5
c
        do i = 1, HASHSIZE
           htable(i) = 0
        end do
        do i = 1, MAXBUSES
           nextptr(i) = 0
        end do

        write (*, 132) 
  132   format (' * Begin pass 1 - load data, search for duplicate',
     .    ' busnames, and do all simple edits.')

        numbus = 0
        numcvt = 0
        numtext = 0
        numxdta = 0
        do while (.not. eof)        	
           read (10, 110, end=160) text(numtext+1)
           numtext = numtext + 1
           if (text(numtext)(1:1) .eq. 'A') then  ! just write it out
           else if (text(numtext)(1:3) .eq. 'BAS') then
              text(numtext) = './ NETWORK_DATA, FILE = *'
           else if (index ('B+XQ', text(numtext)(1:1)) .gt. 0) then
              if (text(numtext)(1:1) .eq. 'B') then
c
c                "check_name" interprets busname as a8, f4.0
c
                 ind = check_name (text(numtext)(7:18))
                 if (ind .lt. 0) then
                    numcvt = numcvt + 1
                    rename(1,numcvt) = text(numtext)(7:18)
                 endif
c  Make swing bus a BS
                 if (text(numtext)(7:18) .eq. swingbus) 
     &              text(numtext)(2:2) = 'S'
c  Store X data
              else if (text(numtext)(1:1) .eq. 'X') then
                 numxdta = numxdta + 1
                 xdata(numxdta) = text(numtext)
              endif
           else if (index ('LERT', text(numtext)(1:1)) .ne. 0) then
              if (text(numtext)(1:1) .eq. 'R' .and. 
c  Steps to Taps
     &            text(numtext)(34:45) .ne. ' ') then
                 read (text(numtext)(56:57), '(bz, i2)') isteps
                 if (isteps .gt. 0) then
                    write (text(numtext)(56:57), '(i2)') isteps + 1
                 endif
c  Bus Ties
              else
                 read(text(numtext)(39:50),'(bz,2F6.5)') rr,xx
                 if (rr.eq.0.0 .and. xx.eq.0.0) 
     &               text(numtext)(45:50) = '0.0001'
              endif
c  Misc. changes
           else if (text(numtext)(1:3) .eq. 'HDG') then
              text(numtext) = './ HEADER '
           else if (text(numtext)(1:1) .eq. ' ') then
              text(numtext)(1:1) = '.'
           else if (text(numtext)(1:1) .eq. 'C') then
              text(numtext)(1:1) = '.'
           else if (text(numtext)(1:2) .eq. 'ZZ') then
              text(numtext) = '.( END ) '
           endif
        enddo
  160   continue
c
c       Second pass: Items 6 and 7
c
        write (*, 182) 
  182   format (' * Begin pass 2 - rename duplicate buses')
c
c       Determine unique bus name
c
        do i = 1, numcvt
           oldname = rename(1,i)
           read (oldname(9:12), '(bz, f4.0)') basekv
           newname = oldname
           j = ichar('A')
           finished = .false.
           do while (j .le. ichar('Z') .and. .not. finished)
              newname(8:8) = char(j)
              h = hashbskv (newname(1:8), basekv)
              p = htable(h)
              do while (p .gt. 0)         !search for existing entities
                 if (newname(1:8) .ne. oldbus(p)(1:8) .or. 
     &               basekv .ne. oldbase(p)) then
                    p = nextptr(p)
                 else
                    p = -p                   
                 endif
              enddo
              if (p .lt. 0) then
                 j = j + 1
              else
                 finished = .true.
                 rename(2,i) = newname
                 write (*, 170) oldname, newname
                 write (11,170) oldname, newname
  170            format (' rename bus ', a, ' new name ', a)
              endif
           enddo
           if (.not. finished) then
               write (*, 180) rename(1,i)
               write (11,180) rename(1,i)
  180          format (' could not generate a unique name for bus ', a)
           endif
        enddo
c
c       Hash converted names 
c
        do i = 1, HASHSIZE
           htable(i) = 0
        end do
        do i = 1, MAXBUSES
           nextptr(i) = 0
        end do

        do ib = 1, numcvt
           ind = add_name (rename(1,ib))
           if (ind .gt. 0) rename(2,ind) = rename(2,ib)
        enddo
c
c       Now test every bus in the network for name conversion
c
        ind = find_name (swingbus)
        if (ind .gt. 0) then
           swingbus = rename(2,ind)
        endif
        numvdta = 0
c
        do ib = 1, numtext
           if (text(ib)(1:1) .eq. 'A') then
              ind = find_name (text(ib)(14:25))
              if (ind .gt. 0) then
                 text(ib)(14:25) = rename(2,ind)
              endif
           else if (text(numtext)(1:3) .eq. 'BAS') then
           else if (index ('B+XQ', text(ib)(1:1)) .gt. 0) then
              ind = find_name (text(ib)(7:18))
              if (ind .gt. 0) then
                 text(ib)(7:18) = rename(2,ind)
              endif
              
              if (text(ib)(1:2) .eq. 'BX') then
c
c                Seach for X record
c
                 found = .false.

                 jb = 1
                 do while (jb .le. numxdta .and. .not. found)
                    if (xdata(jb)(7:18) .eq. text(ib)(7:18)) then
                       found = .true.
                       text(ib)(66:77) = xdata(jb)(21:32)
c
c  Put voltage hold data on remote bus record
c  Store BX record with voltage limits and remote bus name

                       numvdta = numvdta + 1
                       vdata(numvdta) = text(ib)
                    else
                       jb = jb + 1
                    endif
                 enddo
              endif
              if ((text(ib)(1:2) .eq. 'BG' .or. 
     &             text(ib)(1:2) .eq. 'BX') .and. 
     &            text(ib)(66:77) .ne. ' ') then
                 ind = find_name (text(ib)(66:77))
                 if (ind .gt. 0) then
                    text(ib)(66:77) = rename(2,ind)
                 endif
              else if ((text(ib)(1:2) .eq. 'BD' .or. 
     &                  text(ib)(1:2) .eq. 'BM') .and. 
     &                  text(ib)(51:62) .ne. ' ') then
                 ind = find_name (text(ib)(51:62))
                 if (ind .gt. 0) then
                    text(ib)(51:62) = rename(2,ind)
                 endif
              else if (text(ib)(1:1) .eq. 'X' .and. 
     &                 text(ib)(21:32) .ne. ' ') then
                 ind = find_name (text(ib)(21:32))
                 if (ind .gt. 0) then
                    text(ib)(21:32) = rename(2,ind)
                 endif
              endif
           else if (index ('LERT', text(ib)(1:1)) .ne. 0) then
              ind = find_name (text(ib)(7:18))
              if (ind .gt. 0) then
                 text(ib)(7:18) = rename(2,ind)
              endif
              ind = find_name (text(ib)(20:31))
              if (ind .gt. 0) then
                 text(ib)(20:31) = rename(2,ind)
              endif
              if (text(ib)(1:1) .eq. 'R' .and. 
     &            text(ib)(34:45) .ne. ' ') then
                 ind = find_name (text(ib)(34:45))
                 if (ind .gt. 0) then
                    text(ib)(34:45) = rename(2,ind)
                 endif
              endif
           endif
        enddo
c
c       Third pass: Perform step 8.
c
        write (*, 184) 
  184   format (' * Begin pass 3 - rename illegal sections ')

        do ib = 1, numtext
c
c          Check for illegal section "0" 
c
           if (index ('LERT', text(ib)(1:1)) .ne. 0) then
              if (text(ib)(33:33) .ne. ' ' .and. text(ib) .ne. '0') then
                 j = ib - 1
                 do while ( j .gt. 0 .and. j .gt. ib-9 .and.
     &                    (text(ib)(7:18) .eq. text(j)(7:18) .and.
     &                     text(ib)(20:32) .eq. text(j)(20:32)))
                    if (text(j)(33:33) .eq. '0' .or. 
     &                  text(j)(33:33) .eq. ' ') then
                       isect = 0
                       do while ( j .le. numtext .and. 
     &                            isect .lt. 9 .and.
     &                           text(ib)(7:18) .eq. text(j)(7:18) .and.
     &                           text(ib)(20:32) .eq. text(j)(20:32))
                          isect = isect + 1
                          write (*, 185) text(j)(1:33), isect
                          write (11,185) text(j)(1:33), isect
  185                     format (' rename section ', a, 
     &                            ' new section ', i1)
                          write (text(j)(33:33), '(i1)') isect
                          j = j + 1
                       enddo
c
c                      If more than 9 sections consolidate sections
c                      9, ... into section 9.
c
                       if (j .le. numtext .and. isect .ge. 9 .and.
     &                     text(ib)(7:18) .eq. text(j)(7:18) .and.
     &                     text(ib)(20:32) .eq. text(j)(20:32)) then
                          numtemp = 0
                          j = j - 1
                          i = j
                          do while ( j .le. numtext .and. 
     &                          text(ib)(7:18) .eq. text(j)(7:18) .and.
     &                          text(ib)(20:32) .eq. text(j)(20:32))
                             numtemp = numtemp + 1
                             temp(numtemp) = text(j)
                             if (j .ne. i) then
                                write (*, 186) text(j)(1:33)
  186                           format (' section consolidated ', a)
                                text(j)(1:1) = '.'
                             endif
                             j = j + 1
                          enddo
                          status = gtpieqiv (numtemp, temp, newtemp)
                          if (status .eq. 0) then
                             text(i)(1:120) = newtemp
                             write (*,188) text(i)(1:120)
                             write (11,188) text(i)(1:120)
  188                        format ('New equivalent section 9:'/a)
                          endif
                       endif
                       j = 0
                    endif
                    j = j - 1
                 enddo
              endif
           endif
        enddo

c       Fourth pass - Items 9, 10, and write out new file.
c
        write (*, 190)
  190   format (' * Begin pass 4 - put shunt on +A, voltage hold',
     .     ' on remote bus, and write out converted records.')

        do ib = 1, numtext

c    Transfer voltage hold from BX to remote bus
c
           if (text(ib)(1:1) .eq. 'B' .and.
     &         text(ib)(58:65) .eq. ' ') then
              found = .false.
              jb = 1
              do while (jb .le. numvdta .and. .not. found)
                 if (vdata(jb)(66:77) .eq. text(ib)(7:18)) then
                    found = .true.
                    text(ib)(58:65) = vdata(jb)(58:65)
                 else
                    jb = jb + 1
                 endif
              enddo
           endif
 
c   Put shunt on +A record to make it fixed.
c   If +A record is created, write out the bus record and then +A.
 
           if ((text(ib)(1:2) .eq. 'BQ' .or. text(ib)(1:2) .eq. 'BE')
     &         .and.   text(ib)(35:38) .ne. ' ') then
               read (text(ib)(35:38), '(bz, f4.0)') b1
               text(ib)(35:38) = ' '
c   Write out the bus card and then put the +A card in its place.
               last = lastch(text(ib))
               write (12,110) text(ib)(1:last)
               text(ib)(1:2) = '+A'
               text(ib)(19:80) = ' '
               codec = code (b1, 4, 0)
               text(ib)(35:38) = codec(1:4)
           endif
c
c  Write out everything else (except any RF records)
c
           if (text(ib)(1:2) .ne. 'RF') then
               last = lastch(text(ib))
               if (last .eq. 0) then
                  text(ib) = '.'
                  last = 1
               endif
               write (12,110) text(ib)(1:last)
           endif
        end do 
c
c   End of processing
c
        last1 = lastch (infile)
        last2 = lastch (outfile)

        write (*, 200) numtext, infile(1:last1), outfile(1:last2)
        write (11,200) numtext, infile(1:last1), outfile(1:last2)
  200   format (1x, i6, ' Records processed in file ', a, /,
     &          7x,     ' Results written to file ', a)

        close (11)
        close (12)
        return
        end

