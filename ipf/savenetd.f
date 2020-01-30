C    @(#)savenetd.f	20.13 10/13/99
C****************************************************************
C
C   	File: savenetd.f
C
C   	Purpose: Write network data file in four dialects using base 
C                case in residence: BPA, WSCC (IPS), PTI, & PECO.
C                                                                      *
C       Input parameters:
C
C       savfil         - the logical unit opened
C       saveflnm       - the name of the file opened (for diagnostics)
C       dialect        - a character string (BPA, WSCC, WSCC1, PTI)
C                        denoting the dialect of the WSCC record.
c       size           - a character string "80", "120" or "B80/L120"
C                        denoting the bus/branch output record size
c       ratings        - a character string denoting how extended 
c                        line ratings are to be used (EXTENDED,
c                        NOMINAL, or MINIMUM)
c       sections       - a character string <null> or "PSEUDOBUSES"
c                        denoting how line sections are to be 
c                        represented
c       type_e         - a character string <null> or "SYMMETRIC"
c                        denoting how assymetric type E-branches are
c                        to be represented
c       ipsfil         - a logical unit for a WSCC reference data file
c       ipsref_file    - a character string or <null> denoting the name
c                        of the WSCC reference file opened.
c       zonerenfil     - a logical unit for a zone rename change file
c                        name
c       zone_ren_file  - a character string or <null> denoting the name
c                        of a zone rename change reference file opened.
C
C   	Author: Walt Powell            Date: 13 January 1993
C   	Called by: p_svfile.f
C
C****************************************************************
C
      	integer function savenetd (savfil, saveflnm, dialect, size,
     &                             ratings, sections, type_e, ipsfil,
     &                             ipsref_file, zonerenfil, 
     &                             zone_ren_file)
        integer savfil, ipsfil, zonerenfil
        character *(*) saveflnm, dialect, size, ratings, sections,
     &                 type_e, ipsref_file, zone_ren_file
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc' 
        include 'ipfinc/area.inc' 
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/lfiles.inc'
      	include 'ipfinc/prt.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/pti_data.inc'
        include 'ipfinc/wsccbase.inc'

        character ratcod*2, text*120, code*10, zmod(2,15)*2, zn*2
        integer lenrec, ext_ai, ext_bus, ext_brn, ftn_atoi, find_bus,
     &          status, zone_ren
        logical finished

        savenetd = 0
        num0 = 0
        numa = 0
        numb = 0
        numl = 0

        if (index (size, '120') .ne. 0) then
          lenrec = 120
        else
          lenrec = 80
        endif
        ratcod = ratings(1:1) 

        if ( dialect .eq. 'PTI' ) then
 
c          PTI dialect

           do nb = 1, ntot
             if (wsccbase(nb) .ne. ' ') then
               pti_name(nb) = wsccbase(nb)
             else
               pti_name(nb) = code (base(nb), 4, 0)
             endif
           enddo

c          set up comment lines
c                          ....
           if (index (cspare(30), char(0)) .eq. 1 .or.
     &         cspare(30) .eq. ' ') cspare(30) = 'data'
           write (busbrn,110) cspare(30)
           write (busbrn,110) cspare(30)
           write (busbrn,110) cspare(30)
  110      format (' ', a)
C
C          Process zone rename card "Z"
           if (zone_ren_file .ne. ' ' .and. 
     &         zone_ren_file(1:) .ne. char(0)) then
             status = zone_ren (zonerenfil)
           endif

c          Extract area interchanges and area interties
           numa = ext_ai ( savfil, dialect, lenrec, ratcod, sections,
     &                     type_e )

c          Extract bus data
           numb = ext_bus ( savfil, dialect, lenrec, ratcod, sections,
     &                      type_e )

c          Extract branch data
           numl = ext_brn ( savfil, dialect, lenrec, ratcod, sections,
     &                      type_e )
 
        elseif ( dialect(1:3) .eq. 'BPA' .or.
     &           dialect(1:4) .eq. 'WSCC') then
 
           do nb = 1, ntot
             if (wsccbase(nb) .ne. ' ') then
               pti_name(nb) = wsccbase(nb)
             else
               pti_name(nb) = code (base(nb), 4, 0)
             endif
           enddo

           if (dialect(1:4) .eq. 'WSCC' .and. ipsref_file .ne. ' ' .and.
     &         ipsref_file(1:1) .ne. char(0)) then

             text = ' '
             finished = .false.
             do while (.not. finished)
               read (ipsfil, fmt='(a)', end=118) text(1:80)
               if (text(1:1) .eq. 'B') then
                 basekv = ftn_atof (text(15:18))
                 nb = find_bus(text(7:14), basekv)
                 if (nb .ne. 0) then
                   pti_name(nb) = text(15:18)
                 endif
               endif
             enddo
  118        continue
           endif
C
C          Process zone rename card "Z"
C
           if (dialect(1:4) .eq. 'WSCC' .and. zone_ren_file .ne. ' ' 
     &         .and. zone_ren_file(1:1) .ne. char(0)) then
             status = zone_ren (zonerenfil)
           endif

c          Save case comments, headers, etc.
           write ( text, 120 ) basval(4) 
  120      format ( '.#CASE_ID = ', a )
           last = lastch(text)
           if (last .gt. 80 .and. dialect(1:4) .eq. 'WSCC') last = 80
           write (savfil, '(a)') text(1:last)
           num0 = num0 + 1

           write ( text, 122) basval(7) 
  122      format ( '.#CASE_DS = ', a )
           last = lastch (text)
           if (last .gt. 80 .and. dialect(1:4) .eq. 'WSCC') last = 80
           write (savfil, '(a)') text(1:last)
           num0 = num0 + 1

           write ( text, 124) prgvsn, basval(4)(1:10), basval(7), 
     &       basval(5)(1:10)
  124      format ( '.#H1 Version: ', a, ' CaseID: ', a,
     &              ' Description: ', a, ' Date: ', a )
           last = lastch(text)
           if (last .gt. 80 .and. dialect(1:4) .eq. 'WSCC') last = 80
           write (savfil, '(a)') text(1:last)
           num0 = num0 + 1

           do i = 1, 2
              j = index (coment(i), char(0))
              if (j .gt. 0) coment(i)(j:) = ' '
              if (dialect(1:4) .eq. 'WSCC') then
                 write ( text, 126) i+1, coment(i)(1:75)
  126            format ( '.#H', i1, 1x, a )
                 last = lastch(text)
                 write (savfil, '(a)') text(1:last)
              else
                 write ( text, 126) i+1, coment(i)(1:115)
                 last = lastch(text)
                 write (savfil, '(a)') text(1:last)
              endif
              num0 = num0 + 1
           enddo

           do i = 1, ncom
              if (dialect(1:4) .eq. 'WSCC') then
                 write (text, 130) i, com(i)(1:73)
  130            format ('.#C', i3, 1x, a)
                 last = lastch (text)
                 write (savfil, '(a)') text(1:last)
              else
                 write (text, 130) i, com(i)(1:113)
                 last = lastch (text)
                 write (savfil, '(a)') text(1:last)
              endif
              num0 = num0 + 1
   
           enddo

c          Extract area interchanges and area interties
           numa = ext_ai ( savfil, dialect, lenrec, ratcod, sections,
     &                     type_e )
c
c          Redefine lenrec if hybrid 'B80/L120' is used
c
           if (index (size, '80') .ne. 0) then
             lenrec = 80
           else
             lenrec = 120
           endif

c          Extract bus data
           numb = ext_bus ( savfil, dialect, lenrec, ratcod, sections,
     &                      type_e )
c
c          Restore lenrec if hybrid 'B80/L120' is used
c
           if (index (size, '120') .ne. 0) then
             lenrec = 120
           else
             lenrec = 80
           endif

c          Extract branch data
           numl = ext_brn ( savfil, dialect, lenrec, ratcod, sections,
     &                      type_e )
 
        endif
 
        numrecs = num0 + numa + numb + numl
        write (outbuf, 170) numrecs, saveflnm
  170   format (1x, i5, ' records written to NETWORK_DATA file ', a)
        call prtout (1)
        errbuf(1) = outbuf
        call prterx ('I', 1)

        return
        end
