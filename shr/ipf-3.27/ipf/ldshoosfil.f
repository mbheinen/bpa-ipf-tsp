C    @(#)ldshoosfil.f	20.4 8/30/00
C**************************************************************** 
C 
C     File: ldshoosfil.f 
C 
C     Purpose: Routine to load Out of Service shunt data file 
C 
c     Input parameters:
c
c        scrfil      -  logical unit of opened OOS shunt data file
c        filename    -  file name of opened OOS shunt data file
c        option      -  User-selected options
c
c     Output parameters:
c
c        error       -  0/1 (normal/error)
c
C     Author: Walt Powell  Date: 1 Aug 2000
C     Called by: net_data_sub.f 
C 
C**************************************************************** 
        integer function ldshoosfil (scrfil, filename, option, error) 
        integer scrfil, error 
        character filename*(*), option(10)*1
 
        include 'ipfinc/parametr.inc' 
 
        include 'ipfinc/blank.inc' 
        include 'ipfinc/bus.inc' 
        include 'ipfinc/cbus.inc' 
        include 'ipfinc/prt.inc' 
        include 'ipfinc/brtype.inc'

        common /is_batch / is_batch

        integer max_out_of_service
        parameter (MAX_OUT_OF_SERVICE = 200)
        common /out_of_service/ numoossh, shunt_status(MAXCBS),
     &                          shunt_value(16,MAX_OUT_OF_SERVICE), 
     &                          branch_status(MAXBRN)
        integer numoossh, shunt_status, branch_status
        real shunt_value

        integer numrec, find_bus, k1, pold, compare
        character type*1, bus1*8, xbuf*132, month*1, cbtype*1, 
     &            cbown*3, cbyear*2, tempyear*2
        real base1
        logical finished
c 
c       Read in and hash OOS shunt data
C 
        numrec = 0 
        error = 0
        ldshoosfil = 0

        if (filename .eq. ' ') go to 130
        write (*, 10000) 
10000   format(1x, '* Loading Out-of-Service Shunt Data File - this will
     & take a few moments') 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=120) xbuf 
          numrec = numrec + 1
          if (xbuf(1:1) .eq. 'X') then
            icb = 0
            read (xbuf, 10040, err=100) bus1, base1
10040       format (bz, t7, a8, f4.0)

            k1 = find_bus(bus1, base1) 
            if (k1 .le. 0) then
              write (errbuf(1), 10050) xbuf(1:33)
10050         format ('Bus1 on Out-of-service record [', a, '] is not in
     & system')
              call prterx ('W', 1)                
              error = 1
              go to 110
            endif
c
c           Add +bus additions
c
            iyear = 1
            write (tempyear, fmt='(i2.2)') iyear
            pold = 0
            icb = 0
            ncb = kbsdta(15, k1)  
            do while (ncb .gt. 0 .and. icb .eq. 0)
              call getchr (1, cbtype, kbctbl(8,ncb))
              call getchr (3, cbown, kbctbl(10,ncb))
              call getchr (2, cbyear, kbctbl(9,ncb)) 
              compare = kompr (xbuf(1:1) // xbuf(4:6) // tempyear,
     &                         cbtype // cbown // cbyear, junk)
              if (compare .lt. 0) then
c
c               Insert current entity between pold and ncb
c
                if (ntot2 + 1 .ge. MAXCBS) then
                  write (errbuf(1), 10080) MAXCBS
10080             format ('More than ', i5,
     &              ' +bus records. Overflow occurred at +bus:') 
                  write (errbuf(2), 10090) xbuf(1:80)   
10090             format(1x, '[', a, ']')         
                  call prterx ('W', 2)                
                  ntot2 = 1                           
                  if (is_batch .eq. 0) then
                     call prterx ('W', 2)
                  else
                     call prterx ('E', 2)
                  endif
                  go to 110
                endif
                ntot2 = ntot2 + 1
                icb = ntot2
                bctbl_nxt(ntot2) = ncb
                if (pold .eq. 0) then
                  kbsdta(15,k1) = ntot2
                else
                  bctbl_nxt(pold) = ntot2
                endif
              else if (compare .eq. 0) then
                write (errbuf(1), 10110) xbuf(1:33)
10110           format( ' +Bus already in system, record [', a, 
     &            ']')
                call prterx ('W', 1)
                iyear = iyear + 1
                write (tempyear, fmt='(i2.2)') iyear
              else
                pold = ncb
                ncb = bctbl_nxt(ncb)
              endif
            enddo

            if (icb .eq. 0) then
              if (ntot2 + 1 .ge. MAXCBS) then
                write (errbuf(1), 10080) MAXCBS
                write (errbuf(2), 10090) xbuf(1:80)   
                call prterx ('W', 2)                
                ntot2 = 1                           
                if (is_batch .eq. 0) then
                  call prterx ('W', 2)
                else
                  call prterx ('E', 2)
                endif
                go to 110
              endif
              ntot2 = ntot2 + 1
              icb = ntot2
              bctbl_nxt(ntot2) = ncb
              if (pold .eq. 0) then
                kbsdta(15,k1) = ntot2
              else
                bctbl_nxt(pold) = ntot2
              endif
            endif
c
c           Note:  The shunt value must be hidden else IPF will
c           interpret any non-zero value as a valid shunt.
c
            do i = 1, 12
              kbctbl(i,ntot2) = 0
            enddo

            numoossh = numoossh + 1
            kbctbl(1,ntot2) = numoossh

            read (xbuf, 10120, err=100) (shunt_value(k,numoossh),k=1,16)
10120       format(bz, t33, 8(f1.0, f5.0))                      

            call putchr (1, 'X', kbctbl(8,ntot2))
            call putchr (2, '00', kbctbl(9,ntot2))
            call putchr (3, xbuf(4:6), kbctbl(10,ntot2))
            kbctbl(7,ntot2) = intdte ('1', 0)
            shunt_status(ntot2) = 0
            go to 110

  100       error = 1
            write (errbuf(1), 10130) xbuf(1:80)
10130       format (' Illegal data in field : [', a80, ']')
            call prterx ('W', 1)

  110       continue
          endif

        enddo

  120   write (*, 10200) numrec, filename  
10200   format (1x, i4, ' Out-Of-Service records processed in file ',
     & a)

  130   continue

        return
        end
