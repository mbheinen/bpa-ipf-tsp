C    %W% %G%
C****************************************************************
C
C   File: p_gtbase.f
C   Purpose: IPF shell program to process /OLD_BASE commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: 
C
C****************************************************************
C
        integer function p_gtbase (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl

        logical baseok
        character null * 1, linefeed * 1, stext * 50
        integer o2, apdoutbuf, fltstr, kmpzone, bldzone
        external kmpzone, swapzone

        max_buf = len( out_buffer ) - len ( stext ) - 10
        if (max_buf .lt. 0) max_buf = len( out_buffer )
        null = char(0)
        linefeed = char(10)
        p_gtbase = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null
        
        call baseinit
        call ctlpow
        call loadarcv

        buf = inrcd
        crun1(3) = ' '
        call gtbase (obasnm, crun1(3), baseok)
        call prtime('GTBASE')
        if (.not.baseok) then
           jobreq(1) = 'quit'
           p_gtbase = 1
           ostates = 0
        else
           ostates = 0
           if ( lskp .eq. 1  .or.  lskp .eq. 2 ) then
              ostates = 5
           else if ( lskp .eq. 3 ) then
              ostates = 7
           endif
C       
C          Store Q_NET in CAPCOR(2,*)
C       
           do nb = 1, ntot   
              kt = inp2opt(nb) 
              capcor(2,kt) = qnetu(kt)          
           enddo

           if (ntotc .eq. 0) then
c
c*** Build incomplete ACZNAM array from from bus data and sort it 
c
              do nb = 1, MAXCZN
                 nextptr_z(nb) = 0
              enddo
              do nb = 1, HASHSIZE_Z
                 htable_z(nb) = 0
              enddo
              nztot = 0
              do nb = 1, ntot
                 izone = bldzone(zone(nb), jarzn(nb))
              enddo
              if (nztot .gt. 0) then
                 call qiksrt (1, nztot, kmpzone, swapzone)
              endif
           else
              do i = 1, ntotc
                 k1 = karea(1,i)                                              
                 pgen = busdta(8,k1)                                 
                 ncb = kbsdta(15,k1)                                 
                 do while (ncb .gt. 0) 
                   pgen = pgen + bctbl(6,ncb)       
                   ncb = bctbl_nxt(ncb) 
                 enddo 
                 area(7,i) = dble(pgen)                
              enddo
           endif
           bldtbl = .false.
c
c*** Try to catch any problems with "old" oldbase files (PF60xx)
c
c           if ( lskp .eq. 2 ) then
c              ostates = 2
c              stext = '/solution' // linefeed // '>BASE_SOLUTION' //
c     &                  null
c              call cpyinbfil( stext, inp )
c              i = isolton ()
c           endif
c

        endif
c
c************************************************************************
c*** info for debugging
c
c        if (p_gtbase .eq. 0) then
c           last = lastch (obasnm)
c           write (errbuf(1), 336) obasnm(1:last)
c  336      format (' OLD_BASE file ', a, ' opened.')
c           call prterx ('I', 1)
c        else
c           last = lastch (obasnm)
c           write (errbuf(1), 338) obasnm(1:last)
c  338      format (' OLD_BASE file ', a, ' could not be opened.')
c           call prterx ('I', 1)
c        endif
c************************************************************************
c
c       Append error messages to buffer
c
        j = 1 
        length = 1
        o2 = index (out_buffer,null)
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c       Append summary
c
        if ( o2 .gt. max_buf ) then
           o2 = max_buf
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (stext, 340) 'p_gtbase.f', p_gtbase, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, stext, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
        end
