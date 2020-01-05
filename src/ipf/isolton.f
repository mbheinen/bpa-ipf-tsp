C    %W% %G%
C****************************************************************
C
C   File: isolton.f
C   Purpose: IPF shell program to process /SOLUTION commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: p_solton
C
C****************************************************************
C
	integer function isolton() 

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/alpha.inc' 
        include 'ipfinc/alpha2.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/update.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/xdata.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr, reordr, status

        common /is_batch / is_batch       ! 0 = interactive, 1 = batch

        common /ownflg/ ownflg
        logical ownflg

        integer chkerr, open_apnd, ckzoneup
        character  zonex(100)*2, tempc*60

        isolton = 1     ! initialize return to FAIL state
        
        buf = inrcd
        if (ostates .lt. 2) then
           write (errbuf(1), 140)
  140      format(' No base data in residence')
           call prterx ('W', 1)
           goto 900
        else if (ostates .lt. 3) then
           if (numchg .gt. 0 .or. kspare(1) .eq. 1) then
              if (fltstr .eq. -1) then
                 if (nadd .gt. 0 .or. ndel .gt. 0) then
                    fltstr = 1
                 else
                    fltstr = 0
                 endif
              endif
           else
              fltstr = 0
           endif
           call btable(fltstr, .false.)
           call prtime('BUILD_TABLE')
           call osorto
C
C          Process any EPRI_1964 data.
C
           if (nepbus + nepctl .gt. 0) then
              call lodepr(zonex, numznx)
              call prtime('BUILD_EPRI_1964_TABLES')
           else
              numznx = 0
           endif
C
C          Zones declared in d-c are trasnferred to BLDTIE.
C
           call bldtie(zonex, numznx)
           call prtime('BUILD_AREA_ZONE_TABLES')
           inrcd = buf
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 3
           else
              if (chkerr('F') .eq. 0) ostates = 3
           endif
        endif
        if (ostates .eq. 3) then
           status = reordr()
           call prtime('re_order')
           if (status .ne. 0) go to 900
           call chgorder('inp2opt')             !  EXT2IN
           call prtime('chg_order : inp_to_opt')
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 4
           else
              if (chkerr('F') .eq. 0) ostates = 4
           endif
        else if (ostates .gt. 4) then
           call chgorder('inp2opt')             !  EXT2IN
           call prtime('chg_order : inp_to_opt')
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 4
           else
              if (chkerr('F') .eq. 0) ostates = 4
           endif
        endif
        if (ostates .gt. 3) then
           call solton
           call prtime('SOLUTION')
           if (lskp .eq. 1) then
              write (errbuf(1), 150) ittot
  150         format (' Successful solution in ', i2, ' iterations')
              call prterx ('I', 1)
           else
              write (errbuf(1), 160) ittot
  160         format (' Solution diverged in ', i2, ' iterations')
              call prterx ('I', 1)
           endif
           call chgorder('opt2inp')              !  INT2EX
           call prtime('CHG_order : OPT_to_INP')
           kspare(34) = 1
           inrcd = buf
           ierr_f = 1
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ierr_f = 0
           else
              if (chkerr('F') .eq. 0) ierr_f = 0
           endif
           if (ierr_f .eq. 0) then
              if (lskp .eq. 1) then
                 ostates = 5
              else
                 ostates = 7
              endif
c
c             Update pgen for Area, system slack buses
c
              if (iasw .gt. 0) then   
                do j = 1, ntotc
                  nb = karea(1,j) 
                  if (ordtie .eq. 2) nb = opt2inp(nb)
                  kt = inp2opt(nb)
                  pgen = (pnetu(kt) + ploadu(kt)) * bmva
                  busdta(8,nb) = pgen
                  ncb = kbsdta(15,nb)                                 
                  do while (ncb .gt. 0) 
                    pgen = pgen + bctbl(6,ncb)       
                    ncb = bctbl_nxt(ncb) 
                  enddo 
                  area(8,j) = dble(pgen)                
                enddo
              endif   
              do kt = 1, nbslck 
                nb = opt2inp(kt)
                pgen = (pnetu(kt) + ploadu(kt)) * bmva
                busdta(8,nb) = pgen
              enddo
C       
C             Update CAPCOT() for PV buses.   
C       
              do i = 1, ntot
                capcor(1, i) = 0.0
                capcor(2, i) = qnetu(i)
              enddo
              do ib = 1, ntot_alf
                nb = alf2inp(ib)
                kt = inp2opt(nb)
                if (kvolt(kt) .ne. 0) then   
                  qk = qnetu(kt) 
                  call allocq (nb, qk, qgen, qgnmax, qgnmin, qld, 
     &                         totcap, usecap, totrek, userek, unsked, 
     &                         qerr)
                  voltsq = e(kt) ** 2 + f(kt) ** 2
                  if (voltsq .eq. 0.0) voltsq = 1.0
                  used = (usecap + userek) / (voltsq * bmva)
                  total = (totcap + totrek) / (voltsq * bmva)
                  capcor(1, kt) = total - used
                endif
              enddo
              capord = 2
C       
C             Update BUSDTA(6,*) for BX buses.   
C       
              do jt = 1, kxtot
                nb = xdata(1,jt)
                if (nb .gt. 0) then
                  if (ordtbx .eq. 2) nb = opt2inp(nb)
                  kt = inp2opt(nb)
                  if (ntypu(kt) .eq. 11) then
                    skreak = xdata(3, jt)
                    skcap = xdata(4, jt)
                    userek = xdata(5, jt)
                    usecap = xdata(6, jt)
                    busdta(6, nb) = userek + usecap
                  endif
                endif
              enddo
C
C             Write user-specified report if requested.
C
              if (numusr .gt. 0) then

                ownflg = .false.  ! Set flag to force updating OWNLOS
                update(1) = 1     ! Set flag to force updating ZSUM
                ickzone = ckzoneup(status)   ! Determine whether it is 
C                                 ! necessary to update ZSUM after
C                                 ! each solution
C
C               Open OUTPUT files.  If none are explicitly defined,
C               create default names.
C
                tempc = nbasnm
                if (tempc .eq. ' ') tempc = obasnm
                if (tempc .eq. ' ') tempc = 'temporary'
                i = index (tempc, ']') + 1
                i = index (tempc(i:), ':') + i
                j = index (tempc(i:), '.') + i 
                if (j .eq. i) then
                  j = lastch (tempc)
                  tempc(j+1:) = '.'
                  j = j + 1
                endif
                do iuser = 1, numusr
                  if (usrfil(iuser) .eq. ' ') then
                    write (usrfil(iuser), 10170) tempc(1:j),
     &                'USER_REPORT_', iuser
10170               format (a, a, i2.2)
                  endif
                  lun(iuser) = 26
                  call close_file (lun(iuser))
                  status = open_apnd (lun(iuser), usrfil(iuser))
                  if (status .ne. 0) then
     
                    last = lastch(usrfil(iuser))
                    write (errbuf(1), 10180) ios, 
     &                 usrfil(iuser)(1:last), lun(iuser)
10180               format (' Error ', i3, ' opening file ', a,
     &                ' on logical unit ', i2)
                    call prterx ('W', 1)
                  endif
          
                  close(unit=lun(iuser))
                  numupx(iuser) = 0
                enddo

                call postsl
                update(1) = ickzone
                call updzon()
                call prtusr (lun, numupx)
              endif
           endif
           call loadarcv
        else
c
c          Solution rejected - advance control cards
c
           if (buf(1:1) .eq. '/') then
              read (inp, 161, end=162) buf   
  161         format (a)
              do while (buf(1:1) .ne. '/')
                 read (inp, 161, end=162) buf   
              enddo
              go to 164
  162         buf = '( end ) solution' 
  164         continue
           endif
        endif

  900   continue
        if ( ostates .eq. 5 ) isolton = 0  ! return SUCCESS state
        return
	end
