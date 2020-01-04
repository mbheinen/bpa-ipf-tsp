C    %W% %G%
      subroutine lsrch
 
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/matrow.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/search.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/ijrly.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/bname.inc'
 
      common /ipcdn/ipcdn
      common /ipcdc/ipcdc
      character*1 ipcdc

C     FIND IN SWITCH LINE TABLE
      ifl = lst

C     SET PROPER BUS NO. FOR MDE=3 COND.
      if (iabs(mflt(ils)) .eq. 3) then
        nmxt = nmx
        if (ifltsw .eq. 2) nmxt = 1
      endif

C     Start loop for each LS card about to take effect
      do while (ifl .le. iline)

C       Forward orientation
        if (ijsln(1, ifl) .eq. iba) then
          if (ijsln(2, ifl) .eq. jba) then
            if (ijsln(3, ifl) .eq. iabs(ipcdn)) then
              if (ijslc(ifl) .eq. ipcdc) goto 100
            endif
          endif
        endif

C       Reverse orientation
        if (ijsln(1, ifl) .eq. jba) then
          if (ijsln(2, ifl) .eq. iba) then
            if (ijsln(3, ifl) .eq. iabs(ipcdn)) then
              if (ijslc(ifl) .eq. ipcdc) goto 110
            endif
          endif
        endif
        ifl = ifl + 1
      enddo

C     Line not found
C     If load or gen mod then tell MATMOD to modify diagonal
      if (mde .eq. 4) then
        idi = idelh
        return
      endif

C     Here if line not found
      write (errbuf(1), '(2a)') 'LSRCH [f] Couldn''t find switching ',
     & 'card to modify branch:'
      write (errbuf(2), '(4a,a1,i1)') bname(iba), ' to ', bname(jba),
     & ': ', ipcdc, ipcdn
      write (errbuf(3), 10000) iba, jba, ipcdc, ipcdn
10000 format ('  Subroutine LSRCH ', 2i10, 2x, a1, 2x, i5)
      call prterr('E', 3)
      do i = lst, iline
        write (outbuf, 10010) gijs(i), bijs(i), (ijsln(j, i), j = 1, 3)
     &   , ijslc(i)
10010   format (' ', 2f10.3, 3(1x, i5), 4x, a1)
        call prtout(1)
      enddo
      write (outbuf, 10020) lst, iline
10020 format ('0', ' lst, iline = ', i5, 5x, i5)
      call prtout(1)
      call erexit()
      goto 120

C     End loop for each LS card
C     Line found in forward direction
  100 ifi = ifl + ifl - 1
      ifj = ifi + 1
      goto 120

C     Line found in reverse direction
  110 ifi = ifl + ifl
      ifj = ifi - 1
  120 if (iba .gt. jba) then
        lsrch2 = 2
        if (iabs(mflt(ils)) .ne. 3) then
C         Jump for fault mode != 3
          goto 130
        endif
      else
        lsrch2 = 1
C       MODIFY FOR MODE 3 CONDITION  (along line)
        if (iabs(mflt(ils)) .ne. 3) goto 130
      endif
      if (iabs(iftab(ils)) .eq. iba) ib = iftab(ils)
      if (iabs(jftab(ils)) .eq. iba) ib = jftab(ils)
      if (iabs(iftab(ils)) .eq. jba) jb = iftab(ils)
      if (iabs(jftab(ils)) .eq. jba) jb = jftab(ils)

C     Assign faulted bus number
      if (nmxt .eq. iba) ib = nmxt
      if (nmxt .eq. jba) jb = nmxt
      goto 140

C     Here if not a mode 3 fault
  130 if (iba .eq. iabs(iftab(ils))) then
        ib = iftab(ils)
        jb = jftab(ils)
      else
        ib = jftab(ils)
        jb = iftab(ils)
      endif

C     LOCATE LINE POSITION
C     Here if bus numbers IB and JB determined
C     Determine off-diagonal entry to be modified
  140 if (lsrch2 .eq. 2) goto 160
C     here if JBA > IBA
      i1 = idelh
      do i = 1, i1
        if (jba .eq. loch(i)) go to170
      enddo
      goto 150

C     here if JBA < IBA
  160 i1 = idell
      do i = 1, i1
        if (jba .eq. locl(i)) goto 170
      enddo

  150 target = 0.0
      write (errbuf(1), '(2a)') 'LSRCH [f] Couldn''t find off-diagonal'
     & , ' in matrix for switched line: '
      write (errbuf(2), '(4a,a1,i1)') bname(iba), ' to ', bname(jba),
     & ': ', ipcdc, ipcdn
      call prterr('E', 2)
      call erexit()

  170 ijl = i
      idi = idelh
      return
      end
