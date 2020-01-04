C    %W% %G%
      subroutine ssort
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/search.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/lnk12a.inc'
      include 'tspinc/link2.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/lnk1c.inc'
      include 'tspinc/lnk2c.inc'
      include 'tspinc/lnkcd.inc'
      include 'tspinc/rk.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/param1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/equiv.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/ijrly.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/ijfltn.inc'

      dimension istem(MAXLS)
      equivalence (istem, isorti)
      character*1 npc, i1c

      i2 = 1
      i = 1
      do while (i .le. ist)

C       TEST FOR TYPE SWITCHING
        i1 = istem(i)
        if (i1 .lt. 0) then

C         RELAY LINE SWITCHING
          ir =  - i1
          ibr = ijrlyn(1, ir)
          jbr = ijrlyn(2, ir)
          npn = ijrlyn(3, ir)
          npc = ijrlyc(ir)
        else

C         SCHEDULED LINE SWITCHING
          ibr = iabs(iftab(i1))
          jbr = iabs(jftab(i1))
          npc = ipcdtc(i1)
          npn = iabs(ipcdtn(i1))
          mde = iabs(mflt(i1))
          isw55 = mde + 1
          if (mde .eq. 6) then

C           FOR MULTI DC SWITCH CARDS WE ARE ADDING 2000+I1 TO EACH
C           TO HELP THEM SORT PROPERLY BY TAKING THEM OUT OF THE POW
C           BUS NUMBER RANGE
            ibr = ibr + MAXBUS + i1
            jbr = jbr + 2*MAXBUS + i1
          endif
          if (.not. ((isw55 .ge. 1 .and. isw55 .le. 3) .or. isw55 .eq.
     &     6 .or. isw55 .eq. 7)) then
            if (isw55 .eq. 4) then
              isort2 = 1

C             FLTBUS ORDER OPTION
              if (ifltsw .eq. 2) then
                j2 = 1
              else
                j2 = nmx
              endif
              goto 100
            else
              isort2 = 2
              j2 = 0
              j1 = ibr
              npn = 0
              npc = ' '
              goto 110
            endif
          endif
        endif
 
        isort2 = 2
        j2 = ibr
  100   j1 = jbr
        isortn(1, i2) = j1
        isortn(2, i2) = j2
        isortn(3, i2) = npn
        isortc(i2) = npc
        locst(i2) = i1
        ibcd(i2) = 2
        i2 = i2 + 1
  110   isortn(1, i2) = j2
        isortn(2, i2) = j1
        isortn(3, i2) = npn
        isortc(i2) = npc
        locst(i2) = i1
        ibcd(i2) = 1
        i2 = i2 + 1
        if (isort2 .ne. 2) then
          j1 = ibr
          isortn(1, i2) = j1
          isortn(2, i2) = j2
          isortn(3, i2) = npn
          isortc(i2) = npc
          locst(i2) = i1
          ibcd(i2) = 2
          i2 = i2 + 1
          isortn(1, i2) = j2
          isortn(2, i2) = j1
          isortn(3, i2) = npn
          isortc(i2) = npc
          locst(i2) = i1
          ibcd(i2) = 1
          i2 = i2 + 1
        endif
        i = i + 1
        if (iijj1 .ne. 2) then
          iijj1 = 2
          ijft1n = isortn(1, 1)
          ijft2n = isortn(2, 1)
          ijft3n = isortn(3, 1)
          ijfltc = isortc(1)

C         MODIFY FLT BR IDENT. FOR IFLTSW=2 OPTION
          if (ifltsw .ne. 1) then
            if (ijft1n .gt. ijft2n) then
              ijtemp = ijft1n
              ijft1n = ijft2n
              ijft2n = ijtemp
            endif
          endif
        endif
      enddo

C     SORT LINE SWITCHING TABLE
      iss = i2 - 1
      lim1 = i2 - 2
      i3 = 0
      do i = 1, iss
        isorti(i) = i
      enddo
      do while (.true.)
        i = 1
        i3 = 0
        do while (i .le. lim1)
          if (isortn(1, i) .ge. isortn(1, i+1)) then
            if (isortn(1, i) .le. isortn(1, i+1)) then
              if (isortn(2, i) .lt. isortn(2, i+1)) goto 120
              if (isortn(2, i) .le. isortn(2, i+1)) then
                if (isortn(3, i) .le. isortn(3, i+1)) goto 120
              endif
            endif
            i1n1 = isortn(1, i)
            i1n2 = isortn(2, i)
            i1n3 = isortn(3, i)
            i1c = isortc(i)
            i2 = isorti(i)
            isortn(1, i) = isortn(1, i+1)
            isortn(2, i) = isortn(2, i+1)
            isortn(3, i) = isortn(3, i+1)
            isortc(i) = isortc(i+1)
            isorti(i) = isorti(i+1)
            isortn(1, i+1) = i1n1
            isortn(2, i+1) = i1n2
            isortn(3, i+1) = i1n3
            isortc(i+1) = i1c
            isorti(i+1) = i2
            i3 = i - 1
          endif
  120     i = i + 1
        enddo
        if (i3 .eq. 0) goto 130
        lim1 = i3
      enddo
  130 return
      end
