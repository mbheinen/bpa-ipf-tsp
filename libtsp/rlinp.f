C    %W% %G%
      subroutine rlinp
C     
C     THIS SUBROUTINE FORMS THE ROVX TABLES FOR THE DEFAULT DISTAN
C     RELAY MODEL.  ROVX CONTAINS THE R TO X RATIO FOR EACH BASE
C     BASE KV IN THE STUDY.
C     
      include 'tspinc/params.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/ffcard.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/dfalt.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/in1n.inc'

      common /lqiks/lqiks

      external komp31, swap31

C     
C     CHECK IF THE RL CARD IS MISSING
C     
      iswbps = 0
      if (ndfltd .eq. 0) then
        write (errbuf(1), 10000)
        call prterr('W', 1)
10000   format ('0', ' RL CARD IS MISSING.  DEFAULT DISTANCE RELAYING '
     &   , 'WILL BE IGNORED.')
      else
        iswbps = 1
C       
C       CHECK IF ROVX DATA SUPPLIED. IF NOT IT IS AN ERROR
C       
        if (nrlv .le. 0) then
          iabort = 1
          write (errbuf(1), 10010)
          call prterr('E', 1)
10010     format ('0',
     &     ' ERROR - ROVX DATA (RL V CARD) NOT SUPPLIED.')
        else
          lqiks = 20
          call qiksrt(1, nrlv, komp31, swap31)
          lim = nrlv - 1
          do i = 1, lim
            j = i + 1
            if (reappx(j, 2) .le. reappx(i, 3)) then
              write (errbuf(1), 10020) i, j
              call prterr('E', 1)
10020         format (1x, 'LOOK AT VOLTAGES IN ROWS ', i2, ' AND ', i2,
     &         ' IN THE TABLE BELOW.')
              iabort = 1
              do k = i, j
                write (outbuf, 10030) k, (reappx(k, n), n = 1, 3)
                call prtout(1)
              enddo
10030         format (1x, i10, 3f15.5)
            endif
          enddo
          do k = 1, nrlv
            write (outbuf, 10030) k, (reappx(k, i), i = 1, 3)
            call prtout(1)
          enddo
C         
C         CHECKING TO ENSURE THAT ROVX VALUES DECRESE WITH INCREAS
C         
          krovx = 0
          nrlvm = nrlv - 1
          if (nrlvm .ne. 0) then
            do i = 1, nrlvm
              if (reappx(i+1, 1) .gt. reappx(i, 1)) then
                iabort = 1
                krovx = 1
              endif
            enddo
          endif
          if (krovx .ne. 0) then
            write (errbuf(1), 10040)
            call prterr('E', 1)
10040       format ('0',
     &       ' ERROR - R OVER X VALUES MUST NOT INCREASE ',
     &       'WITH INCREASING RATED KV.  CHECK YOUR RL V CARDS.')
          endif
C         
C         FORMING ROVX( ) TABLE
C         DEFINE LOW AND HIGH VALUES OF CKX TO BE USED IF NECESSAR
C         
          cklo = reappx(nrlv, 1)
          ckhi = reappx(1, 1)
          k = 1
          rxfrst = reappx(1, 2)
          rxlast = reappx(nrlv, 3)
          do i = 1, ibxyz
            base = basekv(i)
            if (base .lt. rxfrst) then
C             
C             ASSIGN HIGH VALUE OF CKX FOR THIS BASEKV THAT IS LO
C             INPUT DATA BASEKV ON THE RL V CARDS
C             
              rovx(i) = ckhi
            elseif (base .gt. rxlast) then
C             
C             ASSIGN LOW VALUE OF CKX FOR THIS BASEKV THAT IS HI
C             INPUT DATA BASEKV ON THE RL V CARDS
C             
              rovx(i) = cklo
            else
              do while (.true.)
                rx2 = reappx(k, 2)
                rx3 = reappx(k, 3)
                if (base .ge. rx2 .and. base .le. rx3) goto 100
                rx4 = reappx(k+1, 2)
                if (base .gt. rx3 .and. base .lt. rx4) goto 110
                k = k + 1
              enddo
  100         rovx(i) = reappx(k, 1)
              goto 120
  110         xmdpnt = (rx3+rx4)*0.5
              rovx(i) = reappx(k, 1)
              if (base .gt. xmdpnt) rovx(i) = reappx(k+1, 1)
            endif
  120       continue
          enddo
C         
C         DEFINE ROVX FOR 200 KV AND STORE IN CK230
C         
          if ((basekv(1)-200.) .gt. 0.01) then
            ck230 = 1.001*rovx(1)
          elseif ((200.-basekv(ibxyz)) .gt. 0.01) then
            ck230 = 0.999*rovx(ibxyz)
          else
            do i = 1, ibxyz
              base = basekv(i)
              if (abs(base-200.) .lt. 0.01) then
                if (i .eq. 1) goto 140
              elseif (base .lt. 200.) then
                goto 130
              endif
              if (rovx(i) .lt. rovx(i-1)) goto 140
              do j = i, ibxyz
                rovx(j) = rovx(j)*0.999
              enddo
  130         continue
            enddo
  140       ck230 = rovx(i)
          endif
          if (noprnt .ne. 0) then
            write (outbuf, 10050)
            call prtout(1)
10050       format ('0', 17x, 'I', 7x, 'BASEKV', 9x, 'ROVX')
            do i = 1, ibxyz
              write (outbuf, 10060) i, basekv(i), rovx(i)
              call prtout(1)
            enddo
10060       format (1x, ' TABLES ', i10, 5x, f8.2, 5x, f8.2)
10070       format (1x, ' RELAY BYPASS       ', 1x, i10, 5x, a10, 5x,
     &       a10)
          endif
        endif
      endif
      return
      end
