C    %W% %G%
      subroutine writem(isgg)

C     THIS SUBROUTINE FORMS AND PRINTS A STATEMENT FOR EACH MACHINE 
C     IN THE SWING PROGRAM WHICH DESCRIBES THE MACHINE REPRESENTATION.  
C     INPUT FOR THIS SUBROUTINE IS A LIST OF VARIABLES FORMED IN THE 
C     INPUT SUBROUTINE.  WRITE SELECTS PHRASES FROM FROM THE ARRAY WORDS 
C     AND USES STRMOV TO FORM A COMPLETE STATEMENT IN THE ARRAY ALF.  
C     ALF IS THEN PRINTED.

      include 'tspinc/blkcom1.inc'

      dimension ind(100), nch(100)
      character*110 alf
      include 'tspinc/prt.inc'
      include 'tspinc/lnk33n.inc'
      character*370 ww
      data ww(001:040) / '                **SIMPLE GEN.*NO INERTIA' /
      data ww(041:080) / '*IND. MACH**D,Q REP. CONST. FLUX*CONST. ' /
      data ww(081:120) / 'FIELD VOLT.*SATURATION*DAMPER WINDING*TR' /
      data ww(121:160) / 'ANSF.*FAULT DAMPING*EXCITER TYPE A*B*C*D' /
      data ww(161:200) / '*E*F*G*H*J*SPECIAL*GOVNR*GEN.*HYDRO*TAND' /
      data ww(201:240) / '. CMPD.*CROSS*NEG*POS COEF*SUPL.CONTROL-' /
      data ww(241:280) / 'S**F**P**K*  SINGLE REHEAT*DOUBLE REHEAT' /
      data ww(281:320) / '*HYDRAULIC*NON REHEAT* DMP WDG STATIC VA' /
      data ww(321:360) / 'R SOURCE  FA*FB*FC*FD*FE*FF*FG*FH*FJ*FK*' /
      data ww(361:370) / 'FL*       '/
      k1 = 1
      if (igt .eq. 2) then
        k1 = k1 + 1
        ind(k1) = 17
        nch(k1) = 25

C       SIMPLE GEN.-NO INERTIA

      elseif (igt .eq. 3) then
        k1 = k1 + 1
        ind(k1) = 41
        nch(k1) = 12
        goto 150

C       IND.MACH.

      elseif (igt .eq. 4 .or. igt .eq. 5) then
        k1 = k1 + 1
        ind(k1) = 51
        nch(k1) = 11

C       D,Q REP.

        if (igt .ge. 5) then
          k1 = k1 + 1
          ind(k1) = 303
          nch(k1) = 10
        endif

C       INITAL3 

        if (mdl .eq. 0) then
          k1 = k1 + 1
          ind(k1) = 62
          nch(k1) = 12
C         CONST. FLUX
        elseif (mdl .eq. 11) then
 
C         D,TR068SV01.114 FOR TRAPZ .112 FORTRAPMDC
 
          k1 = k1 + 1
          ind(k1) = 73
          nch(k1) = 20
        elseif (mdl .gt. 11) then
 
C         GENERREX EXCITER
 
          k1 = k1 + 1
          ind(k1) = 140
          nch(k1) = 14

C         EXCITER TYPE

          k1 = k1 + 1
          nch(k1) = 3
          if (mdl .eq. 22) ind(k1) = 331
          if (mdl .eq. 12) ind(k1) = 334
          if (mdl .eq. 13) ind(k1) = 337
          if (mdl .eq. 14) ind(k1) = 340
          if (mdl .eq. 15) ind(k1) = 343
          if (mdl .eq. 16) ind(k1) = 346
          if (mdl .eq. 17) ind(k1) = 349
          if (mdl .eq. 18) ind(k1) = 352
          if (mdl .eq. 19) ind(k1) = 355
          if (mdl .eq. 20) ind(k1) = 358
          if (mdl .eq. 21) ind(k1) = 361
        else
          k1 = k1 + 1
          ind(k1) = 140
          nch(k1) = 14

C         EXCITER TYPE

          k1 = k1 + 1
          nch(k1) = 2
          if (mdl .eq. 2) then
            ind(k1) = 156

C           B

          elseif (mdl .eq. 3) then
            ind(k1) = 158

C           C

          elseif (mdl .eq. 4) then
            ind(k1) = 160

C           D

          elseif (mdl .eq. 5) then
            ind(k1) = 162

C           E

          elseif (mdl .eq. 6) then
            ind(k1) = 164

C           F

          elseif (mdl .eq. 7) then
            ind(k1) = 166

C           G

          elseif (mdl .eq. 8) then
            ind(k1) = 168

C           H

          elseif (mdl .eq. 9) then
            ind(k1) = 170

C           J

          elseif (mdl .eq. 10) then
            ind(k1) = 250

C           K

          else
            ind(k1) = 154

C           A

          endif
        endif
      elseif (igt .eq. 6) then
        k1 = k1 + 1
        ind(k1) = 311
        nch(k1) = 20
        goto 140
      else
        k1 = k1 + 1
        ind(k1) = 17
        nch(k1) = 14

C       SIMPLE GEN.

      endif

C     CONST. FIELD VOLT.

      if (imfs .ne. 0) then
        k1 = k1 + 1
        ind(k1) = 92
        nch(k1) = 12
      endif

C     SATURATION

      if (ldamp .ne. 0) then
        k1 = k1 + 1
        ind(k1) = 103
        nch(k1) = 16
      endif

C     DAMPER WINDING

      if (iutrn .ne. 0) then
        k1 = k1 + 1
        ind(k1) = 118
        nch(k1) = 9
      endif

C     TRANSF.

      if (ifdam .ne. 0) then
        k1 = k1 + 1
        ind(k1) = 126
        nch(k1) = 15
      endif

C     FAULT DAMPING

      if (igov .ne. 0) then
        k1 = k1 + 1
        if (igov .eq. 2) then
          ind(k1) = 190
          nch(k1) = 7
        elseif (igov .eq. 3) then

C         HYDRO

          ind(k1) = 196
          nch(k1) = 6

C         TAND.

        else
          if (igov .ne. 4) then
            if (igov .ne. 5) then
              if (igov .eq. 6) then
                ind(k1) = 171
                nch(k1) = 8
              elseif (igov .ne. 7) then
                if (igov .eq. 8) then
                  ind(k1) = 291
                  nch(k1) = 12

C                 NON REHEAT

                  goto 120
                else
                  if (igov .eq. 9 .or. igov .eq. 10) then
                    ind(k1) = 254
                    nch(k1) = 14
                    k1 = k1 + 1

C                   SINGLE REHEAT

                    jgov = igov - 8
                    if (jgov .eq. 1) goto 110
                    if (jgov .eq. 2) goto 100
                    if (jgov .eq. 3 .or. jgov .eq. 4) goto 120
                  elseif (.not. (igov .eq. 11 .or. igov .eq. 12)) then
                    ind(k1) = 185
                    nch(k1) = 5

C                   GEN.

                    goto 120
                  endif
                  ind(k1) = 268
                  nch(k1) = 14
                  k1 = k1 + 1

C                 DOUBLE REHEAT

                  if (igov .eq. 11) goto 110
                  goto 100
                endif
              endif

C             SPECIAL

              ind(k1) = 281
              nch(k1) = 11

C             HYDRAULIC

              goto 120
            endif
  100       ind(k1) = 208
            nch(k1) = 6
            k1 = k1 + 1
            ind(k1) = 202
            nch(k1) = 6

C           CROSS CMPD.

            goto 120
          endif
  110     ind(k1) = 196
          nch(k1) = 12

C         TAND. CMPD.

        endif
  120   k1 = k1 + 1
        ind(k1) = 180
        nch(k1) = 6
      endif

C     GOVNR.

      if (.not. (ipd .eq. 0 .and. jnd .eq. 0)) then
        if (ipd .ne. 0) then
          k1 = k1 + 1
          ind(k1) = 218
          nch(k1) = 4
C         POS.
          if (jnd .eq. 0) goto 130
        endif
        k1 = k1 + 1
        ind(k1) = 214
        nch(k1) = 4

C       NEG.

  130   k1 = k1 + 1
        ind(k1) = 132
        nch(k1) = 9
      endif

C     DAMPING

      if (idc .ne. 0) then
        k1 = k1 + 1
        ind(k1) = 133
        nch(k1) = 7
        k1 = k1 + 1
        ind(k1) = 222
        nch(k1) = 6
      endif

C     DAMPING COEF.

  140 if (isupp .ne. 0) then
        k1 = k1 + 1
        ind(k1) = 227
        nch(k1) = 14

C       SUPL. CONTROL-

        k1 = k1 + 1
        nch(k1) = 3
        if (isupp .eq. 2) then
          ind(k1) = 244

C         F

        elseif (isupp .eq. 3) then
          ind(k1) = 247

C         P

        elseif (isupp .eq. 4) then
          ind(k1) = 172
          nch(k1) = 8
        else
          ind(k1) = 241

C         S

        endif
      endif

C     SPECIAL

  150 iwrsw = 0
      j1 = 1
      j = 2
      do while (.true.)
        alf = '             '
        do while (j1+nch(j) .le. 110)
          j2 = j1 + nch(j)
          nn = nch(j) - 1
          alf(j1:j1+nn) = ww(ind(j):ind(j)+nn)
          j1 = j2
          j = j + 1
          if (j .gt. k1 .and. iwrsw .eq. 0) goto 160
          if (j .gt. k1 .and. iwrsw .eq. 1) goto 170
        enddo
  160   write (outbuf, 10000) isgg, nbnamo, bkvo, ido, alf(:j2-1)
10000   format (' ', i4, 1x, a8, f6.1, 1x, a1, a)
        call prtout(1)
        iwrsw = 1
        if (j .gt. k1) goto 180
        j1 = 5
      enddo
  170 write (outbuf, 10010) alf
      call prtout(1)
10010 format (' ', a)
  180 if (k1 .eq. 1) then
        write (errbuf(1), 10020)
10020   format (' NO REPRESENTATION DATA ENTERED FOR THIS MACHINE')
        call prterr('E', 1)
      endif
      return
      end
