C    @(#)epclup.f	20.3 2/13/96
      subroutine epclup
C
C     This subroutine cleans up (cosolidates changes) of / EPRI_1964
C     data (EPRI Cleanup).
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/epridc.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      integer bdfld(2,14), bzfld(2,12), ldfld(2,7), md1fld(2,10),
     1        md2fld(2,13), ccfld(2,6), cdfld(2,12), crfld(2,10),
     2        cnfld(2,8)
 
      integer count, delete
      external kmpepb, swpepb, kmpepc, swpepc
 
      data    bdfld / 20, 27, 28, 31, 33, 35, 36, 37, 38, 39, 40, 44,
     1                45, 49, 50, 54, 55, 59, 60, 64, 65, 68, 69, 72,
     2                73, 76, 77, 80 /
      data    bzfld / 20, 27, 28, 31, 33, 36, 37, 42, 43, 48, 49, 49,
     1                50, 54, 55, 59, 60, 65, 66, 71, 72, 77, 78, 80 /
      data    ldfld / 20, 23, 24, 25, 26, 28, 29, 34, 35, 40, 41, 46,
     1                47, 52 /
      data   md1fld /  5,  7,  8, 13, 14, 19, 20, 23, 24, 27, 28, 31,
     1                32, 35, 36, 39, 40, 43, 44, 47 /
      data   md2fld /  5,  8,  9, 12, 13, 16, 17, 20, 21, 24, 25, 28,
     1                29, 32, 33, 36, 37, 40, 41, 44, 45, 48, 49, 58,
     2                59, 68 /
      data    ccfld / 37, 42, 43, 44, 45, 49, 50, 54, 55, 62, 63, 70 /
 
      data    cdfld / 35, 36, 37, 42, 43, 45, 46, 51, 52, 54, 55, 56,
     1                57, 62, 63, 68, 69, 71, 72, 73, 74, 79, 80, 80 /
 
      data    crfld / 35, 38, 39, 40, 41, 42, 43, 48, 49, 51, 52, 53,
     1                54, 59, 60, 67, 68, 71, 72, 72 /
 
      data    cnfld /  6, 10, 11, 15, 16, 20, 21, 25, 26, 35, 36, 45,
     1                46, 55, 56, 65 /
 
C
C     Cleanup any existing EPRI_1964 records.
C
      if (nepbus .gt. 0) then
C
C        Sort according to the following fields:
C
C           Code      (1:2) < 'MD' and Type (4:4)
C           D-C bus 1 (4:11)
C           D-C bus 2 (12:19)
C           Circuit   (32:32) or (20:20)
C
         call qiksrt (1, nepbus, kmpepb, swpepb)
         do 200 i = 1, nepbus
 
            if (epbus(i)(1:2) .eq. 'BD' .or.
     1          epbus(i)(1:2) .eq. 'BZ' ) then
C
C               Check BD and BZ records.
C
               if (epbus(i)(3:3) .ne. ' ') then
                  write (errbuf(1), 110)
  110             format (' Only change data pertains to ',
     1                    'following record:')
                  write (errbuf(2), 120) epbus(i)(1:80)
  120             format (' (', a, ')')
                  call prterx ('W', 2)
                  epbus(i)(3:3) = ' '
               endif
               do 150 j = i+1, nepbus
                  if (epbus(i)(1:2)   .eq. epbus(j)(1:2)   .and.
     1                epbus(i)(4:11)  .eq. epbus(j)(4:11)  .and.
     2                epbus(i)(12:19) .eq. epbus(j)(12:19) .and.
     3                epbus(i)(32:32) .eq. epbus(j)(32:32)) then
                     if (epbus(j)(3:3) .eq. 'D') then
                        epbus(i)(3:3) = 'D'
                        epbus(j) = ' '
                     else if (epbus(j)(3:3) .eq. 'M') then
                        if (epbus(i)(1:2) .eq. 'BD') then
                           do 130 k = 1, 14
                              j1 = bdfld(1,k)
                              j2 = bdfld(2,k)
                              if (epbus(j)(j1:j2) .ne. ' ') then
                                 epbus(i)(j1:j2) = epbus(j)(j1:j2)
                              endif
  130                      continue
                           epbus(j) = ' '
                        else if (epbus(i)(1:2) .eq. 'BZ') then
                           do 140 k = 1, 12
                              j1 = bzfld(1,k)
                              j2 = bzfld(2,k)
                              if (epbus(j)(j1:j2) .ne. ' ') then
                                 epbus(i)(j1:j2) = epbus(j)(j1:j2)
                              endif
  140                      continue
                           epbus(j) = ' '
                        endif
                     endif
                  else
                     go to 200
                  endif
  150          continue
 
            else if (epbus(i)(1:2) .eq. 'LD') then
C
C              Check LD records.
C
               if (epbus(i)(3:3) .ne. ' ') then
                  write (errbuf(1), 110)
                  write (errbuf(2), 120) epbus(i)(1:80)
                  call prterx ('W', 2)
                  epbus(i)(3:3) = ' '
               endif
               do 170 j = i+1, nepbus
                  if (epbus(i)(1:2)   .eq. epbus(j)(1:2)   .and.
     1                epbus(i)(4:11)  .eq. epbus(j)(4:11)  .and.
     2                epbus(i)(12:19) .eq. epbus(j)(12:19) .and.
     3                epbus(i)(20:20) .eq. epbus(j)(20:20)) then
                     if (epbus(j)(3:3) .eq. 'D') then
                        epbus(i)(3:3) = 'D'
                        epbus(j) = ' '
                     else if (epbus(j)(3:3) .eq. 'M') then
                        do 160 k = 1, 7
                           j1 = ldfld(1,k)
                           j2 = ldfld(2,k)
                           if (epbus(j)(j1:j2) .ne. ' ') then
                              epbus(i)(j1:j2) = epbus(j)(j1:j2)
                           endif
  160                   continue
                        epbus(j) = ' '
                     endif
                  else
                     go to 200
                  endif
  170          continue
 
            else if (epbus(i)(1:2) .eq. 'MD') then
C
C           Check MD records.
C
               if (epbus(i)(3:3) .ne. ' ') then
                  write (errbuf(1), 110)
                  write (errbuf(2), 120) epbus(i)(1:80)
                  call prterx ('W', 2)
                  epbus(i)(3:3) = ' '
               endif
               do 190 j = i+1, nepbus
                  if (epbus(i)(1:2)   .eq. epbus(j)(1:2) .and.
     1                epbus(i)(4:4)   .eq. epbus(j)(4:4)) then
                     if (epbus(j)(3:3) .eq. 'D') then
                        epbus(i)(3:3) = 'D'
                        epbus(j) = ' '
                     else if (epbus(j)(3:3) .eq. 'M') then
                        if (epbus(i)(4:4) .eq. 'F' .or.
     1                      epbus(i)(4:4) .eq. 'P') then
                           do 180 k = 1, 10
C
C                             Modifying MD Type F or P record.
C
                              j1 = md1fld(1,k)
                              j2 = md1fld(2,k)
                              if (epbus(j)(j1:j2) .ne. ' ') then
                                 epbus(i)(j1:j2) = epbus(j)(j1:j2)
                              endif
  180                      continue
                           epbus(j) = ' '
                        else if (epbus(i)(4:4) .eq. 'D') then
C
C                          Modifying MD Type D record.
C
                           do 182 k = 1, 13
                              j1 = md2fld(1,k)
                              j2 = md2fld(2,k)
                              if (epbus(j)(j1:j2) .ne. ' ') then
                                 epbus(i)(j1:j2) = epbus(j)(j1:j2)
                              endif
  182                      continue
                           epbus(j) = ' '
                        endif
                     else
                        go to 200
                     endif
                  else
                     go to 200
                  endif
  190          continue
            endif
 
  200    continue
C
C        Compress list
C
         count = 0
         do 210 i = 1, nepbus
            if (epbus(i) .ne. ' ' .and. epbus(i)(3:3) .eq. ' ') then
               count = count + 1
               epbus(count) = epbus(i)
            endif
  210    continue
         delete = nepbus - count
         nepbus = count
 
         write (outbuf, 220) count, delete
  220    format (' Compressing d-c array EPBUS results in ', i3,
     1      ' entities and ', i3, ' deletions ')
         call prtout(1)
      endif
 
      if (nepctl .gt. 0) then
C
C        Sort according to the following fields:
C
C                      CC        CD, CR      CN
C
C           D-C bus 1 (8:15)     (6:13)      (81:88)
C           D-C bus 2 (16:23)    (14:21)     (89:96)
C           A-C bus   (24:35)    (22:33)     (97:108)
C           Circuit   (36:36)    (34:34)     (109:109)
C           Ident     (4:5)      (4:5)       (4:5)
C           Code      (1:2)      (1;2)       (1:2)
C
         call qiksrt (1, nepctl, kmpepc, swpepc)
         do 1200 i = 1, nepctl
 
            if (epctl(i)(1:2) .eq. 'CC') then
C
C              Check CC records.
C
               if (epctl(i)(3:3) .ne. ' ') then
                  write (errbuf(1), 110)
                  write (errbuf(2), 120) epctl(i)(1:80)
                  call prterx ('W', 2)
                  epctl(i)(3:3) = ' '
               endif
               do 1140 j = i+1, nepctl
                  if (epctl(i)(1:2)   .eq. epctl(j)(1:2)   .and.
     1                epctl(i)(8:15)  .eq. epctl(j)(8:15)  .and.
     2                epctl(i)(16:23) .eq. epctl(j)(16:23) .and.
     3                epctl(i)(24:35) .eq. epctl(j)(24:35) .and.
     4                epctl(i)(36:36) .eq. epctl(j)(36:36)  .and.
     5                epctl(i)(4:5)   .eq. epctl(j)(4:5)) then
                     if (epctl(j)(3:3) .eq. 'D') then
                        epctl(i)(3:3) = 'D'
                        epctl(j) = ' '
                     else if (epctl(j)(3:3) .eq. 'M') then
                        do 1130 k = 1, 6
                           j1 = ccfld(1,k)
                           j2 = ccfld(2,k)
                           if (epctl(j)(j1:j2) .ne. ' ') then
                              epctl(i)(j1:j2) = epctl(j)(j1:j2)
                           endif
 1130                   continue
                        epctl(j) = ' '
                     endif
                  else
                     go to 1200
                  endif
 1140          continue
 
            else if (epctl(i)(1:2) .eq. 'CD' .or.
     1               epctl(i)(1:2) .eq. 'CR') then
C
C              Check CD, or CR records.
C
               if (epctl(i)(3:3) .ne. ' ') then
                  write (errbuf(1), 110)
                  write (errbuf(2), 120) epctl(i)(1:80)
                  call prterx ('W', 2)
                  epctl(i)(3:3) = ' '
               endif
               do 1150 j = i+1, nepctl
                  if (epctl(i)(1:2)   .eq. epctl(j)(1:2)   .and.
     1                epctl(i)(6:13)  .eq. epctl(j)(6:13)  .and.
     2                epctl(i)(14:21) .eq. epctl(j)(14:21) .and.
     3                epctl(i)(22:33) .eq. epctl(j)(22:33) .and.
     4                epctl(i)(34:34) .eq. epctl(j)(34:34) .and.
     5                epctl(i)(4:5)   .eq. epctl(j)(4:5)) then
                     if (epctl(j)(3:3) .eq. 'D') then
                        epctl(i)(3:3) = 'D'
                        epctl(j) = ' '
                     else if (epctl(j)(3:3) .eq. 'M') then
                        if (epctl(i)(1:2) .eq. 'CD') then
                           do 1142 k = 1, 12
                              j1 = cdfld(1,k)
                              j2 = cdfld(2,k)
                              if (epctl(j)(j1:j2) .ne. ' ') then
                                 epctl(i)(j1:j2) = epctl(j)(j1:j2)
                              endif
 1142                      continue
                           epctl(j) = ' '
                        else if (epctl(i)(1:2) .eq. 'CR') then
                           do 1144k = 1, 10
                              j1 = crfld(1,k)
                              j2 = crfld(2,k)
                              if (epctl(j)(j1:j2) .ne. ' ') then
                                 epctl(i)(j1:j2) = epctl(j)(j1:j2)
                              endif
 1144                      continue
                           epctl(j) = ' '
                        endif
                     endif
                  else
                     go to 1200
                  endif
 1150          continue
 
            else if (epctl(i)(1:2) .eq. 'CN') then
C
C              Check CN records.
C
               if (epctl(i)(3:3) .ne. ' ') then
                  write (errbuf(1), 110)
                  write (errbuf(2), 120) epctl(i)(1:80)
                  call prterx ('W', 2)
                  epctl(i)(3:3) = ' '
               endif
               do 1170 j = i+1, nepctl
                  if (epctl(i)(1:2)     .eq. epctl(j)(1:2)     .and.
     1                epctl(i)(81:88)   .eq. epctl(j)(81:88)   .and.
     2                epctl(i)(89:96)   .eq. epctl(j)(89:96)   .and.
     3                epctl(i)(97:108)  .eq. epctl(j)(97:108)  .and.
     4                epctl(i)(109:109) .eq. epctl(j)(109:109) .and.
     5                epctl(i)(4:5)   .eq. epctl(j)(4:5)) then
                     if (epctl(j)(3:3) .eq. 'D') then
                        epctl(i)(3:3) = 'D'
                        epctl(j) = ' '
                     else if (epctl(j)(3:3) .eq. 'M') then
                        do 1160 k = 1, 8
                           j1 = cnfld(1,k)
                           j2 = cnfld(2,k)
                           if (epctl(j)(j1:j2) .ne. ' ') then
                              epctl(i)(j1:j2) = epctl(j)(j1:j2)
                           endif
 1160                   continue
                        epctl(j) = ' '
                     endif
                  else
                     go to 1200
                  endif
 
 1170          continue
 
            endif
 
 1200    continue
C
C        Compress list
C
         count = 0
         do 1210 i = 1, nepctl
            if (epctl(i) .ne. ' ' .and. epctl(i)(3:3) .eq. ' ') then
               count = count + 1
               epctl(count) = epctl(i)
            endif
 1210    continue
         delete = nepctl - count
         nepctl = count
 
         write (outbuf, 1220) count, delete
 1220    format (' Compressing d-c array EPCTL results in ', i3,
     1      ' entities and ', i3, ' deletions ')
         call prtout(1)
 
      endif
      return
      end
