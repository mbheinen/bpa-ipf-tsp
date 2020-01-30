C    @(#)merge_diff.f	20.3 2/13/96
        subroutine merge_diff
 
C       Program to merge branch data file with change data file

        implicit none
 
        common /difference/ numtxt1, array1, text1, numtxt2, array2, 
     &                      text2, numchg, change

        integer MAXTEXT, MAXCHG
        parameter (MAXTEXT = 10000)
        parameter (MAXCHG = 4000)

        character text1(MAXTEXT)*120, text2(MAXTEXT)*120,
     &            change(MAXCHG)*120
        integer array1(MAXTEXT), numtxt1, array2(MAXTEXT), numtxt2, 
     &          numchg

        integer ix, jx, inext, jnext, f1, l1, f2, l2, isw, komp,
     &          komp_text, ic, jc
        character errmsg*80

        if (numtxt1 .gt. MAXTEXT .or. 
     &      numtxt2 .gt. MAXTEXT) then
           write (errmsg,770) numtxt2, MAXTEXT
  770      format ('* SIZE OF EDITED FILE EXCEEDS LIMITS - ',i5,' ACTUAL
     1 limit ',i5)
           write (*, 99) errmsg
   99      format (1x, a)
           return
        endif

        errmsg = '* Comparing text1 file with text2 file'
        write (*, 99) errmsg

        numchg = 0
        f1 = 1
        l1 = numtxt1
        f2 = 1
        l2 = numtxt2
        assign 360 to inext
        assign 390 to jnext
        isw = 1
        ix = f1 - 1
        jx = f2 - 1
C
C       Advance base file pointer
C
  330   if (ix .lt. l1) then
           ix = ix + 1
           ic = array1(ix)
        else
           if (isw .eq. 1 .or. isw .eq. 3) isw = isw + 1
        endif
        go to inext
C
C       Advance text2 file pointer
C
  360   if (jx .lt. l2) then
           jx = jx + 1
           jc = array2(jx)
        else
           if (isw .eq. 1 .or. isw .eq. 2) isw = isw + 2
           assign 390 to jnext
        endif
        go to jnext
  390   go to (400, 460, 450, 500) isw
C
C       Compare items
C
  400   komp = komp_text(text1(ic),text2(jc))
        if (komp .lt. 0) then
C
C          text1 data appears as "deletions"
C
           numchg = numchg + 1
           change(numchg) = text1(ic)
           change(numchg)(3:3) = 'D'
           assign 390 to inext
           go to 330

        else if (komp .gt. 0) then
C
C          text2 data appears as "additions"
C
           numchg = numchg + 1
           change(numchg) = text2(jc)
           assign 390 to jnext
           go to 360

        else
C
C          Flag old and new records with special "?" change code
C          to let the actual differences be processed in IPF (module
C          getdif).
C
           if (text1(ic) .ne. text2(jc)) then
              numchg = numchg + 1
              change(numchg) = text1(ic)
              change(numchg)(3:3) = '?'
              numchg = numchg + 1
              change(numchg) = text2(jc)
              change(numchg)(3:3) = '?'
           endif

           assign 360 to inext
           assign 390 to jnext
           go to 330

        endif
C
C       Add text1 data to change
C
  450   numchg = numchg + 1
        change(numchg) = text1(ic)
        assign 390 to inext
        go to 330
C
C       Add text2 data to change
C
  460   numchg = numchg + 1
        change(numchg) = text2(jc)
        assign 390 to jnext
        go to 360
C
C       Finished
C
  500   continue
        return
        end
