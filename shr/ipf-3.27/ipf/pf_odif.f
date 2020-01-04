C    @(#)pf_odif.f	20.6 1/7/99
      subroutine pf_odif(in_buffer, out_buffer, error)

      include 'ipfinc/parametr.inc'
      include 'ipfinc/postdta.inc'

****************** debug stuff **********************
      character in_buffer*(*), out_buffer*(*), 
     &          fg_buf*(MAXBUFFER), bg_buf*(MAXBUFFER)
****************** debug stuff **********************

      character type*1,  null*1, linefeed*1, typfg*1, bupffg*8, 
     &          bupf1fg*8, bupf2fg*8, typbg*1, bupfbg*8, bupf1bg*8, 
     &          bupf2bg*8, acktcnt(3)*7, 
     &          adata(2,3)*1 ! arrays for difference
      dimension fdata(16,3), idata(3) ! array(n,3)=array(n,1)-array(n,2)
      dimension mvadata(2,3)

*** BUS use of fdata *******
*       fdata( 1) = P-gen
*       fdata( 2) = Q-gen
*       fdata( 3) = Voltage - kv
*       fdata( 4) = Q-outngle - degrees
*       fdata( 5) = P-load
*       fdata( 6) = Q-load
*       fdata( 7) = B-shunt used
*       fdata( 8) = B-shunt scheduled
*** BUS use of adata *******
*       adata( 1) = "+"/"-" for  capacitor/reactor symbol 
*** BRANCH use of fdata *******
*       fdata( 1) = P-in
*       fdata( 2) = Q-in
*       fdata( 3) = P-out
*       fdata( 4) = Q-out
*       fdata( 5) = P-loss
*       fdata( 6) = Q-loss
*       fdata( 7) = Critical Line Loading (amps) 
*       fdata( 8) = Critical Line Rating (amps) 
*       fdata( 9) = Critical Transformer Loading (MVA) 
*       fdata(10) = Critical Transformer Rating (MVA) 
*       fdata(11) = Total Line Loading  (%)
*       fdata(12) = Total Line Loading  (amps)
*       fdata(13) = Total Transformer Loading  (%)
*       fdata(14) = Total Transformer Loading  (MVA)
*       fdata(15) = % Compenstaion for Lines, Tap1 for Transformers
*       fdata(16) = Not uses for Lines, Tap2 for Transformers
*** BRANCH use of adata *******
*       adata( 1) = Critical Line Rating Line Code
*       adata( 2) = Critical Transformer Rating Line Code
*       mvadata(1)= 0/1/2 Critical Line Loading (N/A, Near End, Far End) 
*       mvadata(2)= 0/1/2 Critical Transformer Loading (N/A, Near End, Far End) 

      integer error, frstfg, frstbg, savefg, savebg
      null = char(0)
      linefeed = char(10)

      morefg = 2
      morebg = 2
******************* debug stuff **************************
*     morefg = 0
******************* debug stuff **************************
      first = 1
      next = 1

******************* debug stuff *************************
*     print '(a)', ' Entering  pf_odif'
******************* debug stuff *************************

      call gtoutput(in_buffer, fg_buf)

******************* debug stuff ************************
*     print 9901, in_buffer(1:100)
*9901 format (' in_buffer(1:100): ',a)
******************* debug stuff *************************

      frstfg = 1
      lstfg = index (fg_buf, null)
******************** debug stuff **************************
*     print 9911, frstfg, lstfg
*9911 format(' in pf_odif - frstfg,lstfg :', 2i5)
***   print 9931, fg_buf(frstfg:lstfg)
*     print 9931, fg_buf(frstfg:frstfg+100)
******************** debug stuff **************************
      
*************** debug stuff - change pf_output to call for background case
      call gtaltopt(in_buffer, bg_buf)
*************** debug stuff - change pf_output to call for background case
      frstbg = 1
      lstbg = index (bg_buf, null)
******************** debug stuff **************************
*     print 9913, frstbg, lstbg
*9913 format(' in pf_odif - frstbg,lstbg :', 2i5)
***   print 9931, bg_buf(frstbg:lstbg)
*     print 9931, bg_buf(frstbg:frstbg+100)
******************** debug stuff **************************


********** begin - get buffer full of foreground case data ******
  100 if (morefg .gt. 1) then
  110    if (lstfg .gt. frstfg) then
            morefg = 1
            nxtfg = nxt_term(fg_buf(frstfg+1:)) + frstfg
************************** debug stuff ************************
***   print 9931, fg_buf(frstfg:nxtfg-1)
*     print 9931, fg_buf(frstfg:frstfg+100)
*9931    format (1x, a)
************************** debug stuff ************************
            typfg = fg_buf(frstfg:frstfg)
            if (typfg .eq. 'B') then
               itypfg = 1
               read(fg_buf(frstfg:nxtfg-1),111) bupffg, bapffg
     1         ,(fdata(i1,1),i1=1,8)   
  111          format(bz, t7, a8, t15, f4.0, t21, 8f15.7)
               if (fdata(7,1) .ge. 0) then
                  adata(1,1) = '+'
               else
                  adata(1,1) = '-'
               endif
            elseif (typfg .eq. 'L' .or. typfg .eq. 'E' .or.
     1         typfg .eq. 'T') then
               itypfg = 2
               if (typfg .eq. 'T') itypfg = 3 
               read(fg_buf(frstfg:nxtfg-1),121)
     1         bupf1fg, bapf1fg, bupf2fg, bapf2fg, idata(1),
     2         (fdata(i1,1),i1= 1, 8), adata(1,1), mvadata(1,1), 
     3         (fdata(i1,1),i1= 9,10), adata(2,1), mvadata(2,1),
     4         (fdata(i1,1),i1=11,16) 
  121          format(bz, t7, a8, f4.0, t20, a8, f4.0, t33, i1,
     1         6e15.7, 2(e15.7, f8.1, a1, i1), 4e15.7, 2f8.2)
***************** debug stuff ********************
*     print 9941, typfg,bupf1fg, bapf1fg, bupf2fg, bapf2fg
*9941 format('typfg,bupf1fg, bapf1fg, bupf2fg, bapf2fg', a2,2(a12,f7.2))
***************** debug stuff ********************
            else
               itypfg = 9  
            endif
            savefg = frstfg
            frstfg = nxtfg
            if (fg_buf(frstfg:frstfg) .eq. linefeed) frstfg = frstfg + 1
            if (itypfg .eq. 9) go to 110 

         else
            morefg = 0
         endif
      endif
********** begin - get buffer full of background case data ******
      if (morebg .gt. 1) then
  150    if (lstbg .gt. frstbg) then
            morebg = 1
            nxtbg = nxt_term(bg_buf(frstbg+1:)) + frstbg
******************** debug stuff **************************
*     print 9942, frstbg, lstbg
*9942 format(' in pf_odif - frstbg,lstbg :', 2i5)
******************** debug stuff **************************
************************** debug stuff ************************
***   print 9931, bg_buf(frstbg:nxtbg-1)
*     print 9931, bg_buf(frstbg:frstbg+100)
************************** debug stuff ************************
            typbg = bg_buf(frstbg:frstbg)
            if (typbg .eq. 'B') then
               itypbg = 1
               read(bg_buf(frstbg:nxtbg-1),111) bupfbg, bapfbg
     1         ,(fdata(i1,2),i1=1,8)
               if (fdata(7,2) .ge. 0) then
                  adata(1,2) = '+'
               else
                  adata(1,2) = '-'
               endif

            elseif (typbg .eq. 'L' .or. typbg .eq. 'E' .or.
     1         typbg .eq. 'T') then
               itypbg = 2
               if (typbg .eq. 'T') itypbg = 3
               read(bg_buf(frstbg:nxtbg-1),121)
     1         bupf1bg, bapf1bg, bupf2bg, bapf2bg, idata(2),
     2         (fdata(i1,2),i1= 1, 8), adata(1,2), mvadata(1,2), 
     3         (fdata(i1,2),i1= 9,10), adata(2,2), mvadata(2,2),
     4         (fdata(i1,2),i1=11,16) 
 
***************** debug stuff ********************
*     print 9951, typbg,bupf1bg, bapf1bg, bupf2bg, bapf2bg
*9951 format('typbg,bupf1bg, bapf1bg, bupf2bg, bapf2bg', a2,2(a12,f7.2))
***************** debug stuff ********************
            else
               itypbg = 9
            endif
***************** debug stuff ********************
*     do  i1 = 1,16
*        fdata(i1,2) = 0.9 * fdata(i1,2)
*     enddo
*     idata(2) = 3
*     mvadata(1,2) = 1 
*     mvadata(2,2) = 1 
***************** debug stuff ********************
            savebg = frstbg
            frstbg = nxtbg
            if (bg_buf(frstbg:frstbg) .eq. linefeed) frstbg = frstbg + 1
            if ( itypbg .eq. 9) go to 150
 
         else
            morebg = 0
         endif
      endif
********** end   - get buffer full of background case data ******
*** check for no more foreground or background data ****
      if (morefg .eq. 0 .and. morebg .eq. 0) go to 1000
      if (morefg .eq. 1 .and. morebg .eq. 0) go to 300
      if (morefg .eq. 0 .and. morebg .eq. 1) go to 400
*** check for matching branches - buses are known to match
      if (itypbg .eq. 1) go to 200
      if (bupf2fg .lt. bupf2bg .or. 
     1 (bupf2fg .eq. bupf2bg .and. bapf2fg .lt. bapf2bg)) go to 300
      if (bupf2fg .gt. bupf2bg .or. 
     1 (bupf2fg .eq. bupf2bg .and. bapf2fg .gt. bapf2bg)) go to 400


******** records match - build difference into out_buffer
  200 next = nxtfg - savefg + first
      ityp = itypfg 
      morefg = 2
      morebg = 2
******************** debug stuff **************************
*     print 9961, savefg, nxtfg, first, next
*9961 format(' difference-savefg, nxtfg, first, next:', 4i5)
******************** debug stuff **************************
      out_buffer(first:next) = fg_buf(savefg:nxtfg)
      out_buffer(next:next) = linefeed
      if (itypfg .eq. 1) then
      do i1 = 1,12
         fdata(i1,3) = fdata(i1,1) - fdata(i1,2)
      enddo
         adata(1,3) = adata(1,1)
         if (fdata(7,1) .eq. 0.0 .and. fdata(7,2) .ne. 0.0) 
     1     adata(1,3) = adata(1,2)
      elseif (itypfg .ge. 2) then
      do i1 = 1,16
         fdata(i1,3) = fdata(i1,1) - fdata(i1,2)
      enddo
         adata(1,3) = adata(1,1)
         adata(2,3) = adata(2,1)
         mvadata(1,3) = mvadata(1,1)
         mvadata(2,3) = mvadata(2,1)
         if (idata(1) .eq. idata(2)) then
            if (idata(1) .le. 1) then
               write (acktcnt(3), 211)
  211          format('()')
            else
               write(acktcnt(3), 221) idata(1)
  221          format('([',i1,'])')
            endif
         else
            write(acktcnt(3), 231) idata(1), idata(2)
  231       format('([',i1,'/',i1,'])')
         endif
      endif
      go to 600 
******** move forgeground data into out_buffer 
  300 next = nxtfg - savefg + first
      ityp = itypfg
      morefg = 2
******************** debug stuff **************************
*     print 9971, savefg, nxtfg, first, next
*9971 format(' foreground-savefg, nxtfg, first, next:', 4i5)
******************** debug stuff **************************

      out_buffer(first:next) = fg_buf(savefg:nxtfg)
      out_buffer(next:next) = linefeed
      if (itypfg .eq. 1) then
        do i1 = 1,12 
          fdata(i1,3) = fdata(i1,1)
        enddo 
        adata(1,3) = adata(1,1)
      elseif (itypfg .ge. 2) then
        do i1 = 1,16 
          fdata(i1,3) = fdata(i1,1)
        enddo 
        adata(1,3) = adata(1,1)
        adata(2,3) = adata(2,1)
        mvadata(1,3) = mvadata(1,1)
        mvadata(2,3) = mvadata(2,1)
        if (idata(1) .eq. 0) idata(1) = 1
        write(acktcnt(3), 311) idata(1)
  311   format('([',i1,'/0])')
      endif 
      go to 600
******** move NEGATIVE of background data into out_buffer
  400 next = nxtbg - savebg + first 
      ityp = itypbg
      morebg = 2
******************** debug stuff **************************
*     print 9981, savebg, nxtbg, first, next
*9981 format(' foreground-savebg, nxtbg, first, next:', 4i5)
******************** debug stuff **************************

      out_buffer(first:next) = bg_buf(savebg:nxtbg)
      out_buffer(next:next) = linefeed
      if (itypbg .eq. 1) then
        do i1 = 1,12
          fdata(i1,3) =  - fdata(i1,2)
        enddo
        adata(1,3) = adata(1,2)
      elseif (itypbg .ge. 2) then
        do i1 = 1,16
          fdata(i1,3) =  - fdata(i1,2)
        enddo
        adata(1,3) = adata(1,2)
        adata(2,3) = adata(2,2)
        mvadata(1,3) = mvadata(1,2)
        mvadata(2,3) = mvadata(2,2)
        if (idata(2) .eq. 0) idata(2) = 1
        write(acktcnt(3), 411) idata(2)
  411   format('([0/',i1,'])')
      endif
  600 continue 
******************** debug stuff **************************
*     print 9991,  first, next, ityp 
*9991 format(' at 600,  first, next:', 3i5)
******************** debug stuff **************************
 
      if (ityp .eq. 1) then
         write (out_buffer(first+20:first+140), 611) 
     1     (fdata(i1,3),i1=1,8), adata(1,3)
  611    format( 8f15.7, a1) 
      else
         write (out_buffer(first+33:first+255), 621) 
     2     (fdata(i1,3),i1= 1, 8), adata(1,3), mvadata(1,3), 
     3     (fdata(i1,3),i1= 9,10), adata(2,3), mvadata(2,3),
     4     (fdata(i1,3),i1=11,16), acktcnt(3)
 621    format( 6e15.7, 2(e15.7, f8.1, a1, i1), 4e15.7, 2f8.2, a7)
      endif 
******************** debug stuff **************************
*     print 9001,  first, next
*9001 format(' after 600, going bact to 100- first, next:', 2i5)
*     print 9011, out_buffer(first:first+99)
*     print 9011, out_buffer(first+100:first+199)
*     print 9011, out_buffer(first+200:first+256)
*9011 format(' out_buffer(first:first+100) :',a) 
******************** debug stuff **************************
      first = next + 1
      go to 100 

 1000 out_buffer(first:first) = null 
      error  = 0
 2000 continue
******************** debug stuff **************************
*     print 9021
*9021 format(' leaving pf_odif')
******************** debug stuff **************************
      return
      end
 
