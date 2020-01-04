C    @(#)pa_odif.f	20.6 1/7/99
      integer function pa_odif (in_buffer, out_buffer) 

      include 'ipfinc/parametr.inc'
      include 'ipfinc/postdta.inc'

      character in_buffer*(*), out_buffer*(*), 
     &          fg_buf*(MAXBUFFER), bg_buf*(MAXBUFFER)

      character type*1,  null*1, linefeed*1, typfg*1, affg*10, af1fg*10, 
     &          af2fg*10, typbg*1, afbg*10, af1bg*10, af2bg*10
      dimension fdata(4,3), idata(3) ! array(n,3)=array(n,1)-array(n,2)
      integer error, frstfg, frstbg, savefg, savebg, status,
     &        p_gtardata, p_gtxardta 

      null = char(0)
      linefeed = char(10)
      pa_odif = 0

      morefg = 2
      morebg = 2
      first = 1
      next = 1

      status = p_gtardata(in_buffer, fg_buf)
      if (status .ne. 0) then
         pa_odif = 1
         go to 2000
      endif
      frstfg = 1
      lstfg = index (fg_buf, null)

      status = p_gtxardta(in_buffer, bg_buf)
      if (status .ne. 0) then
         pa_odif = 1
         go to 2000
      endif
      frstbg = 1
      lstbg = index (bg_buf, null)
******************** debug stuff **************************
*     print 9911, frstfg, lstfg
*9911 format(' in pf_odif - frstfg,lstfg :', 2i5)
******************** debug stuff **************************


********** begin - get buffer full of foreground case data ******
  100 if (morefg .gt. 1) then
         if (lstfg .gt. frstfg) then
            morefg = 1
            nxtfg = nxt_term(fg_buf(frstfg+1:)) + frstfg
************************** debug stuff ************************
*     print 9931, fg_buf(frstfg:nxtfg-1)
*9931    format (1x, a)
************************** debug stuff ************************
            typfg = fg_buf(frstfg:frstfg)
            if (typfg .eq. 'A') then
               itypfg = 1
               read(fg_buf(frstfg:nxtfg-1),111) 
     1         affg,(fdata(i1,1),i1=1,4)   
  111          format(bz, t3, a10, 1x, 4e15.7)
            elseif (typfg .eq. 'I') then
               itypfg = 2
               read(fg_buf(frstfg:nxtfg-1),121) af2fg, 
     1         (fdata(i1,1),i1=1,3)
  121       format(bz, t14, a10, t25, 3e15.7)
***************** debug stuff ********************
*     print 9941, typfg, af2fg
*9941 format(' typfg, af2fg', a2, a12)
***************** debug stuff ********************
            endif
            savefg = frstfg
            frstfg = nxtfg
            if (fg_buf(frstfg:frstfg) .eq. linefeed) frstfg = frstfg + 1

         else
            morefg = 0
         endif
      endif
********** begin - get buffer full of background case data ******
      if (morebg .gt. 1) then
         if (lstbg .gt. frstbg) then
            morebg = 1
            nxtbg = nxt_term(bg_buf(frstbg+1:)) + frstbg
************************** debug stuff ************************
*     print 9931, bg_buf(frstbg:nxtbg-1)
************************** debug stuff ************************
            typbg = bg_buf(frstbg:frstbg)
            if (typbg .eq. 'A') then
               itypbg = 1
               read(bg_buf(frstbg:nxtbg-1),111) 
     1         afbg, (fdata(i1,2),i1=1,4)
            elseif (typbg .eq. 'I') then
               itypbg = 2
               read(bg_buf(frstbg:nxtbg-1),121) af2bg, 
     1         (fdata(i1,2),i1=1,3)

 
***************** debug stuff ********************
*     print 9951, typbg, af2bg
*9951 format('typbg, af2bg', a2, a12)
***************** debug stuff ********************
            endif
***************** debug stuff ********************
*     do  i1 = 1,4
*        fdata(i1,2) = 0.9 * fdata(i1,2)
*     enddo
***************** debug stuff ********************
            savebg = frstbg
            frstbg = nxtbg
            if (bg_buf(frstbg:frstbg) .eq. linefeed) frstbg = frstbg + 1
 
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
      if (af2fg .lt. af2bg) go to 300
      if (af2fg .gt. af2bg) go to 400


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
      do i1 = 1,4
         fdata(i1,3) = fdata(i1,1) - fdata(i1,2)
      enddo
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
      do i1 = 1,4 
         fdata(i1,3) = fdata(i1,1)
      enddo 
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
      do i1 = 1,4
         fdata(i1,3) =  - fdata(i1,2)
      enddo
  600 continue 
******************** debug stuff **************************
*     print 9991,  first, next, ityp 
*9991 format(' at 600,  first, next:', 3i5)
******************** debug stuff **************************
 
      if (ityp .eq. 1) then
         write (out_buffer(first+13:first+72), 611) 
     1      (fdata(i1,3),i1=1,4)
  611    format( 4e15.7) 
      else
         write (out_buffer(first+24:first+68), 621) 
     1   (fdata(i1,3),i1=1,3) 
  621    format( 3e15.7)
      endif 
******************** debug stuff **************************
*     print 9001,  first, next
*9001 format(' after 600, going bact to 100- first, next:', 2i5)
*     print 9011, out_buffer(first:first+132)
*9011 format(' out_buffer(first:first+132) :',a) 
******************** debug stuff **************************
      first = next + 1
      go to 100 

 1000 out_buffer(first:first) = null 
 2000 continue
******************** debug stuff **************************
*     print 9021
*9021 format(' leaving pf_odif')
******************** debug stuff **************************
      return
      end
