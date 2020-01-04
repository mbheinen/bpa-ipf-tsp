C    %W% %G%
      subroutine postbrn(record)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/postdta.inc'
      include 'ipfinc/bscords.inc'
      include 'ipfinc/pasprm.inc'

      character record*(*), type*1, subtype*1
      character rcrdpar*300 ! record for parallel circuits 
      character floa*25, flob*25, fmtflo*25, fmtflof*25, par*7
      character ratcod*1, ratcod1*1, ovldflg*3, fmtovld*20, ovldval*30
      character crcodam*1, crcodva*1 
      character fmtcmp*15, pctcomp*7
      character begpath*30  
      character parcord(11)*120 ! coordinate records for parallel circuits 
      character in_buffer * (MAXBUFFER)
      character out_pbuf * (MAXBUFFER)
      character null*1, linefeed*1
      character fmttap*16, tapa1*10, tapa2*10  !tx taps 
      character ckt*1             ! Circuit - used for separate parallels 

      integer error, firstp

      data fmtflo /'(i9)'/
      data fmtflof /'(f9.2)'/
      data fmtovld/'(''('',a1,1x,i9,'')'')'/
      data fmtcmp /'(''('',i9,''%)'')'/
      data fmttap /'(''('',f9.2,'')'')'/
      data lenbrec /257/       ! length of branch record + null 

      null = char(0)
      linefeed = char(10)
      lparsw = 0               ! default - processing parallels combined
********************* debug stuff **********************
*     print 9901, record
*9901 format(' entering postbrn record=:',a)
********************* debug stuff **********************
      read(record,101) 
     & type, subtype, buspf1, basepf1, buspf2, basepf2, ckt, numpar,
     & pin, qin, pout, qout, ploss, qloss, 
     & crfloam, crratam, crcodam, mxendam, 
     & crflova, crratva, crcodva, mxendva, 
     & ttpctam, ttfloam, ttpctva, ttflova, par 
  101 format(bz, 2a1, t7, a8, f4.0, 1x, a8, f4.0, a1, i1, 6f15.7,
     & 2(e15.7, f8.1, a1, i1), 4e15.7, t250, a7)
********************* debug stuff **********************
*     print 9901, record
*     print 9901, nxtcord
********************* debug stuff **********************

      if (dgoptyp .eq. 5) then    ! If coordinate only, set bus powerflow 
          type = nxtcord(1:1)     ! identification to coordinate id.
          read (nxtcord, 226)  buspf1, basepf1, buspf2, basepf2
      endif

      indcor2 = ksrchcor(buspf2, basepf2)
********************* debug stuff **********************      
*     print 9931, buspf1, basepf1,buspf2, basepf2, indcor2
*9931 format(' buspf2, basepf2, indcor2 :', 2(a8,f6.1),i5)
*     if (subtype .eq. 'P') print 9933, record(234:249)
*9933 format (a)
********************* debug stuff **********************      

*********** discard lines that are not of interest **************
      if (indcor2 .le. 0) go to 400
      if ( buspf1 .gt. buspf2 .or.
     &    (buspf1 .eq. buspf2 .and. basepf1 .gt. basepf2)) go to 1000
*********** lines that are not of interest have been discarded ********
      ibasepf1 = basepf1
      ibasepf2 = basepf2
      karosg = 1       ! section number that arrow is in

******* build the coordinate track for a line ******************    
      indcor1 = ksrchcor(buspf1, basepf1)
********************* debug stuff **********************      
*     print 9951, buspf1, basepf1, indcor1
*9951 format(' buspf1, basepf1, indcor1', a8,f6.1,i5)
********************* debug stuff **********************
      if (indcor1 .le. 0) go to 1000      
      begpath = '[ '
      if (kprflg(indcor1) .eq. 0) 
     & write(begpath(3:30), 201) xcord(indcor1), ycord(indcor1)
  201 format(2(f6.2, ' cmtr '))
      write (lpost,211) begpath
  211 format(a)

******* look for branch coordinate data

****** get next coordinate record *******************
  220 continue
********************* debug stuff **********************
*     print 9941, kcorflg
*9941 format('kcorflg = :', i2)
********************* debug stuff **********************

      if (kcorflg .eq. 0) then
          read (corfil, 221, end=250) nxtcord
  221     format(a)
          kcorflg = 1
      elseif (kcorflg .eq. 1) then
****** information is already in nxtcord *******************
          continue
      elseif (kcorflg .eq. 2) then
          go to 280
      endif
********************* debug stuff **********************   
*     print 9961, nxtcord
*9961 format('nxtcord = :', a)
********************* debug stuff **********************   

      if ( nxtcord(1:1) .eq. 'L' .or. nxtcord(1:1) .eq. 'T') then
         read (nxtcord, 226) buscd1, basecd1, buscd2, basecd2
  226    format(bz, t3, 2(a8, f4.0) ) 

         if     (buspf1 .gt. buscd1) then  
             kcorflg = 0
             go to 220
         elseif (buspf1  .eq. buscd1 .and. 
     1           basepf1 .gt. basecd1) then
              kcorflg = 0
              go to 220 
         elseif (buspf1 .eq. buscd1 .and. basepf1 .eq. basecd1 .and.
     1           buspf2 .gt. buscd2) then
              kcorflg = 0
              go to 220
         elseif (buspf1 .eq. buscd1 .and. basepf1 .eq. basecd1 .and.
     &           buspf2 .eq. buscd2 .and. basepf2 .gt. basecd2) then
              kcorflg = 0
              go to 220

         elseif (buspf1 .eq. buscd1 .and. basepf1 .eq. basecd1 .and. 
     &           buspf2 .eq. buscd2 .and. basepf2 .eq. basecd2) then
              kcorflg = 0
 
******* begin set up for plotting parallels separately *********
               if (broppar .eq. 1 .and. 
     &            (numpar .gt. 1 .or. vaopdif .eq. 1)) then
               iparcor = 1
               parcord(iparcor) = nxtcord 
  230          if (iparcor .lt. 11) iparcor = iparcor + 1 
               read (corfil, 221, end=232) parcord(iparcor)
               if ( parcord(iparcor)(1:1) .ne. 'L' .and.
     &              parcord(iparcor)(1:1) .ne. 'T') then
                  kcorflg = 3
                  go to 234
               endif

               read (parcord(iparcor), 226)
     &          buscd1, basecd1, buscd2, basecd2
               if (buspf1 .eq. buscd1 .and. basepf1 .eq. basecd1 .and.
     &             buspf2  .eq. buscd2 .and. basepf2 .eq. basecd2)
     &           go to 230
               kcorflg = 3
               go to 234
  232          kcorflg = 2
  234          iparcor = iparcor - 1
***************** debug stuff ********************** 
*     print 9234, iparcor
*9234 format(' postbrn at 234-iparcor: ',i5) 
***************** debug stuff ********************** 
               if (iparcor .gt. 1) then    
                  write(in_buffer, 237) record(1:31) 
  237             format(a)
                  lastp = lastch(in_buffer)
                  in_buffer(lastp+1:) = null
                  out_pbuf(1:) = null
************* debug stuff ***********************      
*     print 9237, in_buffer(1:100)
*9237 format (' in_buffer at 237', a)
************* debug stuff ***********************      
                  if (vaopdif .eq. 0) then
                     call gtparout(in_buffer, out_pbuf, error)

************* debug stuff ***********************
*      print 9235 
* 9235 format (' Returned from gtparout')     
************* debug stuff ***********************
                  else
                     call pp_odif(in_buffer, out_pbuf, error)
************* debug stuff ***********************
*      print 9236 
* 9236 format (' Returned from pp_odif')
************* debug stuff ***********************
                  endif

                  jparcor = 0
                  firstp = 1
                  lastp = index(out_pbuf, null)
                  nparpf = (lastp - firstp + 1)/lenbrec

************* debug stuff ***********************
**** This debug block gave the run time error: 'output statement overflows record'
*      idbg1 = 1
*      do 9240 idbg = 1, nparpf
*      idbg2 = idbg1 + lenbrec - 3
*      idbg2 = idbg1 + 255
*      print 9239, lenbrec, lastp, nparpf, iparcor
* 9239 format(' lenbrec, lastp, nparpf, iparcor ',4i5)
*      print 9238, out_pbuf(idbg1:idbg2)
* 9238 format (' out_pbuf at 237+', a)
*      idbg1 = idbg1 + lenbrc
* 9240 continue
************* debug stuff ***********************




                  if (nparpf .le. iparcor) then
                     lparsw = 1
                     go to 260
                  endif
               endif
            endif
******* end   set up for plotting parallels separately *********
*********** debug stuff ******************
*     print 9241, nxtcord
*9241 format(' in postbrn before 241-nxtcord',a) 
*********** debug stuff ******************

            read (nxtcord, 241) karosg
  241       format(bz, t29, i2)
            if (karosg .eq. 0) karosg = 1
*           kcorflg = 0
            ind1 = 31
            max = 90

            do while (ind1 .lt. max) 
              if (nxtcord(ind1:ind1+11) .eq. ' ') go to 280
              read (nxtcord(ind1:ind1+11), 246) xcorda, ycorda
  246         format(bz, 2f6.2)
              write(lpost,201) xcorda, ycorda 
              ind1 = ind1 + 12
            end do 
         else
            kcorflg = 1
         endif

      endif
      go to 280
  250 kcorflg = 2
*************** begin retrieve coordinate data for each parallel ********  
  260 jparcor = jparcor + 1
      if ( firstp .ge. lastp) then
         print 261
  261    format(' program error in plotting parallel circuits ')
         return  
      else
         nextp = nxt_term(out_pbuf(firstp+1:)) + firstp
**************** debug stuff ********************* 
*      print 9261, firstp, nextp,  lastp, iparcor, jparcor
* 9261 format(' after 261 firstp, nextp,  lastp, iparcor, jparcor ', 5i5)
*      print 9263, out_pbuf(firstp:firstp+32)      
* 9263 format(' out_pbuf(firstp:firstp+32)', a)     
*      print 9264, out_pbuf(firstp+33:firstp+122)
* 9264 format(' out_pbuf(firstp+33:firstp+122)', a)
*
*      print 9265, out_pbuf(firstp+233:firstp+240)      
* 9265 format(' after 261 out_pbuf(firstp+233:firstp+240)', a)
**************** debug stuff ********************* 
         rcrdpar = out_pbuf(firstp:nextp-1)
         read (out_pbuf(firstp:nextp-1),101)
     & type, subtype, buspf1, basepf1, buspf2, basepf2, ckt, numpar,
     & pin, qin, pout, qout, ploss, qloss, 
     & crfloam, crratam, crcodam, mxendam, 
     & crflova, crratva, crcodva, mxendva, 
     & ttpctam, ttfloam, ttpctva, ttflova, par 
         firstp = nextp
         if (out_pbuf(firstp:firstp) .eq. linefeed) firstp = firstp + 1

**************** debug stuff ********************* 
*     print 9262, parcord(jparcor)
*9262 format(' parcord(jparcor) ',a)
**************** debug stuff ********************* 
         read (parcord(jparcor), 241) karosg
         if (karosg .eq. 0) karosg = 1
*        kcorflg = 0
         ind1 = 31
         max = 90
 
         do while (ind1 .lt. max)
            if (parcord(jparcor)(ind1:ind1+11) .eq. ' ') go to 280
            read (parcord(jparcor)(ind1:ind1+11), 246) xcorda, ycorda
            write(lpost,201) xcorda, ycorda
            ind1 = ind1 + 12
         end do
      endif
*************** begin retrieve coordinate data for each parallel ********  

  280 continue
      if (kprflg(indcor2) .eq. 0)
     1 write(lpost,201) xcord(indcor2), ycord(indcor2)

      write (lpost, 291)
  291 format (' ]')

****** build remainder of parameters for call to acLine *********
      ipinflg =  sign (1.0, pin)
****** build flo for PQ, MVA/I, LOSS ************* 
      if     (dgoptyp .eq. 1 .or. dgoptyp .eq. 4) then 
****** build flo for PQ ************* 
****** or for Interchange if there is a branch in the interchange coord file 
         floa = '()'
         flob = '()'
****** build flo for PQ -  Pin, Qin, Pout, Qout ***************
         if (ipinflg .lt. 0) then
            ptmp = pin
            qtmp = qin
            pin = - pout
            qin = - qout
            pout = - ptmp
            qout = - qtmp    
         endif
************* begin formatting sending end P & Q *************
         if (broppsn .eq. 1 .or. bropqsn .eq. 1) then
            nxtflo = 3            ! next flow data starts here 
         floa = '( '

            if (broppsn .eq. 1) then
               apin = abs(pin) + 0.5
               ipin = sign (apin, pin)
               lognum = alog10(amax1(apin, 1.0)) + 1.0
               if (ipin .lt. 0) lognum = lognum + 1  
********************* debug stuff **********************      
*     print 9971, lognum, fmtflo
*9971 format(' in postbrn at 9971, lognum, fmtflo = :',i5,1x,a)  
********************* debug stuff **********************      
               write (fmtflo(3:3), 311) lognum
  311          format(i1)
               lstflo =  nxtflo + lognum  - 1   ! last character of next flow data  
********************* debug stuff **********************      
*     print 9911, fmtflo
*9911 format(' in postbrn, fmtflo = :',a)  
********************* debug stuff **********************      
               write (floa(nxtflo:lstflo), fmtflo) ipin
********************* debug stuff **********************      
*     print 9921, floa
*9921 format(' in postbrn, flo = :',a)  
********************* debug stuff **********************      
               nxtflo = lstflo + 1
               lstflo =  nxtflo
               floa(nxtflo:lstflo) = ' '
               nxtflo = nxtflo + 1
            endif

            if (bropqsn .eq. 1) then
               aqin = abs(qin) + 0.5
               iqin = sign (aqin, qin)
               lognum = alog10(amax1(aqin, 1.0)) + 1.0
               if (iqin .lt. 0) lognum = lognum + 1  
               write (fmtflo(3:3), 311) lognum
 
               lstflo =  nxtflo
               floa(nxtflo:lstflo) = '('
               nxtflo = nxtflo + 1

               lstflo =  nxtflo + lognum  - 1   ! last character of next flow data  
               write (floa(nxtflo:lstflo), fmtflo) iqin
               nxtflo = lstflo + 1
               lstflo =  nxtflo
               floa(nxtflo:lstflo) = ' '
               nxtflo = nxtflo + 1

               lstflo =  nxtflo
               floa(nxtflo:lstflo) = ')'
               nxtflo = nxtflo + 1
            endif

            floa(nxtflo:nxtflo+1) = ' )'
         endif

************* begin to format receiving end P & Q *********

         if (bropprc .eq. 1 .or. bropqrc .eq. 1) then
            nxtflo = 4            ! next flow data starts here
            flob = '( ['
 
            if (bropprc .eq. 1) then
               apout = abs(pout) + 0.5
               ipout = sign (apout, pout)
               lognum = alog10(amax1(apout, 1.0)) + 1.0
               if (ipout .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflo
********************* debug stuff **********************
               write (fmtflo(3:3), 311) lognum
               lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
********************* debug stuff **********************
*     print 9911, fmtflo
********************* debug stuff **********************
               write (flob(nxtflo:lstflo), fmtflo) ipout
********************* debug stuff **********************
*     print 9921, flob
********************* debug stuff **********************
               nxtflo = lstflo + 1
               lstflo =  nxtflo
               flob(nxtflo:lstflo) = ' '
               nxtflo = nxtflo + 1
            endif
 
            if (bropqrc .eq. 1) then
               aqout = abs(qout) + 0.5
               iqout = sign (aqout, qout)
               lognum = alog10(amax1(aqout, 1.0)) + 1.0
               if (iqout .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflo
********************* debug stuff **********************
               write (fmtflo(3:3), 311) lognum
********************* debug stuff **********************
*     print 9911, fmtflo
********************* debug stuff **********************
               lstflo =  nxtflo
               flob(nxtflo:lstflo) = '('
               nxtflo = nxtflo + 1
 
               lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
               write (flob(nxtflo:lstflo), fmtflo) iqout
********************* debug stuff **********************
*     print 9921, flob
********************* debug stuff **********************

               nxtflo = lstflo + 1
               lstflo =  nxtflo
               flob(nxtflo:lstflo) = ' '
               nxtflo = nxtflo + 1
               lstflo =  nxtflo
               flob(nxtflo:lstflo) = ')'
               nxtflo = nxtflo + 1
            endif
 
            flob(nxtflo:nxtflo+2) = '] )'
         endif



      elseif (dgoptyp .eq. 2) then 
****** build flo for MVA and I 
         flob = '()'
         nxtflo = 3            ! next flow data starts here
         floa = '(<'

*************** build current component of flow ****************** 
         if (mxendam .ne. 0) then
            pin = ttfloam 
            apin = abs(pin) + 0.5
            ipin = sign (apin, pin)
            lognum = alog10(amax1(apin, 1.0)) + 1.0
            if (ipin .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflo
********************* debug stuff **********************
            write (fmtflo(3:3), 311) lognum
            lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
********************* debug stuff **********************
*     print 9911, fmtflo
********************* debug stuff **********************
            write (floa(nxtflo:lstflo), fmtflo) ipin
********************* debug stuff **********************
*     print 9921, floa
*     print 9981, mxendva, ttflova
********************* debug stuff **********************
            nxtflo = lstflo + 1
            if (mxendva .ne. 0) then
              floa(nxtflo:nxtflo) = 'a'
              nxtflo = nxtflo + 1
            endif 
         endif

*************** build MVA component of flow *** 
         if (mxendva .ne. 0) then 
           pin = ttflova 
           apin = abs(pin) + 0.5
           ipin = sign (apin, pin)
           lognum = alog10(amax1(apin, 1.0)) + 1.0
           if (ipin .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflo
********************* debug stuff **********************
           write (fmtflo(3:3), 311) lognum
           lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
********************* debug stuff **********************
*     print 9911, fmtflo
********************* debug stuff **********************
           write (floa(nxtflo:lstflo), fmtflo) ipin
********************* debug stuff **********************
*     print 9921, floa
*     print 9981, mxendva, ttflova 
*9981 format('mxendva, ttflova = ',i3, f9.1)
********************* debug stuff **********************
           nxtflo = lstflo + 1
         endif
*** The line component overrides tx component in determining whether maxflow
*** is at the near or far end of mised L & T sections   
         if (mxendam .ne. 0) then 
           mvaend = mxendam 
         else   
           mvaend = mxendva 
         endif
         if ((ipinflg .ge. 0 .and. mvaend .eq. 2) .or. 
     1     (ipinflg .lt. 0 .and. mvaend .eq. 1)) then
            floa(nxtflo:nxtflo) = 'R'
            nxtflo = nxtflo + 1
         endif
         floa(nxtflo:nxtflo+1) = '>)'


      elseif (dgoptyp .eq. 3) then 
****** build flo for Losses 

         floa = '()'
         flob = '()'
         pin = ploss
         qin = qloss
************* begin formatting  P & Q losses *************
************* use broppsn and bropqsn  to select P and/or Q losses ****
         if (broppsn .eq. 1 .or. bropqsn .eq. 1) then
            nxtflo = 2            ! next flow data starts here
         floa = '('
 
            if (broppsn .eq. 1) then
               apin = abs(pin) + 0.5
               ipin = sign (apin, pin)
               lognum = alog10(amax1(apin, 1.0)) + 4.0
               if (ipin .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflof
********************* debug stuff **********************
               write (fmtflof(3:3), 311) lognum
               lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
********************* debug stuff **********************
*     print 9911, fmtflof
********************* debug stuff **********************
               write (floa(nxtflo:lstflo), fmtflof) pin
********************* debug stuff **********************
*     print 9921, floa
********************* debug stuff **********************
               nxtflo = lstflo + 1
               lstflo =  nxtflo
               floa(nxtflo:lstflo) = ' '
               nxtflo = nxtflo + 1
            endif
 
            if (bropqsn .eq. 1) then
               aqin = abs(qin) + 0.5
               iqin = sign (aqin, qin)
               lognum = alog10(amax1(aqin, 1.0)) + 4.0
               if (iqin .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflof
********************* debug stuff **********************
               write (fmtflof(3:3), 311) lognum
 
               lstflo =  nxtflo
               floa(nxtflo:lstflo) = '('
               nxtflo = nxtflo + 1
 
               lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
********************* debug stuff **********************
*     print 9911, fmtflof
********************* debug stuff **********************
               write (floa(nxtflo:lstflo), fmtflof) qin
********************* debug stuff **********************
*     print 9921, floa
********************* debug stuff **********************
               nxtflo = lstflo + 1
*              lstflo =  nxtflo
*              floa(nxtflo:lstflo) = ' '
*              nxtflo = nxtflo + 1
 
               lstflo =  nxtflo
               floa(nxtflo:lstflo) = ')'
               nxtflo = nxtflo + 1
            endif
 
            floa(nxtflo:nxtflo) = ')'
         endif

         continue

 
      elseif (dgoptyp .eq. 5) then 
****** build flo for Coordinates only 
         floa = '()'
         flob = '()' 
********************* debug stuff **********************
*     print 9301, floa
*9301 format(' before 315 in postbrn floa = ',a)
********************* debug stuff **********************
 
      elseif (dgoptyp .eq. 6) then 
****** build flo for % loading 
         floa = '(    %)' 
         flob = '()' 
         if (mxendva .ne. 0) then
           olpct = ttpctva
           if (mxendam .ne. 0 .and. ttpctam .gt. ttpctva) then  
             olpct = ttpctam
             floa = '(    %a)'
           endif
         else
           olpct = ttpctam
         endif
         if (abs(olpct) .le. 999.0) then
             aolpct = abs(olpct) + 0.5
             iolpct = sign (aolpct, olpct)
             write(floa(2:5),315) iolpct
  315        format(i4) 
         else
             floa = '(****%)'
         endif 
      endif


********** flo is complete ****************************
      if (vaopdif .eq. 0) then
         if (lparsw .eq. 1 )  then
             write (par, 320) ckt
  320        format('(#', a1, ')' )
         elseif (numpar .le. 1 )  then
*            if (numpar .le. 1 )  then
             par = '()'
         else
             write (par, 321) numpar
  321        format('([', i1, '])' ) 
         endif
      endif

********* build branch overload information *********
      ovldflg = '()'
      ovldval = '()'
*** check for overloads except for coords only of difference plots 
      if (dgoptyp .ne. 5 .and. vaopdif .eq. 0) then
*** calculate % loading for worst parallel 
        if (mxendam .ne. 0 .and. crratam .gt. 0.0) then
          crpctam = crfloam / crratam * 100.0
        else
          crpctam = 0.0 
        endif
        if (mxendva .ne. 0 .and. crratva .gt. 0.0) then
          crpctva = crflova / crratva * 100.0
        else
          crpctva = 0.0 
        endif

        if (mxendva .ne. 0) then
          if (mxendam .ne. 0) then
***         checking mixed L & T sections in circuit
            ratcod = '*'
            if (crpctva .gt. crpctam) then
***           Transformer is more overloaded 
                ratcod1 = 'm'
                crflo = crflova 
                crrat = crratva
                olpct = crpctva
            else        
***           Line is more overloaded 
                ratcod1 = 'a'
                crflo = crfloam 
                crrat = crratam
                olpct = crpctam
            endif
          else       ! (mxendam .ne. 0) test is false
***         checking for parallel transformer circuits
            if (ttflova .gt. crflova) then
              ratcod = '*'
            else
              ratcod = crcodva  
            endif
            ratcod1 = 'm'
            crflo = crflova 
            crrat = crratva
            olpct = crpctva
          endif      
        else         !(mxendva .ne. 0) test is false
***       checking for parallel line circuits
          if (ttfloam .gt. crfloam) then
            ratcod = '*'
          else
            ratcod = crcodam
          endif
          ratcod1 = 'a'
          crflo = crfloam
          crrat = crratam
          olpct = crpctam
        endif        !(mxendva .ne. 0) test is complete
  

        if (olpct .ge. 100.0) then
          ovldflg = '('//ratcod//')'
        endif
***********debug stuff *************************** 
*     print 9311, olpct
*9311 format (' in postbrn at 321-olpct = ',f8.2) 
***********debug stuff *************************** 
******************************* temp debug stuff*************
        if (olpct .ge. 90.0) then  
*       if (olpct .ge. 10.0) then  
******************************* temp debug stuff*************

          ovldval = '('
          nxtflo = 2
          pin = crflo 
          apin = abs(pin) + 0.5
          ipin = sign (apin, pin)
          lognum = alog10(amax1(apin, 1.0)) + 1.0
          if (ipin .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9971, lognum, fmtflo
********************* debug stuff **********************
          write (fmtflo(3:3), 311) lognum
          lstflo =  nxtflo + lognum  - 1   ! last character of next flow data
********************* debug stuff **********************
*     print 9911, fmtflo
********************* debug stuff **********************
          write (ovldval(nxtflo:lstflo), fmtflo) ipin
********************* debug stuff **********************
*     print 9921, ovldval 
********************* debug stuff **********************
          nxtflo = lstflo + 1
          if (mxendva .ne. 0 .and. mxendam .ne. 0) then
            ovldval(nxtflo:nxtflo) = 'a'
            nxtflo = nxtflo + 1
          endif
 
          ovldval(nxtflo:nxtflo) = ratcod
          kolval = crrat + 0.5
          olvaln = kolval
          lognum = alog10 (amax1(olvaln, 1.0)) + 1
          nxtflo = nxtflo + 1
          lstflo = nxtflo + lognum - 1
          write (fmtflo(3:3), 311) lognum
          write (ovldval(nxtflo:lstflo),fmtflo) kolval
          nxtflo = lstflo + 1
          ovldval(nxtflo:nxtflo) = ')'
        endif    ! end (olpct .ge. 90.0) test
      endif      ! end (dgoptyp .ne. 5 .and. vaopdif .eq. 0) test

      if (type .eq. 'L' .or. type .eq. 'E') then
*************** check for compensation ***************
********** debug stuff ******************
*     print 9341, lparsw
*9341 format(' before 341 lparsw = ',i5) 
********** debug stuff ******************
         if (lparsw .eq. 0) then
            read (record,341)  pctcmp
         else
            read (rcrdpar,341) pctcmp
         endif
  341    format(t234, f8.2)
         if (abs(pctcmp) .gt. 0.1) then
            apctcmp = abs(pctcmp) + 0.5
            ipctcmp = sign(apctcmp, pctcmp) 
            lognum = alog10 (amax1(apctcmp, 1.0)) +1
            if (ipctcmp .lt. 0.0) lognum = lognum + 1
            if (lognum .gt. 4) then
               pctcomp = '(****%)'
            else
               write (fmtcmp(7:7), 311) lognum 
               write (pctcomp, fmtcmp) ipctcmp
            endif
         else
            pctcomp = '()'
         endif
 
*************** write a postscript line record ********** 
         write (lpost, 351) ovldflg, ovldval, pctcomp, karosg, 
     1   ipinflg, floa, flob, par, kotgflg, ibasepf1 
  351    format(a3, 1x, a30, 1x, a7, 1x, i2, 1x, i8, 1x, 2(a25, 1x), a7,
     1   1x, i1, 1x, i5, ' acLine') 
      elseif (type .eq. 'T') then
         if (lparsw .eq. 0) then
            read (record,355)  tap1, tap2
         else
            read (rcrdpar,355) tap1, tap2
         endif

  355    format(t234, 2f8.1)




         atap = abs(tap1) + 0.005
         itap = sign (atap, tap1)
         lognum = alog10(amax1(atap, 1.0)) + 4.0
         if (itap .lt. 0) lognum = lognum + 1
********************* debug stuff **********************
*     print 9871, lognum, fmttap 
*9871 format(' in postbrn at 9871, lognum, fmttap = :',i5,1x,a)  
********************* debug stuff **********************
         if (lognum .gt. 8) then
            tapa1 = '(*****.**)'
         else
            write (fmttap(7:7), 311) lognum
********************* debug stuff **********************
*     print 9811, fmttap 
*9811 format(' in postbrn, fmttap = :',a)  
********************* debug stuff **********************
            write (tapa1, fmttap) tap1 
         endif
********************* debug stuff **********************
*     print 9821, tapa1 
*9821 format(' in postbrn, tapa1 or tapa2 = :',a)  
********************* debug stuff **********************
         if (subtype .eq. 'P') then
           tapa2 = '(DEG PS)'
         else
           atap = abs(tap2) + 0.005
           itap = sign (atap, tap2)
           lognum = alog10(amax1(atap, 1.0)) + 4.0
           if (itap .lt. 0) lognum = lognum + 1
           if (lognum .gt. 8) then
              tapa2 = '(*****.**)'
           else
              write (fmttap(7:7), 311) lognum
              write (tapa2, fmttap) tap2
           endif
         endif
         write (lpost, 361) ovldflg, ovldval, karosg, ipinflg, 
     1   floa, flob, par, kotgflg
  361    format(a3, 1x, a30, 1x, i2, 1x, i8, 1x, 2(a25, 1x), a7, 1x
     1   , i1)
         write (lpost, 363) ibasepf1, ibasepf2, tapa1, tapa2
  363    format(1x, 2(1x, i5), 2(1x, a10), ' acTran')

      endif

****** begin write start of next path if processing circuits separately ********
      if (lparsw .eq. 1) then
************** debug stuff ***********************
*     print 9361, jparcor, nparpf
*9361 format(' after 361 jparcor, nparpf ', 2i5)
************** debug stuff ***********************
         if (jparcor .lt. nparpf) then
            write (lpost, 211) begpath
            go to 260
         else
*           if (kcorflg .eq. 3) then
*              kcorflg = 1
*              nxtcord = parcord(iparcor + 1)
*           endif
         endif
      endif
****** end   write start of next path if processing circuits separately ********

      if (kcorflg .eq. 3) then
         kcorflg = 1
         nxtcord = parcord(iparcor + 1)
********** debug stuff ***************************
*      print 9851, iparcor,nxtcord(1:30)
* 9851 format (' before 400: iparcor, nxtcord(1:10) ', i2, a)
********** debug stuff ***************************
      endif

      go to 1000

********* begin latent branch logic *******************
  400 if (bsopsum .eq. 1) then
         psumflo = psumflo + pin        
         qsumflo = qsumflo + qin        
      endif 
********* end   latent branch logic *******************

 1000 continue
********************* debug stuff **********************
*     print 9999
*9999 format(' exiting postbrn')
********************* debug stuff **********************
      return
      end
