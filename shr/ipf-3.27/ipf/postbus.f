C    @(#)postbus.f	20.6 7/18/96
      subroutine postbus(record)

      include 'ipfinc/postdta.inc'
      include 'ipfinc/pfstates.inc'

      character record*(*)
      character busnm*8, name*13, name2*15 ,bushape*2, 
     1          fmtnam*80, avolt*6, aiang*4,
     2          fmtpqld*30, apqload*20 

*23456789112345678921234567893123456789412345678951234567896123456789712
      data fmtnam(1:72)/
     1'(''[[ '',''('',a13,'')'','' ('',i3,1x,a4,'')'',t72,'' ]'',2(f7.2,
     2'' cmtr''),i2,'' ]'')'/
      data fmtpqld(1:24) /'('' (['',i6,'' ('',i6,'')])'')'/ 
*      12345678911234567892123456789312345678941234567895123456789612345

*************** debug stuff *******************      
*     print 9951
*9951 format(' entering postbus')
*     print 9901, record
*9901 format(' in postbus:'a)
*     print 9901, text
*     print 9901, fmtnam
*************** debug stuff *******************      

         namary = ' '
         busary = ' '
         genary = ' '
         reaary = ' '
         flgary = ' '
         apqload = ' '
         apqsum = ' '
         psumflo = 0.0
         qsumflo = 0.0

***** read bus record from coordinate file *************
      read (text,111) kdspflg, busnm, buskv, name, kvf, xb, yb,
     1xn, yn, anggn, angcp,               bushape, busscale 
  111 format(bz,1x,i1,a8,f4.0,a8,i1,4f6.2,2f3.0,a2, f4.2)
      if (bsopnam .eq. 2) then
         name = busnm(1:8) //' '//text(11:14)
      elseif (bsopnam .eq. 3) then
         name = busnm(1:8) 
      endif
      

***** if there are no name coordinate, offset name near bus symbol
      if (xn .eq. 0.0 .and. yn .eq. 0.0) then
         xn = xb + 0.38 
         yn = yb + 0.45
      endif

******keep voltage printing indicator within bounds
      if (kvf .lt. 1 .or. kvf .gt. 6) kvf = 1 

      if (kdspflg .eq. 0 .and. kotgflg .eq. 0) then
******** read bus record from powerflow file *************
         if (ostates .lt. 5) record(21:141) = ' '
         read (record, 121) pgen, qgen, volt, ang, 
     1   pload, qload, bused, bmax
  121    format(t21, 8f15.7)
              
         if (dgoptyp .eq. 5) then
             volt = buskv * 1.001
             pgen = 1.0 
             bused = 1.0 
         endif 

******** add overvoltage flag sometime ******************      
         puvolt = volt/buskv

         write (busary, 201) xb, yb, bushape, busscale, puvolt, buskv 
  201    format('[ ', 2(f6.2, ' cmtr '), ' (',a2, ') ', f7.2, f7.3, f6.1
     1   , ' ]')

         ipgen = sign ((abs(pgen) + 0.5), pgen)
         iqgen = sign ((abs(qgen) + 0.5), qgen)
         write (genary, 221) anggn, ipgen, iqgen
  221    format('[ ', f5.1, ' (',i5, ')', ' (',i5, ')', ' ]')

         ibused = sign ((abs(bused) + 0.5), bused)
*        iabused = iabs(ibused)
         iabused =      ibused 
         ibmax = sign ((abs(bmax) + 0.5), bmax)
*        iabmax = iabs(ibmax) 
         iabmax =      ibmax  
         if ( ibused .eq. 0 .and. ibmax .ne. 0) ibused = ibmax
         if (vaopdif .eq. 1) then    ! get reactor/capacitor symbol for difference
*           iabused = ibused
*           iabmax = ibmax
            if (record(141:141) .eq. '+') then
               ibused = 1.0
            else
               ibused = -1.0
            endif
         endif
         write (reaary, 241) angcp, ibused, iabmax, iabused
  241    format('[ ', f5.1, 1x, i5, ' (',i5, ')', ' (',i5, ')', ' ]') 

         ivolt =  sign ((abs(volt) + 0.5), volt)
         fvolt = abs(volt)
         if (bsopang .eq. 1) then
             iang = sign ((abs(ang) + 0.5), ang)
             write(aiang, 243) iang
  243        format(i4)
         else
             aiang = ' '
         endif 
******** adjust the format for more compact variables **********
         
************ debug stuff **********************
*     print 9923, bsopvlt
*9923 format( ' in postbus bsopvlt = ',i5) 
************ debug stuff **********************
         if (bsopvlt .eq. 1 .and. fvolt .ge. 69.0) then 
            lognum = alog10(amax1((abs(volt)+0.5),1.0)) + 1.0
            if (volt .lt. 0.0) lognum = lognum + 1
            
************ debug stuff **********************
*     print 9921, lognum
************ debug stuff **********************
            write (fmtnam(25:26), 251) lognum
  251       format('i',i1)
         elseif (bsopvlt .eq. 1 .and. fvolt .lt. 69.0) then 
            write (avolt, 253) fvolt
  253       format(f4.1)
            fmtnam(25:26) = 'a4'
         else
            write (avolt, 255) puvolt
  255       format(f6.3)
            fmtnam(25:26) = 'a6'
         endif
*****THE ENCLOSED LOGIC IS NOT CURRENTLY USED
*        lognum = alog10(amax1((abs(ang)+0.5),1.0)) + 1.0
************ debug stuff **********************
*     print 9921, lognum
************ debug stuff **********************
*        if (ang .lt. 0.0 ) lognum = lognum + 1
************ debug stuff **********************
*     print 9921, lognum
************ debug stuff **********************
*        write (fmtnam(31:32), 251) lognum
************ debug stuff **********************
*****THE ENCLOSED LOGIC IS NOT CURRENTLY USED
*     print 9901, fmtnam
*     print 9921, lognum
*9921 format('lognum = :', i5) 
************ debug stuff **********************
         if (bsopvlt .eq. 1 .and. fvolt .ge. 69.0) then 
            write (namary, fmtnam) name, ivolt, aiang, xn, yn, kvf
         else
            write (namary, fmtnam) name, avolt, aiang, xn, yn, kvf
         endif
         if (bsoplod .eq. 1 .and. 
     1       (abs(pload) .gt. 0.5 .or. abs(qload) .gt. 0.5)) then 

             ipload = sign ((abs(pload) + 0.5), pload)
             lognum = alog10(amax1((abs(pload)+0.5),1.0)) + 1.0
             if (pload .lt. 0.0) lognum = lognum + 1
             if (lognum .gt. 6) then
                 apqload = '(******)'
                 go to 270
             endif
             write (fmtpqld( 9: 9), 261) lognum
  261        format(i1)

             iqload = sign ((abs(qload) + 0.5), qload)
             lognum = alog10(amax1((abs(qload)+0.5),1.0)) + 1.0
             if (qload .lt. 0.0) lognum = lognum + 1
             if (lognum .gt. 6) then
                 apqload = '(******)'
                 go to 270
             endif
             write (fmtpqld(17:17), 261) lognum
************ debug stuff **********************
*     print 9931, fmtpqld
*9931 format(' fmtpqld: ',a)
************ debug stuff **********************
             write(apqload, fmtpqld) ipload, iqload             
************ debug stuff **********************
*     print 9941, pload, qload, ipload, iqload, apqload
*9941 format(' pload,qload, ipload, iqload, apqload ',
*    12f10.1, 2i10,a)
************ debug stuff **********************

  270        namary(33:52) = apqload(1:20)
         endif
************** debug stuff ***************************
*     print 9961, namary
*9961 format(' in postbus - namary: ',a)
************** debug stuff ***************************
         kflgbs = 1
         if (kdspflg .eq. 1) kflgbs = 0
         kflggn = 1
         if (bsopgen .eq. 0) then 
            kflggn = 0
         elseif (bsopgen .eq. 1) then 
            if ((abs(pgen) .lt. 0.1 .and. abs(qgen) .lt. 0.1)
     1      .or. anggn .eq. 0.0) kflggn = 0
         elseif (bsopgen .eq. 2) then 
            if (abs(pgen) .lt. 0.1 .and. abs(qgen) .lt. 0.1)
     1       kflggn = 0
c        elseif (bsopgen .eq. 3) then ***** statement implied *****
c          kflggn = 1                 ***** statement implied *****
         endif
************** debug stuff ***************************
*     print 9971, pgen, qgen, bsopgen, kflggn  
*9971 format('  pgen, qgen, bsopgen, kflggn:',2f7.2, 2i3) 
************** debug stuff ***************************

         kflgre = 1
         if (bsoprea .eq. 0) then
            kflgre = 0
         elseif (bsoprea .eq. 1) then 
            if ((abs(bused) .lt. 0.1 .and. abs(bmax) .lt. 0.1)
     1      .or. angcp .eq. 0.0) kflgre = 0
         elseif (bsoprea .eq. 2) then 
            if (abs(bused) .lt. 0.1 .and. abs(bmax) .lt. 0.1)
     1       kflgre = 0
c        elseif (bsoprea .eq. 3) then ***** statement implied *****
c          kflgre = 1                 ***** statement implied *****
         endif

         kflgnm = 1
         if (name .eq. ' ') kflgnm = 0
         write (flgary,281) kflgbs, kflggn, kflgre, kflgnm
  281    format('[ ', 4i2, ' ]', ' acBus')
 
      elseif (kdspflg .eq. 1 .or. kotgflg .eq. 1) then
         name2 = ' '
         if (name .ne. ' ') name2 = '('//name//')' 
         write (namary, 301) name2, xn, yn
  301    format('[ [ ', a15, ' ] '
     1   , 2(f6.2, ' cmtr ') , ' 5 ]')
         genary = '[]'
         reaary = '[]' 
         if (kotgflg .eq. 1) then
           busscale = 0.0
           kbovflg = 0
           bushape = 'XX'
           write (busary, 201) xb, yb, bushape, busscale, kbovflg
           flgary = '[1 0 0 1] acBus'
         else  
           busary = '[ 0 0 (  ) 0 0 0 ]' 
           flgary = '[0 0 0 1] acBus'
         endif 


      endif


      return
      end

