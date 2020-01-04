C    @(#)postcord.f	20.3 2/13/96
      subroutine postcord(record)
      character record*(*)

      include 'ipfinc/postdta.inc'
      include 'ipfinc/pasprm.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pfstates.inc'

      character orient*1, ornew*1, trnsp*1
      character msgid2b*2

      save

      data  xsize, ysize / 21.59, 27.94/
      data  orient /'P'/
      data  xscale, yscale / 1.0, 1.0/
      data  zero / 0.1 /

********* debug stuff *****************
*     print 9901, record
*9901 format('entering postcord :',a)
********* debug stuff *****************
****** The POSTCORD entry point initializes OPTIONS values *****
      xoff = 0.0
      yoff = 0.0
      dgoptyp = 1
      dgopout = 0
      vaopdif = 0
      bsopnam = 1
      bsopvlt = 1
      bsopang = 1 
      bsopgen = 1 
      bsoprea = 1
      bsoplod = 0
      bsopsum = 0
      broppsn = 1 
      bropqsn = 1 
      bropprc = 0 
      bropqrc = 0
      broptap = 0
      bropcmp = 1
      broppar = 0
      trnsp = 'T'
      ssophdr = 0
      ssoppfc = 0

***** set flags for printing options
      lorient = 0
      lscale = 0 
      loff = 0
      lbord = 0
      lbox = 0
      lcase = 0
      lcomt = 0
      lcrdflg = 0
      lsize = 0
      llgnd = 0 
 
      return

******************** ENTRY POINT **********************

      entry postoptn(record) 
************** debug stuff **************************      
*     print 9911, record
*9911 format(' POSTOPTN RECORD :', a80)
************** debug stuff **************************      

      call uscan(record, prsmsg, nwrds, '@', ' ,=')
************** debug stuff **************************      
*     print 9921, nwrds
*9921 format(' in postcord, nwrds = :',i2 )
************** debug stuff **************************      
      kount = 2
******** ignore records (eg. from gui) that have only the msgid field***
  200 if (kount .ge. nwrds) go to 1000
      msgid = capital(prsmsg(kount)(1:2))
           
************** begin testing for each OPtion ***********
      if ( msgid .eq. 'OR') then 
         ornew = capital(prsmsg(kount+1)(1:1))
         if (ornew .eq. 'P' .or. ornew .eq. 'L')then
            if (ornew .eq. 'L')then
               lorient = 1
*              write (lpost, 211) xsize
  211          format( f7.2,' cmtr 0.0 translate 90.0 rotate' ,/,
     1         '/P-L-orient (L) def')
            endif
            orient = ornew
            kount = kount + 2
         endif 

      elseif ( msgid .eq. 'TR') then
         if (nwrds .lt. (kount + 1)) go to 400
         trnsp = capital(prsmsg(kount+1)(1:1))
         if (trnsp .eq. 'T' .or. trnsp .eq. 'O')then
            kount = kount + 2
         else 
            trnsp = 'T' 
         endif



      elseif (msgid .eq. 'SC') then
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xsc
         read(prsmsg(kount+2),'(f10.0)',err=400) ysc
         if (xsc .gt. 10.0 .or. xsc .lt. 0.1) xsc = 1.0
         if (ysc .gt. 10.0 .or. ysc .lt. 0.1) ysc = 1.0
         lscale = 1
*        write (lpost,221) xsc, ysc 
  221    format(2f7.2, ' setScale')
         xscale = xscale * xsc
         yscale = yscale * ysc
         kount = kount + 3

      elseif (msgid .eq. 'OF') then 
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xoff
         read(prsmsg(kount+2),'(f10.0)',err=400) yoff
         loff = 1
*        write (lpost, 231) xoff, yoff
  231    format(2(f7.2, ' cmtr '), ' translate')
         zero = 0.0
         kount = kount + 3

      elseif (msgid .eq. 'BO') then
         read(prsmsg(kount+1),'(f10.0)',err=400) xbord 
         read(prsmsg(kount+2),'(f10.0)',err=400) ybord
         if (nwrds .lt. (kount + 2)) go to 400
         brdrzro = 0.0
         if (abs(xoff) .lt. .01 .and. abs(yoff) .lt. .01)then
           kofsflg = 1
         else
           kofsflg = 0
         endif
         if (xbord .gt. 0.01 .and. ybord .gt. 0.01) lbord = 1
*        write (lpost, 241) xbord, ybord, trnsp, kofsflg 
  241    format( 2(1x, f7.2, ' cmtr '), '(',a1,')', i3, ' border')
         zero = 0.0
         kount = kount + 3

      elseif (msgid .eq. 'BX') then
***************** debug stuff *****************
*     print 9931, nwrds, kount
*9931    format(' in postcord - nwrds, kount :', 2i5)
***************** debug stuff *****************
***** There should really be 4 values passed
***** this logic overrides an error in the GUI that sometimes sends
***** onlythe xbox and ybox coordinates 
         if (nwrds .lt. (kount + 2)) go to 400
         if (nwrds .eq. (kount + 3)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xbox
         read(prsmsg(kount+2),'(f10.0)',err=400) ybox
         if (nwrds .lt. (kount + 4)) then
             xboxlr = 0.0
             yboxlr = 0.0
             kount = kount + 3
         else 
             read(prsmsg(kount+3),'(f10.0)',err=400) xboxlr
             read(prsmsg(kount+4),'(f10.0)',err=400) yboxlr
             kount = kount + 5
         endif
         lbox = 1
         blrplate = .true.
*        write (lpost, 251) xbox, ybox, xboxlr, yboxlr
  251    format( 4(1x, f7.2, ' cmtr '), ' boxLoc')


      elseif (msgid .eq. 'CA') then
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xcase
         read(prsmsg(kount+2),'(f10.0)',err=400) ycase
         lcase = 1
*        write (lpost, 256) xcase, ycase
  256    format( 2(1x, f7.2, ' cmtr '), ' caseLoc')
         kount = kount + 3



      elseif (msgid .eq. 'CO') then
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xcomt
         read(prsmsg(kount+2),'(f10.0)',err=400) ycomt
         lcomt = 1
*        write (lpost, 261) xcomt, ycomt
  261    format( 2(1x, f7.2, ' cmtr '), ' cmntLoc')
         kount = kount + 3

      elseif (msgid .eq. 'CR') then
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xcord
         read(prsmsg(kount+2),'(f10.0)',err=400) ycord
         lcrdflg = 1
*        write (lpost, 271) xcord, ycord
  271    format( 2(1x, f7.2, ' cmtr '), ' cordLoc')
         kount = kount + 3


      elseif (msgid .eq. 'SI') then
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xsize
         read(prsmsg(kount+2),'(f10.0)',err=400) ysize
         lsize = 1
*        write (lpost, 281) xsize, ysize
  281    format( 2(1x, f7.2, ' cmtr '), ' sizeLoc')
         kount = kount + 3


      elseif (msgid .eq. 'LG') then
         if (nwrds .lt. (kount + 2)) go to 400
         read(prsmsg(kount+1),'(f10.0)',err=400) xlgnd
         read(prsmsg(kount+2),'(f10.0)',err=400) ylgnd
         llgnd = 1
*        write (lpost, 291) xlgnd, ylgnd
  291    format( 2(1x, f7.2, ' cmtr '), ' legndLoc')
         kount = kount + 3



      elseif (msgid .eq. 'DI') then
         if (nwrds .lt. (kount + 1)) go to 400
         msgid2 = capital(prsmsg(kount+1)(1:1))
         if     (msgid2 .eq. 'P') then
            dgoptyp = 1
         elseif (msgid2 .eq. 'M') then
            dgoptyp = 2
         elseif (msgid2 .eq. 'L') then
            dgoptyp = 3
         elseif (msgid2 .eq. 'I') then
            dgoptyp = 4
         elseif (msgid2 .eq. 'C') then
            dgoptyp = 5
         elseif (msgid2 .eq. '%') then
            dgoptyp = 6
         else
            print 901, msgid2, record 
         endif
         kount = kount + 2


      elseif (msgid .eq. 'VA') then
         if (nwrds .lt. (kount + 1)) go to 400
         msgid2 = capital(prsmsg(kount+1)(1:1))
         if     (msgid2 .eq. 'N') then
            vaopdif = 0
         elseif (msgid2 .eq. 'D') then
            vaopdif = 1
          else
            print 901, msgid2, record
         endif
         kount = kount + 2
 

      elseif (msgid .eq. 'FL') then
         msgid2 = capital(prsmsg(kount+1)(1:3))
         msgid2a= capital(prsmsg(kount+1)(1:6))
         if     (msgid2 .eq. 'P_S') then
            broppsn = 1
         elseif (msgid2a .eq. 'NO_P_S') then
            broppsn = 0
         elseif (msgid2 .eq. 'Q_S') then
            bropqsn = 1
         elseif (msgid2a .eq. 'NO_Q_S') then
            bropqsn = 0
         elseif (msgid2 .eq. 'P_R') then
            bropprc = 1
         elseif (msgid2a .eq. 'NO_P_S') then
            bropprc = 0
         elseif (msgid2 .eq. 'Q_R') then
            bropqrc = 1
         elseif (msgid2a .eq. 'NO_Q_R') then
            bropqrc = 0
            
         else
            print 901, msgid2, record 
         endif
         kount = kount + 2


      elseif (msgid .eq. 'BU') then
         msgid2 = capital(prsmsg(kount+1)(1:1))
         msgid2a= capital(prsmsg(kount+1)(1:4))
         msgid2b= capital(prsmsg(kount+1)(1:2))
*************** debug stuff ********************************
*     print 9933, msgid, msgid2, msgid2a
*9933 format (' in postcord msgid, msgid2, msgid2a ', 3a) 
*************** debug stuff ********************************
         if     (msgid2 .eq. 'B') then
            msgid3 = capital(prsmsg(kount+2)(1:1)) 
            if     (msgid3 .eq. 'A') then
               bsopnam = 1 
            elseif (msgid3 .eq. 'P') then
               bsopnam = 2 
            elseif (msgid3 .eq. 'N') then
               bsopnam = 3 
            else
               print 901, msgid3, record
            endif
            kount = kount + 1 
         elseif (msgid2 .eq. 'V') then
            msgid3 = capital(prsmsg(kount+2)(1:1)) 
            if     (msgid3 .eq. 'K') then
               bsopvlt = 1 
            elseif (msgid3 .eq. 'P') then
               bsopvlt = 2 
            else
               print 901, msgid3, record
            endif
            kount = kount + 1 
         elseif (msgid2 .eq. 'A' .and. msgid2b .ne. 'AL') then
            bsopang = 1
         elseif (msgid2a .eq. 'NO_A') then
            bsopang = 0
         elseif (msgid2 .eq. 'G') then
            bsopgen = 1
         elseif (msgid2a .eq. 'NO_G') then
            bsopgen = 0
         elseif (msgid2a .eq. 'MO_G') then
            bsopgen = 2
         elseif (msgid2a .eq. 'AL_G') then
            bsopgen = 3
         elseif (msgid2 .eq. 'S') then
            bsoprea = 1
         elseif (msgid2a .eq. 'NO_S') then
            bsoprea = 0
         elseif (msgid2a .eq. 'MO_S') then
            bsoprea = 2
         elseif (msgid2a .eq. 'AL_S') then
            bsoprea = 3
         elseif (msgid2 .eq. 'L') then
            bsoplod = 1
         elseif (msgid2a .eq. 'NO_L') then
            bsoplod = 0
         elseif (msgid2 .eq. 'T') then
            bsopsum = 1
         elseif (msgid2a .eq. 'NO_T') then
            bsopsum = 0
         elseif (msgid2 .eq. 'O') then
            dgopout = 1
         elseif (msgid2a .eq. 'NO_O') then
            dgopout = 0
         else
            print 901, msgid2a, record 
         endif
         kount = kount + 2


      elseif (msgid .eq. 'BR') then
         msgid2 = capital(prsmsg(kount+1)(1:1))
         msgid2a= capital(prsmsg(kount+1)(1:4))
         if     (msgid2 .eq. 'T') then
            broptap = 1 
         elseif (msgid2a .eq. 'NO_T') then
            broptap = 0 
         elseif (msgid2 .eq. 'C') then
            bropcmp = 1 
         elseif (msgid2a .eq. 'NO_C') then
            bropcmp = 0 
         elseif (msgid2 .eq. 'O') then
            dgopout = 1
         elseif (msgid2a .eq. 'NO_O') then
            dgopout = 0
         elseif (msgid2 .eq. 'P') then
            msgid3 = capital(prsmsg(kount+2)(1:1)) 
            kount = kount + 1
            if (msgid3 .eq. 'S') then
               broppar = 1
            elseif (msgid3 .eq. 'C') then
               broppar = 0 
            else
               print 901, msgid3, record
            endif
         else
            print 901, msgid2a, record
         endif
         kount = kount + 2

**************** Begin-Site Specific options ******************
      elseif (msgid .eq. 'SS') then
         msgid2 = capital(prsmsg(kount+1)(1:1))
         if     (msgid2 .eq. 'H') then
            if (nwrds .lt. kount+2) go to 400
            read(prsmsg(kount+2),'(bn,i5)',err=400)ssophdr            
         elseif (msgid2 .eq. 'C') then
            if (nwrds .lt. kount+2) go to 400
            read(prsmsg(kount+2),'(bn,i5)',err=400)ssoppfc            
         else
            print 901, msgid2a, record
         endif
         kount = kount + 3
************** debug stuff **************************
*     print 9943, ssophdr, ssoppfc 
*9943    format ('at 9943', 2o20)
************** debug stuff **************************
**************** End-Site Specific options ******************


      elseif (msgid .eq. 'EN') then
         kount = kount + 2
************** debug stuff **************************
*     print 9941,  bropcmp, broptap, broppsn, bropqsn, bropprc, bropqrc
*9941    format ('at 301', 6o20)
************** debug stuff **************************
         write (lpost,301) bropcmp, broptap, 
     1   broppsn, bropqsn, bropprc, bropqrc
  301    format(6i2, ' setOpt1')
         if (ostates .lt. 2) dgoptyp = 5 
         write (lpost,311) dgoptyp, vaopdif 
  311    format(2i2, ' setOpt2')
         write (lpost,321) ssophdr, ssoppfc 
  321    format(2i3, ' setOpt3')
         if (lorient .eq. 1) write (lpost, 211) xsize
         if (lscale  .eq. 1) write (lpost,221) xsc, ysc
         if (loff    .eq. 1) write (lpost, 231) xoff, yoff
         if (lbord   .eq. 1)
     1      write (lpost, 241) xbord, ybord, trnsp, kofsflg
         if (lbox    .eq. 1)
     1      write (lpost, 251) xbox, ybox, xboxlr, yboxlr
         if (lcase   .eq. 1) write (lpost, 256) xcase, ycase
         if (lcomt   .eq. 1) write (lpost, 261) xcomt, ycomt
         if (lcrdflg   .eq. 1) write (lpost, 271) xcord, ycord
         if (lsize   .eq. 1) write (lpost, 281) xsize, ysize
         if (llgnd   .eq. 1) write (lpost, 291) xlgnd, ylgnd

      else

         print 901, msgid, record 
  901    format(' Unrecognized Option in Coordinate File ',/,
     1'parameter = ',a10, ' record = ', a80) 
         kount = kount + 1
      endif
      go to 200

  400 write (errbuf(1), 401) record(1:80)
  401 format (' Incomplete processing of record:',a)
      call prterx ('W',1)

 1000 continue
      return
      end
