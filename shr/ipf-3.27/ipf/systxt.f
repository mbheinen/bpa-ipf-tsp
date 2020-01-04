C    @(#)systxt.f	20.3 2/13/96
        subroutine systxt
C
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/com012.inc'
        include 'ipfinc/data.inc'
        include 'ipfinc/merge.inc'
        include 'ipfinc/prt.inc'
C
        dimension word(40)
 
        character word*30
C
C       DETERMINE TEXT COMMAND
C
        idat = 0
  100   call readtx
        if (index('/(',card).ne.0) go to 710
        call scan( buf,word,nwrd )
        iwrd = 1
        islnsw = ickdic( word(iwrd),mrgdic,lmrg )
        if (islnsw.eq.0) then
           write (errbuf(1),130) buf(1:80)
  130      format (' MERGE COMMAND NOT RECOGNIZED :(',a,')')
           call prterx ('W',1)
           inptls=1
           idat = 0
           go to 100
        endif
C
C              IPR,SAR,SZN,SBA,SBU,IBU,EBU,IBR,RNB,EXBR,USE
C
        go to (210,230,150,560,400,480,480,310,630,10310,140) islnsw
C
C       > USE_AIC <
C
  140   comvar(9) = 1
        inptls = 1
        idat = 0
        go to 100
C
C       > SAVE_ZONES, ...  <
C
  150   idatx = nwrd
        idat=0
        iwrd = iwrd + 1
        if (idatx .eq. 1) go to 190
  152   continue
        do 160 i = iwrd, idatx
           if (word(i)(1:1).eq.'#') word(i)(1:1)=' '
           if (word(i)(2:2).eq.'#') word(i)(2:2)=' '
           idat = idat + 1
           if (idat .gt. MAXCZN) then
              write (errbuf(1),170)
  170         format (' MORE THAN 150 "SAVED ZONES" SUBMITTED.  ',
     1               'REMAINING DATA ITEMS IGNORED. ')
              call prterx ('W',1)
              idat=150
              ksw=1
              goto 162
           endif
           savzns(idat) = word(i)(1:2)
  160   continue
  162   continue
  175   if(index(buf,'<').eq.0.and.index(word(idatx),'.').eq.0) then
           call readtx
           call scan( buf, word, idatx)
           iwrd = 1
           go to 152
        endif
        inptls=1
  190   if (idat.gt.0) go to 720
        write (errbuf(1),200)
  200   format (' NO DATA ENCOUNTERED FROM "SAVE ZONES".')
        call prterx ('W',1)
        go to 100
C
C       >INTERFACE_Preference = COMParison<
C                             = REJECT<
C                             = ACCEPT<
C
  210   continue
        iwrd = iwrd + 1
        savbas(1)=1.0
        if( iwrd .le. nwrd ) then
           if( ickdic( word(iwrd),'ACCEPT',1).eq.1)  savbas(1)=2.0
           if( ickdic( word(iwrd),'REJECT',1).eq.1)  savbas(1)=0.0
        endif
        inptls=1
        go to 720
C
C       > SAVE_AREAS <
C
  230 	ksw=0
      	inptls=1
  240 	call readtx
      	if (card.ne.'A') go to 290
      	if (ksw.eq.0) go to 250
      	inptls=1
      	go to 240
  250 	idat=idat+1
      	if (idat.le.MAXCAR) go to 270
      	write (errbuf(1),260)
  260 	format (' MORE THAN ',i3,' "SAVE AREAS". REMAINING DATA ITEMS',
     1        'IGNORED. ') 
      	call prterx ('W',1)
      	idat=MAXCAR
      	ksw=1
      	go to 240
  270 	read (buf,280) savare(idat)
  280 	format (3x,a10)
      	inptls=1
      	go to 240
  290 	if (idat.gt.0) go to 720
      	write (errbuf(1),300)
  300 	format (' NO DATA ENCOUNTERED FROM "SAVE AREAS".')
      	call prterx ('W',1)
      	go to 100
C
C     	> INTERFACE BRANCHES <
C
  310 	ksw=0
      	inptls=1
  320 	call readtx
      	if (index('LTE',card).eq.0) go to 380
      	if (index(buf,'EXCLUDE').ne.0) go to 380
 
  330 	if (ksw.eq.0) go to 340
      	inptls=1
      	go to 320
 
  340 	idat=idat+1
      	if (idat.le.100) go to 360
 
      	write (errbuf(1),350)
  350 	format (' MORE THAN 100 "INTERFACE BRANCHES". ',
     1  	'REMAINING DATA ITEMS IGNORED. ')
	call prterx ('W',1)
      	idat=100
      	ksw=1
      	go to 320
 
  360 	read (buf,370) (facbus(i,idat),facbas(i,idat),i=1,2)
  370 	format (bz, 6x, a8, f4.0, 1x, a8, f4.0)
      	inptls=1
      	go to 320
 
  380 	if (idat.gt.0) go to 720
      	write (errbuf(1),390)
  390 	format (' NO DATA ENCOUNTERED FROM "INTERFACE BRANCHES".')
      	call prterx ('W',1)
      	go to 100
C
C  	> EXCLUDE BRANCHES <
C
10310 	ksw=0
      	inptls=1
 
10320 	call readtx
      	if (index('LTE',card).eq.0) go to 10380
      	if (index(buf,'EXCLUDE').ne.0) go to 10380
 
10330 	if (ksw.eq.0) go to 10340
      	inptls=1
      	go to 10320
 
10340 	idat=idat+1
      	if (idat.le.100) go to 10360
      	write (errbuf(1),10350)
10350 	format (' MORE THAN 100 EXCLUDED BRANCES. REMAINING ITEMS',
     1     'ignored.')
      	call prterx ('W',1)
      	idat=100
      	ksw=1
      	go to 10320
 
10360 	read (buf,10370) (facbus(i,idat),facbas(i,idat),i=1,2)
10370 	format (bz, 6x, a8, f4.0, 1x, a8, f4.0)
      	inptls=1
      	go to 10320
 
10380 	if (idat.gt.0) go to 720
      	write (errbuf(1),10390)
10390 	format (' NO DATA ENCOUNTERED FROM "EXCLUDE BRANCHES".')
      	call prterx ('W',1)
      	go to 100
 
C	> SAVE BUSES <
 
  400 	ksw=0
      	inptls=1
 
  410 	call readtx
      	if (card.ne.'B') go to 460
      	if (ksw.eq.0) go to 420
      	inptls=1
      	go to 410
 
  420 	idat=idat+1
      	if (idat.le.500) go to 440
      	write (errbuf(1),430)
  430 	format (' MORE THAN 500 "SAVE BUSES" ENCOUNTERED.  REMAINING',
     1     'data items ignored. ')
      	call prterx ('W',1)
      	ksw=1
      	go to 410
 
  440 	read (buf,450) savbus(idat),savbas(idat)
  450 	format (bz, 6x, a8, f4.0)
      	inptls=1
      	go to 410
 
  460 	if (idat.gt.0) go to 720
      	write (errbuf(1),470)
  470 	format (' NO DATA ENCOUNTERED FROM "SAVE BUSES".')
      	call prterx ('W',1)
      	go to 100
C
C       > INCLUDE BUSES <
C     	> EXCLUDE BUSES <
C
  480 	ksw=0
      	inptls=1
 
  490 	call readtx
      	if (card.ne.'B') go to 540
      	if (ksw.eq.0) go to 500
      	inptls=1
      	go to 490
 
  500 	idat=idat+1
      	if (idat.le.100) go to 520
      	write (errbuf(1),510)
  510 	format (' More than 100 "INCLUDE" or "EXCLUDE" ',
     1          'buses encountered. Remaining items ignored.')
      	call prterx ('W',1)
      	ksw=1
      	idat=100
	go to 490
 
  520 	read (buf,530) savbus(idat),savbas(idat)
  530 	format (bz, 6x, a8, f4.0)
      	inptls=1
      	go to 490
  540 	if (idat.ne.0) go to 720
      	write (errbuf(1),550)
  550   format (' No data encountered from "INCLUDE BUSES" or "EXCLUDE B
     1USES".')
       	call prterx ('W',1)
      	go to 100
C
C     	PROCESS "SAVE BASES"
C
  560   ksw=0
        idatx = nwrd
        idat=0
        iwrd = iwrd + 1
 
  570   continue
        if( idatx .eq. 1) go to 610
 
        do 572 i = iwrd, idatx
           idat = idat + 1
           savbas( idat ) = rval(word(i))
  572   continue
 
  574   continue
        if (idat .gt. 50) then
           write (errbuf(1),580)
  580      format (' MORE THAN 50 "SAVED BASES" ENCOUNTERD. REMAINING',
     1             'data items ignored.')
           call prterx ('W',1)
           idat=50
           ksw=1
           go to 570
        endif 
        if (index (buf,'<').ne.0) then
           inptls=1
           call readtx
           if (card .eq. ' ') then
              call scan( buf,word,idatx)
              iwrd = 1
              go to 570
           endif
        endif 
  610	if (idat.gt.0) go to 720
        write (errbuf(1),620)
  620   format (' NO DATA ENCOUNTERED FROM "SAVE BASES". ')
        call prterx ('W',1)
        go to 100
C
C     	PROCESS "RENAME BUSES"
C
  630 	ksw=0
      	inptls=1
  640 	call readtx
      	if (card.ne.'B') go to 690
      	if (ksw.eq.0) go to 650
      	inptls=1
      	go to 640
 
  650 	idat=idat+1
      	if (idat .gt. 100) then
      	   write (errbuf(1),660)
  660      format (' More than 100 "RENAME BUSES" encountered.  Remainin
     1g data items ignored.')
           call prterx ('W',1)
      	   idat=100
      	   ksw=1
       	   go to 640
        endif
        read (buf,680) (facbus(i,idat),facbas(i,idat),i=1,2)
  680 	format (bz, 6x, a8, f4.0, 1x, a8, f4.0)
      	inptls=1
      	go to 640
  690 	if (idat.gt.0) go to 720
      	write (errbuf(1),700)
  700 	format (' NO DATA ENCOUNTERED FROM "RENAME BUSES".')
      	call prterx ('W',1)
      	go to 100
  710 	islnsw=0
  720 	return
      	end
