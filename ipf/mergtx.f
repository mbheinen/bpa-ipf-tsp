C    @(#)mergtx.f	20.5 2/26/96
      subroutine mergtx
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/data.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red2.inc'
 

      integer znsrch, arsrch, bsrch2, find_bus, ldata(500), ptr
      external kpzone, spzone, kparea, sparea, kpbase, spbase,
     &         kpbrch, spbrch
      character zn*2
 
C     Set "DELETED BUS" flag

      kerrsw = 0
      bus(ntot+1)=srtlst

C     Establish connection matrix: add 2-terminal dc to y-matrix

      do i = 1, kdtot  
         k1 = dc2t(1,i)   
         k2 = dc2t(3,i)   
         kt = inp2opt(k1)
         mt = inp2opt(k2)

C        Add branch (KT,MT) to y-matrix   

         do isw = 1, 2
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               if (ikmu(l+ln) .eq. mt) go to 102
            enddo
c
c           Appending an entity to km() requires relocating branch
c           list to end of km().
c
            if (yptr + ls + 1 .ge. MAXYE) then
               write (errbuf(1), 100) MAXYE
  100          format(' More than ', i5, ' Y-matrix entities')
               call prterx ('F',1)                            
               kerrsw = kerrsw + 1
               go to 9000
            endif

            do l = 1, ls
               ikmu(l+yptr) = ikmu(l+ln)
            enddo
            ikmu(ls+yptr+1) = mt
            km(kt) = yptr + 1
            kmlen(kt) = ls + 1
            yptr = yptr + ls + 1

  102       continue
            kt = inp2opt(k2)    ! swap kt and mt
            mt = inp2opt(k1)
         enddo
      enddo

C     Add N-terminal d-c to y-matrix

      do i = 1, mtdcln   
         k1 = dcmtln(1,i)
         k2 = dcmtln(2,i)
         kt = inp2opt(k1)
         mt = inp2opt(k2)

         do isw = 1, 2
            ln = km(kt) - 1
            ls = kmlen(kt)
            do l = 1, ls
               if (ikmu(l+ln) .eq. mt) go to 104
            enddo
c
c           Appending an entity to km() requires relocating branch
c           list to end of km().
c
            if (yptr + ls + 1 .ge. MAXYE) then
               write (errbuf(1), 100) MAXYE
               call prterx ('F',1)                            
               kerrsw = kerrsw + 1
               go to 9000
            endif

            do l = 1, ls
               ikmu(l+yptr) = ikmu(l+ln)
            enddo
            ikmu(ls+yptr+1) = mt
            km(kt) = yptr + 1
            kmlen(kt) = ls + 1
            yptr = yptr + ls + 1

  104       continue
            kt = inp2opt(k2)    ! swap kt and mt
            mt = inp2opt(k1)
         enddo
      enddo
C       
C     DESCRIPTION OF "IKK" ARGUMENTS
C       
C     ARGUMENT  ATTRIBUTES  DEFINITION  
C       
C        1         -1       BUS IS SPECIFICALLY IGNORED 
C        1          0       BUS IS IN ELIMINATED SUBSYSTEM  
C                   1       BUS IS IN RETAINED SUBSYSTEM
C       
C        2         0/1      BUS IS NOT/IS SPECIFICALLY CLASSIFIED   
C       
C        3        IFCSW     INTERFACE LEVEL:
C                             0 - NONE  
C                             1 - CONDITIONAL   
C                             2 - UNCONDITIONAL 
C       
C        4        ISYSTM    BUS CLASSIFIED BY SAVE COMMAND SUBSYSTEM
C                           NUMBER  
C       
C     DESCRIPTION OF "IKKIND" ARGUMENTS 
C       
C     ARGUMENT ATTRIBUTE DEFINITIONS
C       
C        1         IS       STARTING BRANCH INDEX   
C        2         IL       BRANCH STRING LENGTH
C       
      itot=1
      do kt=1,ntot  
         k=inp2opt(kt)   
         jtot=0
         if (kmlen(k) .gt. 0) then
            do l = km(k), km(k)-1+kmlen(k)      
               mt = ikmu(l)                                 
               kolum(itot+jtot)=opt2inp(mt)
               jtot=jtot+1   
            enddo
         endif
         if (bus(kt) .eq. '~~~~~~~~') then
            ikk(1,kt)=-1
         else 
            ikk(1,kt)=0   
         endif
         ikk(2,kt)=0   
         ikk(3,kt)=0   
         ikk(4,kt)=0   
         ikkind(1,kt)=itot 
         ikkind(2,kt)=jtot 
         itot=itot+jtot
      enddo

      if (itot .gt. MAXYE) then
         write (errbuf(1),122) itot,MAXYE  
  122    format ('TOTAL BRANCHES ( INCLUDING TRANSPOSES ) IN', 
     1        'SYSTEM IS ',i5,'.  LIMIT IS ',i5,'. ')   
         call prterx ('F',1)   
         kerrsw = kerrsw + 1   
         go to 9000
      endif
C       
C     BEGIN TEXT COMMAND LOOP   
C       
      itface = 0
      itname = 0
      isystm = 0
      ifcsw = 1 
C       
C     Read next record  
C       
      inptls = 1
      call readtx   
      inptls = 0
      call systxt   

      do while (islnsw .gt. 0) 
         go to (140,270,150,210,400,460,520,580,810,10580) islnsw  
C       
C        PROCESS "SAVE INTERFACE"  
C       
  140    ifcsw=savbas(1)   
         go to 900 
C       
C        PROCESS "SAVE ZONES"  
C       
  150    if (idat .gt. 1) call qiksrt (1,idat,kpzone,spzone)   
         isystm=isystm+1   
         do i=1,idat   
            ldata(i)=0
         enddo
         do kt=1,ntot  
            zn=zone(kt)   
            i=znsrch(zn)  
            if (i .gt. 0 .and. ikk(1,kt) .ge. 0) then
               ikk(1,kt)=1
               ikk(2,kt)=1
               ikk(3,kt)=ifcsw
               ikk(4,kt)=isystm   
               ldata(i)=ldata(i)+1
            endif 
         enddo
         write (outbuf,180) isystm,ifcsw   
  180    format ('0 SUBSYSTEM ',i2,'INTERFACE LEVEL ',i2,' ',  
     1        '- "SAVE ZONES" WITH COMPOSITE POPULATIONS:') 
         call prtout(1)
         call space(1) 
         do i=1,idat,10
            n=min0(10,idat-i+1)   
            write (outbuf,190) (savzns(i+j-1),ldata(i+j-1),j=1,n) 
  190       format (10(4x,a2,' -',i4))
            call prtout(1)
         enddo
         call space(1) 
         do i = 1,idat 
            if (ldata(i) .eq. 0) then
               write (errbuf(1),202) savzns(i)
  202          format ('0 DATA ERROR -- ZONE (',a2,
     &            ') IS NOT IN SYSTEM. ZONE IGNORED. ')
               call prterx ('W',1)
            endif
         enddo
         go to 900
C
C        PROCESS "SAVE BASES"
C
  210    if (idat .gt. 1) call qiksrt (1,idat,kpbase,spbase)
         isystm=isystm+1
         do i=1,idat
            ldata(i)=0
         enddo
         do kt=1,ntot  
            i=bsrch2(base(kt))
            if (i .gt. 0 .and. ikk(1,kt) .ge. 0) then 
               ikk(1,kt)=1 
               ikk(2,kt)=1 
               ikk(3,kt)=ifcsw 
               ikk(4,kt)=isystm
               ldata(i)=ldata(i)+1 
            endif  
         enddo
         write (outbuf,240) isystm,ifcsw   
  240    format ('0 SUBSYSTEM ',i2,'INTERFACE LEVEL ',i2,' ',  
     1        ' -"SAVE BASES" WITH COMPOSITE POPULATIONS:') 
         call prtout(1)
         call space(1) 
         do i=1,idat,8 
            n=min0(8,idat-i+1)
            write (outbuf,250) (savbas(i+j-1),ldata(i+j-1),j=1,n) 
  250       format (8(4x,f6.1,' -',i4))   
            call prtout(1)
         enddo
         call space(1) 
         do i = 1,idat 
            if (ldata(i) .eq. 0) then
               write (errbuf(1),262) savbas(i)   
  262          format ('BASE (',f6.1,') IS NOT IN SYSTEM. BASE IGNORED. 
     &')   
               call prterx ('W',1)   
            endif
         enddo
         go to 900 
C       
C        PROCESS "SAVE AREAS"  
C        
  270    if (idat .gt. 1) call qiksrt (1,idat,kparea,sparea)   
         isystm=isystm+1   
         if (ntotc .gt. 0) go to 290   
         write (errbuf(1),280) 
  280    format ('BASE SYSTEM HAS NO AREA INTERCHANGE DATA.')  
         call prterx ('W',1)   
         kerrsw = kerrsw + 1   
         go to 390 

  290    do i=1,idat   
            ldata(i)=0
         enddo
         do kt=1,ntot  
            j=jarzn(kt)   
            i = arsrch (arcnam(j))
            if (i .gt. 0 .and. ikk(1,kt) .ge. 0) then
               ikk(1,kt)=1
               ikk(2,kt)=1
               ikk(3,kt)=ifcsw
               ikk(4,kt)=isystm   
               ldata(i)=ldata(i)+1
            endif 
         enddo

         write (outbuf,360) isystm,ifcsw   
  360    format ('0 SUBSYSTEM ',i2,' INTERFACE LEVEL ',i2,' ', 
     1        ' -"SAVE AREAS" WITH COMPOSITE POPULATIONS:') 
         call prtout(1)
         call space(1) 
         do i=1,idat,6 
            n=min0(6,idat-i+1)
            write (outbuf,370) (savare(i+j-1),ldata(i+j-1),j=1,n) 
  370       format (6(4x,a10,' -',i4))
            call prtout(1)
         enddo
         call space(1) 
         do i = 1,idat 
            if (ldata(i) .eq. 0) then
               write (errbuf(1),382) savare(i)   
  382          format ('AREA (',a10,') IS NOT IN SYSTEM. AREA IGNORED. '
     &)
               call prterx ('W',1)   
            endif
         enddo
  390    continue  
         go to 900 
C       
C        PROCESS "SAVE BUSES"  
C       
  400    isystm=isystm+1   
         do l=1,idat   
            kt = find_bus(savbus(l),savbas(l))
            if (kt .le. 0) then
               write (errbuf(1),410) savbus(l),savbas(l)
  410          format ('0 "SAVE BUS" (',a8,f6.1,
     &            ') IS NOT IN SYSTEM. BUS IGNORED.')
               call prterx ('W',1)
            else
               if (ikk(1,kt) .eq. -1) then
                  write (errbuf(1),420) savbus(l),savbas(l)
  420             format ('0 "SAVE BUS" (',a8,f6.1,
     &              ') IS SPECIFICALLY "EXCLUDED" BUS IGNORED.')
                  call prterx ('W',1)
               else 
                  if (ikk(1,kt) .eq. 1 .and. ikk(4,kt) .ne. isystm) then
                     write (errbuf(1),430) savbus(l),savbas(l)
  430                format ('0 REDUNDANT DATA: "SAVE BUS" (',a8,f6.1,
     &                 ') IS ALREADY RETAINED. ')
                     call prterx ('W',1)
                  endif
               endif
               ikk(1,kt)=1
               ikk(2,kt)=1
               ikk(3,kt)=ifcsw
               ikk(4,kt)=isystm
            endif
         enddo
         go to 900
C
C        PROCESS "INCLUDE BUSES"
C
  460    do l=1,idat
            kt = find_bus (savbus(l),savbas(l))
            if (kt .le. 0) then
               write (errbuf(1),470) savbus(l),savbas(l)
  470          format (' "INCLUDE BUS" (',a8,f6.1,
     &           ') IS NOT IN SYSTEM.  BUS IGNORED.')
               call prterx ('W',1)
            else
               if (ikk(1,kt) .eq. -1) then
                  write (errbuf(1),480) savbus(l),savbas(l)
  480             format ('0 "INCLUDE BUS" (',a8,f6.1,
     &              ') IS SPECIFICALLY "EXCLUDED". BUS IGNORED.')
                  call prterx ('W',1)
               else
                  if (ikk(1,kt) .eq. 1 .and. ikk(4,kt) .ne. isystm)
     &                then
                     write (errbuf(1),490) savbus(l),savbas(l)
  490                format ('0 REDUNDANT DATA: "INCLUDE BUS" (',a8,
     &                  f6.1, ') IS ALREADY RETAINED.')
                     call prterx ('W',1)
                  endif 
                  ikk(1,kt)=1   
                  ikk(2,kt)=1   
                  ikk(3,kt)=ifcsw   
                  ikk(4,kt)=isystm  
               endif
            endif
         enddo
         go to 900 
C       
C        PROCESS "EXCLUDE BUSES"   
C       
  520    do l=1,idat   
            kt = find_bus(savbus(l),savbas(l))
            if (kt .le. 0) then
               write (errbuf(1),530) savbus(l),savbas(l)
  530          format (' "EXCLUDE BUS" (',a8,f6.1,
     &           ') IS NOT IN SYSTEM.  BUS IGNORED.')
               call prterx ('W',1)
            else
               if (ikk(1,kt) .eq. -1) then   
                  write (errbuf(1),550) savbus(l),savbas(l)  
  550             format ('0 DUPLICATE DATA: "EXCLUDE BUS" (',a8,f6.1,
     &               ').')  
                  call prterx ('W',1)
               endif
               ikk(1,kt) = -1
               ikk(2,kt)=1   
               ikk(3,kt)=ifcsw   
               ikk(4,kt)=isystm  
            endif
         enddo
         go to 900 
C       
C        PROCESS "INTERFACE BRANCHES"  
C       
  580    isystm=isystm+1   
         ksw=0 
         do 800 l=1,idat   
            kt = find_bus (facbus(1,l),facbas(1,l))   
            if (kt .le. 0) then   
               write (errbuf(1),590) (facbus(i,l),facbas(1,l),i=1,2),
     &            'BUS1'   
  590          format (' "INTERFACE BRANCH" (',a8,f6.1,2x,a8,f6.1,
     &            ') has non-existent ',a,'. ')  
               call prterx ('W',1)
            else if (ikk(1,kt) .eq. 0 .and. ikk(2,kt) .eq. 1) then 
               write (errbuf(1),610) 'BUS1',(facbus(i,l),facbas(1,l),
     &            i=1,2)   
  610          format (' "INTERFACE BRANCH" ',a,' (',a8,f7.1,
     &           ') is in both systems.  Branch ignored.') 
               call prterx ('W',1)
               kerrsw=kerrsw+1
               kt=0
            else if (ikk(1,kt) .eq. -1) then
               write (errbuf(1),620) 'BUS1',facbus(1,l),facbas(1,l)
  620          format (' "INTERFACE BRANCH" ',a,' (',a8,f6.1,
     &           ') is specifically excluded. ')
               call prterx ('W',1)
               kt = 0
            else 
               if (ikk(2,kt) .eq. 1 .and. ikk(4,kt) .ne. isystm) then
                  write (errbuf(1),630) 'BUS1',facbus(1,l),facbas(1,l)
  630             format (' "INTERFACE BRANCH" ',a,' (',a8,f6.1,
     &              ') may be redundant. ')
                  call prterx ('W',1)
               endif 
               ikk(1,kt)=1   
               ikk(2,kt)=1   
               ikk(3,kt)=ifcsw   
               ikk(4,kt)=isystm  
            endif

            mt = find_bus(facbus(2,l),facbas(2,l))
            if (mt .le. 0) then   
               write (errbuf(1),590) (facbus(i,l),facbas(1,l),i=1,2),
     &            'BUS2'   
               call prterx ('W',1)
            else if (ikk(1,mt) .eq. 1) then
               write (errbuf(1),610) 'BUS2',(facbus(i,l),facbas(1,l),
     &            i=1,2)   
               call prterx ('W',1)
               kerrsw=kerrsw+1
               mt=0   
            else if (ikk(1,mt) .eq. -1) then   
               write (errbuf(1),620) 'BUS2',facbus(2,l),facbas(2,l)   
               call prterx ('W',1)
               mt = 0 
            else 
               if (ikk(2,mt) .eq. 1 .and. ikk(4,mt) .ne. isystm) then
                  write (errbuf(1),630) 'BUS2',facbus(2,l),facbas(2,l)
                  call prterx ('W',1)
               endif 
               ikk(1,mt)=0   
               ikk(2,mt)=1   
               ikk(3,mt)=ifcsw   
               ikk(4,mt)=isystm  
            endif

            if (kt .gt. 0 .and. mt .gt. 0) then
               i5=ikkind(1,kt)   
               i6=ikkind(2,kt)+i5-1  
               do j=i5,i6
                  if (kolum(j) .eq. mt) go to 750   
               enddo
               write (errbuf(1),740) (facbus(i,l),facbas(1,l),i=1,2) 
  740          format ('0 "INTERFACE BRANCH" (',a8,f6.1,2x,a8,f6.1,')',  
     1            ' is not in system. branch ignored.') 
               call prterx ('W',1)   
               go to 800 

  750          if (ksw .eq. 0) then
                  itface=itface+1   
                  if (itface .gt. 200) then
                     write (errbuf(1),760) 
  760                format ('More than 200 "interface branches".  Remai
     &ning data items ignored.')
                     call prterx ('W',1)
                     itface=200
                     ksw=1 
                  endif
                  write (outbuf,780) (facbus(i,l),facbas(1,l),i=1,2)
  780             format (' "INTERFACE BRANCH" IGNORED :(',a8,f6.1,2x,
     1               a8,f6.1,')')
                  call prterx ('W',1)
                  go to 800
               endif

               face(1,itface)=kt
               face(2,itface)=mt
               face(3,itface)=ifcsw
               face(4,itface)=0
            endif
  800    continue
         go to 900
C
C        PROCESS "EXCLUDE BRANCHES"
C
10580    ndbr = 0
         ksw = 0
         do 10800 l=1,idat
            kt = find_bus (facbus(1,l),facbas(1,l))
            if (kt .le. 0) then
               write (errbuf(1),10590) (facbus(i,l),facbas(i,l),i=1,2),
     &            'BUS1'
10590          format (' "EXCLUDE BRANCH" (',a8,f6.1,2x,a8,f6.1,
     &           ') has non-existent ',a,'.')
               call prterx ('W',1)
            endif 
            mt = find_bus(facbus(2,l),facbas(2,l))
            if (mt .le. 0) then   
               write (errbuf(1),10590) (facbus(i,l),facbas(i,l),i=1,2),
     &            'BUS2' 
               call prterx ('W',1)
            endif
            if (kt .gt. 0 .and. mt .gt. 0) then
               if (ikk(1,kt) .eq. 0 .and. ikk(1,mt) .eq. 0) then
                  write (errbuf(1),10672) (facbus(i,l),facbas(i,l),
     &               i=1,2)
10672             format ('0 "EXCLUDE BRANCH" (',a8,f6.1,2x,a8,f6.1,
     &               ') is entirely in eliminated subsystem.')
                  call prterx ('W',1)
               endif
               k1 = kt
               k2 = mt
               do lsw = 1, 2
                  i5=ikkind(1,k1)
                  i6=ikkind(2,k1)+i5-1
                  do j=i5,i6
                     if (k2 .eq. kolum(j)) go to 10750
                  enddo
                  write (errbuf(1),10740) (facbus(i,l),facbas(i,l),
     &               i=1,2)
10740             format (' "EXCLUDE BRANCH" (',a8,f6.1,2x,a8,f6.1,
     &               ') is not in system.  branch ignored.')
                  call prterx ('W',1)
                  go to 10800

10750             if (j .ne. i6) then
                     do k=j+1,i6 
                        kolum(k-1)=kolum(k)   
                     enddo
                  endif
                  ikkind(2,k1)=ikkind(2,k1)-1   
                  if (ksw .eq. 0) then
                     ndbr = ndbr + 1   
                     if (ndbr .gt. 200) then
                        write (errbuf(1),10760)
10760                   format ('More than 200 "EXCLUDE BRANCHES".  Rema
     &ining data items ignored.')
                        call prterx ('W',1)
                        ndbr=200
                        ksw=1
                     else
                        ndelbr(1,ndbr) = k1
                        ndelbr(2,ndbr) = k2
                     endif
                  endif
                  if (ksw .eq. 1) then
                     write (errbuf(1),10780) (facbus(i,l),facbas(i,l),
     &                   i=1,2)
10780                format ('0 "EXCLUDE BRANCH" not processed: (',a8,
     &                  f6.1,2x,a8,f6.1,')')
                     call prterx ('W',1)
                     go to 10800
                  endif

                  k1 = mt             ! Swap terminals for second loop
                  k2 = kt
               enddo
            endif
10800    continue

         if (ndbr .eq. 0) go to 900
         call qiksrt (1,ndbr,kpbrch,spbrch)
         do i = 1,ndbr   
            kt = ndelbr(1,i)  
            mt = ndelbr(2,i)  
            ptr = kbsdta(16,kt)
            do while (ptr .gt. 0)
               k2 = ky(ptr)
               if (k2 .eq. mt) then
                  ky(ptr) = ntot + 1 
                  write (outbuf,10850) i, bus(k1), base(k1), bus(k2),
     &               base(k2), brid(ptr), brsect(ptr), brtype(ptr)
10850             format (' "BRANCH EXCLUDED" ',i2,2x,a8,f6.1,2x,a8,
     &               f6.1,2x,a1,2i3)
                  call prtout(1)
               endif
               ptr = brnch_nxt(ptr)
            enddo
         enddo
         go to 900 
C       
C        PROCESS "RENAME BUSES"
C       
  810    ksw=0 
         do 890 l=1,idat   
            kt = find_bus(facbus(1,l),facbas(1,l))
            if (kt .le. 0) then
               write (errbuf(1),820) (facbus(i,l),facbas(i,l),i=1,2)
  820          format ('0 "RENAME BUS" (',a8,f6.1,2x,a8,f6.1,
     &            ') - bus1 - is not in system.  bus ignored.')
               call prterx ('W',1)
               go to 890  
            endif 
            mt = find_bus(facbus(2,l),facbas(2,l))
            if (mt .gt. 0) then
               write (errbuf(1),840) (facbus(i,l),facbas(i,l),i=1,2)
  840          format ('0 "RENAMED BUS" (',a8,f6.1,2x,a8,f6.1
     1            ,') is not unique.  new name ignored. ')
               call prterx ('W',1)
               go to 890
            endif
            if (ikk(1,kt) .eq. -1) then
               write (errbuf(1),850) savbus(l),savbas(l)
  850          format ('0 "RENAME BUS" (',a8,f6.1,
     &            ') is specifically "excluded". bus ignored.')
               call prterx ('W',1)
               go to 890  
            endif 
            if (ksw .eq. 0) then  
               itname=itname+1
               if (itname .gt. 100) then
                  write (errbuf(1),860)
  860             format ('0 MORE THAN 100 "RENAME BUSES".  REMAINING ',
     1              'data items ignored.')
                  call prterx ('W',1)
               ksw=1
            else
               irname(itname)=kt
               renamc(itname)=facbus(2,l)
               rename(itname)=facbas(2,l)
            endif
         else
            write (errbuf(1),880) (facbus(i,l),facbas(i,l),i=1,2)
  880       format ('0 "RENAME BUS" IGNORED: (',
     1          a8,f6.1,2x,a8,f6.1,')')
            call prterx ('W',1)
         endif 
  890    continue  
C       
C        END TEXT COMMAND LOOP 
C       
  900    call systxt   
      enddo

 9000 if (kerrsw .eq. 0) go to 9020
      write (errbuf(1), 9010) kerrsw  
 9010 format ('0 ',i2,' ERRORS ENCOUNTERED.')   
      call prterx ('W',1)   
 9020 continue  
      return
      end   
