C    %W% %G%
      subroutine shdinp                                                 
C                                                                  
C     THIS SUBROUTINE READS THE LOAD SHEDDING DATA CARDS              
C     AND FORMS THE INITIAL TABLES. IT IS CALLED BY INPUT2            
C                                                                  
      include 'tspinc/params.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/brnch.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/shdlod.inc' 
      include 'tspinc/sumuf.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/areanz.inc' 
      include 'tspinc/ffcard.inc' 
      include 'tspinc/workc.inc' 
      include 'tspinc/reread.inc' 
      include 'tspinc/ecstbb.inc' 

c     -  Local variables
      integer nxznen(4,MAXBUS), nxzflag(4*MAXBUS)
      real temp(20)                                                
      character*10 blnk10, area1, c10
      character*8  namec                                                
      character*7  c7                                                   
      character*2  zone1, c2                                             
      character*1  pshdc, subtyp                                         
      logical debug

      data blnk10 /'          '/                                        
c
c     -    Begin    Begin    Begin    Begin    Begin    Begin
      call mpost('SHDINP')                                              
      debug = .false.                                                   !dem
      if (debug) call dbgeko ('SHDINP - list of bad freqs on UF cards') !dem
C                                                                  
C     INITIALIZE UNDERFREQUENCY LOAD SHEDDING SUMMARY TABLES FOR      
C     SUBROUTINE SUMUFL                                               
C                                                                  
      tuflod = 0.0                                                      
      indfeq = 0                                                        
      iznuf = 0                                                         
      do itrr = 1,MXITFR                                             !DEM 
        uffreq(itrr) = 0.0                                                
        uflod(itrr) = 0.0                                                 
        ibusfq(itrr) = 0                                                  
        ufznto(itrr) = 0.0                                                
        ufzone(itrr) = '  '                                               
      enddo
      do itrr = 1, 4*MAXBUS
        nxzflag(itrr) = 0
      enddo
      do itrr = 1,MXLSFR                                              !dem
        do jtrr = 1,MXITFR                                             !dem
          ufbusn(itrr,jtrr) = '        '                                    
          ufbkv(itrr,jtrr) = 0.0                                            
          ufcyc(itrr,jtrr) = 0.0                                            
          ufblod(itrr,jtrr) = 0.0                                           
          ufznld(itrr,jtrr) = 0.0                                           
          iufzon(itrr,jtrr) = '  '                                          
        enddo
      enddo
      do i1= 1,4                                                    
        do  i2 = 1,ntotd                                               
           nxznen(i1,i2) = 0                                                 
        enddo
      enddo
C                                                                  
C     READ LOAD SHED CARD IMAGES                                      
C                                                                  
      lss = ls                                                          
      itrr = 1                                                          
      call whrec1 ('LSH', ilo, ihi, isz)                                !dem
c     if (debug) then                                                   !dem
c       call dbgeko ('SHDINP - reading load shedding stuff from ',      !dem
c    +    'temp file # 1')                                              !dem
c       call dbgwri ('  record # = ',ihi)                               !dem
c     endif                                                             !dem
 105  if (lss .le. 100) then                                              
        indx1 = itrr*100 - 99                                           
        read (l1,rec = ihi+itrr) ((work(ll,nn), ll=1,8), nn = indx1,ls)   !dem
c       read (l1,REC = 170+ITRR) ((WORK(LL,NN),LL=1,8),NN = INDX1,LS)     
        lss = 0                                                         
      else                                                              
        indx2 = itrr*100                                                
        indx1 = indx2-99                                                
        read (l1,rec=ihi+itrr) ((work(ll,nn), ll=1,8), 
     &                           nn = indx1,indx2)  !dem
c       read (l1,REC = 170+ITRR) ((WORK(LL,NN),LL=1,8),
c                                 NN = INDX1,INDX2)  
        lss = lss - 100                                                 
        itrr = itrr+1                                                   
      endif                                                             
      if (lss .gt. 0) go to 105                                          
C                                                                  
C     DETERMINE IF LOAD SHEDDING IS ENTERED BY:                       
C     BUS,BASE NUMBER 1 PRIORITY**ZONE     NUMBER 2 PRIORITY          
C     AREA     NUMBER 3 PRIORITY**TOTAL    NUMBER 4 PRIORITY          
C     CARD TYPES ARE PROCESSED IN REVERSE ORDER SO PRIORITY           
C     IS MAINTAINED                                                   
C     NEXZNEW(1,IBUS) CONTAINS CARD IMAGE FOR UNDER VOLTAGE DATA      
C     NEXZNEW(2,IBUS) CONTAINS CARD IMAGE FOR DELTA VOLTAGE DATA      
C     NEXZNEW(1,IBUS) CONTAINS CARD IMAGE FOR UNDER FREQ DATA         
C                                                                  
      do 600 ilss = 1,ls                                                
C                                                                  
C     TOTAL REPRESENTATION                                            
C                                                                  
      iarsw = 0                                                         
      iznsw = 0                                                         
      read (work80(ilss),110) c7                                          
  110 format (bz,9x,a7)                                                     
      if (c7 .eq. '       ') then                                         
         read (work80(ilss),120) namec                                    
  120    format (3x,a8)                                                  
         if (namec .eq.'TOTAL   ') then                                   
             read (work80(ilss),125) c2                                   
  125        format (2a)                                                 
             if (c2 .eq. 'UV') icde = 1                                   
             if (c2 .eq. 'UD') icde = 2                                   
             if (c2 .eq. 'UF') icde = 3                                   
           do 130 i1 = 1,ntotd                                          
  130        nxznen(icde,i1) = ilss                                     
             go to 600                                                  
         endif                                                          
      endif                                                             
C                                                                  
C     CHECK FOR AREA REPRESENTATION                                   
C                                                                  
      read (work80(ilss),140) namec,zone1                                 
  140 format (3x,a8,2x,a2)                                               
      if ( zone1 .eq. '  ') then                                          
         read (work80(ilss),150) area1                                    
  150    format (3x,a10)                                                 
         do 400 i1 = 1,ntotc                                            
         if (area1 .ne. areanc(i1)) go to 400                            
         iarsw = 1                                                      
         do 300 i2 = 1,10                                               
         zone1 = areazc(i2,i1)                                          
         if (zone1 .eq. '  ')go to 300                                   
         read (work80(ilss),125) c2                                       
         if (c2 .eq. 'UV')icde = 1                                       
         if (c2 .eq. 'UD')icde = 2                                       
         if (c2 .eq. 'UF')icde = 3                                       
         do 200 i3 = 1,ntotd                                            
         if (zone1 .eq. exzonc(i3)) then                                  
            nxznen(icde,i3) = ilss                                      
         endif                                                          
  200    continue                                                       
  300    continue                                                       
         go to 600                                                      
  400    continue                                                       
         if (iarsw .eq. 0) then                                          
            write(outbuf,520) (work(iy,ilss),iy = 1,8)                  
            call prtout(1)                                              
            write(errbuf(1),405) area1                                  
  405       format('Area ', a10, 
     &             ' on preceding record is not in system')
            call prterr('E',1)                                          
            iabort = 1                                                   
         endif                                                          
         go to 600                                                      
      endif                                                             

C     CHECK FOR ZONE REPRESENTATION                                   
C                                                                  
      read (work80(ilss),410) c10                                         
  410 format (3x,a10)                                                    
      if (c10 .eq. blnk10) then                                           
         read (work80(ilss),420) zone1                                   
  420    format (13x,a2)                                                 
         read (work80(ilss),125) c2                                       
         if (c2 .eq. 'UV')icde = 1                                       
         if (c2 .eq. 'UD')icde = 2                                       
         if (c2 .eq. 'UF')icde = 3                                       
         do i3 = 1, ntotd                                           
           if (zone1 .eq. exzonc(i3)) then                                  
             nxznen(icde,i3) = ilss                                   
             iznsw = 1                                                
           endif                                                          
         enddo
         if (iznsw .eq. 0) then                                          
            write(outbuf,520) (work(iy,ilss),iy = 1,8)                  
 520        format('0', 8a10)                                                
            call prtout(1)                                              
            write(errbuf(1),510) zone1                                  
 510        format('Zone ', a2,
     &             ' on the preceding record is not in system')
            call prterr('E',1)                                          
            iabort = 1                                                   
         endif                                                          
         go to 600                                                      
      endif                                                             
C                                                                  
C     BUS,BASE REPRESENTATION                                         
C                                                                  
      read (work80(ilss),515) namec,base                                
 515  format (bz,3x,a8,f4.0)                                               
      kb=nambas(base)                                                   
      i3 = inam(namec,kb)                                               
      if (i3 .ne. 0) then                                                 
         read (work80(ilss),125) c2                                       
         if (c2 .eq. 'UV')icde = 1                                       
         if (c2 .eq. 'UD')icde = 2                                       
         if (c2 .eq. 'UF')icde = 3                                       
         nxznen(icde,i3) = ilss                                         
         go to 600                                                      
      else                                                              
         write (outbuf,520) (work(iy,ilss),iy = 1,8)                     
         call prtout(1)                                                 
         write(errbuf(1),530)                                           
 530     format('0  THE NAME AND/OR BASE KV ON THE ABOVE CARD IS ',     
     1         'INCORRECT')                                             
         call prterr('E',1)                                             
         iabort =1                                                      
      endif                                                             
 600  continue                                                          
C                                                                  
C     FORM LOAD SHEDDING TABLES ACCORDING TO NXZNEN HIERARCHY         
C                                                                  
      i1 = 0                                                            
      do 2000 itot = 1,ntotd                                            
      do 1900 indx = 1,3                                                
      ilss = nxznen(indx,itot)                                          
      if (ilss .eq. 0) go to 1900                                        
C                                                                  
C     DECODE FOR BPA/WSCC CARD FORMAT                                 
C                                                                  
  700 i1 = i1 + 1                                                        
      if ( i1 .gt. MAXBUS) then                                           
         write(errbuf(1),720)                                           
  720    format('0 LOAD SHEDDING DATA TABLE HAS OVERFLOWED. ')          
         call prterr('E',1)                                             
         iabort = 1                                                     
         ls = 0                                                         
         return                                                         
      endif                                                             
      lshdno(i1) = itot                                                 
      lshdlv(i1) = 0                                                    
      lshdsh(i1) = 0                                                    
      read (work80(ilss),750) pshdc                                     
  750 format (79x,a1)                                                    
C                                                                  
C     PSHDC = 'W' MEANS THE BPA FORMAT                                
C                                                                  
      if (pshdc.eq.'W') then                                             
         read (work80(ilss),800) subtyp,(temp(k),k=1,15)                 
  800    format (bz,1x,a1,13x,5(f4.2,f3.1,f5.0))                            
         lshdfr(i1) = 1                                                 
         if (keybrd(31) .ne. 0) then                                      
            write(outbuf,830) exnamc(itot)                              
            call prtout(1)                                              
            write(outbuf,520) (work(iy,ilss),iy = 1,8)                  
            call prtout(1)                                              
         endif                                                          
        go to 860                                                       
      endif                                                             
C                                                                  
C     IWSCC = 0 OLD WSCC FORMAT IWSCC = 1 NEW WSCC FORMAT             
C                                                                  
      if (iwscc .ne. 0) then                                              
         read (work80(ilss),830) subtyp,pshdc,(temp(k),k=1,20)           
  830    format (bz,1x,a1,13x,a1,4(f5.3,f3.0,f2.0,f3.3),f4.2,f3.0,f2.0,
     +     f3.3)
         if (keybrd(31) .ne. 0) then                                      
            write (outbuf,835) exnamc(itot)                              
            call prtout(1)                                              
  835       format ('0      NAME = ',a8)                                
            write (outbuf,520) (work(iy,ilss),iy = 1,8)                  
            call prtout(1)                                              
         endif                                                          
C                                                                  
C     PSHDC = 'S' MEANS DROP SHUNT LOAD                               
C                                                                  
         if (pshdc .eq. 'S')lshdsh(i1) = 1                               
         lshdfr(i1) = 0                                                 
         if (subtyp .eq. 'F') then                                        
           if (debug) call dbgwrc ('  UF card = ',work80(ilss))        !dem
           do k = 1,17,4                                             
             if (temp(k) .gt. 0.0 .and. nxzflag(ilss) .eq. 0 .and.
     &          (temp(k) .lt. 50.0 .or. temp(k) .gt. 60.0)) then              
               if (debug) then                                          !dem
                 call dbgwri ('  K /freq indx in TEMP/ = ',k)           !dem
                 call dbgwrf ('  Freq read = ',temp(k))                 !dem
               endif                                                    !dem
               write (errbuf(1),840) temp(k)                            !dem    
 840           format('Load shed frequency on next card is ',           !dem
     1                'below 50 hz or above 60 hz [',f5.2,'].')         !dem
               write(errbuf(2),837) work80(ilss)                        
 837           format(a80)                                              
               call prterr('W',2)                                       
c              iabort = 1                                               
               nxzflag(ilss) = 1
             endif                                                       
           enddo
         endif                                                          
      else                                                              
C                                                                  
C     OLD WSCC FORMAT                                                 
C                                                                  
        read (work80(ilss),850) subtyp,(temp(k),k=1,15)                 
  850   format (bz,1x,a1,13x,5(f5.3,f3.1,f3.2))                            
        if (keybrd(31) .ne. 0) then                                      
          write (outbuf,830) exnamc(itot)                              
          call prtout(1)                                              
          write (outbuf,520) (work(iy,ilss),iy = 1,8)                  
          call prtout(1)                                              
        endif                                                          
        if (subtyp .eq. 'F') then                                        
          do k = 1,13,3                                             
            if (temp(k) .gt. 0.0 .and. nxzflag(ilss) .eq. 0 .and.
     &         (temp(k) .lt. 55.0 .or. temp(k) .gt. 60.0)) then              
              write (errbuf(1), 842) temp(k)                           !dem
              write (errbuf(2),837) work80(ilss)                       !dem
              call prterr('W',2)                                       
              nxzflag(ilss) = 1
c             iabort = 1                                               
            endif                                                       
          enddo
        endif                                                          
      endif                                                             

C     UNDER VOLTAGE SHEDDING                                          
  860 if (subtyp .eq. 'V') lshdcd(i1) = 1                                

C     VOLTAGE DEVIATION SHEDDING                                      
      if (subtyp .eq. 'D') lshdcd(i1) = 2                                

C     UNDERFREQUENCY SHEDDING                                         
      if (subtyp .eq. 'F') lshdcd(i1) = 3                                
      k1 = 0                                                            
      if (lshdfr(i1) .eq. 0 .and. iwscc .ne. 0) go to 1000               
C                                                                  
C     FORM TABLES FOR BPA FORMAT AND OLD WSCC FORMAT                  
C                                                                  
      do 900 k=1,13,3                                                   
      if (temp(k).lt..001) go to 900                                    
      if (temp(k+2).lt..00) go to 900                                    
      if (lshdcd(i1) .eq. 3 ) then                                        
         temp(k) = 6.2831852 * ( 1. - temp(k) / frqbse )                
         ifqsw = 1                                                      
      endif                                                             
      k1 = k1 + 1                                                       
C      ITEMP1 = IFIX(TEMP(K)*1000.)                                     
C      ITEMP2 = IFIX(TEMP(K+1)*10.)                                     
C      ITEMP3 = IFIX(TEMP(K+2)*10000.)                                  
C      TEMP(K) = FLOAT(ITEMP1)*.001                                     
C      TEMP(K+1) = FLOAT(ITEMP2)*.1                                     
C      TEMP(K+2) = FLOAT(ITEMP3)*.0001                                  
      shdlev(k1,i1) = temp(k)                                           
      shdtim(k1,i1) = temp(k+1)                                         
      shdlde(k1,i1) = temp(k+2)                                         
      shdcde(k1,i1) = 0.0                                               
      shdpcb(k1,i1) = 0.0                                               
      shddel(k1,i1) = 0.0                                               
  900 continue                                                          
      lshdlv(i1) = k1                                                   
      go to 1900                                                        
C                                                                  
C     FORM TABLES FOR NEW WSCC OPTION                                 
C                                                                  
 1000 do 1100 k=1,17,4                                                  
      if (temp(k).lt..001) go to 1100                                   
C                                                                  
C     CHECK FOR LOWER LIMITS ON DELAY AND PSHED. NEGATIVE VALUES     
C     ARE NOT CONSIDERED BUT JOB NOT TERMINATED.                     
C                                                                  
      if (temp(k+3).lt. 0.0) go to 1100                                   
C                                                                  
C     CHECK FOR UPPER LIMIT ON THRESHOLD UNDER FREQUENCY VALUE. IT   
C     MUST BE LESS THAN FREQUENCY BASE (I.E. 60 HZ) OTHERWISE IGNORED
C                                                                  
      if (lshdcd(i1) .eq. 3 ) then                                        
         if (temp(k) .ge. frqbse .and. nxzflag(ilss) .eq. 0) then
            write (errbuf(1),842) temp(k), frqbse                    !dem
 842        format('Load shed frequency [', f5.2, 
     &          '] on above base frequency [', f5.2, ']')
            write (errbuf(2),837) work80(ilss)                       !dem
            call prterr('W',2)                                       
            nxzflag(ilss) = 1
         endif
         temp(k) = 6.2831852 * ( 1. - temp(k) / frqbse )                
         ifqsw = 1                                                      
      endif                                                             
      k1 = k1 + 1                                                       
      shdlev(k1,i1) = temp(k)                                           
      shdtim(k1,i1) = temp(k+1)                                         
      shdpcb(k1,i1) = temp(k+2)                                         
      shdlde(k1,i1) = temp(k+3)                                         
      shdcde(k1,i1) = 0.0                                               
      shddel(k1,i1) = 0.0                                               
 1100 continue                                                          
      lshdlv(i1) = k1                                                   
      if (k1 .eq. 0) i1 = i1-1                                           
 1900 continue                                                          
 2000 continue                                                          
      ls = i1                                                           
      return                                                            
      end                                                               
