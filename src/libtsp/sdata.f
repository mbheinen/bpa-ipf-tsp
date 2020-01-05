C    %W% %G%
      subroutine sdata                                                  
c                                                                       
c     MERGES DATA FROM AN EXISTING SAVED DATA FILE AND CHANGE         
c     DATA CARDS FROM THE INPUT DATA FILE (FOR005).  IT IS            
c     CALLED BY INPUT1 AND CALLS SAVFIL.                              
c                                                                       
C     Revs:
c     Mar/06/92 - DEM
c        Modified calls to REPET1 so that input index to fetch file 1 
c        record numbers is char[3] (was int). 
c     - 
      external komp31,swap31                                            
      common /lqiks/ lqiks                                              
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/ecstbd.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/relays.inc' 
      include 'tspinc/ecio.inc' 
      include 'tspinc/in1n.inc' 
      include 'tspinc/in1c.inc' 
      include 'tspinc/lnet.inc' 
      include 'tspinc/titles.inc' 

      common /start/ iswt1,iswt2,iswt3,iswt4,kswt1,kswt2                
c     -  /SATSW/ is shared with DERIV and LODCUR
      common /satsw/ satsw                                              
C                                                                       
C     FOR BRNCH TABLE NOT PUT ON ECS                                  
C                                                                       
      equivalence (sater,dtc),(isg2,istp)                               

      character*80 work80(100), jwrk80(100)                           
      character*10 jwork(8,100), work(8,100)                           
      equivalence (jwork,jwrk80), (work,work80)
      dimension basold(MAXKV)

c     -  local variables

c     TEMP_ECSADDR must agree with subroutine INPUT1

      integer TEMP_ECSADDR
      parameter (TEMP_ECSADDR = 2101)

      character*1 typex, subtp, subtpx, type, subtyp, kchgcd, id, 
     &            idx, blnk1                                               
      character*4 c41, c41x                                              
      character*5 plotd                                               
      character*8 name2x, name3x, name4x, jnkbsc, name1c, name2c
      character*10 relayf, junk, remotf, serief, capcpf, loadf,     
     &             reprf, shedf, machnf, jblnk, drectf, curntf,     
     &             lnetf, dat, area, zones, busbas, 
     &             nme10c, name1x, nme10x

c     -  Initializing of items in /TITLES/ moved to STABDAT
      logical debug                                                     !dem

      data plotd/'PLOTT'/                                               
      data blnk1 / ' '/                                                 
c
c     -     Begin     Begin     Begin     Begin     Begin     Begin
      debug = .false.                                                   !dem
      call mpost ('SDATA')                                              

      if (savin .eq. ' ' .and. savout .eq. ' ') go to 5785               
      if (savin .eq. ' ' .and. savout .ne. ' ') go to 5790               
      if (savin .ne. ' ' .and. savout .eq. ' ') go to 5795               
      isav=4                                                            
      go to 5805                                                        
 5785 isav=1                                                            
      go to 8800                                                        
 5790 isav=2                                                            
      go to 5800                                                        
 5795 isav=3                                                            
      go to 5805                                                        
C                                                                       
C     CREATE A STRUCTURED FILE UNDER THE FILE NAME (SDATA)              
C                                                                       
 5800 call savfil                                                       
      call mpost ('SDATA')                                              
      go to 8800                                                        
C                                                                       
C     MERGING A STRUCTURED FILE,CALLED SDATA,WITH CHANGE CARDS          
 5805 continue                                                          
 5810 jreuse=1                                                          
      lrept=lrep                                                      
      lrepk=lrep                                                      
      ldzt=ldz                                                        
      ldzk=ldz                                                        
      ldart=ldar                                                      
      ldark=ldar                                                      
      lnt=ln                                                          
      lnk=ln                                                          
      lst=ls                                                          
      lsk=ls                                                          
      lsct=lsc                                                        
      lsck=lsc                                                        
      lrdt=lrd                                                        
      lrdk=lrd                                                        
      lrrt=lrr                                                        
      lrrk=lrr                                                        
      mact=mac                                                        
      mack=mac                                                        
      jdct=jdc                                                        
      jdck=jdc                                                        
      jdctot=jdc                                                      
C                                                                       
C     MERGING LOCAL RELAY CARDS                                         
C                                                                       
      read (l9) relayf,junk,dat,lrdf                                    
      call repet1 (lrdf,lrdk,iswt,'RD ',msold,msnew)                    !dem
c     CALL REPET1 (LRDF,LRDK,ISWT,1,MSOLD,MSNEW)                        
      ldumbb=lrdf                                                     
      ldumb=lrdf                                                      
      ldumcc=lrdk                                                     
      ldumc=lrdk                                                      
      iswtbk=1                                                          
      go to 6020                                                        
C                                                                       
C     MERGING REMOTE RELAY CARDS                                        
C                                                                       
 5840 read (l9) remotf,relayf,dat,lrrf                                  
      call repet1 (lrrf,lrrk,iswt,'RR ',msold,msnew)                    !dem
c     CALL REPET1 (LRRF,LRRK,ISWT,2,MSOLD,MSNEW)                        
      ldumbb=lrrf                                                     
      ldumb=lrrf                                                      
      ldumcc=lrrk                                                     
      ldumc=lrrk                                                      
      iswtbk=2                                                          
      go to 6020                                                        
C                                                                       
C     MERGING SERIES CAPACITOR CARDS                                    
C                                                                       
 5860 read (l9) serief,capcpf,dat,lscf                                  
      ldumbb=lscf                                                     
      ldumb=lscf                                                      
      ldumcc=lsck                                                     
      ldumc=lsck                                                      
      call repet1 (lscf,lsck,iswt,'RG ',msold,msnew)                    !dem
c     CALL REPET1 (LSCF,LSCK,ISWT,3,MSOLD,MSNEW)                        
      iswtbk=3                                                          
      go to 6020                                                        
C     *                                                                 
C     MERGING LOAD REPRESENTATION CARDS BY AREA INTERCHANGE             
C     *                                                                 
 5880 read (l9) loadf,reprf,area,ldarf                                  
      call repet1 (ldarf,ldark,iswt,'LDA',msold,msnew)                  !dem
c     CALL REPET1 (LDARF,LDARK,ISWT,4,MSOLD,MSNEW)                      
      ldumbb=ldarf                                                    
      ldumb=ldarf                                                     
      ldumcc=ldark                                                    
      ldumc=ldark                                                     
      iswtbk=4                                                          
      go to 6020                                                        
C     *                                                                 
C     MERGING LOAD REPRESENTATION CARDS BY ZONES                        
C     *                                                                 
 5900 read (l9) loadf,reprf,zones,ldzf                                  
      call repet1 (ldzf,ldzk,iswt,'LDZ',msold,msnew)                    !dem 
c     CALL REPET1 (LDZF,LDZK,ISWT,5,MSOLD,MSNEW)                        
      ldumbb=ldzf                                                     
      ldumb=ldzf                                                      
      ldumcc=ldzk                                                     
      ldumc=ldzk                                                      
      iswtbk=5                                                          
      go to 6020                                                        
C     *                                                                 
C     MERGING LOAD REPRESENTATION CARDS BY BUS - BASE IDENTIFICATION    
C     *                                                                 
 5920 read (l9) loadf,reprf,busbas,lrepf                                
      call repet1 (lrepf,lrepk,iswt,'LDB',msold,msnew)                  !dem
c     CALL REPET1 (LREPF,LREPK,ISWT,6,MSOLD,MSNEW)                      
      ldumbb=lrepf                                                    
      ldumb=lrepf                                                     
      ldumcc=lrepk                                                    
      ldumc=lrepk                                                     
      iswtbk=6                                                          
      go to 6020                                                        
C                                                                       
C     MERGING LOAD SHEDDING CARDS                                       
C                                                                       
 5940 read (l9) loadf,shedf,dat,lsf                                     
      ldumbb=lsf                                                      
      ldumb=lsf                                                       
      ldumcc=lsk                                                      
      ldumc=lsk                                                       
      call repet1 (lsf,lsk,iswt,'LSH',msold,msnew)                      !dem
c     CALL REPET1 (LSF,LSK,ISWT,7,MSOLD,MSNEW)                          
      iswtbk=7                                                          
      go to 6020                                                        
C     *                                                                 
C     MERGING LOAD NETTING CARDS                                        
C     *                                                                 
 5960 iswtbk=8                                                          
      go to 8060                                                        
C     *                                                                 
C     MERGING MACHINE DATA CARDS                                        
C     *                                                                 
 5980 read (l9) machnf,jblnk,dat,macf                                   
      ldumbb=macf                                                     
      ldumb=macf                                                      
      ldumcc=mack                                                     
      ldumc=mack                                                      
      call repet1 (macf,mack,iswt,'MAC',msold,msnew)                    !dem
c     CALL REPET1 (MACF,MACK,ISWT,9,MSOLD,MSNEW)                        
      iswtbk=9                                                          
      go to 6020                                                        
C     *                                                                 
C     * MERGING DIRECT CURRENT DATA CARDS                               
C     *                                                                 
 6000 read (l9) drectf,curntf,dat,jdcf                                  
      ldumbb=jdcf                                                     
      ldumb=jdcf                                                      
      ldumcc=jdck                                                     
      ldumc=jdck                                                      
      call repet1 (jdcf,jdck,iswt,'DC ',msold,msnew)                    !dem
c     CALL REPET1 (JDCF,JDCK,ISWT,10,MSOLD,MSNEW)                       
      iswtbk=10                                                         
C                                                                      
C      COMMON LOGIC FOR ALL GROUPS OF INPUT CARDS                     
C                                                                      
 6020 kecs=TEMP_ECSADDR                                                        
      jswtb=0                                                           
      lcount=0                                                          
      ich=0                                                             
      if (keybrd(32) .eq. 0) go to 6050                                     
      call forbtm                                                       
      call fortop                                                       
      write (outbuf,6040) iswtbk                                        
 6040 format (' ISWTBK= ',i5)                                           
      call prtout (1)                                                   
 6050 continue                                                          
      go to (6060,6080,6100,6120), iswt                                 
C                                                                       
C     NO BASE CASE CARDS AND NO CHANGE CASE CARDS                       
C                                                                       
6060  write (l1,rec=msnew) lcount,lcount                                
      go to (5840,5860,5880,5900,5920,5940,5960,5980,6000,8800), iswtbk 
C                                                                       
C     NO BASE CASE CARDS BUT CHANGE CASE CARDS EXITS                    
C                                                                       
 6080 iswt2=2                                                           
      kswt1=1                                                           
      go to 6260                                                        
C                                                                       
C     YES BASE CASE CARDS BUT NO CHANGE CASE CARDS                      
C                                                                       
 6100 iswt3=3                                                           
      kswt2=2                                                           
C                                                                       
C     YES BASE CASE CARDS AND CHANGE CASE CARDS EXISTS                  
C                                                                       
 6120 if (ldumb .gt. 100) go to 6140                                      
      i100=ldumb                                                        
      go to 6160                                                        
 6140 i100=100                                                          
 6160 ldumb=ldumb-i100                                                  
      kwords=8*i100                                                     
      read (l9) ((work(j,i),j=1,8),i=1,i100)                            
      if (keybrd(32) .eq. 0) go to 6190                                     
      do i = 1,i100                                                
         write (outbuf,6180) (work(j,i),j=1,8)                          
 6180    format (1x,8a10)                                                  
         call prtout (1)                                                
      enddo
      write (outbuf,6185)                                               
 6185 format('0',24x,'************************************************',
     1 '***')                                                           
      call prtout (1)                                                   
 6190 continue                                                          
      im=0                                                              
      if (iswt3 .ne. 3) go to 6240                                        
      im=1                                                              
 6200 if (iswtbk .ne. 10) go to 6230                                        
C                                                                       
C     MERGING REMAINDER OF DC CARDS ON SAVED DATA FILE.                 
C     ELIMINATE DB AND DC CARDS                                         
C                                                                       
 6210 if (im .gt. jdcf) go to 7520                                          
      read (work80(im),6780) subtp,name1c,base1                          
      call ritecc(work(1,im),kecs,8)                                    
      kecs=kecs+8                                                       
      im=im+1                                                           
      go to 6210                                                        
 6230 if (kwords .gt. 0)call ritecc(work(1,im),kecs,kwords)              
      kecs=kecs+kwords                                                  
      if (ldumb .eq. 0) go to 7520                                        
      go to 6120                                                        
 6240 if (jswtb .eq. 1) go to 6620                                        
 6260 if (ldumc .gt. 100) go to 6280                                      
      k100=ldumc                                                        
      go to 6300                                                        
 6280 k100=100                                                          
 6300 ldumc=ldumc-k100                                                  
      msold=msold+1                                                     
      read (l1,rec=msold) (( jwork(j,i),j=1,8),i=1,k100)                 
      if (keybrd(32) .eq. 0) go to 6330                                 
        do i = 1,k100                                              
           write (outbuf,6320) jwrk80(i)                                
 6320      format (5x,a)                                                     
           call prtout (1)                                              
        enddo
        write (outbuf,6185)                                               
        call prtout (1)                                                   
 6330 ix=0                                                              
 6340 ix=ix+1                                                           
      if (ix .gt. k100) go to 6580                                        
      go to (6360,6360,6360,6400,6440,6480,6480,6520,6540,6480), iswtbk 
 6360 read (jwrk80(ix),6380) subtyp,kchgcd,name1x,base1x,name2x,      
     1  base2x,idx,ksectx                                                   
 6380 format (bz,1x,a1,a1,3x,a8,f4.0,1x,a8,f4.0,a1,i1)                     
      go to 6620                                                        
 6400 read (jwrk80(ix) ,6420) kchgcd,name1x                              
 6420 format (bz,2x,a1,14x,a10)                                            
      go to 6620                                                        
 6440 read (jwrk80(ix) ,6460) kchgcd,name1x                              
 6460 format (bz,2x,a1,12x,a2)                                             
      go to 6620                                                        
 6480 if (iswtbk .eq. 7) then                                            
         read (jwrk80(ix),6510) subtpx,kchgcd,name1x,c41x               
      else                                                              
         read (jwrk80(ix),6500) subtpx,kchgcd,name1x,base1x             
      endif                                                             
 6510 format (bz,1x,a1,a1,a8,a4)                                            
 6500 format (bz,1x,a1,a1,a8,f4.0)                                          
      go to 6620                                                        
 6520 go to 8060                                                        
 6540 read (jwrk80(ix) ,6560) typex,subtpx,kchgcd,name1x,base1x,idx      
 6560 format (bz,2a1,a1,a8,f4.0,a1)                                        
      go to 6620                                                        
 6580 if (ldumc .ne. 0) go to 6260                                        
      if (kswt1 .eq. 1 .and. iswt2 .eq. 2) go to 7520                         
      if (im.lt.i100) go to 6600                                        
      if (ldumb .eq. 0) go to 7520                                        
 6600 kswt2=2                                                           
      iswt3=3                                                           
      num=i100-im                                                       
      kwords=8*num                                                      
      im=im+1                                                           
      go to 6200                                                        
 6620 if (kswt1 .eq. 1) go to 7320                                        
      im=im+1                                                           
      if (im .gt. i100) go to 6840                                        
      go to (6640,6640,6640,6680,6720,6760,6760,8060,6800,6760), iswtbk 
 6640 read (work80(im) ,6660) name1c,base1,name2c,base2,id,ksect         
 6660 format (bz,6x,a8,f4.0,1x,a8,f4.0,a1,i1)                              
      go to 6880                                                        
 6680 read (work80(im) ,6700) nme10c                                     
 6700 format (17x,a10)                                                  
      go to 6880                                                        
 6720 read (work80(im) ,6740) name1c                                     
 6740 format (15x,a2)                                                   
      go to 6880                                                        
 6760 if (iswtbk .eq. 7) then                                            
         read (work80(im),6790)  subtp,name1c,c41                       
 6790    format (bz,1x,a1,1x,a8,a4)                                            
      else                                                              
         read (work80(im),6780)  subtp,name1c,base1                     
 6780    format (bz,1x,a1,1x,a8,f4.0)                                          
      endif                                                             
      go to 6880                                                        
 6800 read (work80(im) ,6820) type,subtp,name1c,base1,id                 
 6820 format (bz,2a1,1x,a8,f4.0,a1)                                        
      go to 6880                                                        
 6840 if (ldumb .eq. 0) go to 6860                                        
      jswtb=1                                                           
      go to 6120                                                        
 6860 kswt1=1                                                           
      go to 7320                                                        
 6880 if (iswtbk .eq. 8) go to 8060                                         
c     -  Discard unconditional debugging bypass
c     GO TO 7080                                                        
 6881 if (keybrd(32) .eq. 0) go to 7080                                 
      go to (6900,6900,6900,6940,6980,7020,7020,8060,7060,7020),iswtbk  
 6900 write (outbuf,6920) kchgcd,ix,name1x,base1x,name2x,base2x,idx,    
     1  ksectx                                                          
 6920 format ('0',a1,2x,i3,2x,a8,2x,f5.1,2x,a8,2x,f5.1,2x,a1,2x,i1)     
      call prtout (1)                                                   
      write (outbuf,6920) kchgcd,im,name1c,base1,name2c,base2,id,ksect  
      call prtout (1)                                                   
      go to 7080                                                        
 6940 write (outbuf,6960) kchgcd,ix,nme10x                              
 6960 format ('0',a1,2x,i3,2x,a10)                                      
      call prtout (1)                                                   
      write (outbuf,6960) kchgcd,im,nme10c                              
      call prtout (1)                                                   
      go to 7080                                                        
 6980 write (outbuf,7000) kchgcd,ix,name1x                              
 7000 format ('0',a1,2x,i3,2x,a3)                                       
      call prtout (1)                                                   
      write (outbuf,7000) kchgcd,im,name1c                              
      call prtout (1)                                                   
      go to 7080                                                        
 7020 write (outbuf,7040) kchgcd,ix,name1x,base1x                       
 7040 format ('0',a1,2x,i3,2x,a8,2x,f5.1)                               
      call prtout (1)                                                   
      write (outbuf,7040) kchgcd,im,name1c,base1                        
      call prtout (1)                                                   
      go to 7080                                                        
 7060 continue                                                          
 7080 if (iswtbk  .eq. 4)then                                              
      if (kompr(nme10c,name1x,kdum)) 7280,7100,7300                     
      else                                                              
      if (kompr(name1c,name1x,kdum)) 7280,7100,7300                     
      endif                                                             
 7100 if (iswtbk .eq. 4) go to 7220                                       
      if (iswtbk .eq. 5) go to 7220                                       
      if (iswtbk .eq. 7) then                                           
        if (kompr(c41,c41x,kdum))7280,7120,7300                        
      endif                                                             
      if (base1-base1x) 7280,7120,7300                                  
 7120 if (iswtbk .eq. 6) go to 7220                                       
      if (iswtbk .eq. 7) go to 7220                                       
      if (iswtbk .eq. 9) go to 7160                                       
      if (iswtbk .eq. 10) go to 7222                                       
      if (kompr(name2c,name2x,kdum)) 7280,7140,7300                     
 7140 if (base2-base2x) 7280,7160,7300                                  
 7160 if (kompr(id,idx,kdum)) 7280,7180,7300                            
 7180 if (iswtbk .eq. 9) go to 7880                                       
      if (ksect-ksectx) 7280,7200,7300                                  
 7200 if (iswtbk .eq. 2) go to 7380                                       
      go to 7220                                                        
C                                                                       
C     MERGING DIRECT CURRENT SUB TYPES                                  
C                                                                       
 7222 if (subtyp .eq. 'B') go to 7290                                     
      if (subtp .eq. subtpx) go to 7220                                     
      if (subtp .eq. ' '   ) go to 7280                                     
      go to 7320                                                        
7220  if (kchgcd .eq. 'D') goto 7260                                     
      ich=ich+1                                                         
      if (kchgcd .eq. 'M') goto 7360                                     
      ich=ich+1                                                         
      write (errbuf(1),7240) jwrk80(ix)                               
 7240 format ('0', a  )                                                 
      write (errbuf(2),7241)                                          
 7241 format ('  THIS CHANGE CARD ALREADY EXISTS IN THE BASE CASE. HENCE
     1,IT CAN NOT BE ADDED.')                                           
      call prterr ('E',2)                                             
      iabort=1                                                          
      go to 6340                                                        
 7260 ich=ich+2                                                         
      go to 6340                                                        
 7280 if (iswtbk .ne. 10) go to 7285                                        
      if (subtyp .eq. 'B') go to 7290                                    
 7285 call ritecc(work(1,im),kecs,8)                                    
      kecs=kecs+8                                                       
      go to 6620                                                        
 7290 ich = ich + 1                                                     
      go to 6620                                                        
 7300 continue                                                          
7320  if (kchgcd .eq. ' ') goto 7360                                     
      write (errbuf(1),7340) jwrk80(ix)                               
 7340 format ('0', a  )                                                 
      write (errbuf(2),7341)                                          
 7341 format (' THIS CHANGE CARD DOES NOT EXIST IN THE BASE CASE. HENCE,
     1COLUMN 3 MUST BE BLANK.')                                         
      call prterr ('E',2)                                             
      iabort=1                                                          
      im=im-1                                                           
      ich=ich+1                                                         
      go to 6340                                                        
7360  call ritecc (jwork(1,ix),kecs,8)                                
      kecs=kecs+8                                                       
      if (kswt1 .eq. 1) go to 6340                                          
      if (kchgcd .eq. 'M') goto 6340                                     
      im=im-1                                                           
      go to 6340                                                        
 7380 read (jwrk80(ix) ,7400) name3x,base3x,name4x,base4x,idx,ksectx     
 7400 format (bz,34x,a8,f4.0,2x,a8,f4.0,a1,i1)                             
      read (work80(im) ,7400) name3c,base3,name4c,base4,id,ksect         
      if (kompr(name3c,name3x,kdum)) 7280,7420,7300                     
 7420 if (base3-base3x) 7280,7440,7300                                  
 7440 if (kompr(name4c,name4x,kdum)) 7280,7460,7300                     
 7460 if (base4-base4x) 7280,7480,7300                                  
 7480 if (kompr(id,idx,kdum)) 7280,7500,7300                            
 7500 if (ksect-ksectx) 7280,7220,7300                                  
C                                                                       
C     READING CARDS IMAGES AND IDENTIFICATION TABLES FROM E.C.S. AND    
C     STORING THEM IN MASS STORAGE                                      
C                                                                      
 7520 itotal=ldumbb+ldumcc-ich                                          
      if (iswtbk .eq. 1) lrd = itotal                                    
      if (iswtbk .eq. 2) lrr = itotal                                    
      if (iswtbk .eq. 3) lsc = itotal                                    
      if (iswtbk .eq. 4) ldar = itotal                                   
      if (iswtbk .eq. 5) ldz = itotal                                    
      if (iswtbk .eq. 6) lrep = itotal                                   
      if (iswtbk .eq. 7) ls = itotal                                     
      if (iswtbk .eq. 8) ln = itotal                                     
      if (iswtbk .eq. 9) mac = itotal                                    
      if (iswtbk .eq. 10) jdctot = itotal                                
      jdc = jdctot                                                      
      write (l1,rec=msnew) itotal, itotal                                 
      if (keybrd(20) .eq. 0) go to 7542                                     
      go to (7561,7562,7563,7564,7565,7566,7567,7568,7569,7570),iswtbk  
 7561 write (outbuf,7571)                                               
 7571 format ('0',20x,' LOCAL RELAY CARDS ')                             
      call prtout (1)                                                   
      go to 7585                                                        
 7562 write (outbuf,7572)                                               
 7572 format ('0',20x,' REMOTE RELAY CARDS ')                            
      call prtout (1)                                                   
      go to 7585                                                        
 7563 write (outbuf,7573)                                               
 7573 format ('0',20x,' SERIES CAPACITOR CARDS ')                        
      call prtout (1)                                                   
      go to 7585                                                        
 7564 write (outbuf,7574)                                               
 7574 format ('0',20x,' LOAD REPRESENTATION CARDS BY AREA INTERCHANGE ') 
      call prtout (1)                                                   
      go to 7585                                                        
 7565 write (outbuf,7575)                                               
 7575 format ('0',20x,' LOAD REPRESENTATION CARDS BY ZONE ')             
      call prtout (1)                                                   
      go to 7585                                                        
 7566 write (outbuf,7576)                                               
 7576 format ('0',20x,' LOAD REPRESENTATION CARDS BY BUS - BASE ')       
      call prtout (1)                                                   
      go to 7585                                                        
 7567 write (outbuf,7577)                                               
 7577 format ('0',20x,' LOAD SHEDDING CARDS ')                           
      call prtout (1)                                                   
      go to 7585                                                        
 7568 write (outbuf,7578)                                               
 7578 format ('0',20x,' LOAD NETTING BUSSES ' )                          
      call prtout (1)                                                   
      go to 7585                                                        
 7569 write (outbuf,7579)                                               
 7579 format ('0',20x,' MACHINE DATA CARDS ')                            
      call prtout (1)                                                   
      go to 7585                                                        
 7570 write (outbuf,7582)                                               
 7582 format ('0',20x,' DIRECT CURRENT CARDS ')                          
      call prtout (1)                                                   
 7585 continue                                                          
      write (outbuf,7541) itotal,msnew                                  
 7541 format ('0','ITOTAL=',i10,'MSNEW= ',i10)                           
      call prtout (1)                                                   
 7542 kecs=TEMP_ECSADDR                                                        
      if (itotal .eq. 0) go to 7675                                        
 7580 if (itotal .gt. 100) go to 7600                                     
      i100=itotal                                                       
      kwords=8*i100                                                     
      call redecc (work,kecs,kwords)                                    
      go to 7620                                                        
 7600 i100=100                                                          
      kwords=8*i100                                                     
      call redecc (work,kecs,kwords)                                    
      kecs=kecs+kwords                                                  
 7620 itotal=itotal-i100                                                
      msnew=msnew+1                                                     
      write (l1,rec=msnew) ((work(j,i),j=1,8),i=1,i100)                 
      if (keybrd(20) .ne. 0) then
         do i = 1,i100                                                
            write (outbuf,7660) (work(j,i),j=1,8)                          
 7660       format (10x,8a10)                                                 
            call prtout (1)                                                
         enddo
         write (outbuf,6185)                                               
         call prtout (1)                                                   
      endif
      if (itotal .ne. 0) go to 7580                                       
 7675 continue                                                          
      go to (5840,5860,5880,5900,5920,5940,5960,8060,6000,8800), iswtbk 
 7677 continue                                                          
C                                                                       
C     MERGING PLANT DATA TYPES                                          
C                                                                       
 7880 if (type .eq. 'M') go to 7960                                        
      if (typex .eq. 'M') go to 7320                                       
      if (type .eq. 'G') go to 7900                                         
      if (typex .eq. 'G') go to 7320                                        
      if (type .eq. 'T') go to 7920                                         
      if (typex .eq. 'T') go to 7320                                        
      if (type .eq. 'E') go to 7940                                         
      if (typex .eq. 'E') go to 7320                                        
      if (type .eq. 'V') go to 7945                                       
      if (typex .eq. 'V') go to 7320                                      
      if (type .eq. 'W') go to 7950                                       
      if (typex .eq. 'W') go to 7320                                      
      if (type .eq. 'X') go to 7955                                    !dem
c     if (TYPE .EQ. 'X') GO TO 7955                                       
      if (typex .eq. 'X') go to 7320                                   !dem
c     if (TYPEX .EQ. 'X') GO TO 7320                                      
      if (type .eq. 'S' .and. subtp .ne. 'T') go to 8030                   
      if (typex .eq. 'S' .and. subtpx .ne. 'T') go to 7320                 
      if (type .eq. 'S' .and. subtp .eq. 'T') go to 8035                   
      if (typex .eq. 'S' .and. subtpx .eq. 'T') go to 7320                 
      go to 7220                                                        
 7900 if (typex .eq. 'G') go to 7220                                        
      go to 7280                                                        
 7920 if (typex .eq. 'T') go to 7220                                        
      go to 7280                                                        
 7940 if (typex .eq. 'E') go to 7220                                        
      go to 7280                                                        
7945  if (typex  .eq.  'V') go to 7220                                      
      go to 7280                                                       
7950  if (typex  .eq.  'W') go to 7220                                      
      go to 7280                                                       
 7955 if (typex .eq. 'X') go to 7220                                    !dem
c7955 if (TYPEX .EQ. 'X') GO TO 7220                                      
      go to 7280                                                       
 7960 if (typex .ne. 'M') go to 7280                                       
      if (subtp .eq. ' ') go to 8020                                    
      if (subtp .eq. 'Z') go to 7965                                          
      if (subtpx .eq. ' ') go to 7300                                     
      if (subtpx .eq. 'Z') go to 7280                                        
      go to 7220                                                        
 7965 if (subtpx .eq. 'Z') go to 7220                                        
      go to 7300                                                        
 8020 if (subtpx .eq. ' ') go to 7220                                   
      go to 7280                                                        
 8030 if (typex .eq. 'S' .and. subtpx .ne. 'T') go to 7220                
      go to 7280                                                        
 8035 if (typex .eq. 'S' .and. subtpx .eq. 'T') go to 7220                
      go to 7280                                                        
C                                                                       
C     MERGING LOAD NETTING CARDS                                        
C                                                                       
 8060 read (l9) loadf,lnetf,dat,lnf,ibold                               
      lns=lnf                                                           
      call repet1 (lnf,lnk,iswt,'LN ',msold,msnew)                      !dem
c     CALL REPET1 (LNF,LNK,ISWT,8,MSOLD,MSNEW)                          
      ln=0                                                              
      lswt=0                                                            
      go to (5980,8100,8120,8140), iswt                                 
C                                                                       
C     NO BASE CASE CARDS BUT CHANGE CARDS EXISTS                        
C                                                                       
 8100 jstart=1                                                          
      go to 8680                                                        
C                                                                       
C     YES BASE CASE CARDS BUT NO CHANGE CASE CARDS                      
C                                                                       
 8120 lnx=lnf                                                           
C                                                                       
C     YES BASE CASE CARDS AND YES CHANGE CASE CARDS                     
C                                                                       
 8140 read(l9) (jnetc(i),i=1,lnf),(jnetn(i),i=1,lnf),                   
     1 (basold(ii),ii=1,ibold)                                          
      do i=1,lnf                                                   
        kbold=jnetn(i)                                                  
        baseb4=basold(kbold)                                              
        kbnew=nambas(baseb4)                                              
        jnetn(i)=kbnew                                                  
      enddo
      if (keybrd(32) .eq. 0) go to 8170                                     
      do i=1,lnf                                                   
        kb=jnetn(i)                                                     
        baseb4=basekv(kb)                                                 
        write ( l6,8164) jnetc(i),baseb4                                  
 8164   format (1x,a8,2x,f6.1)                                             
      enddo
 8170 continue                                                          
      if (iswt .eq. 3)then                                                 
         do itrr = 1,lnf                                           
           lnetc(itrr) = jnetc(itrr)                                      
           lnetcc(itrr) = jnetcc(itrr)                                    
           lnetn(itrr) = jnetn(itrr)                                      
         enddo
         go to 8710                                                     
      endif                                                             
      msold=msold+1                                                     
      if (keybrd(32) .eq. 0) go to 8190                                     
      do i = 1,lnk                                                 
         write (outbuf,8180) lnetc(i),lnetn(i),lnetcc(i)                
 8180    format (10x,a8,i5,2x,a10)                                          
         call prtout (1)                                                
      enddo
 8190 continue                                                          
      jstart=0                                                        
      lstart=0                                                        
      lmod=0                                                          
 8440 jstart=jstart+1                                                   
      if (jstart .gt. lnk) go to 8680                                     
      jnkbsc=lnetc(jstart)                                            
      jnkbsn=lnetn(jstart)                                            
      kb=jnkbsn                                                       
      base=basekv(kb)                                                   
      lstart=lstart+1                                                   
      if (lstart .gt. lnf) go to 8680                                       
      do ll=lstart,lnf                                             
        if (jnkbsc .ne. jnetc(ll).or.                                      
     1     jnkbsn .ne. jnetn(ll)) go to 8600                              
        lstart=ll                                                         
        if (lnetcc(jstart) .eq. 'D') then
           lnetc(jstart)='ZZZZZZZZ'                                        
           lnetn(jstart)=99999                                             
           jnetc(ll)='ZZZZZZZZ'                                            
           jnetn(ll)=99999                                                 
           lmod=lmod+2                                                       
           go to 8440                                                        
        else
           write (outbuf,8450) jnkbsc,base                                   
 8450      format (1x,a8,2x,f6.1)                                             
           call prtout (1)                                                   
           if (lnetcc(jstart) .ne. 'M') then
              errbuf(1) = outbuf                                                
              write (errbuf(1)(17:),8540)                                       
 8540         format (' THIS LOAD NETTING BUS ALREADY EXITS IN THE BASE 
     & DATA.') 
              call prterr ('E',1)                                               
           else

              errbuf(1) = outbuf                                                
              write (errbuf(1)(17:),8580)                                       
 8580         format (' THERE IS NOTHING TO MODIFY ON THIS LOAD NETTING 
     & BUS.')  
              call prterr ('E',1)                                               
           endif
           go to 8650                                                        
         endif
 8600    continue
      enddo
      lstart=lstart-1                                                   
      if (lnetcc(jstart) .eq. ' ') goto 8440                             
      write (errbuf(1),8640) jnkbsc,base                                
 8640 format ('0',a8,f6.1,' THIS IS A CHANGE CARD CASE. HENCE, EACH NEW 
     1 BUS MUST HAVE BLANK CHANGE CODE...LOAD NETTING CD ERR')          
      call prterr ('E',1)                                               

 8650 istop3=1                                                          
      lswt=1                                                            
      lmod=lmod+1                                                       
      lnetc(jstart)='ZZZZZZZZ'                                        
      lnetn(jstart)=99999                                             
 8660 continue                                                          
      go to 8440                                                        

 8680 continue                                                          
      do i=1,lnk                                                   
         jnetc(lnf + i) = lnetc(i)                                     
         jnetn(lnf + i) = lnetn(i)                                     
      enddo
      lntotl=lnf+lnk                                                    
      lnx=lnf+lnk-lmod                                                  
      do itrr = 1,lntotl                                           
         lnetc(itrr) = jnetc(itrr)                                         
         lnetcc(itrr) = jnetcc(itrr)                                       
         lnetn(itrr) = jnetn(itrr)                                         
      enddo
      lqiks=13                                                        
      call qiksrt (1,lntotl,komp31,swap31)                              
 8710 if (keybrd(20) .ne. 0) then
         write (outbuf,8732)                                               
 8732    format ('0',8x,' LOAD NETTING BUSSES')                             
         call prtout (1)                                                   
         call skipln (1)                                                   
         do i=1,lnx                                                   
            kb = jnetn(i)                                                 
            base = basekv(kb)                                                 
            write (outbuf,8733) jnetc(i), base                            
 8733       format (1x,2hln,1x,a8,2x,f6.1)                                     
            call prtout (1)                                               
         enddo
      endif
      ln = lnx                                                          
      msnew=msnew+1                                                     
      go to 5980                                                        
C                                                                       
 8800 go to (8840,8840,8840,8820), isav                                 
 8820 isav=1                                                            
      go to 5800                                                        
 8840 return                                                            
      end                                                               
