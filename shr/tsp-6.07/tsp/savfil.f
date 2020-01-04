C    %W% %G%
      subroutine savfil                                                 
c                                                                       
c     CREATES A NEW SAVED DATA FILE (FOR015) FROM DATA ENTERED        
c     ON THE INPUT FILE (FOR005).  CALLED BY SDATA.                   
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/reread.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/in1n.inc' 
      include 'tspinc/in1c.inc' 
      include 'tspinc/lnet.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/titles.inc' 

      character*10 machin(8,100),lsrcp(8,100),                        
     &             lrelay(8,100),lrrod(8,100),                         
     &             lrepdt(8,100),
     &             lshed(8,100),dccard(8,150)               
      character*8 plotd                                               
      logical debug                                                     !dem

      data plotd /'VER01'/                                              
c
c     Begin     Begin     Begin     Begin     Begin     Begin

      call mpost('SAVFIL')                                              
      debug = .true.                                                    !dem
      blnk10 = '          '

      if (debug)                                                        !dem
     &  call dbgeko ('SAVFIL - at start - fetching file 1 rec nums')    !dem

      call whrec1 ('MAC', ilo, ihi, isz)                                !dem
      if (debug)                                                        !dem
     &  call dbgwri ('  for gens, count rec # = ',ihi)                  !dem
      read (l1,rec=ihi) mac                                              !dem
c     read (l1,rec=101) MAC                                              
      call whrec1 ('LDA', ilo, ihi, isz)                                !dem
      if (debug)                                                        !dem
     &  call dbgwri ('  for area load rep, count rec # = ',ihi)         !dem
      read (l1,rec=ihi) ldar                                             !dem
c     read (l1,rec=139) LDAR                                             
      call whrec1 ('LDZ', ilo, ihi, isz)                                !dem
      if (debug)                                                        !dem
     &  call dbgwri ('  for zone load rep, count rec # = ',ihi)         !dem
      read (l1,rec=ihi) ldz                                              !dem
c     read (l1,rec=141) LDZ                                              
      call whrec1 ('LDB', ilo, ihi, isz)                                !dem
      if (debug)                                                        !dem
     &  call dbgwri ('  for bus  load rep, count rec # = ',ihi)         !dem
      read (l1,rec=ihi) lrep                                             !dem
c     read (l1,rec=145) LREP                                             
      call whrec1 ('LSH', ilo, ihi, isz)                                !dem
      if (debug)                                                        !dem
     &  call dbgwri ('  for load shedding, count rec # = ',ihi)         !dem
      read (l1,rec=ihi) ls                                               !dem
c     read (l1,rec=170) LS                                               
      call whrec1 ('RG ', ilo, ihi, isz)                                !dem
      read (l1,rec=ihi) lsc                                              !dem
c     read (l1,rec=177) LSC                                              
      call whrec1 ('RD ', ilo, ihi, isz)                                !dem
      read (l1,rec=ihi) lrd                                              !dem
c     read (l1,rec=180) LRD                                              
      call whrec1 ('RR ', ilo, ihi, isz)                                !dem
      read (l1,rec=ihi) lrr                                              !dem
c     read (l1,rec=184) LRR                                              
      call whrec1 ('DC ', ilo, ihi, isz)                                !dem
      read (l1,rec=ihi) jdctot,jdc                                       !dem
c     read (l1,rec=187) JDCTOT,JDC                                       
      call opents(4)                                                    

c     ! Write header
      write (l15) swdata,savout,swdate,swdate                            

c     ! Write number of base kV's
      write (l15) ibxyz,(basekv(i),i=1,ibxyz)                            

c     ! Write LOCAL RELAY header and number of records
      write (l15) relay,blnk10,datx,lrd                                 
      if (lrd .gt. 0) then
         call whrec1 ('RD ', ilo, ihi, isz)                                !dem
c        MSLRD=181                                                         
         mslrd = ihi + 1                                                   !dem
         if (debug)                                                        !dem
     &     call dbgwri ('  for local relays, data rec # = ',mslrd)         !dem
         numrec = lrd
         do while (numrec .gt. 0)
            num = min (100, numrec)
            read (l1,rec=mslrd) ((lrelay(i,j),i=1,8),j=1,num)                
            write (l15) ((lrelay(i,j),i=1,8),j=1,num)                         
            mslrd = mslrd + 1                                                     
            numrec = numrec - num
         enddo
      endif

c     ! Write REMOTE RELAY header and number of records
      write (l15) remote,relay,datx,lrr                                 
      if (lrr .gt. 0) then
         call whrec1 ('RR ', ilo, ihi, isz)                                !dem
         mslrr = ihi + 1                                                   !dem
c        MSLRR=185                                                         
         if (debug)                                                        !dem
     &     call dbgwri ('  for remote relays, data rec # = ',mslrr)        !dem
         read (l1,rec=mslrr) lrrod                                        
         write (l15) ((lrrod(i,j),i=1,8),j=1,lrr)                         
      endif

c     ! Write SERIES CAPACITOR header and number of records
      write (l15) series,capac,datx,lsc                                 
      if (lsc .gt. 0) then
         call whrec1 ('RG ', ilo, ihi, isz)                                !dem
         mslsc = ihi + 1                                                   !dem
c        MSLSC=178                                                         
         if (debug)                                                        !dem
     &     call dbgwri ('  for series cap relays, data rec # = ',mslsc)         !dem
         read (l1,rec=mslsc) lsrcp                                        
         write (l15) ((lsrcp(i,j),i=1,8),j=1,lsc)                         
      endif

c     ! Write LOAD REPRESENTATION BY AREA header and number of records
      write (l15) load,repr,datx,ldar                                   
      if (ldar .gt. 0) then
         call whrec1 ('LDA', ilo, ihi, isz)                                !dem
         mslrep = ihi + 1                                                  !dem
c        MSLREP=140                                                        
         if (debug)                                                        !dem
     &     call dbgwri ('  for area load rep, data rec # = ',mslrep)       !dem
         read (l1,rec=mslrep) (( lrepdt(i,j),i=1,8),j=1,ldar)              
         write (l15) ((lrepdt(i,j),i=1,8),j=1,ldar)                       
      endif

c     ! Write LOAD REPRESENTATION BY ZONE header and number of records
      write (l15) load,repr,datx,ldz                                    
      if (ldz .gt. 0) then
         call whrec1 ('LDZ', ilo, ihi, isz)                                !dem
         mslrep = ihi + 1                                                  !dem
c        MSLREP=142                                                        
         if (debug)                                                        !dem
     &      call dbgwri ('  for zone load rep, data rec # = ',mslrep)       !dem
         numrec = ldz
         do while (numrec .gt. 0)
            num = min (100, numrec)
            read (l1,rec=mslrep) ((lrepdt(i,j),i=1,8),j=1,num)                
            write (l15) ((lrepdt(i,j),i=1,8),j=1,num)                         
            mslrep = mslrep + 1                                                     
            numrec = numrec - num
         enddo
      endif

c     ! Write LOAD REPRESENTATION BY BUS header and number of records
      write (l15) load,repr,datx,lrep                                   
      if (lrep .gt. 0) then
         call whrec1 ('LDB', ilo, ihi, isz)                                !dem
         mslrep = ihi + 1                                                  !dem
c        MSLREP=146                                                        
         if (debug)                                                        !dem
     &     call dbgwri ('  for bus load rep, data rec # = ',mslrep)        !dem
         numrec = lrep
         do while (numrec .gt. 0)
            num = min (100, numrec)
            read (l1,rec=mslrep) ((lrepdt(i,j),i=1,8),j=1,num)                
            write (l15) ((lrepdt(i,j),i=1,8),j=1,num)                         
            mslrep = mslrep + 1                                                     
            numrec = numrec - num
         enddo
      endif

c     ! Write LOAD SHEDDING header and number of records
      write (l15) load,shed,datx,ls                                     
      if (ls .gt. 0) then
         call whrec1 ('LSH', ilo, ihi, isz)                                !dem
         msls = ihi + 1                                                    !dem
c        MSLS=171                                                          
         if (debug)                                                        !dem
     &     call dbgwri ('  for load shedding, data rec # = ',msls)         !dem
         numrec = ls
         do while (numrec .gt. 0)
            num = min (100, numrec)
            read (l1,rec=msls) ((lshed(i,j),i=1,8),j=1,num)                
            write (l15) ((lshed(i,j),i=1,8),j=1,num)                         
            msls = msls + 1                                                     
            numrec = numrec - num
         enddo
      endif

c     ! Write LOAD NETTING header and number of records
      write (l15) load,net,datx,ln,ibxyz                                 
      if (ln .gt. 0) then
         call whrec1 ('LN ', ilo, ihi, isz)                                !dem
         msln = ihi + 1                                                    !dem
c        MSLN=168                                                          
         if (debug)                                                        !dem
     &     call dbgwri ('  for load netting, data rec # = ',msln)          !dem
         write (l15) (lnetc(i),i=1,ln),(lnetn(i),i=1,ln),                   
     &               (basekv(ii),ii=1,ibxyz)                                 
1060     write (l15) machn,blnk10,datx,mac                                 
      endif

c     ! Write MACHINE (PLANT DATA) header and number of records
      if (mac .gt. 0) then
         call whrec1 ('MAC', ilo, ihi, isz)                                !dem
         msmac = ihi + 1                                                   !dem
c        MSMAC=102                                                         
         if (debug)                                                        !dem
     &      call dbgwri ('  for machines, data rec # = ',msmac)             !dem
         numrec = mac
         do while (numrec .gt. 0)
            num = min (100, numrec)
            read (l1,rec=msmac) ((machin(i,j),i=1,8),j=1,num)                
            write (l15) ((machin(i,j),i=1,8),j=1,num)                         
            msmac = msmac + 1                                                     
            numrec = numrec - num
         enddo
      endif

c     ! Write DIRECT CURRENT header and number of records
      write (l15) direct,currnt,datx,jdc                                 
      if (jdc .gt. 0) then
         call whrec1 ('DC ', ilo, ihi, isz)                                !dem
         msjdc = ihi + 1                                                   !dem
c        MSJDC=188                                                         
         if (debug)                                                        !dem
     &      call dbgwri ('  for dc convertors, data rec # = ',msjdc)        !dem
        read (l1,rec=msjdc) (( dccard(i,j),i=1,8),j=1,jdc)               
        write (l15) ((dccard(i,j),i=1,8),j=1,jdc)                        
      endif
 
      write (l15) swdata,xend,blnk10,blnk10                              
      call closts (l15,0)                                              

      return                                                            
      end                                                               
