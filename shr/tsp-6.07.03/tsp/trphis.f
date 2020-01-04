C    %W% %G%
      subroutine trphis(itparn,ibusn,jbusn,gijt,bijt,giot,              
     1           biot,gjot,bjot,icden,idpar,jobdo,mcode)                
c                                                                      
C     This subroutine forms table TRIPL which is a history of       
C     lines that have been tripped.                                 
c
C     ITPARN is the branch parallel
c     CODE = 99 means from the default distance relay                                                
C     IBUSN, JBUSN are the bus numbers for the branch               
C     IRCAL denotes the type of relay operation:                    
C        1 = distance relay
C        2 = series cap
C        3 = power rate
C        4 = out of step (not available)
C        5 = under frequency line        
C        6 = remote relays type M,R,D
C        7 = default distance relay trip   
C        8 = default distance (reclose)                                 
c
c     GIJT,BIJT,GIOT,BIOT,GJOT,BJOT are pi equivalent data for branch  
c
c     ICDEN is flag to denote type of action:
c        1 = reclosing action (first entry)
C       -1 = tripping action (first entry)
C        2 = reclosing action (not first entry)
C       -2 = tripping action (not first entry)      
c
c     JOBDO is a flag to indicate relay action:
c        1 = do not trip       
c        2 = trip 
c
c     IDPAR is the branch parallel id                        
c     MCODE is a remote relay action code:
c        1 = branch impedance modification.                                                 
c                                                                      
      include 'tspinc/params.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/relays.inc' 
      include 'tspinc/trip1.inc' 
      include 'tspinc/ldropn.inc' 
      character*1 idpar                                                 

      ijsame = 0                                                        
      i99 = 0                                                           
      lspce = 0                                                         
      jobdo = 0                                                         
c                                                                      
c         CONVERT BRANCH PARALLEL CODE FROM CHARACTER TO INTEGER       
c                                                                      
      if(itparn .ne. 99) itparn = ichar(idpar)                          
      if(ntripl .eq. 0) go to 1800                                      
      do 1600 it = 1, ntripl                                            
      if((ltrpln(1, it) .ne. ibusn) .or. (ltrpln(2, it) .ne.            
     1     jbusn)) go to 1600                                           
c                                                                      
c        ENTRY WITH SAME BUS PAIR NAMES ALREADY EXISTS                 
c                                                                      
      ijsame = 1                                                        
      itbus = ltrpln(1, it)                                             
      jtbus = ltrpln(2, it)                                             
      itpar = ltrpln(3, it)                                             
      icde = ltrpln(4, it)                                              
      if(itparn.ne.99) go to 1000                                       
      i99 = 1                                                           
      icde = isign(icde,icden)                                          
      ltrpln(1, it) = itbus                                             
      ltrpln(2, it) = jtbus                                             
      ltrpln(3, it) = itpar                                             
      ltrpln(4, it) = icde                                              
 1000 if(iabs(icde).ne.1) go to 1200                                    
      gije = tripl(1, it + 1)                                           
      bije = tripl(1, it + 2)                                           
 1200 if(itpar.ne.itparn) go to 1600                                    
      if(ircall .eq.2) go to 1400                                       
      if(ircall.eq.6.and.mcode.eq.1) go to 1400                         
      if(i99.eq.1) go to 1600                                           
      if(isign(1,icde).eq.isign(1,icden)) go to 3000                    
c                                                                      
c       CHANGE IN BRANCH STATUS                                        
c                                                                      
      icde = isign(icde,icden)                                          
      ltrpln(1, it) = itbus                                             
      ltrpln(2, it) = jtbus                                             
      ltrpln(3, it) = itpar                                             
      ltrpln(4, it) = icde                                              
      jobdo = 1                                                         
      go to 3000                                                        
 1400 if(icde.gt.0) jobdo = 1                                           
      go to 3000                                                        
 1600 continue                                                          
 1800 if(ijsame.eq.1) icden = icden + icden                             
      jobdo = 1                                                         
      if(ntripl + 5 .gt. 500)then                                       
         write(errbuf(1),2000)                                          
 2000    format(' ARRAY TRIPL IN SUBROUTINE TRPHIS IS OVERFLOWING.',    
     1          ' CONSULT A PROGRAMMER.')                               
         call prterr('E',1)                                             
         call erexit                                                    
      endif                                                             
      ltrpln(1, ntripl + 1) = ibusn                                     
      ltrpln(2, ntripl + 1) = jbusn                                     
      ltrpln(3, ntripl + 1) = itparn                                    
      ltrpln(4, ntripl + 1) = icden                                     
      nt = 1                                                            
c                                                                      
c       BYPASS STORRING OF GIJE,BIJE FOR CERTAIN CONDITIONS            
c                                                                      
      if(ndfltd.eq.0) go to 2800                                        
      if(trclse.eq.0.) go to 2800                                       
      if(ircall .lt.3) go to 2800                                       
      if(iabs(icden).ne.1) go to 2800                                   
c                                                                      
c       GET GIJE, BIJE FOR RELAYED BRANCH                              
c                                                                      
      call getmat(ibusn, ii)                                                
      do 2200 i = 4, ii, 3                                              
      if(jbusn.eq.matrow(i)) go to 2600                                 
 2200 continue                                                          
      call mpost('TRPHIS')                                              
      name1 = bname(ibusn)                                              
      kv1 = buskv(ibusn)                                                
      name2 = bname(jbusn)                                              
      kv2 = buskv(jbusn)                                                
      write (errbuf(1), 2400) name1,kv1,name2,kv2                       
      call prterr ('E',1)                                               
 2400 format(1h0, 5x, 'ERROR IN AUTOMATIC LINE SWITCHING --UNABLE TO',  
     1           ' FIND LINE ',2(a8,1x,f5.1,2x),' IN ADMITTANCE TABLE') 
      call erexit                                                       
 2600 tripl(1, 2 + ntripl) = -atrow(i + 1)                              
      tripl(1, 3 + ntripl) = -atrow(i + 2)                              
      nt = nt + 2                                                       
 2800 if(itparn .ne. 99) then                                           
          tripl(1, nt + 1 + ntripl) = gijt                              
          tripl(2, nt + 1 + ntripl) = bijt                              
          tripl(1, nt + 2 + ntripl) = giot                              
          tripl(2, nt + 2 + ntripl) = biot                              
          tripl(1, nt + 3 + ntripl) = gjot                              
          tripl(2, nt + 3 + ntripl) = bjot                              
           nt = nt + 3                                                  
      endif                                                             
      ntripl = ntripl + nt                                              
 3000 if(ircall .eq. 4) then                                            
         write (errbuf(1),3200)                                         
         call prterr ('E',1)                                            
         call mpost('TRPHIS')                                           
 3200    format(1h0,5x,'ERROR...YOU HAVE ENTERRED THE LOGIC RESERVED ', 
     1   'FOR OUT OF STEP RELAY WHICH IS NOT YET OPERATIONAL.')         
         call erexit                                                    
      endif                                                             
      return                                                            
      end                                                               
