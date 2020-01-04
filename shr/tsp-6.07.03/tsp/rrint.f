C    %W% %G%
      subroutine rrint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONVERTS THE BUS NUMBERS TO THE SWING           
C * * * INTERNAL ORDER AND FINDS THE INDEX IN IGENT TABLES              
C * * * FOR THE GENERATORS.  IT IS CALLED BY INITL2.                    
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/igentn.inc' 
      include 'tspinc/rrcom.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      character*8 name1c                                                
      character*1 id1,id2                                               
C * * *                                                                 
C * * * REORDER BUS NUMBERS IN REMOTE RELAY TABLE                       
C * * *                                                                 
      do 100 itrr = 1,kntrr                                             
      ibs1rr(itrr) = indx2n(ibs1rr(itrr))                               
      jbs1rr(itrr) = indx2n(jbs1rr(itrr))                               
      if(ibs2rr(itrr) .ne. 0)ibs2rr(itrr)=indx2n(ibs2rr(itrr))          
      if(jbs2rr(itrr) .ne. 0)jbs2rr(itrr)=indx2n(jbs2rr(itrr))          
  100 continue                                                          
C * * *                                                                 
C * * * FIND THE INDICES IN THE GENERATOR IGENT TABLES FOR ANY          
C * * * GENERATORS SPECIFIED ON A REMOTE RELAY CARD                     
C * * *                                                                 
      idum = 0                                                          
      do 2000 ind = 1,kntrr                                             
      if(mkodrr(ind) .ne. 5) go to 2000                                 
      ibus = ibs2rr(ind)                                                
      jbus = jbs2rr(ind)                                                
      id1 = ipr2rr(ind)                                                 
      id2 = jpr2rr(ind)                                                 
      if(ibus.eq.0) go to 1840                                          
C * * *                                                                 
C * * * DETECT LP MACHINE ID AND IGNORE THE MACHINE                     
C * * *                                                                 
      if(id1 .eq. 'L' .or. id2 .eq. 'L') then                           
         name1c = exnamc(ibus)                                          
         kv = ixnamn(ibus)                                              
         bkv1 = basekv(kv)                                              
         write(errbuf(1),1720) name1c,bkv1                              
 1720    format(' LOW LEVEL GENERATOR ENTERED ON REMOTE RELAY ',        
     1          'CARD ',a8,1x,f5.1,' MUST BE HIGH LEVEL.')              
         iabort = 1                                                     
         call mpost('RRINT')                                            
         call prterr('E',1)                                             
         if(jbus .ne. idum) go to 1840                                  
         go to 2000                                                     
      endif                                                             
 1740 do 1760 i=1,isg                                                   
      if(igentn(1,i) .ne. ibus) go to 1760                              
      if(igentc(i) .eq. id1) go to 1790                                 
 1760 continue                                                          
      name1c = exnamc(ibus)                                             
      kv = ixnamn(ibus)                                                 
      bkv1 = basekv(kv)                                                 
      write (errbuf(1), 1780) name1c, bkv1,id1                          
      call mpost('RRINT')                                               
      call prterr ('E',1)                                               
 1780 format(1h0, 5x, 'ERROR IN REMOTE GENERATION DROPPING--CANNOT ',   
     1           'FIND GENERATOR SPECIFIED AT ', a8, 1x, f5.1, 1x, a1)  
      iabort = 1                                                        
 1790 if(jbus.ne.idum) go to 1840                                       
      idg2rr(ind) = i                                                   
      go to 2000                                                        
 1840 idum=ibus                                                         
      idg1rr(ind) = i                                                   
      if(jbus.eq.0) go to 2000                                          
      ibus=jbus                                                         
      jbus=idum                                                         
      id1=id2                                                           
      go to 1740                                                        
 2000 continue                                                          
      return                                                            
      end                                                               
