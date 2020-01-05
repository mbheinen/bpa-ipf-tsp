C    %W% %G%
      subroutine endsum                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE WRITES AN UNDERFREQUENCY LOAD SHEDDING          
C * * * SUMMARY TO THE OUTPUT FILE AT THE END OF A STUDY.               
C * * * IT IS CALLED BY CNTRL.                                          
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/sumuf.inc' 
      include 'tspinc/prt.inc' 
      character*2 izone                                                 
      if(tuflod .eq. 0.0)return                                         
      if(indfeq .eq. 0)return                                           
      call forbtm                                                       
      call fortop                                                       
      call skipln(5)                                                    
      write(outbuf,100)                                                 
 100  format(49x,'UNDERFREQUENCY LOAD SHEDDING SUMMARY')                
      call prtout(1)                                                    
      call skipln(2)                                                    
      write(outbuf,200)tuflod                                           
 200  format(45x,'TOTAL UNDERFREQUENCY LOAD SHED = ',f8.3,' MW ')       
      call prtout(1)                                                    
      call skipln(2)                                                    
      do 220 itrr = 1, iznuf                                            
      write(outbuf,210)ufzone(itrr),ufznto(itrr)                        
 210  format(47x,'TOTAL LOAD SHED IN ZONE ',a2,' = ',f8.3,' MW ')       
      call prtout(1)                                                    
 220  continue                                                          
      call skipln(2)                                                    
      do 600 itrr = 1, indfeq                                           
      if(uflod(itrr) .eq. 0.0)go to 600                                 
      write(outbuf,300) uffreq(itrr), uflod(itrr)                       
 300  format(40x,'UNDERFREQUENCY LOAD SHED AT ',f5.2,' HZ = ',f8.3,     
     1       ' MW TOTAL')                                               
      call prtout(1)                                                    
      do 500 jtrr = 1, iznuf                                            
      if(ufznld(itrr,jtrr) .eq. 0.0)go to 500                           
      write(outbuf,310) ufzone(jtrr),ufznld(itrr,jtrr)                  
 310  format(45x,'LOAD SHED IN ZONE ',a2,' = ',f8.3,' MW AT BUSES:')    
      call prtout(1)                                                    
      do 400 ktrr = 1,ibusfq(itrr)                                      
      izone = iufzon(itrr,ktrr)                                         
      if(izone .eq. ufzone(jtrr))then                                   
         write(outbuf,320)ufbusn(itrr,ktrr),ufbkv(itrr,ktrr),           
     1         ufblod(itrr,ktrr),ufcyc(itrr,ktrr)                       
 320     format(44x,a8,1x,f5.1,5x,f7.3,' MW AT ',f7.2,' CYCLES')        
         call prtout(1)                                                 
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      call skipln(2)                                                    
 600  continue                                                          
      return                                                            
      end                                                               
