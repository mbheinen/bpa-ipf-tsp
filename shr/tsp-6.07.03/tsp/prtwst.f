C    %W% %G%
      subroutine prtwst(eyrint,eyiint)                                  
c                                                                       
c       THIS SUBROUTINE WRITES THE 20 LOWEST BUS VOLTAGES AND BUS       
c       FREQUENCIES TO THE OUTPUT FILE AND TO A CHARACTER ARRAY         
c       WRSCOM WHICH IS USED IN SUBROUTINE PLTCOM.  IT IS CALLED        
c       BY NOUT2.                                                       
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/wcom.inc' 
      include 'tspinc/wga.inc' 
      include 'tspinc/worst.inc' 
      include 'tspinc/wgv.inc' 
      include 'tspinc/wgs.inc' 
      include 'tspinc/wgf.inc' 
      include 'tspinc/wfeq.inc' 
      include 'tspinc/hivlt.inc' 
      include 'tspinc/fltim.inc' 

      dimension eyrint(MAXBUS),eyiint(MAXBUS)                           
      dimension rellov(20),relhiv(20)                                   

      do 20 itr=1,20                                                    
      if(wrstkv(itr) .eq. 0.0)go to 20                                  
      ibno = iwrsno(itr)                                                
      vmag = sqrt(eyrint(ibno)*eyrint(ibno) + eyiint(ibno)*             
     1         eyiint(ibno))                                            
      rellov(itr) = (wrstvt(itr)/vmag)*100.                             
  20  continue                                                          
      do 30 itr=1,20                                                    
      if(hivkv(itr) .eq. 0.0)go to 30                                   
      ibno = ihivno(itr)                                                
      vmag = sqrt(eyrint(ibno)*eyrint(ibno) + eyiint(ibno)*             
     1         eyiint(ibno))                                            
      relhiv(itr) = (hivvt(itr)/vmag)*100.                              
  30  continue                                                          
      call forbtm                                                       
      call fortop                                                       
      call skipln (1)                                                   
      if(ifltkt .ne. 0)then                                             
         write(outbuf,100)                                              
         call prtout(1)                                                 
         iwckt = iwckt + 1                                              
         write(wrscom(iwckt),100)                                       
 100     format(5x,'LOW BUS VOLTAGES WERE NOT CALCULATED')                                              
         write(outbuf,200)                                              
         call prtout(1)                                                 
         iwckt = iwckt + 1                                              
         write(wrscom(iwckt),200)                                       
 200     format(5x,'DURING THE FOLLOWING FAULT PERIODS.')               
         do 400 l = 1,ifltkt                                            
         write(outbuf,300)fstrt(l), fstop(l)                            
         call prtout(1)                                                 
         iwckt = iwckt + 1                                              
         write(wrscom(iwckt),300)fstrt(l),fstop(l)                      
 300     format(15x,f8.3,' CYCLES TO ',f8.3,' CYCLES ')                 
 400     continue                                                       
      endif                                                             
      call skipln (5)                                                   
      write(outbuf,500)                                                 
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),500)                                          
 500  format(5x,'THE FOLLOWING 20 BUSES HAVE THE LOWEST BUS VOLTAGES')  
      write(outbuf,600)                                                 
 600  format(5x,'DURING NONFAULT PERIODS. ')                            
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),600)                                          
      call skipln (1)                                                   
      write(outbuf,700)                                                 
 700  format(5x,'BUS NAME',2x,'BASE KV',2x,'VOLTAGE(PU)',2x,            
     1          'RELATIVE %',2x,'TIME(CYCLES)')                         
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),700)                                          
      do 900 itrr = 1,20                                                
      if(wrstkv(itrr) .eq. 0.0) go to 900                               
      write(outbuf,800)wrstnm(itrr),wrstkv(itrr),wrstvt(itrr),          
     1                     rellov(itrr),wrsttm(itrr)                    
  800 format(5x,a8,3x,f5.1,6x,f6.4,8x,f6.2,7x,f7.2)                     
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),800)wrstnm(itrr),wrstkv(itrr),wrstvt(itrr),   
     1                     rellov(itrr),wrsttm(itrr)                    
  900 continue                                                          
      call forbtm                                                       
      call fortop                                                       
      call skipln (5)                                                   
      write(outbuf,920)                                                 
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),920)                                          
 920  format(5x,'THE FOLLOWING 20 BUSES HAVE THE HIGHEST BUS VOLTAGES') 
      if(wtim2 .ne. -1.)then                                            
         write(outbuf,1050)wtim1,wtim2                                  
         call prtout(1)                                                 
         iwckt = iwckt +1                                               
         write(wrscom(iwckt),1050)wtim1,wtim2                           
      endif                                                             
      call skipln (1)                                                   
      write(outbuf,960)                                                 
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),960)                                          
 960  format(5x,'BUS NAME',2x,'BASE KV',2x,'VOLTAGE(PU)',2x,            
     1          'RELATIVE %',2x,'TIME(CYCLES)')                         
      do 980 itrr = 1,20                                                
      if(hivkv(itrr) .eq. 0.0) go to 980                                
      write(outbuf,970)hivnm(itrr),hivkv(itrr),hivvt(itrr),             
     1                     relhiv(itrr),hivtim(itrr)                    
      call prtout(1)                                                    
  970 format(5x,a8,3x,f5.1,6x,f6.4,7x,f6.2,8x,f7.2)                     
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),970)hivnm(itrr),hivkv(itrr),hivvt(itrr),      
     1                     relhiv(itrr),hivtim(itrr)                    
  980 continue                                                          
      call forbtm                                                       
      call fortop                                                       
      call skipln (5)                                                   
      write(outbuf,1000)                                                
      call prtout(1)                                                    
 1000 format(5x,'THE FOLLOWING 20 BUSES HAVE THE LOWEST BUS ',          
     1        'FREQUENCIES')                                            
      iwckt = iwckt+1                                                   
      write(wrscom(iwckt),1000)                                         
      if(wtim2 .eq. -1.)then                                            
         write(outbuf,600)                                              
         call prtout(1)                                                 
         iwckt = iwckt + 1                                              
         write(wrscom(iwckt),600)                                       
      else                                                              
         write(outbuf,1050)wtim1,wtim2                                  
 1050    format(5x,'DURING TIME WINDOW ',f8.2,' CYCLES TO ',f8.2,       
     1          ' CYCLES.')                                             
         call prtout(1)                                                 
         iwckt = iwckt +1                                               
         write(wrscom(iwckt),1050)wtim1,wtim2                           
      endif                                                             
      call skipln (1)                                                   
      write(outbuf,1100)                                                
      call prtout(1)                                                    
      iwckt = iwckt+1                                                   
      write(wrscom(iwckt),1100)                                         
 1100 format(5x,'BUS NAME',2x,'BASE KV',2x,'FREQUENCY(HERTZ)',          
     1          2x,'TIME(CYCLES)')                                      
      do 1300 itrr = 1,20                                               
      if(wfeqkv(itrr) .eq. 0.0) go to 1300                              
      write(outbuf,1200)wfeqnm(itrr),wfeqkv(itrr),wfeqmg(itrr),         
     1                     wfeqtm(itrr)                                 
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),1200)wfeqnm(itrr),wfeqkv(itrr),wfeqmg(itrr),  
     1                     wfeqtm(itrr)                                 
 1200 format(5x,a8,3x,f5.1,6x,f8.4,12x,f7.2)                            
 1300 continue                                                          
      if(iwgfsw .eq. 0) go to 1750                                      
      call forbtm                                                       
      call fortop                                                       
      call skipln (5)                                                   
      write(outbuf,1400)                                                
      call prtout(1)                                                    
 1400 format(5x,'THE FOLLOWING 20 GENERATORS HAVE THE HIGHEST ',        
     1        'FREQUENCY DEVIATIONS')                                   
      iwckt = iwckt +1                                                  
      write(wrscom(iwckt),1400)                                         
      call skipln (1)                                                   
      write(outbuf,1500)                                                
      call prtout(1)                                                    
 1500 format(5x,'GEN NAME',2x,'BASE KV',2x,'ID',2x,'frequency(hertz)',
     1          2x,'time(cycles)')                                      
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),1500)                                         
      do 1700 itrr = 1,20                                               
      if(wgfkv(itrr) .eq. 0.0) go to 1700                               
      write(outbuf,1600)wgfnam(itrr),wgfkv(itrr),wgfid(itrr),           
     1                  wgfval(itrr),wgftim(itrr)                       
      call prtout(1)                                                    
 1600 format(5x,a8,3x,f5.1,2x,a2,2x,f8.4,12x,f7.2)                      
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),1600)wgfnam(itrr),wgfkv(itrr),wgfid(itrr),    
     1                  wgfval(itrr),wgftim(itrr)                       
 1700 continue                                                          
 1750 if(iwgvsw .eq. 0) go to 2200                                      
      call forbtm                                                       
      call fortop                                                       
      call skipln (5)                                                   
      write(outbuf,1800)                                                
      call prtout(1)                                                    
 1800 format(5x,'The following 20 generators have the highest ',
     1        'field voltage deviations')                               
      iwckt = iwckt + 1                                                 
      write (wrscom(iwckt),1800)                                        
      call skipln (1)                                                   
      write(outbuf,1900)                                                
      call prtout(1)                                                    
 1900 format(5x,'GEN NAME',2x,'BASE KV',2x,'ID',2x,'FIELD VLT DEV',
     1          2x,'TIME(CYCLES)')                                      
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),1900)                                         
      do 2100 itrr = 1,20                                               
      if(wgvkv(itrr) .eq. 0.0) go to 2100                               
      write(outbuf,2000)wgvnam(itrr),wgvkv(itrr),wgvid(itrr),           
     1                  wgvval(itrr),wgvtim(itrr)                       
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),2000)wgvnam(itrr),wgvkv(itrr),wgvid(itrr),    
     1                  wgvval(itrr),wgvtim(itrr)                       
 2000 format(5x,a8,3x,f5.1,2x,a2,2x,f8.4,12x,f7.2)                      
 2100 continue                                                          
 2200 if(iwgssw .eq. 0) go to 2700                                      
      call forbtm                                                       
      call fortop                                                       
      call skipln (5)                                                   
      write(outbuf,2300)                                                
      call prtout(1)                                                    
 2300 format(5x,'THE FOLLOWING 20 GENERATORS HAVE THE HIGHEST ',        
     1        'PSS OUTPUT DEVIATIONS')                                  
      iwckt = iwckt + 1                                                 
      write (wrscom(iwckt),2300)                                        
      call skipln (1)                                                   
      write(outbuf,2400)                                                
      call prtout(1)                                                    
 2400 format(5x,'GEN NAME',2x,'BASE KV',2x,'ID',2x,'PSS OUT DEV', 
     1          2x,'TIME(CYCLES)')                                      
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),2400)                                         
      do 2600 itrr = 1,20                                               
      if(wgskv(itrr) .eq. 0.0) go to 2600                               
      write(outbuf,2500)wgsnam(itrr),wgskv(itrr),wgsid(itrr),           
     1                  wgsval(itrr),wgstim(itrr)                       
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),2500)wgsnam(itrr),wgskv(itrr),wgsid(itrr),    
     1                  wgsval(itrr),wgstim(itrr)                       
 2500 format(5x,a8,3x,f5.1,2x,a2,2x,f8.4,8x,f7.2)                       
 2600 continue                                                          
 2700 if(iwgasw .eq. 0) go to 3200                                      
c                                                                       
c     CONVERT ANGLE DEVIATION FROM RADIANS TO DEGREES                 
c                                                                       
      do 2750 jtrr = 1,20                                               
      wgaval(jtrr) = wgaval(jtrr)*57.29578                              
 2750 continue                                                          
      call forbtm                                                       
      call fortop                                                       
      call skipln (5)                                                   
      write(outbuf,2800)                                                
      call prtout(1)                                                    
 2800 format(5x,'THE FOLLOWING 20 GENERATORS HAVE THE HIGHEST ',        
     1        'ANGLE DEVIATIONS')                                       
      iwckt = iwckt + 1                                                 
      write (wrscom(iwckt),2800)                                        
      call skipln (1)                                                   
      write(outbuf,2900)                                                
      call prtout(1)                                                    
 2900 format(5x,'GEN NAME',2x,'BASE KV',2x,'ID',2x,' ANGLE  DEV',   
     1          2x,'TIME(CYCLES)')                                      
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),2900)                                         
      do 3100 itrr = 1,20                                               
      if(wgakv(itrr) .eq. 0.0) go to 3100                               
      write(outbuf,2500)wganam(itrr),wgakv(itrr),wgaid(itrr),           
     1                  wgaval(itrr),wgatim(itrr)                       
      call prtout(1)                                                    
      iwckt = iwckt + 1                                                 
      write(wrscom(iwckt),3000)wganam(itrr),wgakv(itrr),wgaid(itrr),    
     1                  wgaval(itrr),wgatim(itrr)                       
 3000 format(5x,a8,3x,f5.1,2x,a2,8x,f8.4,2x,f7.2)                       
 3100 continue                                                          
 3200 call forbtm                                                       
      call fortop                                                       
      return                                                            
      end                                                               
