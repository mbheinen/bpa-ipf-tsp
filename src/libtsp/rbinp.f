C    %W% %G%
      subroutine rbinp                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES THE RB RESISTANCE BREAK DATA CARD       
C * * * AND PLACES DATA IN THE BRAKE DATA ARRAY.  IT IS CALLED          
C * * * BY INPUT1                                                       
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/rbcom.inc' 
      include 'tspinc/brakn.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/reread.inc' 
      character*8 bus1c,bus2c,bus3c                                     
      character*1 iparc,id3                                             
C     -     begin     begin     begin     begin     begin     begin 
      read (buffer,1000) bus1c,base1,bus2c,base2,iparc,bus3c,base3,     
     1   id3,gbrake,ptrig1,vlow,tw,ttrip1,tdead,tinsrt,pbias1             
 1000 format (bz,6x,a8,f4.0,1x,a8,f4.0,a1,1x,a8,f4.0,a1,2f5.1,f4.3,f3.1,   
     1  2f4.1,f4.2,f5.1)                                                  
      if (tdead.le.0.0) tdead=0.01                                      
      if (tinsrt.le.0.0) tinsrt=0.01                                    
      if (gbrake.le.0.0) go to 2000                                     
      if(ptrig1) 2000,1200,1400                                         
C * * *                                                                 
C * * * PTRIG1 (PDROP) IS SET EQUAL TO A HIGH VALUE IF BLANK            
C * * *                                                                 
 1200 if(sign(1.,ptrig1).gt.0.) go to 2000                              
      ptrig1 = 1.0e10                                                   
 1400 if(vlow) 2000,1600,1800                                           
C * * *                                                                 
C * * *  VLOW IS SET EQUAL TO A HIGH VALUE IF BLANK                     
C * * *                                                                 
 1600 if(sign(1.,vlow).gt.0.) go to 2000                                
      vlow = 1.0e10                                                     
 1800 if(tw.le.0.0) go to 2000                                          
      if (ttrip1.le.0.0) go to 2000                                     
      go to 2600                                                        
 2000 iabort=1                                                          
      write (errbuf(1),2200) bus3c,base3,id3                            
      write (errbuf(2),2400)                                            
      call prterr ('E',2)                                               
 2200 format ('0  CHECK RB CARD FOR BRAKE AT ',a8,2x,f5.1,2x,a1)        
 2400 format ('   BRAKE PWR, POWER DROP, UNDERVOLTAGE, TW, AND TMAX ALL 
     1MUST BE POSITIVE NUMBERS')                                        
 2600 kb1=nambas(base1)                                                 
      kb2=nambas(base2)                                                 
      kb3=nambas(base3)                                                 
      ibus1=inam(bus1c,kb1)                                             
      jbus1=inam(bus2c,kb2)                                             
      ibusb=inam(bus3c,kb3)                                             
      if (ibus1.eq.0.or.jbus1.eq.0.or.ibusb.eq.0)then                   
         iabort = 1                                                     
         return                                                         
      endif                                                             
      irb=irb+1                                                         
      if(irb .gt. MAXRB)then                                            
         write(errbuf(1),2610)MAXRB                                     
 2610    format(5x,' MORE THAN ',i5,' RB CARDS.  JOB WILL ABORT.')      
         call prterr('E',1)                                             
         iabort = 1                                                     
         return                                                         
      endif                                                             
      if (iparc.eq.'0') iparc=' '                                       
      ibkbno(irb) = ibusb                                               
      ibkid(irb) = id3                                                  
      ibkib1(irb) = ibus1                                               
      ibkjb1(irb) = jbus1                                               
      irbidc(irb)=iparc                                                 
      irbidn(1,irb)=ibus1                                               
      irbidn(2,irb)=jbus1                                               
      irbidn(3,irb)=irb                                                 
      ircidc(irb)=' '                                                   
      ircidn(1,irb)=0                                                   
      ircidn(2,irb)=0                                                   
      ircidn(3,irb)=0                                                   
      bktw(irb) = tw                                                    
      bkpbi1(irb) = pbias1                                              
      bktrg1(irb) = ptrig1                                              
      bkvlow(irb) = vlow                                                
      bkgbrk(irb) = gbrake                                              
      bktnst(irb) = tinsrt                                              
      bktrp1(irb) = ttrip1                                              
      bktded(irb) = tdead                                               
      return                                                            
      end                                                               
