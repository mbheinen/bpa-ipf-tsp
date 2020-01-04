C    %W% %G%
      subroutine maxmin                                                 
c                                                                       
C     This subroutine calculates the machine with the largest and 
c     smallest internal angle and writes the names of these machines on 
c     the output file.  It also calculates the angle difference between 
c     these two machines and will abort the job it this angle exceeds 
c     500.0 degrees. It is called by subroutine CNTRL.                       
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/machd1.inc' 
      include 'tspinc/igentn.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
c *** csw addition
      include 'tspinc/vrgov.inc'
c *** csw end addition

      character*1 mxid,mnid                                        
      character*8 mxname, mnname                                        
      character string*130
      logical finished

      string = '*'

      i=1                                                               
      finished = .false.
      do while (i .le. isg .and. .not. finished)
         if (vfldtn(1,i) .le. 999.0) then
            if (govpwr(maxg) .ne. 0.0 .and. govpwr(ming) .ne. 0.0) then
               finished = .true.
            else
               maxg = i                                                            
               ming = i                                                            
               i = i + 1                                                             
            endif
         else
            i = i + 1                                                             
         endif
      enddo

      i = 1                                                               
      do while (i .le. isg) 
         if (vfldtn(1,i) .le. 999.0) then
            if (govpwr(i) .eq. 0.0) then
               if (anglt(i) .le. 10000.0) then
                  anglt(i)=10001.0                                                  
               else
                  anglt(i)=100001.0                                                  
               endif
            else if (anglt(maxg) .lt. anglt(i)) then
               maxg = i                                                            
               imxmn2 = 2                                                          
            else if (anglt(ming) .gt. anglt(i)) then
               ming = i                                                            
               imxmn2 = 2                                                          
            endif
         endif
         i = i + 1                                                             
      enddo

      idela = (anglt(maxg) - anglt(ming)) * 19.2 / 57.2957795                   
      if (limang .le. 0 .and. idela .ge. 170) then
         jexit=1                                                           
         write (errbuf(1), 280)                                            
  280    format ('0  ANGLE  BETWEEN  FARTHEST  TWO  MACHINES  EXCEEDS  5
     &00 degrees, case is  being  terminated  ')                   
         call prterr ('E',1)                                               
         maxp = igentn(1,maxg)                                             
         minp = igentn(1,ming)                                             
         mxname = bname(maxp)                                              
         mnname = bname(minp)                                              
         bkvmx = buskv(maxp)                                               
         bkvmn = buskv(minp)                                               
         mxid = igentc(maxg)                                               
         mnid = igentc(ming)                                               
         write (outbuf,900) anglt(maxg), mxname, bkvmx, mxid, 
     &      anglt(ming), mnname, bkvmn, mnid                              
 900     format(5x,' MAX ANGLE IS ',f10.2,' DEGREES AT ', a8, 1x,
     &      f5.1,1x,a1,' MIN ANGLE IS ',f10.2,' DEGREES AT ',a8,1x,
     &      f5.1,1x,a1)    
         call prtout(1)                                                    
         imxmn2 = 2                                                          
      else
         if (idela .gt. 128) idela = 128                                    
         if (idela .lt. 11) idela = 11                                        
         string(idela:idela) = '*'                                        
         write (string(1:10),312) to, lppwr                                        
  312    format (f7.2,'/',i2)                                                     
C                                                                       
C        SET CRTSW TO DISPLAY MONITOR GRAPH                            
C                                                                       
         ictemp = crtsw                                                    
         crtsw = 1                                                         
         write (outbuf, 320) string                                        
  320    format(1x, a)                                                     
         call prtout (1)                                                   
         crtsw = ictemp                                                    
      endif

      if (imxmn2 .eq. 2) then
         imxmn2 = 1                                                          
         maxp = igentn(1,maxg)                                             
         minp = igentn(1,ming)                                             
         mxname = bname(maxp)                                              
         mnname = bname(minp)                                              
         write (outbuf, 360) to, mxname, mnname                            
  360    format('+',83x,f7.2,' CYC   MAXGEN ', a8, 3x, 'MINGEN ',a8)         
         call prtout (1)                                                   
      endif
      return                                                            
      end                                                               
