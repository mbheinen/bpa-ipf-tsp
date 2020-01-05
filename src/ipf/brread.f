C    @(#)brread.f	20.5 2/13/96
      subroutine brread 
     
      include 'ipfinc/parametr.inc' 

      include 'ipfinc/addata.inc' 
      include 'ipfinc/arcntl.inc' 
      include 'ipfinc/area.inc' 
      include 'ipfinc/arsort.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/branch.inc' 
      include 'ipfinc/brsrt.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/filnam.inc' 
      include 'ipfinc/jobctl.inc' 
      include 'ipfinc/lfiles.inc' 
      include 'ipfinc/mrgtxt.inc' 
      include 'ipfinc/oldfil.inc' 
      include 'ipfinc/oldsrt.inc' 
      include 'ipfinc/prt.inc' 
 
      common /is_batch / is_batch

      character xbuf*120, subtyp*1, areac*10, datebr*3,  skiprec * 8,
     &          xbuf132*132
      integer error 
      logical eof 

c     Initialize parameters     
 
      skiprec = '.Cc*! ' // char(10) // char(12) 
 
c     The branch data is processed in a sequential order,           
c     defined by MBRSW.  The order is
                                                                        
c     MBRSW = 1 : branch data from master branch data file(s)            
c             2 : branch data from merged text                           
c             3 : branch data from input file                            
c             4 : branch data from datai file                            
c             5 : branch data base case in residence                     
c             6 : no additional branch data                              
                                                                        
                                                                        
      if (brdnam.ne.' ') then                                           
 
c        Process branch data file. Note special xbuf132
 
         oldbrd = brdnam                                                  
         inptx = brndta                                                   
         ierr=0                                                           
         call opnfil (inptx,brdnam,ierr)                                  
         if (ierr.ne.0) go to 110                                         
         datebr = crun1(1)(1:1) // crun1(2)(1:2)                          
         yearbr = energd (datebr)                                         
         if (yearbr .eq. -9999.0) then                                    
            write (errbuf(1), 80) datebr                                  
   80       format ('Illegal "date-in" data (', a,  
     &              ') on control record') 
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            call erexit (0)                                                 
         endif                                                            
         card = xbuf(1:1) 
         mbrsw = 1 
         eof = .false. 
         do while (.not. eof)         ! Read branch data file 
            read (inptx, 100, end=110) xbuf132
  100       format (a) 
            card = xbuf132(1:1) 
            if (index ('LERT', card) .ne. 0) then  
               call ldbrdata(xbuf132, mbrsw, yearbr, error) 
            else if (xbuf(1:2) .eq. '.#') then
c
c              Process .# case info 
c
               call casetxt (xbuf)
            else if (index (skiprec, card) .ne. 0) then ! Skip illegal  
C                                                       ! records 

            else 
               write (errbuf(1), 120)              
               write (errbuf(2), 130) xbuf132(1:80)   
               call prterx ('W',2)                 
            endif 
         enddo 
  110    continue 
         xbuf = inrcd                                                   
         call close_file (inptx)                                               
         brdnam = ' ' 
      endif 
                                                                       
      if  (bsbrnm.ne.' ') then                                           
 
c        Process bus/branch data file 
 
         oldbsd = bsbrnm                                                  
         inptx = busbrn                                                   
         ierr=0                                                           
         call opnfil (inptx,bsbrnm,ierr)                                  
         if (ierr .ne. 0) go to 180                                         
         datebr = crun1(1)(1:1) // crun1(2)(1:2)                          
         yearbr = energd (datebr)                                         
         if (yearbr .eq. -9999.0) then                                    
            write (errbuf(1), 80) datebr                                  
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            call erexit (0)                                                 
         endif                                                            
         card = xbuf(1:1) 
         mbrsw = 1 
         eof = .false. 
         do while (.not. eof)         ! Read branch data file 
            read (inptx, 100, end=180) xbuf 
            card = xbuf(1:1) 
            if (index ('LERT', card) .ne. 0) then  
               call ldbrdata(xbuf, mbrsw, yearbr, error) 
            else if (index ('B+X', card) .ne. 0) then ! Skip illegal  
C                                                     ! records 
               write (errbuf(1), 120) 
  120          format('Data record out of order. Card ignored.') 
               write (errbuf(2), 130) xbuf(1:80)                 
  130          format(11x,'(',a80,')')                           
               call prterx ('W',2)                               
            else if (card .eq. 'I') then                         
               write(errbuf(1),140)                              
  140          format ('Area intertie "I" record out of order.')
               write (errbuf(2), 120) xbuf(1:80)        
               if (is_batch .eq. 0) then
                  call prterx ('E',2)
               else
                  call prterx ('F',2)
               endif
               kabort = 1                        
               ntotic = 0                          
            else if (card .eq. 'A') then          
               subtyp = xbuf(2:2)            
               areac = xbuf(4:13)                 
               if (subtyp .eq. 'C' .or .subtyp .eq. ' ') then    
                  write (errbuf(1), 150) areac    
  150             format('0 Area interchange record ',a10, 
     &                   ' is out of order') 
                  write (errbuf(2),160)                                        
  160             format('Area interchange control aborted.')              
                  call prterx ('W',2)                                          
                  kabort = 1                                                   
                  ntotc=0                                                      
               else                                                            
                  write (errbuf(1), 170) areac    
  170             format ('Area sort record ',a10, 
     &                    ' is out of order.  Area sort abandoned.') 
                  call prterx ('W',1)                                          
                  natot=0                                                      
               endif 
            else if (xbuf(1:2) .eq. '.#') then
c
c              Process .# case info 
c
               call casetxt (xbuf)
            else if (index (skiprec, card) .ne. 0) then ! Skip illegal  
C                                                       ! records 
            else 
               write (errbuf(1), 120)                 
               write (errbuf(2), 130) xbuf(1:80)      
               call prterx ('W',2)                    
            endif 
         enddo 
 180     continue 
         xbuf = inrcd                                                   
         call close_file (inptx)                                               
         bsbrnm = ' ' 
      endif 
 
      if (nbrmrg .gt. 0) then 
  
c        Process base merge file 
  
         card = xbuf(1:1) 
         mbrsw = 2 
         eof = .false. 

         do while (.not. eof)  ! Read bus merge file 
            read (logmbr, 100, end=190) xbuf 
            card = xbuf(1:1) 
            if (index ('LERT', card) .ne. 0) then  
               call ldbrdata(xbuf, mbrsw, yearbr, error) 
            else if (index ('B+X', card) .ne. 0) then ! Skip illegal  
C                                                     ! records 

               write (errbuf(1), 120)                 
               write (errbuf(2), 130) xbuf(1:80)      
               call prterx ('W',2)                    
            else if (card .eq. 'I') then              
               write(errbuf(1),140)                                            
               write (errbuf(2), 120) xbuf(1:80)                                
               if (is_batch .eq. 0) then
                  call prterx ('E',2)
               else
                  call prterx ('F',2)
               endif
               kabort = 1                                                      
               ntotic = 0                                                      
            else if (card .eq. 'A') then                       
               subtyp = xbuf(2:2)                                              
               areac = xbuf(4:13)                                               
               if (subtyp .eq. 'C' .or .subtyp .eq. ' ') then 
                  write (errbuf(1), 150) areac                 
                  write (errbuf(2),160)                                        
                  call prterx ('W',2)                                          
                  kabort = 1                                                   
                  ntotc=0                                                      
               else                                                            
                  write (errbuf(1), 170) areac                 
                  call prterx ('W',1)                                          
                  natot=0                                                      
               endif 
            else if (xbuf(1:2) .eq. '.#') then
c
c              Process .# case info 
c
               call casetxt (xbuf)
            else if (index (skiprec, card) .ne. 0) then ! Skip illegal  
C                                                       ! records 

            else 
               write (errbuf(1), 120)                         
               write (errbuf(2), 130) xbuf(1:80)              
               call prterx ('W',2)                            
            endif 
         enddo 
 190     continue 
         xbuf = inrcd                                                   
      endif 
 
c     Process normal input file. Note that existing "buf" entity 
c     forces process-read instead of read-process. 
  
      xbuf = buf 
      card = xbuf(1:1) 
      mbrsw = 3 
      eof = .false. 

      do while (.not. eof)  ! Read bus merge file 
         if (index ('LERT', card) .ne. 0) then  
            call ldbrdata(xbuf, mbrsw, yearbr, error) 
         else if (index ('B+X', card) .ne. 0) then ! Skip illegal  
C                                                  ! records 
            write (errbuf(1), 120)                                              
            write (errbuf(2), 130) xbuf(1:80)                                   
            call prterx ('W',2)                                                
         else if (card .eq. 'I') then                         
            write(errbuf(1),140)                                            
            write (errbuf(2), 120) xbuf(1:80)                                
            if (is_batch .eq. 0) then
               call prterx ('E',2)
            else
               call prterx ('F',2)
            endif
            kabort = 1                                                      
            ntotic = 0                                                      
         else if (card .eq. 'A') then                                         
            subtyp = xbuf(2:2)                                              
            areac = xbuf(4:13)                                               
            if (subtyp .eq. 'C' .or .subtyp .eq. ' ') then                      
               write (errbuf(1), 150) areac                                   
               write (errbuf(2),160)                                        
               call prterx ('W',2)                                          
               kabort = 1                                                   
               ntotc=0                                                      
            else                                                            
               write (errbuf(1), 170) areac                                   
               call prterx ('W',1)                                          
               natot=0                                                      
            endif 
         else if (xbuf(1:2) .eq. '.#') then
c
c           Process .# case info 
c
            call casetxt (xbuf)
         else if (index (skiprec, card) .ne. 0) then ! Skip illegal  
C                                                    ! records 
         else 
            go to 220 
         endif 
         read (inp, 100, end=200) xbuf 
         card = xbuf(1:1) 
      enddo 
  200 write (errbuf(1), 210)                                              
  210 format('Premature end of file in input stream. ( STOP ) assumed.') 
      call prterx ('w',1)                                                
      xbuf = '( stop ) end of file '                                     
      card = xbuf(1:1) 
  220 continue 
                                                                        
c     End of branch input-sort branch data                               
                                                                        
      if (ltot .eq. 0) then 
         write (errbuf(1),1210)                                             
 1210    format('0 No branch data in system.')                              
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         call erexit (0)                                                        
      else if (ltot .gt. MAXBRN) then 
         write (errbuf(1),1170) ltot 
 1170    format (' Maximum number of branch records ',i5, ' exceeded.') 
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         call erexit (0)                                                        
      endif 
 
      call srtbrnch 

      buf = xbuf                                                         
      return                                                             
      end                                                                
