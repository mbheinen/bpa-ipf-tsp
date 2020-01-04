C    @(#)loadarea.f	20.5 1/4/99
      subroutine loadarea (xbuf, error)
      character xbuf*(*)
      integer error
c                                                                       
c     load area records into tables
c                                                                       
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arsort.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/prt.inc'
 
      common /is_batch / is_batch

      if (xbuf(1:2) .eq. 'AC' .or. xbuf(1:2) .eq. 'A ' .or. 
     &    xbuf(1:2) .eq. 'A0') then
         ntotc=ntotc+1                                                     
         if (ntotc .le. MAXCAR) then                                       
            read (xbuf,270,err=900) arcnam(ntotc),arcbus(ntotc),
     1         arcbas(ntotc),arcnet(ntotc),(arczns(i,ntotc),i=1,MAXCAZR)    
  270       format (bz,3x,a10,a8,f4.0,1x,f8.0,10(1x,a2))                      
            arcnet(ntotc)=arcnet(ntotc)/bmva                               
            do i = MAXCAZR+1, MAXCAZ
               arczns(i,ntotc) = ' '
            enddo
         else                                                              
            write (errbuf(1),290) MAXCAR                                   
  290       format (' More than ',i3,' area interchange control areas,',   
     1              ' interchange aborted.')                                    
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kase1(6)=0                                                     
            ntotc=0                                                        
            kabort=1                                                       
         endif

      else if (xbuf(1:2) .eq. 'AO' ) then
         natot=natot+1                                                     
         if (natot .gt. 90) then                                           
            write (errbuf(1),310)                                          
  310       format(' More than 90 area output sort areas. ',
     1             'AREA sort aborted.')
            call prterx ('W',1)                                            
            minerr=1                                                       
            natot=0                                                        
         else                                                              
            read (xbuf,330,err=900) arsnam(natot),                         
     1         (arsens(k,natot),k=1,22)                                
  330       format (bz,3x,a10,22(1x,a2))                                      
            do k = 23, MAXCAZ
               arsens(k,natot) = ' '
            enddo
         endif                                                             

      else if ( xbuf(1:1) .eq. 'A' ) then
         read (xbuf(2:2), '(i1)') isubtyp
         if ( isubtyp .lt. MAXCAZ / MAXCAZR ) then
            j1 = isubtyp * MAXCAZR + 1
            j2 = (isubtyp+1) * MAXCAZR 
            read (xbuf,360,err=900) (arczns(i,ntotc),i=j1,j2)
         endif
  360    format (t35,10(1x,a2))                      

      else if (xbuf(1:1) .eq. 'I') then
c                                                                       
c        process area intertie "i" records                                 
c                                                                       
         ntotic = ntotic+1                                                 
         if (ntotic.gt.5*MAXCAR-1) then                                    
            write(errbuf(1),392) 5*MAXCAR                                  
  392       format(' More than ',i4,
     &             ' area intertie "I" records. Interchange aborted.')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            ntotic = 0                                                     
            kabort = 1                                                     
         else                                                              
            read (xbuf,393,err=900) arcint(1,ntotic),arcint(2,ntotic),     
     1        arcinp(ntotic)                                                 
  393       format(bz,3x,a10,1x,a10,2x,f8.0)                                  
            ntotic = ntotic+1                                              
            arcint(1,ntotic) = arcint(2,ntotic-1)                          
            arcint(2,ntotic) = arcint(1,ntotic-1)                          
            arcinp(ntotic) = -arcinp(ntotic-1)                             
         endif                                                             

      endif

  500 continue
      return

  900 write (errbuf(1),910) xbuf(1:80)                                  
  910 format (' Illegal data in field :(',a80,')')                      
      call prterx ('W',1)                                               
      go to 500

      end
