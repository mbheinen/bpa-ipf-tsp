C    @(#)bldmdc.f	20.4 2/13/96
        subroutine bldmdc (kxx, kerr)  
c                                                                       
c       This subroutine generates entities into arrays "dcmtbs" and     
c       "dcmtln".                                                       
c                                                                       
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
c	Global variables used:
c		mtdcln, mtdcbs
        include 'ipfinc/branch.inc'
c	Global variables used:
c		kx, ky, brnch
        include 'ipfinc/bus.inc'
c	Global variables used:
c		busdta, base
        include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtln(r*8), dcmtbs(r*8)
        include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
        include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
c
c	Local variables
c

        common /is_batch / is_batch
c                                                                       
        character dccode*1, xbuf*120                                      
        integer p, char
        save                      
c                                 
c**********************************************************************
c**********************************************************************
c       bus entry                 
c                                 
        entry dcntbs (nb,kerr)    
        kerr = 0                  
        mtdcbs = mtdcbs + 1       
        if (mtdcbs .gt. MAXMDC) then
           write (errbuf(1),100) MAXMDC  
  100      format (' More than ',i3,' type "BM" buses.')
           if (is_batch .eq. 0) then
             call prterx ('E',1)
           else
             call prterx ('F',1)
           endif
           kerr = 1                     
           mtdcbs = 1                   
        endif
        do 120 j = 1,36                                                 
  120   dcmtbs(j,mtdcbs) = 0.0d0
        call getchr(1,dccode,kbsdta(12,nb))
        call putchr_8(1,dccode,dcmtbs(4,mtdcbs))
        dcmtbs(1,mtdcbs) = nb                                           
        dcmtbs(2,mtdcbs) = dble(base(nb))
        dcmtbs(3,mtdcbs) = kbsdta(9,nb)                                 
        dcmtbs(5,mtdcbs) = dble(busdta(13,nb))
        dcmtbs(6,mtdcbs) = dble(busdta(14,nb))
        dcmtbs(7,mtdcbs) = dble(busdta(3,nb))
        dcmtbs(8,mtdcbs) = dble(busdta(7,nb))
        dcmtbs(9,mtdcbs) = dble(busdta(8,nb))
        dcmtbs(10,mtdcbs) = 0.01745329252d0*dble(busdta(5,nb))
        dcmtbs(11,mtdcbs) = 0.01745329252d0*dble(busdta(6,nb))          
        dcmtbs(12,mtdcbs) = 0.01745329252d0*dble(busdta(10,nb))
        dcmtbs(29,mtdcbs) = 0.01745329252d0*dble(busdta(11,nb))         
        dcmtbs(14,mtdcbs) = dble(busdta(4,nb))
c                                                                       
c       check passive dc bus                                            
c                                                                       
        if (dccode .eq. ' ') then                                         
           if (dcmtbs(3,mtdcbs) .ne. 0) then                            
              write (errbuf(1),150) dccode                                
  150         format (' Converter type "',a1,'" is reserved for ',     
     1               'passive "BM" with blank commutator bus name.')    
              errbuf(2)=' '                                             
              call bcdbus (nb,xbuf)                                     
              write (errbuf(3),170) xbuf(1:80)                          
  170         format (2x,'(',a80,')')                                   
              call prterx ('W',3)                                       
           endif                                                        
c                                                                       
c       check non-passive dc bus                                        
c                                                                       
        else                                                            
c                                                                       
c          Insert default angles if omitted                             
c                                                                       
           if (dcmtbs(10,mtdcbs) .eq. 0.0d0) then                         
              dcmtbs(10,mtdcbs) = 0.01745329252d0 * 5.0d0                   
           endif                                                        
           if (dcmtbs(11,mtdcbs) .eq. 0.0d0) then                         
              dcmtbs(11,mtdcbs) = 0.01745329252d0 * 105.0d0                 
           endif                                                        
           if (dccode .eq. 'R') then                                      
              if (dcmtbs(12,mtdcbs) .eq. 0.0d0) then                      
                 dcmtbs(12,mtdcbs) = 0.01745329252d0 * 15.0d0
              endif                                                     
           else if (dccode .eq. 'I') then                                 
              if (dcmtbs(12,mtdcbs) .eq. 0.0d0) then                      
                 dcmtbs(12,mtdcbs) = 0.01745329252d0 * 21.0d0
              endif                                                     
           else if (dccode .eq. 'M') then                                 
              if (dcmtbs(12,mtdcbs) .eq. 0.0d0) then                      
                 dcmtbs(12,mtdcbs) = 0.01745329252d0 * 16.0d0               
              endif                                                     
           endif                                                        
           if (dcmtbs(29,mtdcbs) .eq. 0.0d0) then                         
              dcmtbs(29,mtdcbs) = 0.01745329252d0 * 16.0d0
           endif                                                        
           if (dcmtbs(10,mtdcbs) .ge. dcmtbs(11,mtdcbs)) then           
              write (errbuf(1),190) dccode,busdta(5,nb),busdta(6,nb)      
  190         format (' Type "BM" bus code "',a1,'" has amin (',f5.1,  
     1                ') .ge. astop (',f5.1,')') 
              errbuf(2)=' '                                             
              call bcdbus (nb,xbuf)                                     
              write (errbuf(3),170) xbuf(1:80)                          
              call prterx ('W',3)                                       
           endif                                                        
           if (dccode .eq. 'I') then                                      
              if (dcmtbs(29,mtdcbs) .ge. dcmtbs(12,mtdcbs)) then        
                 write (errbuf(1),210) dccode,busdta(11,nb),
     &              busdta(10,nb) 
  210            format (' Type "BM" bus code "',a1,'" has gmin (',    
     1                   f5.1,') .ge. gnom (',f5.1,')')
                 errbuf(2)=' '                                          
                 call bcdbus (nb,xbuf)                                  
                 write (errbuf(3),170) xbuf(1:80)                       
                 call prterx ('W',3)                                    
              endif                                                     
           else if (dccode .eq. 'M') then                                 
              if (dcmtbs(29,mtdcbs) .gt. dcmtbs(12,mtdcbs)) then        
                 write (errbuf(1),220) dccode,busdta(11,nb),
     &              busdta(10,nb) 
  220            format ('0 type "BM" bus code "',a1,'" has gmin (',    
     1                   f5.1,') .gt. gnom (',f5.1,')')
                 errbuf(2)=' '                                          
                 call bcdbus (nb,xbuf)                                  
                 write (errbuf(3),170) xbuf(1:80)                       
                 call prterx ('W',3)                                    
              endif                                                     
                                                                        
           else if (dccode .eq. 'R') then                                 
                                                                        
           else                                                         
              write (errbuf(1),230) dccode                                
  230         format (' type "BM" bus has illegal code "',a1,'".')     
              errbuf(2)=' '                                             
              call bcdbus (nb,xbuf)                                     
              write (errbuf(3),170) xbuf(1:80)                          
              if (is_batch .eq. 0) then
                call prterx ('E',3)
              else
                call prterx ('F',3)
              endif
           endif                                                        
                                                                        
           if (dcmtbs(7,mtdcbs) .lt. 1.0d0) then                          
              write (errbuf(1),270) dcmtbs(7,mtdcbs)                    
  270         format (' Type "BM" bus has an illegal no. of bridges :',
     1                f4.1,'. Default is 1.0 bridge.') 
              errbuf(2)=' '                                             
              call bcdbus (nb,xbuf)                                     
              write (errbuf(3),170) xbuf(1:80)                          
              call prterx ('W',3)
              dcmtbs(7,mtdcbs) = 1.0d0
           endif                                                        
                                                                        
        endif                                                           
                                                                        
        dcmtbs(8,mtdcbs) = 0.001d0*dcmtbs(7,mtdcbs)*dcmtbs(8,mtdcbs)      
                                                                        
        return                                                          
c                                                                       
c**********************************************************************
c**********************************************************************
c       line entry                                                      
c                                                                       
        entry dcntln (p, kerr)                                         

        k1  = kx(p)
        k2  = ky(p)
        nbr = brnch_ptr(p)
        jbr = iabs(nbr)
        kerr = 0                                                        
        do i = 1,mtdcln
           if (dcmtln(1,i) .eq. k1 .and. dcmtln(2,i) .eq. k2) goto 900
           if (dcmtln(1,i) .eq. k2 .and. dcmtln(2,i) .eq. k1) goto 900
        enddo

        mtdcln = mtdcln + 1                                             
        if (mtdcln .gt. MAXDCL) then
           write (errbuf(1),320) MAXDCL     
  320      format (' More than ',i3,' type "LM" branches.')
           mtdcln = 1                       
           kerr = 1                         
           if (is_batch .eq. 0) then
             call prterx ('E',1)
           else
             call prterx ('F',1)
           endif
        endif
        do 340 j = 7,10                                                 
  340   dcmtln(j,mtdcln) = 0                                            
        dcmtln(1,mtdcln) = kx(p)
        dcmtln(2,mtdcln) = ky(p)
        do 350 j = 4,6                                                  
  350   dcmtln(j,mtdcln) = dble(brnch(j+1,jbr))
        if (dcmtln(4,mtdcln) .le. 0.0d0) then
           write (errbuf(1),360) dcmtln(4,mtdcln)          
  360      format (' Type "LM" line has illegal resistance: ',f7.1,
     &             '.  Chaged to "0.001".') 
           errbuf(2)=' '                                   
           call bcdbrn (p,xbuf)                            
           write (errbuf(3),170) xbuf(1:80)                
           call prterx ('W',3)                             
           dcmtln(4,mtdcln) = 0.001d0
        endif
  900   return 
        end                                                             
