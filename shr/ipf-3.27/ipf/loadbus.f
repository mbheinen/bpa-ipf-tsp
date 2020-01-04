C    @(#)loadbus.f	20.7 11/12/98
      subroutine loadbus (xbuf, error)
      character xbuf*(*)
c
      integer error
c
c     read bus records from different sources.
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tempbsnm.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/wsccbase.inc'

      common /is_batch / is_batch

      character bus1*8, bus2*8, bus3*8, tp_code*2, subtyp*1, month*1,
     &          id*2
      integer add_bus, find_bus

      if (xbuf(1:1) .eq. 'B') then
c
c        read and decode the bus data cards.
c
         read (xbuf, 500, err=900) bus1, base1
  500    format (bz, t7, a8, f4.0)
         if (base1 .eq. 0.0) then                                     
            kberr=2
            write (errbuf(1),640) bus1                                
  640       format(' Illegal base for bus ',a8,'. Assumed 100.0kv.')
            call prterx ('W',1)
            base1 = 100.0
         endif
         indx = add_bus (bus1, base1, ntot+1)
C
C        This hashing function checks for duplicates, array overflow, 
C        and automatically inserts the new bus in position ntot+1.
c
         if (indx .lt. 0) then
            write (errbuf(1), 650) 
  650       format (' Duplicate "B" record. Second one ignored.')
            errbuf(2)=' '
            write (errbuf(3),400) xbuf(1:80)
  400       format(2x, a80)
            call prterx ('W',3)
            go to 920
         else if (indx .eq. 0) then
            write (errbuf(1),670) MAXBUS
  670       format (' More than ',i4,' buses in base system ')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
               return
            else
               call prterx ('F',1)
               call erexit()
            endif
         else
            ntot = indx
            wsccbase(ntot) = xbuf(15:18)
            wsccflag = .true.
         endif
         kbsdta(15,ntot) = 0
         kbsdta(16,ntot) = 0
         subtyp = xbuf(2:2)
         call typnam(subtyp,ktype)
         if (subtyp .eq. 'D') then
c
c           Decode type "BD" bus
c
            read (xbuf,570,err=900) owner(ntot), zone(ntot), 
     &         (busdta(k,ntot),k=3,8), bus2, base2                
  570       format (bz,t4,a3,t19,a2,3x,f2.0,5f5.1,a8,f4.0)
            if (bus2.eq.' ') then
               kbsdta(9,ntot)=0
            else
               ix = find_bus (bus2, base2)
               if (ix .gt. 0) then
                  kbsdta(9,ntot)=ix
               else
                  ibuscb=ibuscb+1  
                  if (ibuscb .gt. MXBSTP) then     
                     write (errbuf(1),380) MXBSTP  
  380                format(' Array over flow - more than ',i4,
     1                  ' temporary bus names.')               
                     errbuf(2)=' '                             
                     write (errbuf(3),400) xbuf(1:80)          
                     if (is_batch .eq. 0) then
                        call prterx ('E',3)
                     else
                        call prterx ('F',3)
                     endif
                     ibuscb=1                                  
                  endif                                        
                  buscb(ibuscb) = bus2                         
                  basecb(ibuscb) = base2                       
                  itype(ibuscb) = 1
                  ixndx(ibuscb) = ntot
                  kbsdta(9,ntot)=-ibuscb                       
               endif
            endif
         else if (subtyp .eq. 'M') then
c                                                              
c           Read and decode type "BM" bus                      
c                                                              
            read (xbuf,590,err=900) owner(ntot), zone(ntot),
     &         (busdta(k,ntot),k=3,8), bus2, base2,            
     &         busdta(12,ntot), (busdta(k,ntot),k=10,11),   
     &         (busdta(k,ntot),k=13,14)                     
  590       format(bz,t4,a3,t19,a2,3x,f2.0,5f5.1,a8,f4.0,a1,2f3.1,f6.1,

     &         f5.1)
            if (bus2.eq.' ') then                   
               kbsdta(9,ntot)=0                     
            else                                    
               ix = find_bus (bus2, base2)
               if (ix .gt. 0) then
                  kbsdta(9,ntot)=ix
               else
                  ibuscb=ibuscb+1
                  if (ibuscb .gt. MXBSTP) then
                     write (errbuf(1),380) MXBSTP
                     errbuf(2)=' '
                     write (errbuf(3),400) xbuf(1:80)
                     if (is_batch .eq. 0) then
                        call prterx ('E',3)
                     else
                        call prterx ('F',3)
                     endif
                    ibuscb=1
                  endif
                  buscb(ibuscb) = bus2
                  basecb(ibuscb) = base2
                  itype(ibuscb) = 1
                  ixndx(ibuscb) = ntot
                  kbsdta(9,ntot)=-ibuscb
               endif
            endif
         else 
c
c           Read and decode all other type buses
c
            read (xbuf, 502, err=900) owner(ntot), zone(ntot), 
     &         (busdta(k,ntot),k=3,12)                             
  502       format (bz,t4,a3,t19,a2,2f5.0,3f4.0,3f5.0,2f4.3)
            if (ktype .eq. 3) then
               read (xbuf(62:65), '(bz, f4.1)', err=900) busdta(12,ntot)
            endif
            do 510 i=13,16
  510       busdta(i,ntot)=0.0
            kbsdta(1,ntot)=ktype
c
c           Decode type "BG" or "BX" buses
c
            if (subtyp .eq. 'G' .or. subtyp .eq. 'X') then
               read (xbuf,530,err=900) bus2,base2,pct     
  530          format (bz,t66,a8,f4.0,f3.0)                  
               ix = find_bus (bus2, base2)
               if (ix .gt. 0) then
                  kbsdta(13,ntot)=ix
               else
                  if (subtyp .eq. 'X' .and. bus2 .eq. ' ') then
                     kbsdta(13,ntot)=0
                  else
                     ibuscb=ibuscb+1                         
                     if (ibuscb .gt. MXBSTP) then            
                        write (errbuf(1),380) MXBSTP         
                        errbuf(2)=' '                        
                        write (errbuf(3),400) xbuf(1:80)     
                        if (is_batch .eq. 0) then
                           call prterx ('E',3)
                        else
                           call prterx ('F',3)
                        endif
                        kberr=2                              
                        ibuscb=1                             
                     endif                                   
                     buscb(ibuscb) = bus2                    
                     basecb(ibuscb) = base2                  
                     itype(ibuscb) = 1
                     ixndx(ibuscb) = ntot
                     kbsdta(13,ntot)=-ibuscb                 
                  endif
               endif
               busdta(14,ntot)=pct                        
            endif
         endif
         kbsdta(1,ntot)=ktype
         call vltlim(ntot, vlimn(ntot), vlimx(ntot), vstart(ntot)) 
         e(ntot) = vstart(ntot)
         f(ntot) = 0.0d0   
         capcor(1,ntot) = 0.0d0
         capcor(2,ntot) = -9.0e10
         inp2opt(ntot) = ntot
         opt2inp(ntot) = ntot
      else if (xbuf(1:1) .eq. '+') then
         if (ntot2 .gt. MAXCBS+1) then
            write (errbuf(1),760) MAXCBS
  760       format (' More than ',i4,' continuation bus cards')
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            ntot2=0
         endif
         ntot2 = ntot2 + 1
         read (xbuf,700,err=900) bctbl(8,ntot2),bctbl(10,ntot2),
     &      bus2,base2,bctbl(9,ntot2),(bctbl(k,ntot2),k=2,6),
     &      bctbl(11,ntot2),bctbl(12,ntot2),month,nyear
  700    format (bz,1x,a1,1x,a3,a8,f4.0,a2,2f5.0,2f4.0,4x,3f5.0,t75,
     &       a1,i2)                                                
         kbctbl(7,ntot2)=intdte(month,nyear)
         ix = find_bus (bus2, base2)
         if (ix .gt. 0) then
            kbctbl(1,ntot2)=ix
            call linkcbus (ntot2,error)
         else
            ibuscb=ibuscb+1
            if (ibuscb .gt. MXBSTP) then
               write (errbuf(1),380) MXBSTP
               errbuf(2)=' '
               write (errbuf(3),400) xbuf(1:80)
               if (is_batch .eq. 0) then
                  call prterx ('E',3)
               else
                  call prterx ('F',3)
               endif
               kberr=2
               ibuscb=1
            endif
            buscb(ibuscb) = bus2
            basecb(ibuscb) = base2
            itype(ibuscb) = 2
            ixndx(ibuscb) = ntot2
            kbctbl(1,ntot2)=-ibuscb
         endif
         kbctbl(7,ntot2) = intdte(month,nyear)
      else if (xbuf(1:1) .eq. 'X') then
c
c        Switched reactance "X" data
c
         kxtot=kxtot+1
         if (kxtot .gt. MAXXDT) then
            write (errbuf(1),350) MAXXDT
  350       format(' More than ',i3,' switched reactance entities.')
            errbuf(2)= ' '
            write (errbuf(3),400) xbuf(1:80)
            if (is_batch .eq. 0) then
               call prterx ('E',3)
            else
               call prterx ('F',3)
            endif
            kxtot=1
         else
            read (xbuf,370,err=900) bus2,base2,bus3,base3,
     1         (xdata(k,kxtot),k=7,22)
  370       format(bz,6x,a8,f4.0,2x,a8,f4.0,8(f1.0,f5.0))
            ix = find_bus (bus2, base2)
            if (ix .gt. 0) then
               xdata(1,kxtot) = ix
            else
               ibuscb=ibuscb+1
               if (ibuscb .gt. MXBSTP) then
                  write (errbuf(1),380) MXBSTP
                  errbuf(2)=' '
                  write (errbuf(3),400) xbuf(1:80)
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
                  ibuscb=1
               endif
               buscb(ibuscb) = bus2
               basecb(ibuscb) = base2
               itype(ibuscb) = 3
               ixndx(ibuscb) = kxtot
               xdata(1,kxtot) = -ibuscb
            endif
            if (bus3 .eq. ' ') then
               xdata(2,kxtot)=xdata(1,kxtot)
            else
               ix = find_bus (bus3, base3)
               if (ix .gt. 0) then
                  xdata(2,kxtot) = ix
               else
                  ibuscb=ibuscb+1                         
                  if (ibuscb .gt. MXBSTP) then            
                     write (errbuf(1),380) MXBSTP         
                     errbuf(2)=' '                        
                     write (errbuf(3),400) xbuf(1:80)     
                     if (is_batch .eq. 0) then
                        call prterx ('E',3)
                     else
                        call prterx ('F',3)
                     endif
                     ibuscb=1                             
                  endif                                   
                  buscb(ibuscb) = bus3                    
                  basecb(ibuscb) = base3                  
                  itype(ibuscb) = 3
                  ixndx(ibuscb) = kxtot
                  xdata(2,kxtot) = -ibuscb
               endif
            endif                                 
            totrek = 0.0                          
            totcap = 0.0                          
            do 460 k=7,21,2                       
               xtot = xdata(k,kxtot) * xdata(k+1,kxtot)
               totrek = totrek + amin1 (0.0,xtot)    
  460       totcap = totcap + amax1 (0.0,xtot)       
            total = totrek + totcap                  
            xdata(3,kxtot)=totrek                    
            xdata(4,kxtot)=totcap                    
            xdata(5,kxtot)=totrek                    
            xdata(6,kxtot)=totcap                    
         endif                                       
      else if (xbuf(1:1) .eq. 'Q') then
c
c        Generator P/Q curve data
c
         read (xbuf,800, err=900) tp_code, id, bus1, base1
  800    format (bz, a2, 1x, a2, 1x, a8, f4.0)
         ix = find_bus (bus1,base1)
         if (ix .gt. 0) then
c           see if this is a new pqcurve record
            ieq = 0
            do i = 1,numcurv
               if ( pqbusptr(i) .eq. ix ) ieq = i
            enddo
            if ( ieq .eq. 0 ) then
c              new record
               if (numcurv .eq. MAXCRV) then
                  write (errbuf(1),805) numcurv
  805             format (' Array overflow - more than',i5,' pq curves')
                  errbuf(2) = ' '
                  write (errbuf(3),400) xbuf(1:80)
                  call prterx ('W',3)
                  goto 920
               endif
               numcurv = numcurv + 1
               ieq = numcurv
               pqbusptr(ieq) = ix
               pqactive(ieq) = .true.
               pqid(ieq) = id
            endif
            if (tp_code .eq. 'QP') then
               read (xbuf, 810, err=900) (pqpgen(i,ieq),i=-1,15)
  810          format ( bz, t21, 2f5.1, 15f6.2 )
            else if (tp_code .eq. 'QX') then
               read (xbuf, 811, err=900) (pqqmax(i,ieq),i=-1,15)
  811          format ( bz, t21, f5.3, f5.1, 15f6.2 )
            else if (tp_code .eq. 'QN') then
               read (xbuf, 811, err=900) (pqqmin(i,ieq),i=-1,15)
            else
               write (errbuf(1),815) tp_code
  815          format (' Unrecognized P-Q curve data (', a2,
     &                 ') curve deactivated!' )
               errbuf(2) = ' '
               write (errbuf(3),400) xbuf(1:80)
               call prterx ('W',3)
               pqactive(ieq) = .false.
            endif
         else
c           BUS record has not been processed yet.
c           look in the buscb() temporary tables to see if 
c           another part of this curve has already been stashed.
c
            ieq = 0
            do i = 1, ibuscb
               if (buscb(i) .eq. bus1 .and. basecb(i) .eq. base1 .and.
     1             itype(i) .eq. 4) then
                  ieq = ixndx(i)
               endif
            enddo
            if (ieq .eq. 0 ) then
c              new record
               ibuscb=ibuscb+1                         
               if (ibuscb .gt. MXBSTP) then            
                  write (errbuf(1),380) MXBSTP         
                  errbuf(2)=' '                        
                  write (errbuf(3),400) xbuf(1:80)     
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
                  ibuscb=1                             
               endif                                   
               buscb(ibuscb) = bus1
               basecb(ibuscb) = base1
               itype(ibuscb) = 4
               if (numcurv .eq. MAXCRV) then
                  write (errbuf(1),805) numcurv
                  errbuf(2) = ' '
                  write (errbuf(3),400) xbuf(1:80)
                  call prterx ('W',3)
                  goto 920
               endif
               numcurv = numcurv + 1
               ieq = numcurv
               ixndx(ibuscb) = ieq
               pqbusptr(ieq) = -ibuscb
               pqactive(ieq) = .true.
               pqid(ieq) = ' '
            endif
c
c           store this section of the curve data
c
            if (tp_code .eq. 'QP') then
               read (xbuf, 810, err=900) (pqpgen(i,ieq),i=0,15)
            else if (tp_code .eq. 'QX') then
               read (xbuf, 811, err=900) (pqqmax(i,ieq),i=0,15)
            else if (tp_code .eq. 'QN') then
               read (xbuf, 811, err=900) (pqqmin(i,ieq),i=0,15)
            else
               write (errbuf(1),815) tp_code
               errbuf(2) = ' '
               write (errbuf(3),400) xbuf(1:80)
               call prterx ('W',3)
               pqactive(ieq) = .false.
            endif
         endif
      endif
      go to 920 
c         
  900 write (errbuf(1),910) xbuf(1:80)  
  910 format (' Illegal data in field : (', a80, ')') 
      call prterx ('W',1)
c          
  920 continue   
      return       
      end                  
