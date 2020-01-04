C    @(#)bldtbx.f	20.7 1/4/99
        subroutine bldtbx (nb, jtb, jxb, kerr)
c
c       This routine builds a complete entry in TBX corresponding to   
c       data for bus "NB".
c       input data:
c
c               nb:   Bus number of completed and linked "busdta" and
c                     "bctbl",and if applicable,"xdata" arrays
c               jtb:  Tbx index in which entry will be made
c               jxb:  Xdata index of pertinent xdata (if applicable)
c
c       output data:
c
c               tbx:  Complete data assigned
c               kerr: Error status switch 
c                       0 - normal
c                       1 - minor errors
c                       2 - fatal error
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com008.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'

      common /dbkk/ dgkk, dbkk, bkkadj
      double precision dgkk,dbkk, bkkadj

c     Local variables changed to double precision

      double precision qgen, qmax, qmin, qadj, totcap, totrek, 
     &       usecap, userek
      character xbuf*120
      integer oldltp, olditp, bustype, status, ptr
c
c       build TBX entries according to type                            
c
        bustype = kbsdta(1,nb) 
        ltype = tbxtyp(bustype)
        itype = 1              
        if (ltype .eq. 0) then
           write (errbuf(1),130) 
  130      format(' Illegal bus type call to "BLDTBX".') 
           errbuf(2) = ' '       
           call bcdbus (nb,xbuf) 
           write (errbuf(3),150) xbuf(1:80)
  150      format(2x,a80)         
           call prterx ('W',3)    
           go to 230              
        endif
c
c       search for entry                                               
c 
        ntotb = ntotb + 1      
        if (ntotb .gt. MAXTBX) then         
           write (errbuf(1),60) MAXTBX,bus(nb),base(nb)      
   60      format (' More than ',i4,' special buses (types ',
     &             '"BV", "BQ", "BG", "BO", "BX", "BF".  ',
     &             'Overflow encountered at bus ',a8,f6.1)   
           call prterx ('W',1)       
           ntotb=1     
        endif         
        jtb=ntotb      
c
c       obtain bus quantities 
c               
        qmax = busdta(9,nb)/bmva
        qmin = busdta(10,nb)/bmva 
        ptr = kbsdta(15,nb)
        do while (ptr .gt. 0)
          qmax = qmax + bctbl(11,ptr)/bmva
          qmin = qmin + bctbl(12,ptr)/bmva
          ptr = bctbl_nxt(ptr)
        enddo

        qgen = qnetu(nb) + qloadu(nb)

        go to(170,180,190,180,190,170) ltype      
c                            
c       "BV" and "BF"    
c                           
  170   tbx(8,jtb)=0                                                   

        tbx(3,jtb)=vlimx(nb)
        tbx(4,jtb)=vlimn(nb)
        tbx(5,jtb)=(qgen-qloadu(nb))
        tbx(6,jtb)=0.0
        go to 206
c
c       "BQ", "BO", and "BL"
c                                         
  180   tbx(8,jtb)=0              
        qadj=bkkadj*vstart(nb)**2     
        tbx(3,jtb)=qmax-qloadu(nb)-dmin1(0.0d0, qadj)    
        tbx(4,jtb)=qmin-qloadu(nb)-dmax1(0.0d0, qadj)  
        tbx(5,jtb)=vstart(nb)       
        tbx(6,jtb)=bkkadj
        go to 202                      
c                                   
c       "BG" and "BX" bus       
c                                    
  190   tbx(8,jtb)=kbsdta(13,nb)    
        tbx(3,jtb)=qmax-qloadu(nb)
        tbx(4,jtb)=qmin-qloadu(nb)
        if (ltype .eq. 3) then
          tbx(5,jtb)=busdta(14,nb) 
c
c         Reset % to zero if qmin=qmax
c
          if (tbx(3,jtb) - tbx(4,jtb) .lt. 0.01) then
            write (errbuf(1),10203) qmax,qmin    
10203       format('Insufficent var range: QMAX (', f6.1,
     &             '), QMIN (', f6.1,') is <= 1 MVAR.')
            write (errbuf(2),10204) 
10204       format('% is set to 0.0 on the following bus') 
            call bcdbus (nb,xbuf)            
            write (errbuf(3),150) xbuf(1:80)    
            call prterx ('I',3)             
            tbx(5,jtb) = 0.0
          endif
          tbx(6,jtb)=bkkadj     
        else
          tbx(5,jtb)=jxb           
          tbx(6,jtb)=bkkadj     
c
c         Type X-bus may be initialized in a state other than ITYPE = 1.
c
          totrek = xdata(3,jxb)
          totcap = xdata(4,jxb)
          userek = xdata(5,jxb)
          usecap = xdata(6,jxb)
          if (totrek .ne. 0.0 .and. totcap .ne. 0.0) then
            if (totrek .eq. userek .and. totcap .eq. userek) then
              itype = 1
            else if (userek .eq. 0.0 .and. usecap .eq. totcap) then
              itype = 3
            else if (usecap .eq. 0.0 .and. userek .eq. totrek) then
              itype = 2
            else
              itype = 4
            endif
          else if (totrek .ne. 0.0) then
            if (userek .eq. totrek) then
              itype = 2
            else if (userek .eq. 0.0) then
              itype = 3
            else
              itype = 4
            endif
          else
            if (usecap .eq. totcap) then
              itype = 3
            else if (usecap .eq. 0.0) then
              itype = 2
            else
              itype = 4
            endif
          endif
        endif
c
c       check q limits
c
  202   if (qmax .lt. qmin) then
           if (abs (qmax-qmin) .lt. 0.1) then
              busdta(10,nb) = qmax
           else                                 
              write (errbuf(1),204) qmax,qmin    
  204         format('0 "qmax" (',f6.1,') is less than "qmin" (', 
     1                f6.1,') ', 'on the following bus:') 
              errbuf(2) = ' '                
              call bcdbus (nb,xbuf)            
              write (errbuf(3),150) xbuf(1:80)    
              call prterx ('W',3)             
           endif
        endif
c
c       Reset "TBX" for hot start
c
  206   call getoldtb (bus(nb), base(nb), oldjtb, oldltp, olditp,
     &                 status)
        if (status .eq. 1) then
           if (ltype .eq. oldltp .and. itype .ne. olditp) then
              itype = olditp
              call updatetb(nb, oldjtb, jtb)
           endif
        endif
c
c       complete "tbx"
c
        tbx(1,jtb) = ltype
        tbx(2,jtb) = nb
        tbx(7,jtb) = itype

  230   return
        end
