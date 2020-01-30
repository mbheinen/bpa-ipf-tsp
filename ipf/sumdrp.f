C    @(#)sumdrp.f	20.3 2/13/96
      subroutine sumdrp
C
C     summarize generation dropped and reallocated.
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
C
      character btyp * 1
 
C
      call forbtm
      write (outbuf,10)
   10 format (t10,' Status of machines with generation ',
     &            'dropped or picked up ')
      call shdlod(1)
      write (outbuf,20) 
   20 format('0 Generator', t19, 'Type', t25, 'Zone',   
     1      t31, '   ----------------- Generation -----------------',   
     2      t95,'Comments') 
      call shdlod(2)
 
      write (outbuf,30)
   30 format(t34,'Minimum    Maximum    Initial    Final    Change')
      call shdlod(3)
 
      write (outbuf,40)
   40 format(t34,' (MW)       (MW)       (MW)      (MW)       (MW)')
      call shdlod(4)
      outbuf = ' '  
      call shdlod(5)
      call fortop   
      totdrp = 0.0  
      totdrp = totdrp + drptot  
      write (outbuf, 50) drptot, 'Initially dropped generation' 
   50 format (t71, f10.1, t95, a)   
      call prtout (1)   
      do 110 i = 1, numdrp  
         nb = gndpno(i) 
         kt = inp2opt(nb) 
         pmaxmw = gndpmx(i) 
         pminmw = gndpmn(i) 
         pgen = pgenmw / bmva   
         pmax = pmaxmw / bmva   
         pgen = pnetu(kt) + ploadu(kt)         
         poldmw = gndpol(i) 
         pgenmw = pgen * bmva   
         dropmw = pgenmw - poldmw   
         totdrp = totdrp + dropmw   
         call typno (btyp, ntypu(kt))              
         write (outbuf, 100) bus(nb), base(nb), btyp, zone(nb), 
     1      pminmw, pmaxmw, poldmw, pgenmw, dropmw, 'Dropped Generator' 
  100    format (t3, a8, f6.1, t21, a1, t27, a2, t31, 5f10.1, t95, a)   
         if (gndpty(i) .ne. 0) outbuf(122:) = '(Slack bus)' 
         call prtout (1)
  110 continue  
      totpku = 0.0  
      do 230 i  = 1, numgen 
         nb = gennum(i) 
         kt = inp2opt(nb) 
         pmaxmw = busdta(7,nb)  
         pminmw = 0.0   
         pgen = pnetu(kt) +ploadu(kt)          
         poldmw = genpol(i) 
         pgenmw = pgen * bmva   
         pickup = pgenmw - poldmw   
         totpku = totpku + pickup   
         call typno (btyp, ntypu(kt))              
        
         if (pmaxmw .gt. 0.0) then  
            pct = 100.0 * pickup / pmaxmw   
         else   
            pct = 0.0   
         endif  
         write (outbuf, 220) bus(nb), base(nb), btyp, zone(nb), 
     1      pminmw, pmaxmw, poldmw, pgenmw, pickup, pct 
  220    format (t3, a8, f6.1, t21, a1, t27, a2, t31, 5f10.1,   
     1      t95, 'Generation Pickup', f7.2, ' %')   
         if (gentyp(i) .ne. 0) outbuf(122:) = '(Slack bus)' 
         call prtout (1)
        
  230 continue  
        
      write (outbuf, 232) totdrp
  232 format ('0 Total dropped', t71, f10.1)
      call prtout (1)   
      write (outbuf, 240) totpku
  240 format ('  Total Picked Up', t71, f10.1)  
      call prtout (1)   
      if (totpku - totdrp .gt. 1.0) then
         write (outbuf, 250)
  250    format ('0 Note: Total picked up does not balance with ',
     &           'total dropped because of losses.')
         call prtout (1)
      endif 
C       
C     Blank out all subheaders used in this module. 
C       
      do 260 i = 1, 5   
         outbuf = ' '   
         call shdlod( i )   
  260 continue  
        
      return
      end   
