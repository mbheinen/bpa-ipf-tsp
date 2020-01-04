C    @(#)agcsum.f	20.3 2/13/96
      subroutine agcsum
 
C     summarize the status of AGC control.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
c	Global variables used:
c		agc, numagc, kagc
      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		None
      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva
      include 'ipfinc/bus.inc'
c	Global variables used:
c		base, inp2opt
      include 'ipfinc/com007.inc'
c	Global variables used:
c		bustyp
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
 
      real savepk(MAXAGC)
      character type*2
 
      if (numagc .eq. 0) go to 900
 
        call forbtm
        write (outbuf,110)
  110   format (t53,' Summary of AGC Control ')
        call shdlod(1)
        write (outbuf,120)
  120   format('0Type Generator',
     1        t30, '/-------- Generation (MW) -----------/',
     2        t74, '/ % Participation /',
     3        t100, 'Comments')
        call shdlod(2)
        write (outbuf,130)
  130   format(t30, '   min     max    base  actual delta_P',
     1         t74,' actual scheduled')
        call shdlod(3)
        outbuf= '0'
        call shdlod(4)
        outbuf= ' '
        call shdlod(5)
        call fortop
C
C       Compute total participation
C
        totbas = 0.0
        totgen = 0.0
        do 140 jt = 1, numagc   
           nb = kagc(1,jt)  
           kt = inp2opt(nb)   
           call nrpqv (kt, pk, dpk, qk, dqk, vk)
           pgen = pk + agc(8,jt)
           savepk(jt) = pgen
           totbas = totbas + agc(7,jt)  
           totgen = totgen + pgen   
  140   continue
        totdlp = totgen - totbas
        totpc1 = 0.0
        totpc2 = 0.0
        do 200 jt = 1, numagc   
        
           nb = kagc(1,jt)  
           delp = savepk(jt) - agc(7,jt)
           if (totdlp .ne. 0.0) then  
              pct = delp / totdlp   
           else 
              pct = 0.0 
           endif
           pmin = agc(6,jt) * bmva  
           pmax = agc(5,jt) * bmva  
           pbas = agc(7,jt) * bmva  
           pgen = savepk(jt) * bmva 
           type = 'B' // bustyp(kagc(12,jt))
           write (outbuf, 150) type, bus(nb), base(nb), pmin, pmax, 
     1        pbas, pgen, delp * bmva, pct * 100.0, 
     2        agc(4,jt) * 100.0 
  150      format (1x, a2, 3x, a8, f6.1, t28, 5f8.1, t74, 2f8.1)
        
           totpc1 = totpc1 + pct
           totpc2 = totpc2 + agc(4,jt)  
           dp = dim (pmin, pgen)
           if (dp .gt. 0.1) then
              write (outbuf(100:), 180) -dp 
  180         format ('P_min limit violated ',f6.1) 
           endif
        
           dp = pgen - pmin 
           if (abs(dp) .le. 0.1) then   
              outbuf(100:) = 'P_min limit reached ' 
           endif
        
           dp = dim (pgen, pmax)
           if (dp .gt. 0.1) then
              write (outbuf(100:), 190) dp  
  190         format ('P_max limit violated ',f6.1) 
           endif
        
           dp = pgen - pmax 
           if (abs(dp) .le. 0.1) then   
              outbuf(100:) = 'P_max limit reached ' 
           endif
        
           if (kagc(10,jt) .eq. 1) then 
              if (agc(4,jt) .gt. 0.0) then  
                 ratio = pct / agc(4,jt)
                 if (abs (ratio - 1.0) .gt. 0.01) then  
                    write (outbuf(100:), 198) pct / agc(4,jt)   
  198               format ('Actual % / Sched % = ', f6.2)  
                 endif  
              endif 
           endif
           call prtout (1)  
        
  200   continue
        
        write (outbuf, 210) totbas * bmva, totgen * bmva,   
     1     totdlp * bmva, totpc1 * 100.0, totpc2 * 100.0
  210   format ('0Total', t44, 3f8.1, t74, 2f8.1)   
        call prtout (1) 
        
        outbuf = ' '
        call rpnlod 
        call shdlod (1) 
        call shdlod (2) 
        call shdlod (3) 
        call shdlod (4) 
        call shdlod (5) 
        
        outbuf = '0End of AGC Control Summary ' 
        call prtout(1)  
        call forbtm 
        
  900   continue
        
        return  
        end 
