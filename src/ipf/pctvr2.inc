c
        common /pctvr/ xmtrx(20*MAXPVC), pctsr(8,25), 
     &                 pctvr(8,20*MAXPVC), 
     &                 numpctvr, kpctvr(3,MAXPVC)
        double precision xmtrx, pctvr, pctsr
        integer numpctvr, kpctvr
c
c       kpcvtr(1,*) = controlled bus
c             (2,*) = number of controlling buses
c             (3,*) = pointer to pcvtr(1,*) for first controlling 
c                     generator
c
c       pctvr(1,ptr1) = controlling generator number
c            (2,ptr1) = index to TBX(*,*)
c            (3,ptr1) = pointer (ptr2) to dF/dX terms in XMTRX(*)
c            (4,ptr1) = length of row of dF/dX terms in XMTRX(*)
c            (5,ptr1) = Percentage
c            (6,ptr1) = Qmax
c            (7,ptr1) = Qmin
c            (8,ptr1) = pointer to next entity in pctvr(*)
c
c       pctsr(1,*) = Controlling generator number
c            (2,*) = kode (1 = V, 2 = Qmin, 3 = Qmax, 4 = Q%,
c                          5 = Vmin, 6 = Vmax)
c            (3,*) = loop history of kode
c            (4,*) = dv (estimated voltage excursion)           
c            (5,*) = qk
c            (6,*) = dqk           
c            (7,*) = vk          
c            (8,*) = index to pctvr(*,*)          
c
c       xmtrx(ptr2+0) = Generator number (> 0 = PQ, < 0 = PV)
c            (ptr2+1) = qk
c            (ptr2+2) = percent normalized
c            (ptr2+3) = Jacobian column number (in blocks of three)
c            (ptr2+4) = dQ/dT  
c            (ptr2+5) = dQ/dV
c            (ptr2+6) = ...
c

