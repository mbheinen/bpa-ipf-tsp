C    @(#)dydb.f	20.3 2/13/96
        subroutine dydb (k1,m1,id1,ksect1,dydz)
 
C       This subroutine computes dY/dB, the 2-port Y-matrix sensitivity
C       with respect to a small change dBij in the passive node section
C       K1-M1-ID1-KSECT1.
 
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		None
      include 'ipfinc/branch.inc'
c	Global variables used:
c		kbrnch
      include 'ipfinc/bus.inc'
c	Global variables used:
c		kbsdta, inp2opt
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/prt.inc'
c	Global variables used:
c		None
 
        common /mtrx/ mtrx(2,MAXBUS)
 
        character id1 * 1, id2 * 1
c
        complex oldy(2,2), y(2,2), dydz(2,2)
c
	complex * 16 temp_y(2,2)
c
        integer count1, count2, first1, first2, error
c
        logical found
c
        count1 = 0
        count2 = 0
 
        found = .false.
        first1 = 0
        last1 = 0
        last2 = 0
        first2 = 0
        match = 0
        do 100 l = 1,2
           do 100 k = 1,2
              oldy(k,l) = cmplx (0.0,0.0)
  100   continue
 
        do 140 i = kbsdta(16,k1),kbsdta(16,k1+1)-1
        if (kbrnch(12,i) .eq. m1) then
 
           if (kbrnch(1,i) .eq. 4 .or. kbrnch(1,i) .eq. 9) go to 140
           call getchr(1,id2,kbrnch(13,i))
           if (id1 .eq. id2) then
              if (kbrnch(14,i) .eq. 0) then
                 call pieqiv (i,temp_y,error)
                 do 120 k = 1,2
                 do 120 l = 1,2
  120            y(k,l) =cmplx(temp_y(k,l))
                 if (kbrnch(1,i) .eq. 1) go to 140
              endif
              if (ksect1 .ne. kbrnch(14,i)) then
                 if (.not.found) then
                    count1 = count1 + 1
                    if (first1 .eq. 0) first1 = i
                    last1 = i
                 else
                    count2 = count2 + 1
                    if (first2 .eq. 0) first2 = i
                    last2 = i
                 endif
              else
                 match = i
                 if (.not.found) then
                    found = .true.
                 endif
              endif
           endif
        else if (kbrnch(12,i) .gt. m1) then
           go to 150
        endif
  140   continue
  150   continue
C
C       Compute the pertubed Y-matrix where the admittance of the
C       specified section is changed an amount
C
C       Yij' = Yij + jDelta(Bij)
C
C       Step 1. Get sections to left of KSECT1
C
        nsect = 0
        if (count1 .gt. 0) then
           do 160 i = first1,last1
           if (kbrnch(1,i) .ne. 4 .and. kbrnch(1,i) .ne. 9) then
              call pieqiv (i,temp_y,error)
                 do 155 k = 1,2
                 do 155 l = 1,2
  155            y(k,l) =cmplx(temp_y(k,l))
              nsect = nsect + 1
              if (nsect .eq. 1) then
                 call firsec (y)
              else
                 call nexsec (y)
              endif
           endif
160        continue
        endif
C
C       Step 2. Perturb B12
C
        call pieqiv (match,temp_y,error)
           do 165 k = 1,2
           do 165 l = 1,2
  165      y(k,l) =cmplx(temp_y(k,l))
        g12 = real (-y(1,2))
        b12 = aimag (-y(1,2))
        g1 = real (y(1,1) + y(1,2))
        b1 = aimag (y(1,1) + y(1,2))
        g2 = real (y(2,2) + y(2,1))
        b2 = aimag (y(2,2) + y(2,1))
        db12 = 0.001 * b12
        if (db12 .eq. 0.0) db12 = 0.100
        y(1,2) = -cmplx(g12,b12+db12)
        y(2,1) = y(1,2)
        y(1,1) = cmplx (g1,b1) - y(1,2)
        y(2,2) = cmplx (g2,b2) - y(1,2)
        nsect = nsect + 1
        if (nsect .eq. 1) then
           call firsec (y)
        else
           call nexsec (y)
        endif
C
C       Step 3. Get sections to right of KSECT1
C
        if (count2 .gt. 0) then
           do 170 i = first2,last2
           if (kbrnch(1,i) .ne. 4 .and. kbrnch(1,i) .ne. 9) then
              call pieqiv (i,temp_y,error)
                 do 166 k = 1,2
                 do 166 l = 1,2
  166            y(k,l) =cmplx(temp_y(k,l))
              nsect = nsect + 1
              if (nsect .eq. 1) then
                 call firsec (y)
              else
                 call nexsec (y)
              endif
           endif
  170      continue
        endif
C
C       Step 4. Get equivalent, perturbed 2-port admittance.
C
        call finsec (y)
C
C       Now compute "differential" admittance
C
        do 180 k = 1,2
        do 180 l = 1,2
  180   dydz(k,l) = (y(k,l) - oldy(k,l)) / db12
C
C       Debug dump
C
        kt = inp2opt(k1)
        mt = inp2opt(m1)
        write (dbug,190) intbus(kt),intbas(kt),intbus(mt),intbas(mt),
     1     id1,ksect1
  190   format ('0 Y-matrix sensitivity of branch ',a8,f6.1,1x,a8,f6.1,
     1     1x,a1,1x,i1,/)
        do 210 i = 1,2
        do 210 j = 1,2
        write (dbug,200) i,j,dydz(i,j)
  200   format ('  dY/dB ',2i3,' (',e12.5,',',e12.5,')')
  210   continue
 
        return
        end
