C    @(#)bsanrp.f	20.3 2/13/96
        subroutine bsanrp  (ksw,subttl,lprsx1,fchsx1)

c       This subroutine writes two analysis reports to the
c       power flow output (.PFO) file, one titled 'Summary
c       of Unscheduled Reactive' and the other 'Bus Quantities'.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/anlys.inc'
c	Global variables used:
c		None
        include 'ipfinc/arcntl.inc'
c	Global variables used:
c		None
        include 'ipfinc/blank.inc'
c	Global variables used:
c		ntot_alf, lskp, bmva
        include 'ipfinc/bus.inc'
c	Global variables used:
c		zone, bus, inp2alf, base
        include 'ipfinc/busanl.inc'
c	Global variables used:
c		kabus, abus, nba
        include 'ipfinc/com010.inc'
c	Global variables used:
c		None
        include 'ipfinc/optim.inc'
c	Global variables used:
c		None
        include 'ipfinc/owncom.inc'
c	Global variables used:
c		None
        include 'ipfinc/prt.inc'
c	Global variables used:
c		lprtsw, fichsw, outbuf
        include 'ipfinc/slnopt.inc'
c	Global variables used:
c		option
        include 'ipfinc/zonlst.inc'
c	Global variables used:
c		None
 
        dimension subttl(14)
        external kompan,swapan
 
        character zn*2, a1*12, a2*8, pfcc*5, own*3, blank*1, type*1
 
        integer fchsx1
 
        blank = ' '
        qtol = bmva*amax1 (option(4),option(7))
 
        do 100 i = 1,12
          subttl(i) = 0.0
  100   continue
 
        do 1000 nb = 1,nba
           k1 = kabus(1,nb)
           if (inp2alf(k1) .eq. 0 .or. inp2alf(k1) .gt. ntot_alf) 
     &        go to 1000
           call getchr(1,type,kabus(2,nb))
           voltpu=abus(4,nb)
           degree=abus(14,nb)
           voltkv = voltpu*base(k1)
           zn = zone(k1)
           own = owner(k1)
 
           if (ksw .eq.0 ) then
C                          KSW = 0 : UNSCHEDULED REACTIVE REPORT
              if ((index('EV',type) .ne. 0) .or. 
     &             (abs(abus(11,nb)) .gt. qtol .and. lskp .ne. 3)) then
                 lprtsw = lprsx1
                 fichsw = fchsx1
                 if (lprtsw.ne.0.or.fichsw.ne.0) then
                    subttl(7) = subttl(7) + amax1(0.0,abus(11,nb))
                    subttl(10) = subttl(10) + amin1(0.0,abus(11,nb))
 
                    write (outbuf,930) own, zone(k1), bus(k1), base(k1),
     &                                 type, abus(11,nb), voltkv
  930               format(40x,a3,2x,a2,2x,a8,f7.1,4x,a1,f12.1,f12.1)
                    call prtout (1)
                 endif
              endif
           else
 
C             KSW = 1 : BUS ANALYSIS REPORT
C             TEST FOR PARTIAL SUMMARY BY ZONE/OWNER

              lprtsw = ipzo(zn,own,lprsx1)
              fichsw = ifzo(zn,own,fchsx1)
 
C             PARTIAL BUS SUMMARY OPTION HAS BEEN SELECTED.
C             ONE OF THE FOLLOWING CRITERIAHASBEENFULFILLED.
 
C                 1 -- OWNERSHIP SPECIFIED
C                 2 -- CONTAINSGENERATION,SHUNTSUSCEPTANCE,
C                      OR 230/500 KV BANK TAPS
 
 
              if ( fichsw + lprtsw .gt. 0 ) then
                 a1 = ' '
                 write (a2,820) abus(5,nb)
  820            format(f8.1)
                 if (abus(5,nb).eq.0.0) then
                    if (abus(6,nb).ne.0.0) a2 = '    COND'
                 endif
                 if (abus(3,nb).ne.0.0) then
                    write (a1,840) abus(3,nb),abus(13,nb)
  840               format(f6.1,'/',f5.1)
                 endif
                 pfcc = ' '
                 if (abs(abus(5,nb)).ge.0.001) then
                    pfc = -abus(5,nb)
     &                  / sqrt(abus(5,nb)**2+abus(6,nb)**2)
     &                  * sign(1.0,abus(6,nb))
 
                    if (abs(pfc) .ge. 0.80) then
                       pfcc = ' '
                    else
                       write (pfcc,870) pfc
  870                  format(f5.2)
                    endif
                 endif
C
C                ABUS(9,nb) = MVARs of available capacitors
C                ABUS(16,nb) = MVARs of available reactors
C                ABUS(10,nb) = MVARs of used capacitors
C                ABUS(17,nb) = MVARs of used reactors
C
C                If the current bus has capacitors and reactors 
C                coexisting, write two lines of bus solution 
C                information to the .PFO file. The first line contains 
C                the amount of used and available reactors, while the 
C                second line contains only the amount of used and 
C                available capacitors.
C
                 if (abus(9,nb) .ge. 1.0 .and. abus(16,nb) .le. -1.0)
     &              then
                    write (outbuf,890) bus(k1), base(k1), voltkv,a1,a2,
     1                 (abus(i,nb),i=6,8),abus(17,nb),abus(16,nb),
     2                  abus(11,nb),type,own,zone(k1),pfcc,voltpu,degree
  890               format(1x,a8,f6.1,f7.1,a12,a8,3f8.1,3f9.1,4x,a1,4x,
     1                  a3,2x,a2,1x,a5,f9.3,'/',f6.1)
                    call prtout (1)
                    write (outbuf,892) abus(10,nb),abus(9,nb)
  892               format(t67,2f9.1)
C
C                If the current bus does not have capacitors and 
C                reactors coexisting at the bus, write one line of bus 
C                solution information to .PFO file
C
                 else
                    qused = abus(10,nb) + abus(17,nb)
                    qsched = abus(9,nb) + abus(16,nb)
                    write (outbuf,894) bus(k1),base(k1),voltkv,a1,
     1                  a2,(abus(i,nb),i=6,8),qused,qsched,
     2                  abus(11,nb),type,own,zone(k1),pfcc,voltpu,degree
  894               format(1x,a8,f6.1,f7.1,a12,a8,3f8.1,3f9.1,4x,a1,
     &                     4x,a3,2x,a2,1x,a5,f9.3,'/',f6.1)
                 endif
                 call prtout (1)
C
                 do 895 i = 1,4
                    subttl(i) = subttl(i) + abus(i+4,nb)
  895            continue
                 subttl(5) = subttl(5) + abus(9,nb)
                 subttl(6) = subttl(6) + abus(10,nb)
                 subttl(7) = subttl(7) + amax1 (abus(11,nb),0.0)
                 subttl(8) = subttl(8) + abus(16,nb)
                 subttl(9) = subttl(9) + abus(17,nb)
                 subttl(10) = subttl(10) + amin1 (abus(11,nb),0.0)
 
                 if (abs(abus(5,nb)).gt.0.05) then
                    subttl(11) = subttl(11) + abus(6,nb)
                 else
                    subttl(12) = subttl(12) + abus(6,nb)
                 endif
              endif
           endif
1000    continue
 
        return
        end
