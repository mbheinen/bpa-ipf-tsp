C    @(#)ldoldbse.f	20.5 2/13/96
        subroutine ldoldbse (case, bdate, loaded)
        character case *(*), bdate*(*)
c
c       To do: 
C
C       1. 11 October: Update in LTRAN the y-matrix pointers
C       2. 11 October: Update in TIE the BRNCH pointers
c 
c       Load old base and internally convert into new base
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkku(r*8), bkku(r*8), qloadu(r*8), pnetu(r*8), qnetu(r*8),
c		ineti(r*8), inetr(r*8), bkmu(r*8), gkmu(r*8), ploadu(r*8),
c		vlimn(r*8), vlimx(r*8), km, kmlen, yptr, nspar
      include 'ipfinc/anlys.inc'
c	Global variables used:
c		None
      include 'ipfinc/arcntl.inc'
c	Global variables used:
c		arcnam, arcbus, arcbas, arcnet, arczns
c		arcnum, arcnam, ntotic, arcinp, arcint
      include 'ipfinc/area.inc'
c	Global variables used:
c		jarzn, tie(r*8), area(r*8)
      include 'ipfinc/basval.inc'
c	Global variables used:
c		basval
      include 'ipfinc/blank.inc'
c	Global variables used:
c		nztot, mtdcbs, mtdcln, kbrknt, kbsknt, kdchg, nslkxx,
c		ltot, kdtot, jtie, jphno, ntotb, ntotc, ntotcs, ntot, 
c		ntot2, bmva
      include 'ipfinc/branch.inc'
c	Global variables used:
c		brnch_nxt, brid, brtype, kx, ky, brnch_ptr, brsect,
c		kbrnch, ltot2, brnch_nxt, rateln
      include 'ipfinc/bus.inc'
c	Global variables used:
c		vstart, kbsdta, bus, inp2opt, opt2inp, zone, owner, base
c		alf2inp, inp2alf, e(r*8), f(r*8), capcor(r*8), busdta
      include 'ipfinc/busanl.inc'
c	Global variables used:
c		zsum, jowner
      include 'ipfinc/bushasht.inc'
c	Global variables used:
c		htable_b, nextptr_b
      include 'ipfinc/cbus.inc'
c	Global variables used:
c		kbctbl, bctbl_nxt
      include 'ipfinc/com008.inc'
c	Global variables used:
c		None
      include 'ipfinc/coment.inc'
c	Global variables used:
c		None
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtbs(r*8), dcmtln(r*8)
      include 'ipfinc/delete.inc'
c	Global variables used:
c		None
      include 'ipfinc/dtaiop.inc'
c	Global variables used:
c		kount, clabl2, pversn, usrnam
      include 'ipfinc/epridc.inc'
c	Global variables used:
c		nepbus, nepctl, epbus, epctl
      include 'ipfinc/filnam.inc'
c	Global variables used:
c		None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		datai, lprt
      include 'ipfinc/losanl.inc'
c	Global variables used:
c		ownnam, ownlos
      include 'ipfinc/oldbus.inc'
c	Global variables used:
c		oldbus, oldbas, olde
      include 'ipfinc/oldtbx.inc'
c	Global variables used:
c		oldtbx, numtbx
      include 'ipfinc/ordsta.inc'
c	Global variables used:
c		None
      include 'ipfinc/pfdata.inc'
c	Global variables used:
c		None
      include 'ipfinc/phase.inc'
c	Global variables used:
c		jphid
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
      include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8)
      include 'ipfinc/tran.inc'
c	Global variables used:
c		tran(r*4), tap(r*8)
      include 'ipfinc/xdata.inc'
c	Global variables used:
c		xdata(r*8)
      include 'ipfinc/ycomp.inc'
c	Global variables used:
c		nycomp, kycomp
 
        common /scratch/ xcomments(MAXCMT), xdelete(MAXDEL),
     &                   oldarczns(MAXCAZR,MAXCAR)
        character xcomments * 120, xdelete * 129, oldarczns * 2
        real temp(MAXYE), x22_temp(22,MAXXDT), x2_temp(2,MAXBUS), 
     &       x8_temp(8,MAXTBX), x10_temp(10,MAXTIE), 
     &       x45_temp(45,MAX2DC), x36_temp(36,MAXMDC)
        integer kx22_temp(22,MAXXDT), kx8_temp(8,MAXTBX),
     &          kx10_temp(10,MAXTIE), kx45_temp(45,MAX2DC),
     &          kx36_temp(36,MAXMDC)
        equivalence (oldarczns, temp, x22_temp, kx22_temp, x2_temp, 
     &               x8_temp, kx8_temp, x10_temp, kx10_temp,
     &               x45_temp, kx45_temp, x36_temp, kx36_temp)

        parameter (MAXECS = 13 * MAXBUS + 4 * MAXBRN)
        common /amtrx/ kk(2,MAXBUS), ecs(MAXECS), branch(18,2*MAXBRN)
        integer iecs(MAXECS), linxrf(2*MAXBRN), kbranch(18,2*MAXBRN),
     &          xbctbl(11,MAXCBS)
        real xrateln(3,2*MAXBRN)
        equivalence (ecs, iecs, xbctbl, linxrf), 
     &              (ecs(2*MAXBRN+1), xrateln),
     &              (branch, kbranch)

        common /is_batch / is_batch

        logical stab, rcflag, ldflow, error, found
        integer histin, p, h, bus_hash, pold
        character brn_type(9)*2
        character name * 80, tname * 80, backslash * 1
     
        data brn_type /'E*','LM','L ','R ','T ','TP','LD','E ','RZ'/

        backslash = char(92)
        loaded = 1

c       Description of COUNT array 
C
c       COUNT(*)   Description
C
C         1        Solution   -   0 = diverged
C                                 1 = converged   
c
C         2        Program version when file was created  
C                  Version  Date         Description
c
C                     0     -----------  Original Version
c
C                     1     13-JUL-1987  Sorted Area Interchange records
c
C                     2                  Skipped!
c
C                     3                  Extended branch ratings 
C                                        0 = none
C                                        1 = winter ratings
C                                        2 = winter ratings
C
C                     4                  Revised for HASHING
c
C         3        Extended branch ratings - 0 = none 
C                                            1 = winter ratings
C                                            2 = summer ratings
C       Description of KOUNT array   
c
C       KOUNT(*)   Counter   Description  
c
C         1        NZTOTX    Counter of ownerships in array ZSUM
c
C         2        JOWNER    Cnt of ownerships in arrays OWNNAM 
C                            and OWNLOS  
c
C         3        NTOT      Counter of buses in array VSTART   
c
C         4        NTOTIC    Counter of area intertie "I" records 
C                            in arrays ARCINT and ARCINP  
c
C         5        NYCOMP    Cnt of type "RZ" line compensation   
C                            branches in array YCOMP
c
C         6        (not used)
c
C         7        TIME      Base case time:
C                            10000 * hr + 100*min + sec
C
C         8        NEPBUS    Number of EPRI RP1964 d-c buses in case
C
C         9        NEPCTL    Number of EPRI RP1964 d-c control
C                            records in case
c
C        10        LTOT2     Number of double-entry branch records
c                            (Not used in old format)
C
C        11        NDEL      Number of data deletion records
c
C        12        NCOM      Number of case comment records
c
        read (datai) ntot, ntot2, ltot, ntota, ntotb, ntotc, kdtot,
     &               mtdcbs, mtdcln, kxtot, bmva, jtie, kecsy, kbsknt,
     &               kbrknt, jphno, nztot, nspare, ntotcs, nbslck, 
     &               nslkxx, kount

        kdchg = 1 
        ntotcs = 0
        ntot_alf = ntot

c       bus names, kv, zone code, ownership records  

        call rddac (datai, bus, ntot) 
        call rddat (datai, base, ntot)
        call rddac (datai, zone, ntot)
        call rddac (datai, owner, ntot)   
c
c       Read segmented bus data records (new form is non-segmented)

        do i1 = 1, ntot, 100                                    
           i2 = min0 (100, ntot-i1+1)                                  
           call rddat (datai, busdta(1,i1), i2*16)                   
        enddo

c       Create bus hash arrays

        call bushinit ()       ! initialize the bus hash tables...

        do i = 1, ntot
           h = bus_hash (bus(i), base(i))
           p = htable_b(h)
           do while (p .gt. 0)         ! Position to end of linked list
              p = nextptr_b(p)
           enddo
           nextptr_b(i) = htable_b(h)
           htable_b(h) = i             ! Inserts new bus at top of stack
        enddo
c
c       Read segmented continuation bus data records (new form is 
c       non-segmented and redimensioned (12,*). Relink using linked
c       lists.

        if (ntot2.gt.0) then                                      
           do i1 = 1, ntot2, 100                                
              i2 = min0 (100, ntot2-i1+1)                              
              call rddat (datai, xbctbl(1,i1), i2*11)                 
           enddo
           iold = 0
           pold = 0
           do i = 1, ntot2
              do j = 1, 11
                 kbctbl(j,i) = xbctbl(j,i)
              enddo
              if (iold .ne. kbctbl(1,i)) pold = 0
              iold = kbctbl(1,i)
              kbctbl(12,i) = 0
              bctbl_nxt(i) = 0
              if (pold .gt. 0) bctbl_nxt(pold) = i
              pold = i
           enddo

        endif 

c       Read interchange controls records 

        if (ntotc.gt.0) then  
           call rddac (datai, arcnam, ntotc)  
           call rddac (datai, arcbus, ntotc)  
           call rddat (datai, arcbas, ntotc)  
           call rddat (datai, arcnet, ntotc)  
           call rddac (datai, oldarczns, ntotc * MAXCAZR)   
           do i = 1, ntotc
              do j = 1, MAXCAZR
                 arczns(j,i) = oldarczns(j,i)
              enddo
              do j = MAXCAZR+1, MAXCAZ
                 arczns(j,i) = ' '
              enddo
           enddo
           call rddat (datai, acznum, nztot)  
           call rddac (datai, acznam, nztot)  
           call rddat (datai, jarzn, ntot)
           call rddat (datai, area, ntotc * 8)  
        endif 

c       Read switched reactive data record  

        if (kxtot.gt.0) then  
           call rddat (datai, x22_temp, kxtot * 22)
           do i = 1, kxtot
              xdata(1,i) = kx22_temp(1,i)
              xdata(2,i) = kx22_temp(2,i)
              do j = 3, 22
                 xdata(j,i) = x22_temp(j,i)
              enddo
           enddo
        endif 

c       Read segmented branch data records, defer conversion until
c       extended line ratings are loaded

        do i1 = 1, ltot, 100                                    
           i2 = min0(100,ltot-i1+1)                                
           call rddat (datai, branch(1,i1), i2*18)                  
        enddo

c       internal, external bus numbers e + Jf voltage tables 
c       capacitor correction tables   
C
        call rddat (datai, opt2inp, ntot)   
        call rddat (datai, inp2opt, ntot)   
        do i = 1, ntot
           alf2inp(i) = i       ! Note that the old scheme is 
           inp2alf(i) = i       ! input order == alpha order
        enddo

        call rddat (datai, temp, ntot)
        do i = 1, ntot
           e(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           f(i) = temp(i)
        enddo
        call rddat (datai, x2_temp, ntot * 2)
        do i = 1, ntot
           capcor(1,i) = x2_temp(1,i)
           capcor(2,i) = x2_temp(2,i)
        enddo

C       Store old bus names, voltages in alpha order  

        do 122 i = 1, ntot
           oldbus(i) = bus(i) 
           oldbas(i) = base(i)
           olde(i) = e(inp2opt(i))  
           oldf(i) = f(inp2opt(i))  
  122   continue  
        numobs = ntot 
C
c       conditional] phase shifter data record   

        if (jphno.gt.0) then  
           call rddat (datai, jphid, jphno * 8) 
        endif 

C       Load in and convert Y-matrix and solution arrays
c
        call rddat (datai, kk, ntot*2)                            

        do i1 = 1, kecsy, 4000                               
           last = min0(4000,kecsy-i1+1)                        
           call rddat (datai, iecs(i1), last)                         
        enddo

        yptr = 0
        do i = 1, ntot
           is = kk(1,i)
           ie = kk(2,i)
           pnetu(i) = ecs(is)
           qnetu(i) = ecs(is+1)
           gkku(i) = ecs(is+2)
           bkku(i) = ecs(is+3)
           ntypu(i) = iecs(is+4)
           nspar(i) = iecs(is+5)
           vlimn(i) = ecs(is+6)
           vlimx(i) = ecs(is+7)
           ploadu(i) = ecs(is+8)
           qloadu(i) = ecs(is+9)
           inetr(i) = ecs(is+10)
           ineti(i) = ecs(is+11)
           
           km(i) = yptr + 1
           kmlen(i) = (ie - 12) / 3
           do j = is+12, is+ie-1, 3
              yptr = yptr + 1
              ikmu(yptr) = iecs(j)
              gkmu(yptr) = ecs(j+1)
              bkmu(yptr) = ecs(j+2)
           enddo
        enddo
C
c       Load segmented TBX array
C
        if (ntotb.gt.0) then  
           do i1 = 1, ntotb, 50                                  
              i2 = min0(50, ntotb-i1+1)                                
              call rddat (datai, x8_temp(1,i1), i2*8)                    
           enddo
           do i = 1, ntotb
              ltyp = kx8_temp(1,i)
              tbx(1,i) = ltyp
              tbx(2,i) = kx8_temp(2,i)
              tbx(3,i) = x8_temp(3,i)
              tbx(4,i) = x8_temp(4,i)
              if (ltyp .eq. 5) then
                 tbx(5,i) = kx8_temp(5,i)
              else
                 tbx(5,i) = x8_temp(5,i)
              endif
              tbx(6,i) = x8_temp(6,i)
              tbx(7,i) = kx8_temp(7,i)
              tbx(8,i) = kx8_temp(8,i)
           enddo
c
C          Convert any distantly remote controlled buses;  
C          store original TBX array.   
c
           do j = 1, ntotb   
              ltyp = kx8_temp(1,i)
              if (ltyp .eq. 3 .or. ltyp .eq. 5) then  
                 if (tbx(8,j) .lt. 0) then  
                    m = tbx(8,j)
                    m = iabs(m)
                    tbx(8,j) = opt2inp(m) 
                 endif   
              endif  
              do k = 1, 8
                 oldtbx(k,j) = tbx(k,j)
              enddo
           enddo
           numtbx = ntotb 

        endif 
C
c       Load segmented LTC (automatic transformer) arrays
C
        if (ntota.gt.0) then  
           do i1 = 1, ntota,50                                  
              i2 = min0(50, ntota-i1+1)                                
              call rddat (datai, tran(1,i1), i2*12)                  
              call rddat (datai, temp(i1), i2)                        
           enddo
           do i = 1, ntota
              tap(i) = temp(i)
           enddo
        endif 
C
c       Load segmented tie line array
C
        if (jtie.gt.0) then   
           do i1 = 1, jtie, 100                                  
              i2 = min0(100,jtie-i1+1)                                 
              call rddat (datai, x10_temp(1,i1), i2*10)                   
           enddo
           do i = 1, jtie
              tie(1,i) = kx10_temp(1,i)
              tie(2,i) = kx10_temp(2,i)
              do k = 3, 6
                 tie(k,i) = x10_temp(k,i)
              enddo
              do k = 7, 10
                 tie(k,i) = kx10_temp(k,i)
              enddo
           enddo
        endif 
C
c       Load d-c system arrays
C
        if (kdtot .gt. 0) then
           call rddat (datai, x45_temp, kdtot * 45) 
           do i = 1, kdtot
              dc2t(1,i) = kx45_temp(1,i)
              dc2t(2,i) = x45_temp(2,i)
              dc2t(3,i) = kx45_temp(3,i)
              do k = 4, 6
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(7,i) = kx45_temp(7,i)
              do k = 8, 19
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(20,i) = kx45_temp(20,i)
              do k = 21, 32
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(33,i) = kx45_temp(33,i)
              dc2t(34,i) = kx45_temp(34,i)
              do k = 35, 36
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(37,i) = kx45_temp(37,i)
              dc2t(38,i) = kx45_temp(38,i)
              do k = 39, 45
                 dc2t(k,i) = x45_temp(k,i)
              enddo
           enddo
        endif 
        if (mtdcbs.gt.0) then 
           call rddat (datai, x36_temp, mtdcbs * 36)  
           do i = 1, mtdcbs
              dcmtbs(1,i) = kx36_temp(1,i)
              dcmtbs(2,i) = x36_temp(2,i)
              dcmtbs(3,i) = kx36_temp(3,i)
              dcmtbs(4,i) = kx36_temp(4,i)
              do k = 5, 14
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              dcmtbs(15,i) = kx36_temp(15,i)
              do k = 16, 20
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              dcmtbs(21,i) = kx36_temp(21,i)
              dcmtbs(22,i) = kx36_temp(22,i)
              do k = 23, 26
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              dcmtbs(27,i) = kx36_temp(27,i)
              dcmtbs(28,i) = kx36_temp(28,i)
              do k = 29, 30
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              do k = 31, 36
                 dcmtbs(k,i) = kx36_temp(k,i)
              enddo
           enddo
        endif 
        if (mtdcln.gt.0) then 
           call rddat (datai, x10_temp, mtdcln * 10)  
           do i = 1, mtdcln
              do k = 1, 3
                 dcmtln(k,i) = kx10_temp(k,i)
              enddo
              do k = 4, 6
                 dcmtln(k,i) = x10_temp(k,i)
              enddo
              do k = 7, 10
                 dcmtln(k,i) = kx10_temp(k,i)
              enddo
           enddo
        endif 

        nztotx = kount (1)
        jowner = kount (2)
        if (nztotx.gt.0)  then
           read (datai) (zsum(5,i),zsum(6,i) ,i=1, nztotx)   
        endif 
        if (jowner.gt.0) then 
           call rddac (datai, ownnam, jowner) 
           call rddat (datai, ownlos, jowner * 2)   
        endif 

        i = kount (3) 
        if (i.gt.0) call rddat (datai, vstart, i) 

        ntotic = kount(4) 
        if (ntotic.gt.0) then 
           call rddat (datai,arcinp,ntotic)   
           call rddac (datai,arcint,2 * ntotic) 
        endif 

        nycomp = kount(5) 
        if (nycomp .gt. 0) then   
           call rddat(datai,kycomp,48 * nycomp) 
        endif 
c
c       Load extended line rating array with the following steps:
c
c       1. Load from old base into temporary array rateln(*,*)
c       2. Expand rateln(*,*) into xrateln(*,*) which is parallel 
c          with branch(*,*)
c       3. Load xrateln(*,*) into rateln(*,*)
c
        numlrt = kount(6)                                         
        if (numlrt .gt. 0) then                                   
           do 180 i1 = 1, ltot, 2000                                
              i2 = min0(2000, ltot-i1+1)                             
              call rddatx (datai, linxrf(i1), i2, error)             
              if (error) then                                        
                 write (*,176) numlrt                                
  176            format (' Record fragment in extended line ratings.'
     1,i6,' data items are ignored.')                                   
                 write (errbuf(1),176) numlrt                        
                 call prterx ('w',1)                                 
                 numlrt = 0                                          
                 go to 191                                           
              endif                                                  
  180      continue                                               
           do 190 i1 = 1, numlrt, 100                               
              i2 = min0(100, numlrt-i1+1)                            
              call rddatx (datai, rateln(1,i1), 3*i2, error)         
              if (error) then                                        
                 write (*,176) numlrt                                
                 write (errbuf(1),176) numlrt                        
                 call prterx ('w',1)                                 
                 numlrt = 0                                          
                 go to 191                                           
              endif                                                  
  190      continue                                               
  191      continue                                               
        else                                                      
           do i = 1, ltot                                     
              linxrf(i) = 0                                          
           enddo
        endif                                                     
c
c       Build branch data records
c
        iold = 0
        ltot2 = ltot
        ltot = 0
c
c       Initialize xrateln
c
        do i = 1, ltot2
           do k = 1, 3
              xrateln(k,i) = 0.0
           enddo
        enddo
c
c       Transfer compact rateln into xrateln. It will now be parallel 
c       with branch
c
        do i = 1, ltot2
           j = linxrf(i)
           if (j .gt. 0) then
              do k = 1, 3
                 xrateln(k,i) = rateln(k,j)
              enddo
           endif
        enddo

        do i = 1, ltot2
           brtype(i) = kbranch(1,i)
           kx(i) = kbranch(2,i)
           ky(i) = kbranch(12,i)
           if (brtype(i) .eq. 4) then
              brid(i) = ' '
              brsect(i) = 0
           else
              call getchr (1, brid(i), kbranch(13,i))
              brsect(i) = kbranch(14,i)
           endif
           if (kbranch(17,i) .eq. 0) then
              ltot = ltot + 1
              do j = 1,18
                 kbrnch(j,ltot) = kbranch(j,i)
              enddo
              do j = 1, 3
                 rateln(j,ltot) = xrateln(j,i)
              enddo
              brnch_ptr(i) = ltot
           else
              brnch_ptr(i) = 0
           endif    
           brnch_nxt(i) = 0
           if (kx(i) .eq. iold) then
              brnch_nxt(jold) = i
           else
              iold = kx(i)
              kbsdta(16,iold) = i
           endif
           jold = i
        enddo
c
c       Now catch transposes which were skipped.
c
        do i = 1, ltot2
           if (brnch_ptr(i) .eq. 0) then
              found = .false.
              p = kbsdta(16,ky(i))
              do while (p .gt. 0 .and. .not. found)
                 if (ky(p) .eq. kx(i) .and.
     &               brid(p) .eq. brid(i) .and.
     &               brsect(p) .eq. brsect(i) .and.
     &               brtype(p) .eq. brtype(i)) then
                    brnch_ptr(i) = -brnch_ptr(p)
                    found = .true.
                 else
                    p = brnch_nxt(p)
                 endif
              enddo
              if (.not. found) call erexit(1)
           endif
        enddo
c
c       If all is well, ltot == ltot2 / 2
c
        error = .false.
        if (2 * ltot .eq. ltot2) then
        else if (2 * ltot .ne. ltot2) then
           error = .true.
           write (errbuf(1), 192) ltot, ltot2/2
  192      format (' Internal branch conversion error - ',
     1             'number of single-entry branches counted = ',i5,
     2             ', computed = ', i5)
           write (errbuf(2), 193) 
  193      format (' Duplicately constructed branches are flagged')
           call prterx ('W', 2)
           do i = 1, ltot2
              if (brnch_ptr(i) .eq. 0) then
                 write (errbuf(1), 194) brn_type(brtype(i)), bus(kx(i)),
     &              base(kx(i)), bus(ky(i)), base(ky(i)), brid(i),
     &              brsect(i)
  194            format (' Omitted branch ', a, 1x, a8, f7.1, 1x, a7,
     &              f7.1, 1x, a1, i2)
              else
                 found = .false.
                 p = kbsdta(16,ky(i))
                 do while (p .gt. 0 .and. .not. found)
                    if (ky(p) .eq. kx(i) .and.
     &                  brid(p) .eq. brid(i) .and.
     &                  brsect(p) .eq. brsect(i) .and.
     &                  brtype(p) .eq. brtype(i)) then
                       found = .true.
                    else
                       p = brnch_nxt(p)
                    endif
                 enddo
                 if (found) then
                    if (brnch_ptr(i) .eq. 0) then
                    else if (isign(1,brnch_ptr(i)) .eq. 
     &                       isign(1,brnch_ptr(p))) then
                       write (errbuf(1), 195) brn_type(brtype(i)), 
     &                    bus(kx(i)), base(kx(i)), bus(ky(i)), 
     &                    base(ky(i)), brid(i), brsect(i)
  195                  format (' Duplicately constructed branch ', a, 
     &                    1x, a8, f7.1, 1x, a7, f7.1, 1x, a1, i2)
                       call prterx ('w', 1)
                    else
                    endif
                 endif
              endif
           enddo
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
        endif
C
C       Retrieve any EPRI RP1964 d-c data.
C
        nepbus = kount(8)
        nepctl = kount(9)
C
C       EPBUS = character array containing BD, BZ, LD, MD records.
C       EPCTL = character array containing CC, CD, CN, CR records.

        if (nepbus .gt. 0) then
           call rddac (datai, epbus, nepbus)
           if (nepctl .gt. 0) call rddac (datai, epctl, nepctl)

           iver = 5
           histin = datai    ! RCFLAG -  recover saved case flag.
           jtape = lprt
           stab = .false.
           ldflow = .true.
           rcflag = .true.   ! RCFLAG -  recover saved case flag.
C
C          IVER = 5 corresponds with highest version in EPRI
C          powerflow.

           call loaddc (iver, histin, rcflag, jtape, ldflow, stab)
           call loadcc (histin)
        endif

        ndelx = kount(11)
        ncomx = kount(12)

        if (ncomx .gt. 0) then
           call rddac(datai, xcomments, ncomx)
        endif

        if (ndelx .gt. 0) then
           call rddac(datai, xdelete, ndelx)
        endif

        return
        end
