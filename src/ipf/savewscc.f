C    @(#)savewscc.f	20.5 3/29/99
c***********************************************************************  
c  
c     File: savewscc.f  
c  
c     Purpose: Save base case history data for WSCC Transient  
c              Stability program.  
c  
c     Author: Bert Schmidt            Date: 7 December 1992  
c  
***** Modified: 6/07/93 by Merilyn George  
*     Added array identification to ASCII output  
*     Corrected error in calculation of qg (generated vars)  
*     Changed iprog (integer) to prgvsn (char) to handle new IPF version  
*       and moved it to the first record (before TITLE)  
*     Added logic to recreate the bus optimal ordering table, ktonpi,  
*       in natural order, without DC buses. Write i=1,ntoti (not nobus).  
*     Changed for new bus name processing.  All bus tables are written  
*        in 'input' (alpha) order.  Removed intnum and ixcros. Stability  
*        will reindex bus names on input.  
*     Removed variables not used by Stability: nremot, mkswxc, swbsr  
*  
***** Modified: 10/27/93, J.Coleman  (fix R record circuit ID)  
*     Fixed logic for R records so linked list pointers are traversed  
*     properly and output arrays are incremented properly  
*  
***** Modified:  11/1/93 by Ronald Schellberg (format ASIF)  
*     Changed all list directed output for ASIF file to formatted.  
*     Changed writes of some data to orthogonal order for readability.  
*  
***** Modified: 12/15/93, M.George   (fix R record transposition)  
*     loc (returned value of function orienttx) was not used.  
*     But in the 0102hw1 test case, 4 of 13 regulating phase shifters  
*     have loc=2, while all the rest have loc=1.  3 of these 4 have  
*     the wrong sign on the phase shift angle, and the 4th has a  
*     zero angle. Corrected code to work correctly for this case;  
*     it is still unknown exactly what is going on here.  
*  
***** Modified: 3/24/94, M.George  (initialize lindx)  
*     Building of the line index array (lindx) assumed that initial  
*     values in the scratch array were zero.  This is not the case  
*     if another case has been processed already. (GUI only)  
c  
c  
c     Called by: prodat.f   from the batch version  
c                p_svfile.f from the GUI  
c  
c***********************************************************************  
c  
      integer function savewscc (savfil,filetype)  
      integer savfil                      ! logical unit number  
      character filetype*(*)              ! BINARY or ASCII  
   
   
      include 'ipfinc/parametr.inc'  
   
      include 'ipfinc/alpha.inc'  
      include 'ipfinc/arcntl.inc'  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/branch.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/cbus.inc'  
      include 'ipfinc/dc2t.inc'  
      include 'ipfinc/dcmt.inc'  
      include 'ipfinc/header.inc'  
      include 'ipfinc/jobctl.inc'  
      include 'ipfinc/tran.inc'  
      include 'ipfinc/xdata.inc'  
      include 'ipfinc/bstype.inc'  
      include 'ipfinc/brtype.inc'  
   
************************************************************************  
************************************************************************  
************************************************************************  
*  WSCC tables are built in temporary scratch storage prior to writing.  
*  Use scratch area already defined for other routines.  
************************************************************************  
      common /scratch/ scratch( 25*MAXBUS )  
      common /scrtch/ scrchar( 25*MAXBUS )  
      character scrchar*1  
   
***********************************  
*  For Area interchange and Zones **************************************  
***********************************  
*** MAXCAZR (=10) is used because current IPS cases allow only 10  
*** zones/area.  IPF allows 50.  This dimension, and processing in  
*** stability, will have to be changed sometime in the future.  
   
      integer intzon(MAXCZN), ixzon(MAXCZN), idarzn(MAXCAR,MAXCAZR),  
     1        intara(MAXCZN)  
   
      equivalence (scratch( 1         ),intzon(1))  
     1           ,(scratch(   MAXCZN+1),ixzon(1))  
     2           ,(scratch( 2*MAXCZN+1),intara(1))  
     3           ,(scratch( 3*MAXCZN+1),idarzn(1,1))  
   
******************  
*  For Bus info  *******************************************************  
******************  
      integer itypabus(16),narea(MAXBUS),  
     1        kind(MAXBUS,2),ktonpi(MAXBUS),newnum(MAXBUS)  
   
      real bkv(MAXBUS), pl(MAXBUS), ql(MAXBUS), ea(MAXBUS), eb(MAXBUS),  
     2     pg(MAXBUS), qg(MAXBUS),gsr(MAXBUS),bsr(MAXBUS)  
   
      equivalence (scratch( 1         ),bkv(1))  
     3           ,(scratch(   MAXBUS+1),pl(1))  
     4           ,(scratch( 2*MAXBUS+1),ql(1))  
     5           ,(scratch( 3*MAXBUS+1),ea(1))  
     6           ,(scratch( 4*MAXBUS+1),eb(1))  
     7           ,(scratch( 5*MAXBUS+1),pg(1))  
     8           ,(scratch( 6*MAXBUS+1),qg(1))  
     9           ,(scratch( 7*MAXBUS+1),gsr(1))  
     a           ,(scratch( 8*MAXBUS+1),bsr(1))  
     b           ,(scratch( 9*MAXBUS+1),narea(1))  
     c           ,(scratch(10*MAXBUS+1),kind(1,1))  
     d           ,(scratch(11*MAXBUS+1),kind(1,2))  
     e           ,(scratch(12*MAXBUS+1),ktonpi(1))  
     f           ,(scratch(13*MAXBUS+1),newnum(1))  
   
      character title(3)*72, iname(MAXBUS)*8, rsubtyp*1  
      equivalence (scrchar(1),title(1))  
      equivalence (scrchar(220),iname(1))  
   
   
*********************  
*  For Branch info  ****************************************************  
*********************  
      integer lkind(MAXBRN),lcirc(MAXBRN,2),  
     1        lindx(MAXBRN2), linab(MAXBRN2,2), ltra(MAXLTC),  
     2        ltrb(MAXLTC)  
   
      real    g(MAXBRN), b(MAXBRN), pgc1(MAXBRN), pbc1(MAXBRN),  
     1        pgc2(MAXBRN), pbc2(MAXBRN), atap(MAXLTC)  
   
      equivalence (scratch( 1         ),lkind(1))  
     1           ,(scratch(   MAXBRN+1),lcirc(1,1))  
     2           ,(scratch( 3*MAXBRN+1),g(1))  
     3           ,(scratch( 4*MAXBRN+1),b(1))  
     4           ,(scratch( 5*MAXBRN+1),pgc1(1))  
     5           ,(scratch( 6*MAXBRN+1),pbc1(1))  
     6           ,(scratch( 7*MAXBRN+1),pgc2(1))  
     7           ,(scratch( 8*MAXBRN+1),pbc2(1))  
     8           ,(scratch( 9*MAXBRN+1),lindx(1))  
   
****************************  
*  For 2-terminal DC info  *********************************************  
****************************  
   
      integer idcex(MAX2DC,2), idcint(MAX2DC,2), idcbus(2*MAX2DC),  
     1        newdc1(2*MAX2DC)  
   
      real tlohm(MAX2DC), curpp(MAX2DC), curnt(MAX2DC), dckv(2*MAX2DC),  
     1     dcmw(2*MAX2DC), vdrop(2*MAX2DC), rohmdc(2*MAX2DC),  
     2     xohmdc(2*MAX2DC), firang(2*MAX2DC), alfmin(2*MAX2DC),  
     3     tr(2*MAX2DC), dcbase(2*MAX2DC), pgen(2*MAX2DC),  
     4     qgen(2*MAX2DC), dcirat(2*MAX2DC)  
   
   
   
      equivalence (scratch( 1         ),idcex(1,1) )  
     1           ,(scratch( 2*MAX2DC+1),idcint(1,1))  
     2           ,(scratch( 4*MAX2DC+1),idcbus(1)  )  
     3           ,(scratch( 6*MAX2DC+1),newdc1(1)  )  
     4           ,(scratch( 8*MAX2DC+1),tlohm(1)   )  
     5           ,(scratch( 9*MAX2DC+1),curpp(1)   )  
     6           ,(scratch(10*MAX2DC+1),curnt(1)   )  
     7           ,(scratch(11*MAX2DC+1),dckv(1)    )  
     8           ,(scratch(13*MAX2DC+1),dcmw(1)    )  
     9           ,(scratch(15*MAX2DC+1),vdrop(1)   )  
     a           ,(scratch(17*MAX2DC+1),rohmdc(1)  )  
     b           ,(scratch(19*MAX2DC+1),xohmdc(1)  )  
     c           ,(scratch(21*MAX2DC+1),firang(1)  )  
     d           ,(scratch(23*MAX2DC+1),alfmin(1)  )  
   
      equivalence (scratch(25*MAX2DC+1),tr(1)      )  
     1           ,(scratch(27*MAX2DC+1),dcbase(1)  )  
     2           ,(scratch(29*MAX2DC+1),pgen(1)    )  
     3           ,(scratch(31*MAX2DC+1),qgen(1)    )  
     4           ,(scratch(33*MAX2DC+1),dcirat(1)  )  
   
   
   
*********************  
*  local variables  ****************************************************  
*********************  
   
      logical found, ascii  
      integer pnxt,p,q, orienttx, error  
   
      integer find_zon  
      external find_zon  
   
***************************  
*  codes for circuit id   **********************************************  
***************************  
      character ialfa*38  
      data ialfa /' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*'/  
   
************************************************************************  
*  END of WSCC scratch storage  
************************************************************************  
   
   
************************************************************************  
************************************************************************  
*     Begin to write the WSCC file, Converting from BPA tables as we go.  
************************************************************************  
************************************************************************  
   
   
      if ( mtdcbs .gt. 0 ) then  
c  
c        Data contains multi_terminal dc modeling.  WSCC cannot handle  
c        this.  Flag an error and return....  
c  
         savewscc = 1                 !   status bad, file not written  
         return  
      endif  
   
      if (filetype .eq. 'ASCII') then  
         ascii = .true.  
      else  
         ascii = .false.  
      endif  
   
      error = 0  
   
**********************  
*  Program version   ***************************************************  
**********************  
      if (ascii) then  
         write (savfil,'(A,A)') 'ASIF From IPF ',prgvsn  
      else  
         write (savfil)   'BSIF From IPF ',prgvsn  ! Program rev. number  
      endif  
**********************  
*     Title records  ***************************************************  
**********************  
      title(1)      = ' ( Powerflow Caseid : '//chase1(1)  
      title(1)(33:) = '   Project = '//chase1(34)//chase1(35)//' )'  
      title(2)      = coment(1)  
      title(3)      = coment(2)  
      if ( ascii ) then  
         write (savfil,'(A72)') title  
      else  
         write (savfil) title  
      endif  
   
*****************  
*     switches  ********************************************************  
*****************  
   
      if ( lskp .eq. 0 ) then  
*        Good data, but unsolved case (initial value before solution)  
         idat = 0  
         istat1 = 0  
      else if ( lskp .eq. 1  .or.  lskp .eq. 2 ) then  
*        Good data and solved case  
         idat = 0  
         istat1 = 10  
      else if ( lskp .eq. 3 ) then  
*        Good data, but solution diverged  
         idat = 0  
         istat = 1  
      else  
*        Unknown status  
         idat = 1  
         istat1 = 0  
      endif  
   
      if (ascii) then  
         write (savfil,'(A,2I5)') 'case status and state',  
     1              idat,   ! Flag - 0 for a good case  
     2              istat1  ! Flag - 10 for a solved case  
      else  
         write (savfil)  
     1              idat,   ! Flag - 0 for a good case  
     2              istat1  ! Flag - 10 for a solved case  
      endif  
   
   
******************  
*    Zone data   *******************************************************  
******************  
      numzon = nztot               ! Number of total zones in system  
c  
c     Assuming that the zone id's were input in alfa order, this  
c     will make the input order and the alphabetic order the same.  
c     Also, the bus tables will point to the zone with the alpha index  
c     which is easy to get.  
c  
      do iz = 1,numzon  
         intzon(iz) = iz          ! Input order zone numbers  
         ixzon(iz)  = iz          ! Location of zone in zone name list  
      enddo  
   
c     Alpha sort list of all zones codes (A2)  
c     Alpha order list of input order zone numbers and  
c     input order list of pointers to the alpha zone codes  
   
      if (ascii) then  
         write (savfil,'(A8,I4)') ' numzon ',numzon  
         write (savfil,'(8X,50A2)') (acznam(i),i=1,numzon)   ! namzon  
         write (savfil,'(A8,(30I4))') ' intzon ',(intzon(i),i=1,numzon)  
         write (savfil,'(A8,(30I4))') ' ixzon  ',(ixzon(i) ,i=1,numzon)  
      else  
         write (savfil) numzon,  
     1                 (acznam(i),i=1,numzon),  
     2                 (intzon(i),i=1,numzon),  
     3                 (ixzon(i) ,i=1,numzon)  
      endif  
   
   
************************  
*   Area control data  *************************************************  
************************  
      nmarea = ntotc            ! Number of control areas in system  
   
      do ia=1,nmarea  
         intara(ia) = ia        ! input order area number in alpha order  
c  
c        build internal number of zones for each area  
c               (bpa/wscc indices are transposed)  
   
         idarzn(ia,1) = find_zon(arczns(1,ia))  
         do iz = 2,MAXCAZR  
            if (arczns(iz,ia).ne.'  ') then  
               idarzn(ia,iz) = find_zon(arczns(iz,ia))  
            else  
               idarzn(ia,iz) = 0  
            endif  
         enddo  
      enddo  
   
      if (ascii) then  
         write (savfil,'(A8,I4)') ' nmarea ',nmarea  
         write (savfil,'(8X,10A10)') (arcnam(ia),ia=1,nmarea)   ! iarea  
         write (savfil,'(A8,(30I4))')' intara ',(intara(ia),ia=1,nmarea)  
         write (savfil,'(A8,(10I4))')' idarzn ',  
     &              ((idarzn(ia,iz),iz=1,MAXCAZR),ia=1,nmarea)  
      else  
         write (savfil) nmarea,  
     1              (arcnam(ia),ia=1,nmarea),  ! Sorted area names (A10)  
     2              (intara(ia),ia=1,nmarea),  ! internal area numbers  
     3             ((idarzn(ia,iz),iz=1,MAXCAZR),ia=1,nmarea) !zn by area  
      endif  
   
*****************  
*    Bus Data   *********************************  
*****************  
c     initialize the wscc itypa codes for the 16 bus types  
      itypabus(BSTYP_B ) = 2  
      itypabus(BSTYP_BE) = 7  
      itypabus(BSTYP_BS) = 26  
      itypabus(BSTYP_BC) = 8  
      itypabus(BSTYP_BD) = 20  
      itypabus(BSTYP_BV) = 6  
      itypabus(BSTYP_BQ) = 3  
      itypabus(BSTYP_BG) = 9  
      itypabus(BSTYP_BO) = 31     ! Optimal bus  
      itypabus(BSTYP_BT) = 11  
      itypabus(BSTYP_BX) = 22  
      itypabus(BSTYP_BM) = 32     ! Multi_terminal DC  
      itypabus(BSTYP_BF) = 2      ! Pseudo "BE" becomes "B "  
      itypabus(BSTYP_BJ) = 2      ! Pseudo slack BS becomes "B "  
      itypabus(BSTYP_BK) = 7      ! Pseudo slack BS becomes "BE"  
      itypabus(BSTYP_BL) = 3      ! Pseudo slack BS becomes "BQ"  
   
   
c     set up the bus data records  
c  
c     The BPA bus records are stored in input order with sort keys and  
c     pointers.  Deleted data may be left in the tables and just the  
c     pointers changed to skip the unused data.  
c  
c     The alpha sort order key is kept up to date with holes removed.  
c     By using a loop on BPA's alpha key, the WSCC tables are built in  
c     alpha_numeric order (A8,F4.0) as the input order.  
   
   
      nobus = ntot_alf             ! Number of buses in the system  
      ntoti = nobus                ! Currently, num of buses in Ymat=  
c                                  ! num of buses in system...  
      do ib = 1,nobus  
         nb = alf2inp(ib)  
         iname(ib) = bus(nb)       ! 8 character IPF name  
         bkv(ib) = base(nb)        ! nominal bus kv  
         kt = inp2opt(nb)          ! optimal BPA solution order  
         newnum(kt) = ib           ! BPA optimal order to alpha order  
         vk = dsqrt (e(kt)**2 + f(kt)**2)  
         ea(ib)  = vk              ! magnitude of solution volts (PU)  
         eb(ib)  = datan2(f(kt),e(kt)) ! angle (radians)  solution volts  
   
         if ( kbsdta(1,nb) .ne. BSTYP_BD .and.  
     &        kbsdta(1,nb) .ne. BSTYP_BM) then  
            pg(ib) = pnetu(kt) + ploadu(kt)  
***         qg(ib) = qnetu(kt) + qloadu(kt) - capcor(1,kt) * vk**2  !orig.  
***  With original logic, qg was twice too big.  Each of try1 and try2  
***    produces an approx. correct number, but try2 gives -0.0's and  
***    is more complicated arithmetically, so use try1.  
            qg(ib) = qnetu(kt) + qloadu(kt)                         !try1  
***         qg(ib) = - capcor(1,kt) * vk**2                         !try2  
            pl(ib) = ploadu(kt) + inetr(kt) * vk  
            ql(ib) = qloadu(kt) - ineti(kt) * vk  
            pg(ib) = pg(ib)*100.0  
            qg(ib) = qg(ib)*100.0  
            pl(ib) = pl(ib)*100.0  
            ql(ib) = ql(ib)*100.0  
   
            gsr(ib) = busdta(5,nb)  ! MW shunt  
            bsr(ib) = busdta(6,nb)  ! MVAR shunt  
   
c           Collect any additional customer bus data for this bus.  
            ncb = kbsdta(15,nb)     ! Pointer to first customer record  
            do while (ncb .gt. 0)   ! 0 means end of cust bus for bus nb  
               gsr(ib) = gsr(ib)+ bctbl(4,ncb) ! MW shunt (G in MW)  
               bsr(ib) = bsr(ib)+ bctbl(5,ncb) ! MVAR shunt (B in MVAR)  
               ncb = bctbl_nxt(ncb)  
            enddo  
            gsr(ib) = gsr(ib)/100.0    ! convert to pu  
            bsr(ib) = bsr(ib)/100.0    ! convert to pu  
         else  
            pl(ib) = busdta(6,nb)   ! alpha max (stop) degrees  
            ql(ib) = 0.0  
            pg(ib) = 0.0  
            qg(ib) = 0.0  
            gsr(ib) = busdta(4,nb)  ! ls  ( Henries )  
            bsr(ib) = busdta(5,nb)  ! alpha min for dc bus  
         endif  
c        area number that the bus is located in  
         iz = find_zon(zone(nb))  
         iz = find_zon(zone(nb))  
         if (iz.gt.0) then  
            narea(ib) = acznum(iz)  
         else  
            narea(ib) = 0  
         endif  
         kind(ib,1) = itypabus(kbsdta(1,nb))  
         kind(ib,2) = iz  
      enddo  
   
c     Find all BS (slack) buses and put them in their natural optimal  
c        position, in ktonpi  
   
      ntoti = 0  
      nslk = 0  
      do ib = 1,nobus  
         ktonpi(ib) = 0  
      enddo  
      do ib = 1,nobus  
         if (kind(ib,1) .eq. 26) then  
           nslk = nslk + 1  
           nb = nslkxx(1,nslk)  
           ktonpi(nslkxx(2,nslk)) = inp2alf(nb)  
         endif  
      enddo  
   
c     Fill in the rest of ktonpi from newnum, skipping BS buses,  
c         which are already in the table, and BD buses, which  
c         we do not want in the table.  
   
      ntoti = 0  
      do ib = 1,nobus  
         nb = newnum(ib)  
         if (kind(nb,1) .ne. 20 .and. kind(nb,1) .ne. 26) then  
 100        ntoti = ntoti + 1  
            if (ktonpi(ntoti) .eq. 0) then  
               ktonpi(ntoti) = nb  
            else                   ! Slot already has a BS bus  
               go to 100  
            endif  
         end if  
      enddo  
   
c    Close up any remaining zero entries in ktonpi  
   
      nb = ntoti + 1  
      do ib = nb,nobus  
         if (ktonpi(ib) .ne. 0) then  
            ntoti = ntoti + 1  
            ktonpi(ntoti) = ktonpi(ib)  
         endif  
      enddo  
   
      if (ascii) then  
         write (savfil,'(2I6,A25)') nobus,ntoti,  
     &                          ' * nobus,ntoti *'  
         write (savfil,'(A)') '      bus name bkv     narea  kind1'//  
     &                     ' kind2'  
         write (savfil,'(8X,A)') '       pl            ql       '//  
     &        '     pg            qg           gsr      '//  
     &        '     bsr            ea            eb'  
         do 1000 ib = 1,nobus  
         write (savfil,'(I6,A8,F8.3,3I6)') ib,iname(ib),bkv(ib),  
     &          narea(ib),kind(ib,1),kind(ib,2)  
         write(savfil,'(8X,8E14.7)') pl(ib),ql(ib),  
     &          pg(ib),qg(ib),gsr(ib),bsr(ib),ea(ib),eb(ib)  
1000     continue  
         write (savfil,'(A8,(20I6))') ' ktonpi ',  
     &                                        (ktonpi(ib),ib=1,ntoti)  
      else  
         write (savfil) nobus, ntoti,  
     1      (iname(ib),  ib=1,nobus), ! bus name in alfa order (A8)  
     4      (bkv(ib),    ib=1,nobus), ! base kv in input order  
     5      (pl(ib),     ib=1,nobus), ! Pload MW  in input order  
     6      (ql(ib),     ib=1,nobus), ! Qload MVAR in input order  
     7      (ea(ib),     ib=1,nobus), ! Real part solution voltage e+-jf  
     8      (eb(ib),     ib=1,nobus), ! Imag part solution voltage e+-jf  
     9      (pg(ib),     ib=1,nobus), ! Pgen MW  
     a      (qg(ib),     ib=1,nobus), ! Qgen/Qmax MVAR  
     b      (gsr(ib),    ib=1,nobus), ! Shunt conductance MW  
     c      (bsr(ib),    ib=1,nobus), ! Shunt susceptance MVAR  
     d      (narea(ib),  ib=1,nobus), ! Bus area number  
     e      (kind(ib,1), ib=1,nobus), ! Bus record type number  
     f      (kind(ib,2), ib=1,nobus), ! Bus zone number  
     g      (ktonpi(ib), ib=1,ntoti)  ! index to optimal order  
      endif  
   
********************  
*    Branch Data   *****************************************************  
********************  
   
c     Since we are building the WSCC data in alpha_numeric order as  
c     their input (internal) order, we have to renumber BPA data to  
c     BPA alpha instead of BPA input.  
c  
c     Branch tables may have deleted data holes also, but since the  
c     data is chained to the proper buses with a linked list scheme,  
c     we won't have to worry about that. We will count the branches  
c     as we go.  
c  
c     Loop through the buses in Alpha_numeric order, and loop through  
c     each branch connected to each bus building the WSCC tables as  
c     they are encountered.  Except for building the connection matrix  
c     information, the transposed branches are skipped.  The forward  
c     input direction records are set up in the WSCC tables.  After  
c     the last bus is processed, another pass is made to find the  
c     pointers to the transposed data.  
c  
   
      unused = 0.0  
      nlin  = 0    ! number of single entry branch data  
      nolin = 0    ! number of double entry branch data  
      notra = 0    ! number of regulating transformers  
      do il = 1,MAXBRN2  
         lindx(il) = 0      !initialize lindx so leftovers will not  
      enddo                 ! cause bad data in successive cases  
   
c     Begin bus loop:  
      do ib = 1, nobus  
         nb = alf2inp(ib)  
         p = kbsdta(16,nb)   ! pointer to 1st branch connected to bus nb  
   
c        Begin branch loop  
         do while (p .gt. 0) ! 0 means no more branches for bus nb  
            ltype = brtype(p)  
            if (ltype .eq. BRTYP_PEQ) then  
c              PI Equivalent of series and parallels for YMAT - skip  
            else  
               k1 = kx(p)       ! input order bus 1 number - BPA inp  
               ka = inp2alf(k1) ! alpha order bus 1 number - WSCC inp  
               k2 = ky(p)       ! input order bus 2 number - BPA inp  
               kb = inp2alf(k2) ! alpha order bus 2 number - WSCC inp  
               nolin = nolin+1  ! add to double entry connection matrix  
               linab(nolin,1) = ka    ! from bus  
               linab(nolin,2) = kb    ! to bus  
               q = brnch_ptr(p)  
               if (q .gt. 0) then     ! input order, not a transpose  
                  nlin = nlin+1       ! add to single entry tables  
                  lindx(nolin) = nlin ! store pointer to single tables  
                  lcirc(nlin,1) = index(ialfa,brid(p)) ! parallel code  
                  isect = brsect(p)  
                  if ( isect .eq. 0 ) isect=-1 ! handle blank/zero sect  
                  lcirc(nlin,2) = isect + 2    ! section id index number  
   
                  if (ltype .eq. BRTYP_L ) then  
                     lkind(nlin) = 1               ! itypa code  
                     r = brnch(5,q)  
                     x = brnch(6,q)  
                     denom2 = (r**2 + x**2)  
                     g(nlin) = r/denom2            ! g12  
                     b(nlin) = x/denom2            ! b12  
                     pgc1(nlin) = brnch(7,q)       ! g/2 = g11  
                     pbc1(nlin) = brnch(8,q)       ! b/2 = b11  
                     pgc2(nlin) = brnch(7,q)       ! g/2 = g22  
                     pbc2(nlin) = brnch(8,q)       ! b/2 = b22  
   
                  else if (ltype .eq. BRTYP_E ) then  
                     lkind(nlin) = 10              ! itypa code  
                     r = brnch(5,q)  
                     x = brnch(6,q)  
                     denom2 = (r**2 + x**2)  
                     g(nlin) = r/denom2            ! g12  
                     b(nlin) = x/denom2            ! b12  
                     pgc1(nlin) = brnch(7,q)       ! g11  
                     pbc1(nlin) = brnch(8,q)       ! b11  
                     pgc2(nlin) = brnch(9,q)       ! g22  
                     pbc2(nlin) = brnch(10,q)      ! b22  
   
                  else if (ltype .eq. BRTYP_T ) then  
                     lkind(nlin) = 4               ! itypa code  
                     r = brnch(5,q)  
                     x = brnch(6,q)  
                     denom2 = (r**2 + x**2)  
                     g(nlin) = r/denom2            ! g12  
                     b(nlin) = x/denom2            ! -b12  
                     pgc1(nlin) = brnch(9,q)       ! Tap1  
                     pbc1(nlin) = brnch(10,q)      ! Tap2  
                     pgc2(nlin) = brnch(7,q)       ! 0/1 g tx real chg  
                     pbc2(nlin) = -abs(brnch(8,q)) ! 0/1 b tx react chg  
   
                  else if (ltype .eq. BRTYP_TP) then  
                     lkind(nlin) = 12              ! itypa code  
                     r = brnch(5,q)  
                     x = brnch(6,q)  
                     denom2 = (r**2 + x**2)  
                     g(nlin) = r/denom2            ! g12  
                     b(nlin) = x/denom2            ! -b12  
                     pgc1(nlin) = brnch(9,q)       ! phase shift angle  
                     pbc1(nlin) = 0.0              ! 0.0 for phase shift  
                     pgc2(nlin) = brnch(7,q)       ! 0/1 g tx real chg  
                     pbc2(nlin) = -abs(brnch(8,q)) ! 0/1 b tx react chg  
   
                  else if (ltype .eq. BRTYP_R ) then  
c                    automatic transformer  
c                    set up the automatic regulating xmfr data  
c                    BPA program uses:  
c                        tap(n) = (tap_fixed 0/1)/(tap_variable 0/1)  
c                               = (tapk/basek) / (tapm/basem) where  
c                                 k is fixed side, m is variable side  
   
                     q = iabs (brnch_ptr(p))  
                     write (rsubtyp,'(a1)') brnch(3,q)  
  
c                    look ahead to next branch to get taps  
                     pnxt = brnch_nxt(p)  
                     loc = orienttx (p, pnxt, k1x, k2x, tap1, tap2)  
  
c***                 write (47,*) loc, bus(k1x), base(k1x), bus(k2x),  
c*** &                            base(k2x), tap1, tap2  
  
                     notra = notra + 1  
                     ltra(notra) = inp2alf(k2x) !  variable tap  
                     ltrb(notra) = inp2alf(k1x) !  fixed tap side  
c  
c                    Set up all T records associated with this R   
c                    record as a type 14 regulator; set up all TP   
c                    associated with this R record as a type 15.  
c  
                     found = .true.  
                     do while ( found )  
  
                        if (index (' VQN', rsubtyp) .ne. 0) then  
                           lkind(nlin) = 14         ! itypa code  
                           pgc1(nlin) = tap2        ! variable tap  
                           pbc1(nlin) = tap1        ! fixed Tap  
                           atap(notra) = (tap2/base(k2x))   
     &                                 / (tap1/base(k1x))  
                        else  
                           lkind(nlin) = 15         ! itypa code  
                           pgc1(nlin) = -tap1       ! variable tap  
                           pbc1(nlin) = tap2        ! fixed Tap  
c                          phase shifter brtyp_TP (radians)  
                           atap(notra) = -0.0174532925 * tap1  
                        endif  
                        q = iabs (brnch_ptr(pnxt))  
                        r = brnch(5,q)  
                        x = brnch(6,q)  
                        denom2 = (r**2 + x**2)  
                        g(nlin) = r/denom2       ! g12  
                        b(nlin) = x/denom2       ! -b12  
                        pgc2(nlin) = brnch(7,q)  ! 0/1 g tx real chg  
                        pbc2(nlin) = -abs(brnch(8,q))  ! 0/1 b tx react chg  
   
c                       correcting upper loop "R" card processing      
c                       parallel code  
                        lcirc(nlin,1) = index(ialfa,brid(pnxt))  
                        isect = brsect(pnxt)  
  
c                       handle blank/zero sect  
                        if ( isect .eq. 0 ) isect=-1  
                        lcirc(nlin,2) = isect+2 ! sect id index num.  
   
                        p = pnxt  
                        pnxt = brnch_nxt(pnxt)  
                        if ( pnxt.gt.0 .and. k2.eq.ky(pnxt) ) then  
                           nolin = nolin + 1  
                           linab(nolin,1) = inp2alf(k1)  
                           linab(nolin,2) = inp2alf(k2)  
                           nlin = nlin + 1  
c                                        store pointer to single tables  
                           lindx(nolin) = nlin  
   
                           found = .true.  
                        else  
                           found = .false.  
                        endif  
                     enddo  
c  
                  else if (ltype .eq. BRTYP_LD) then  
                     lkind(nlin) = 21          ! itypa code  
                     g(nlin) = brnch(5,q)      ! Rdc  
                     b(nlin) = brnch(6,q)      ! Ldc  
                     pgc1(nlin) = brnch(7,q)   ! Cdc  
                     pbc1(nlin) = brnch(8,q)   ! Pdc  
                     pgc2(nlin) = brnch(10,q)  ! Alpha N if sec = 1 rect  
c                                              ! Gamma 0 if sec = 2 invt  
                     pbc2(nlin) = brnch(18,q)  ! Gamma 0 if sec = 1 rect  
c                                              ! Alpha N if sec = 2 invt  
   
                  else if (ltype .eq. BRTYP_LM) then  
                     lkind(nlin) = 33          ! itypa: multi-term DC  
                     g(nlin) = brnch(5,q)      ! Rdc  
                     b(nlin) = brnch(6,q)      ! Ldc  
                     pgc1(nlin) = brnch(7,q)   ! Cdc  
                     pbc1(nlin) = unused  
                     pgc2(nlin) = unused  
                     pbc2(nlin) = unused  
c                            sec = 1 for rectifier, sec = 2 for inverter  
   
                  else if (ltype .eq. BRTYP_RZ) then  
                     lkind(nlin) = 34 ! itypa: Series Comp. RANI model  
                     g(nlin) = brnch(5,q)      ! Pmax MW  
                     b(nlin) = brnch(6,q)      ! Pmin MW  
                     pgc1(nlin) = brnch(7,q)   ! Irate Amps  
                     pbc1(nlin) = brnch(8,q)   ! Xij_max 0/1  
                     pgc2(nlin) = brnch(9,q)   ! Xij_min 0/1  
                     pbc2(nlin) = brnch(10,q)  ! Bi_shunt 0/1  
                  endif  
               else  
c                    transpose record  
                  if ( ltype .eq. BRTYP_R ) then  
c  
c                    handle all following T rcds that go with this R  
c                    rcd as if they also are transposed.  
c  
                     p = brnch_nxt(p)    ! skip the R recd  
                     found = .true.  
                     do while ( found )  
                        pnxt = brnch_nxt(p)  
                        if ( pnxt.gt.0 .and. k2.eq.ky(pnxt) ) then  
                           p = pnxt  
                           found = .true.  
                           nolin = nolin + 1  
                           linab(nolin,1) = inp2alf(k1)  
                           linab(nolin,2) = inp2alf(k2)  
                        else  
                           found = .false.  
                        endif  
                     enddo  
                  endif  
               endif  
            endif  
            p = brnch_nxt(p) ! Update pointer to next branch  
         enddo               ! End of branch loop  
      enddo                  ! End of bus loop  
   
c   First pass through the branches is complete.  The connection matrix  
c   has been built containing both the input and transposed order, but  
c   the pointer array LINDX( ) contains only the input order pointers.  
c  
c     We must now loop through LINDX and fill in the missing data.  
   
      do i = 1,nolin  
         if ( lindx(i) .eq. 0 ) then  
            ka = linab(i,1)          ! bus a number of transpose  
            kb = linab(i,2)          ! bus b number of transpose  
c           find branch kb-ka and get its pointer  
            found = .false.  
            j = 0  
            do while (j .lt. nolin .and. .not. found)  
               j = j + 1  
               if (linab(j,1).eq.kb .and. linab(j,2).eq.ka  
     &                                   .and. lindx(j).gt.0) then  
c                 set the pointer to ka-kb = to neg of kb-ka and make a  
c                 temporary mark on this branch as found so we keep  
c                 the sections and parallels straight.  
                  lindx(i) = - lindx(j)  
9898              linab(j,1) = -kb  
                  found = .true.  
                endif  
            enddo  
         endif  
      enddo  
c     undo the temporary mark on the forward branchs since we have found  
c     the matching transposed entries....  
      do i = 1, nolin  
         if (linab(i,1) .lt. 0 ) linab(i,1) = - linab(i,1)  
      enddo  
   
   
   
   
********************  
*    Branch Data   **** Single entry branch tables  ********************  
********************  
   
      if (ascii) then  
         write (savfil,'(A8,I6)') ' nlin   ',nlin  
         write (savfil,'(A)') '  i   lkind  lcirc         g     '//  
     &    '        b             pcg1          pbc1     '//  
     &    '     pgc2          pbc2'  
         do 1100 i = 1,nlin  
         write (savfil,'(I6,3I4,6E14.7)') i,lkind(i),lcirc(i,1),  
     &           lcirc(i,2),g(i),b(i),pgc1(i),pbc1(i),pgc2(i),pbc2(i)  
1100     continue  
      else  
        write (savfil) nlin,  
     1      (g(i),       i=1,nlin),   ! 0/1 g12 conductance  
     2      (b(i),       i=1,nlin),   ! 0/1 b12 susceptance  
     3      (pgc1(i),    i=1,nlin),   ! 0/1 g11, tap1, phase shift angle  
     4      (pbc1(i),    i=1,nlin),   ! 0/1 b11, tap2, 0  
     5      (pgc2(i),    i=1,nlin),   ! 0/1 g22  
     6      (pbc2(i),    i=1,nlin),   ! 0/1 b22  
     7      (lkind(i),   i=1,nlin),   ! branch type code  
     8      (lcirc(i,1), i=1,nlin),   ! branch circuit ID code  
     9      (lcirc(i,2), i=1,nlin)    ! branch sectin num.  
      endif  
   
   
********************  
*    Branch Data   **** Double entry branch tables  ********************  
********************  
   
      if (ascii) then  
         write(savfil,'(A8,I6)') ' nolin  ',nolin  
         write(savfil,'(A8,(20I6))')' linab1 ',(linab(i,1),i=1,nolin)  
         write(savfil,'(A8,(20I6))')' linab2 ',(linab(i,2),i=1,nolin)  
         write(savfil,'(A8,(20I6))')' lindx  ',(lindx(i),  i=1,nolin)  
      else  
         write (savfil) nolin,  
     1      (linab(i,1), i=1,nolin),  ! internal from bus num  
     2      (linab(i,2), i=1,nolin),  ! internal to bus num  
     3      (lindx(i),   i=1,nolin)   ! pointer to single entry data  
c                                     ! tables. +ptr = input direction  
c                                     !         -ptr = transpose direct.  
      endif  
   
   
   
   
********************  
*    Branch Data   **** Regulating Transformer data ********************  
********************  
   
      if (ascii) then  
         write(savfil,'(A8,I6)') ' notra  ',notra  
         write(savfil,'(A8,(20I6))')' ltra   ',(ltra(i), i=1,notra)  
         write(savfil,'(A8,(20I6))')' ltrb   ',(ltrb(i), i=1,notra)  
         write(savfil,'(A8,(8E14.7))')' tap    ',(atap(i),i=1,notra)  
      else  
         write (savfil) notra,  
     1      (ltra(i), i=1,notra),     ! int bus num of variable tap side  
     2      (ltrb(i), i=1,notra),     ! int bus num of fixed tap side  
     3      (atap(i), i=1,notra)      ! final 0/1 tap or p_ang in rad  
      endif  
   
   
   
   
********************  
*    DC Data        **** Two Terminal DC *******************************  
********************  
   
c     DC data in BPA's program has the bus numbers in alfa order already  
c     For the 2 terminal dc model in BPA's program, each dc line has all  
c     the rectifier and inverter info stored in the same row with the  
c     line data.  
   
   
      pi = 4.0 * atan(1.0)  
   
      n2dcbus = 0  
      do n2dclin = 1, kdtot  
   
c        dc line tables  
         nr = dc2t(1,n2dclin)               ! rect. bus num  
         nra = inp2alf(nr)                  ! alpha order rect. bus num  
         idcex(n2dclin,1) = nra  
         ni = dc2t(3,n2dclin)               ! invt. bus num  
         nia = inp2alf(ni)                  ! alpha order invt. bus num  
         idcex(n2dclin,2) = nia  
         tlohm(n2dclin)   =  dc2t(8,n2dclin) ! DC line resistance - ohms  
         curpp(n2dclin)   =  dc2t(25,n2dclin) ! gamma_min - radians  
         curnt(n2dclin)   =  dc2t(39,n2dclin)/1000.0 ! dc cur.: kiloamps  
   
c        dc bus tables - rectifier end  
         n2dcbus = n2dcbus + 1  
         idcint(n2dclin,1) = n2dcbus  ! index of rect in dc bus tables  
         idcbus(n2dcbus)   = nra  
         dcbase(n2dcbus)   = dc2t(2, n2dclin) ! dc BKV*numbrdgs at rect  
         dcbase(n2dcbus)   = dcbase(n2dcbus) * dc2t(12,n2dclin)  
         vdrop(n2dcbus)    = dc2t(13,n2dclin) ! kv across rect. valves  
         dcirat(n2dcbus)   = dc2t(14,n2dclin)/1000.0 ! rect rate Kamps  
         alfmin(n2dcbus)   = dc2t(18,n2dclin) ! alfa min valve 1  
         firang(n2dcbus)   = dc2t(22,n2dclin) ! alfa fireing ang - rect  
         rohmdc(n2dcbus)   = dc2t(29,n2dclin) ! rect comm. tx R ohms  
         rohmdc(n2dcbus)   = rohmdc(n2dcbus)*dc2t(12,n2dclin)*18.0/pi**2  
         xohmdc(n2dcbus)   = dc2t(30,n2dclin) ! rect comm. tx X ohms  
         xohmdc(n2dcbus)   = xohmdc(n2dcbus) * dc2t(12,n2dclin)  
         nrc               = dc2t(33,n2dclin)  !com bus num  
         newdc1(n2dcbus)   = inp2alf(nrc)       !com bus num alpha  
         tr(n2dcbus)       = tap(ifix(sngl(dc2t(37,n2dclin))))  
C                                               ! tap ratio rect tx  
         dckv(n2dcbus)     = dc2t(40,n2dclin) ! computed rect dc kv  
         dcmw(n2dcbus)     = dckv(n2dcbus) * curnt(n2dclin) ! kv*kamps  
         call calcflow(nrc,nr,'*',pin,qin,pout,qout,error)  
         pgen(n2dcbus)     = pin              ! rect. pload on AC (MW)  
         qgen(n2dcbus)     = qin              ! rect. qload on AC (MVAR)  
   
c        dc bus tables - inverter end  
         n2dcbus = n2dcbus + 1  
         idcint(n2dclin,2) = n2dcbus  ! index of invt in dc bus tables  
         idcbus(n2dcbus)   = nia  
         dcbase(n2dcbus)   = dc2t(4, n2dclin) ! dc BKV * numbrdg invt  
         dcbase(n2dcbus)   = dcbase(n2dcbus) * dc2t(15,n2dclin)  
         vdrop(n2dcbus)   = - dc2t(16,n2dclin) !- kv across invt. valves  
         dcirat(n2dcbus)   = dc2t(17,n2dclin)/1000.0 ! invt rate : Kamps  
         alfmin(n2dcbus)   = dc2t(23,n2dclin) ! alfa min valve 2  
         firang(n2dcbus)   = dc2t(26,n2dclin) ! gamma extnct ang - inv  
         rohmdc(n2dcbus)   = dc2t(31,n2dclin) ! invt comm. tx R  
         rohmdc(n2dcbus)  = rohmdc(n2dcbus)*dc2t(15,n2dclin)*18.0/pi**2  
         xohmdc(n2dcbus)   = dc2t(32,n2dclin) ! invt comm. tx X ohms  
         xohmdc(n2dcbus)   = xohmdc(n2dcbus) * dc2t(15,n2dclin)  
         nic               = dc2t(34,n2dclin)  !com bus num  
         newdc1(n2dcbus) = inp2alf(nic)         !com bus num alpha  
         tr(n2dcbus)       = tap(ifix(sngl(dc2t(38,n2dclin))))  
C                                               ! tap ratio invt tx  
         dckv(n2dcbus)     = dc2t(41,n2dclin) ! computed invt dc kv  
         dcmw(n2dcbus)     = dckv(n2dcbus) * curnt(n2dclin) ! kv*kamps  
         call calcflow(nic,ni,'*',pin,qin,pout,qout,error)  
         pgen(n2dcbus)     = -pin   ! invt. pload on AC (MW dc to ac)  
***  WSCC Stability wants this to always be positive  
         qgen(n2dcbus)     = abs(qin)   ! invt. qload on AC (MVAR dc to ac)  
   
      enddo  
      n2dclin = kdtot  
   
      if (ascii) then  
         write (savfil,'(A8,I4))') ' ndclin ',n2dclin  
         write (savfil,'(A8,(20I6))') ' idcex1 ',  
     &                                  (idcex(id,1),id=1,n2dclin)  
         write (savfil,'(A8,(20I6))') ' idcex2 ',  
     &                                  (idcex(id,2),id=1,n2dclin)  
         write (savfil,'(A8,(8E14.7))') ' tlohm  ',  
     &                                  (tlohm(id),id=1,n2dclin)  
         write (savfil,'(A8,(8E14.7))') ' curpp  ',  
     &                                  (curpp(id),id=1,n2dclin)  
         write (savfil,'(A8,(20I6))') 'idcint1 ',  
     &                                  (idcint(id,1),id=1,n2dclin)  
         write (savfil,'(A8,(20I6))') 'idcint2 ',  
     &                                  (idcint(id,2),id=1,n2dclin)  
         write (savfil,'(A8,(8E14.7))') ' curnt  ',  
     &                                  (curnt(id),id=1,n2dclin)  
      else  
         write (savfil) n2dclin,                 ! # of 2_term DC lines  
     1              (idcex(id,1),id=1,n2dclin),  ! rect bus num  
     2              (idcex(id,2),id=1,n2dclin),  ! invt bus num  
     3              (tlohm(id),  id=1,n2dclin),  ! dc line R (ohms)  
     4              (curpp(id),  id=1,n2dclin),  ! gamma_min (radians)  
     5              (idcint(id,1),id=1,n2dclin), ! rect index to bus tbl  
     6              (idcint(id,2),id=1,n2dclin), ! invt index to bus tbl  
     7              (curnt(id),  id=1,n2dclin)   ! solved Idc (Kamps)  
      endif  
   
      if (ascii) then  
         write (savfil,'(A8,I4))') ' ndcbus ',n2dcbus  
         write (savfil,'(A8,(20I6))') ' idcbus ',  
     &                               (idcbus(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' dckv   ',  
     &                               (dckv(id),   id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' dcmw   ',  
     &                               (dcmw(id),   id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' vdrop  ',  
     &                               (vdrop(id),  id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' rohmdc ',  
     &                               (rohmdc(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' xohmdc ',  
     &                               (xohmdc(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' firang ',  
     &                               (firang(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' alfmin ',  
     &                               (alfmin(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' tr     ',  
     &                               (tr(id),     id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' dcbase ',  
     &                               (dcbase(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' pgen   ',  
     &                               (pgen(id),   id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' qgen   ',  
     &                               (qgen(id),   id=1,n2dcbus)  
         write (savfil,'(A8,(20I6))') ' newdc1 ',  
     &                               (newdc1(id), id=1,n2dcbus)  
         write (savfil,'(A8,(8E14.7))') ' dcirat ',  
     &                               (dcirat(id), id=1,n2dcbus)  
   
      else  
         write (savfil) n2dcbus,                 ! # of 2_term DC buses  
     1              (idcbus(id), id=1,n2dcbus),  ! dc bus number  
     2              (dckv(id),   id=1,n2dcbus),  ! dc kv valve side  
     3              (dcmw(id),   id=1,n2dcbus),  ! dc pflow valve side  
     4              (vdrop(id),  id=1,n2dcbus),  ! kv drop across valve  
     5              (rohmdc(id), id=1,n2dcbus),  ! Req ohms reflected dc  
     6              (xohmdc(id), id=1,n2dcbus),  ! Xeq ohms  
     7              (firang(id), id=1,n2dcbus),  ! solvd conv fire angle  
     8              (alfmin(id), id=1,n2dcbus),  ! rect op. Alfa_min  
     9              (tr(id),     id=1,n2dcbus),  ! comm. tx. tap ratio  
     a              (dcbase(id), id=1,n2dcbus),  ! dcvolt * num_bridges  
     b              (pgen(id),  id=1,n2dcbus), ! net ac load mw of dc  
     c              (qgen(id),  id=1,n2dcbus), ! net ac load mvar of dc  
     d              (newdc1(id), id=1,n2dcbus),  ! comm bus internal num  
     e              (dcirat(id), id=1,n2dcbus)   ! valve Kamp rating  
      endif  
   
   
      savewscc = 0                 !   status ok, file written  
      return  
      end  
   
   
