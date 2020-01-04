C    @(#)out_cbs.f	20.7 7/18/96
C****************************************************************
C
C   File: out_cbs.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM
C            data
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
        subroutine out_cbs (ncb, datarec)

        parameter (MAXDATAREC = 258)
        character datarec * (MAXDATAREC)
c
c       This subroutine returns WSCC-formated output data records.
c       Output parameter:
c
c       datarec - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/ordsta.inc'

        character kode2 * 1, kodeyr * 2, kowner * 3

        bequiv = 0
        gequiv = 0
        bfixed = 0

        numerrcnt = errcnt(2)
        call bcdcbs (ncb, datarec)
c
c       Disable error count if only input record template is requested
c
        if (errcnt(2) .gt. 0 .and. numerrcnt .eq. 0) errcnt(2) = 0
        
        k1 = kbctbl(1,ncb)
        if (ordvlt .eq. 1) then
           kt = k1
        else
           kt = inp2opt(k1)
        endif
        voltpu = dsqrt (e(kt) ** 2 + f(kt) ** 2)
        pload2=bctbl(2,ncb)
        qload2=bctbl(3,ncb)
        skcon2=bctbl(4,ncb)
        sksus2=bctbl(5,ncb)
        pgen2=bctbl(6,ncb)
        qgen2=bctbl(11,ncb)
        call getchr(1,kode2,kbctbl(8,ncb))
        call getchr(2,kodeyr,kbctbl(9,ncb))
        call getchr(3,kowner,kbctbl(10,ncb))
        skcon2=skcon2*voltpu**2
        sksus2=sksus2*voltpu**2
C
C       Convert constant current and constant admittance loads back to
C       constant MVA.
C
        if (kode2 .eq. 'A') then
           if (kodeyr .eq. '01' .or. kodeyr .eq. '*I') then
              pload2 = pload2*voltpu
              qload2 = qload2*voltpu
              if (kowner .eq. '***') then
                 gequiv = gequiv + skcon2
                 bequiv = bequiv + sksus2
                 skcon2 = 0.0
                 sksus2 = 0.0
              else
                 pload2 = pload2 + skcon2
                 qload2 = qload2 - sksus2
                 skcon2 = 0.0
                 sksus2 = 0.0
              endif
           else if (kodeyr .eq. '02' .or. kodeyr .eq. '*P') then
              if (kowner .eq. '***') then
                 gequiv = gequiv + skcon2
                 bequiv = bequiv + sksus2
                 skcon2 = 0.0
                 sksus2 = 0.0
              else
                 pload2 = pload2 + skcon2
                 qload2 = qload2 - sksus2
                 skcon2 = 0.0
                 sksus2 = 0.0
              endif
           else
              bfixed = bfixed + sksus2
              sksus2 = 0.0
           endif
        else if (kodeyr .eq. '*I') then
           pload2 = pload2*voltpu
           qload2 = qload2*voltpu
           if (kowner .eq. '***') then
              gequiv = gequiv + skcon2
              bequiv = bequiv + sksus2
              skcon2 = 0.0
              sksus2 = 0.0
           else
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
           endif
        else if (kodeyr .eq. '*P') then
           if (kowner .eq. '***') then
              gequiv = gequiv + skcon2
              bequiv = bequiv + sksus2
              skcon2 = 0.0
              sksus2 = 0.0
           else
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
           endif
        endif
        write( datarec(21:), 160 )  pgen2, qgen2, pload2, qload2, 
     1     skcon2, sksus2
  160   format(6e15.7)
        return
        end 
