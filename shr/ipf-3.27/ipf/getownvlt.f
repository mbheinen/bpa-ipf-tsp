C    @(#)getownvlt.f	20.3 2/13/96
	integer function getownvlt (own, basekv)
        character own *(*)
c
c       This function returns the owner-voltage index LOV to OVLOS for
c       the given OWN and BASEKV.
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
        include 'ipfinc/anlys.inc'
c	Global variables used:
c		lowner, avolt
        include 'ipfinc/busanl.inc'
c	Global variables used:
c		kov, novls
        include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
        include 'ipfinc/ownhash.inc'
c	Global variables used:
c		None
  
        integer bldowner, bldbsekv

        io = iabs (bldowner(own))
        if (io .eq. 0) then
           write (errbuf(1), 180) MAXOWN
  180      format('0 Overflow of "OWNER" array - limit =', i4)
           call prterx ('w',1)
        else
           lowner(io) = own
        endif

        iv = iabs (bldbsekv(basekv))
        if (iv .eq. 0) then
           write (errbuf(1), 190) MAXVLT
  190      format('0 overflow of "AVOLT" array - limit = ' ,i4)
           call prterx ('w',1)
        else
           avolt(iv) = basekv
        endif

        lov = kov(iv,io)
        if (lov .eq. 0) then
           novls = novls + 1
           if (novls .eq. MAXOVL+1) then
              write (errbuf(1), 200) MAXOVL
  200         format('0 overflow of "OWNER-VOLTAGE" loss array',
     1           ' limit = ', i4)
              call prterx ('w',1)
           else if (novls .le. MAXOVL) then
              kov(iv,io) = novls
              lov = novls
           else
           endif
        endif                                                                                
        getownvlt = lov
        return
        end
