C    @(#)ipf_intfce.f	20.4 11/11/97
	subroutine ipf_intfce (string, value)
        character string *(*)
        integer value

C       This subroutine provides an single but limited variable 
c       interface to IPF without recourse to IPF common data blocks.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/mrgtxt.inc'
        include 'ipfinc/optim.inc'
        include 'ipfinc/pageno.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/topbot.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/zonlst.inc'

        integer ix
        real     x
        equivalence (x, ix)

        if (string .eq. 'GET_DATAI') then
           datai = value
        else if (string .eq. 'PUT_DATAI') then
           value = datai
        endif
        return
        end
