C    @(#)gtchgtyp.f	20.3 2/13/96
C****************************************************************
C
C       File: gtchgtyp.f
C
C       Purpose: Returns an integer denoting all possible change
C                records.
C
C       Author: Walt Powell            Date: 13 November 1992
C       Called by: redchgs.f
C
C****************************************************************
C
        integer function gtchgtyp (type)
        character *(*) type

        gtchgtyp = index ('/.DPZAIB+XQEL$$R$T', type(1:1))
        if (type(1:2) .eq. 'RZ') gtchgtyp = gtchgtyp + 1
        if (type(1:2) .eq. 'LD') gtchgtyp = gtchgtyp + 1
        if (type(1:2) .eq. 'LM') gtchgtyp = gtchgtyp + 2

        return
        end
