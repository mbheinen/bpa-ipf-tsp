C    @(#)l_redchg.f	20.3 2/13/96
C****************************************************************
C
C   	File: l_redchg.f
C
C   	Purpose: 1. Reduce the following record types:
C                    "E", "L", "LD", "LM", "R", "RZ", "T"
C                2. Convert one of the change records into an
C                   equivalent record.
C                3. Delete all other change records by setting
C                   an error flag.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: redchgs.f
C
C****************************************************************
C
      	subroutine l_redchg (nx, nxdx)
        integer nx, nxdx(*)
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/changr.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'

        common /scratch/ array(MAXCHG)
        integer array

        external get_rule
        integer fieldptr(6), field(2,80), get_rule

        data fieldptr / 1, 18, 32, 42, 52, 63 /

        data field /
c
c           Line type E
c
     &       4,  6, 19, 19, 34, 37, 38, 38, 39, 44, 45, 50, 51, 56, 
     &      57, 62, 63, 68, 69, 74, 75, 75, 76, 77, 78, 78, 79, 80, 
     &      81, 84, 85, 88,  0,  0,
c
c           Line type LD
c
     &       4,  6, 34, 37, 38, 43, 44, 49, 50, 55, 56, 56, 57, 61,
     &      62, 66, 67, 70, 71, 73, 74, 78, 81, 84, 85, 88,  0,  0,
c
c           Line type LM
c
     &       4,  6, 34, 37, 38, 43, 44, 49, 50, 55, 71, 73, 74, 78, 
     &      81, 84, 85, 88,  0,  0,
c
c           Regulator type R
c
     &       4,  6, 19, 19, 34, 41, 42, 45, 46, 50, 51, 55, 56, 57, 
     &      58, 62, 63, 67,  0,  0,
c
c           Regulator type RZ
c
     &       4,  6, 19, 19, 34, 34, 35, 39, 40, 44, 45, 48, 49, 54, 
     &      55, 60, 61, 66, 67, 72,  0,  0,
c
c           Transformer type T or phase shifter type TP
c
     &       4,  6, 19, 19, 34, 37, 38, 38, 39, 44, 45, 50, 51, 56, 
     &      57, 62, 63, 67, 68, 72, 75, 75, 76, 77, 78, 78, 79, 80, 
     &      81, 84, 85, 88, 89, 92,  0,  0 /

        ic = array(nxdx(1))

        ictype = index ('EL$$R$T', oldchg(ic)(1:1))
        if (oldchg(ic)(1:2) .eq. 'LD') ictype = ictype + 1
        if (oldchg(ic)(1:2) .eq. 'LM') ictype = ictype + 2
        if (oldchg(ic)(1:2) .eq. 'RZ') ictype = ictype + 1
c
c       "get_rule" obtains the basic change reduction rule:
c
c       1. Del(ic) + Add(jc) -> Mod(ic)
c       2. Del(ic) + Res(jc) -> NULL
c       3. Add(ic) + Mod(jc) -> Add(ic)
c       4. Add(ic) + Del(jc) -> NULL
c       5. Mod(ic) + Mod(jc) -> Mod(ic)
c       6. Mod(ic) + Del(jc) -> Del
c
        num_rule = get_rule(nx, nxdx, ic, jc)
        do while (num_rule .gt. 0)
           if (num_rule .eq. 1 .or. num_rule .eq. 3 .or. 
     &         num_rule .eq. 5) then
c
c             1. Reduce oldchg(jc) into oldchg(ic)
c             2. Delete oldchg(jc)
c             3. Update change codes if necessary.
c
              i = fieldptr(ictype)
              do while (field(1,i) .gt. 0)
                 i1 = field(1,i)
                 i2 = field(2,i)

                 if (oldchg(jc)(i1:i2) .ne. ' ') then
                    oldchg(ic)(i1:i2) = oldchg(jc)(i1:i2)
                 endif
                 i = i + 1
              enddo
c
c             Delete oldchg(jc) by setting error flag
c
              oldchg(jc)(126:126) = 'E'
              if (num_rule .eq. 1) then
                 oldchg(ic)(3:3) = 'M'
              endif
c
c             If oldchg(jc) is a pseudo change, overwrite with
c             code for oldchg(jc).  This assures that explicit changes
c             performed on pseudo changes will emerge as explicit
c             changes.
c
              if (oldchg(ic)(121:121) .eq. 'P') then
                  oldchg(ic)(121:121) = oldchg(jc)(121:121)
              endif

           else if (num_rule .eq. 2 .or. num_rule .eq. 4) then
c
c             1. Delete oldchg(ic) and oldchg(jc) by setting error 
c                flag
c
              oldchg(ic)(126:126) = 'E'
              oldchg(jc)(126:126) = 'E'

           else if (num_rule .eq. 6) then
c
c             1. Delete oldchg(ic) by setting error flag
c
              oldchg(ic)(126:126) = 'E'
c
c             If oldchg(jc) is a pseudo change, overwrite with
c             code for oldchg(jc).  This assures that explicit changes
c             performed on pseudo changes will emerge as explicit
c             changes.
c
              if (oldchg(ic)(121:121) .eq. 'P') then
                  oldchg(ic)(121:121) = oldchg(jc)(121:121)
              endif

           endif
           num_rule = get_rule(nx, nxdx, ic, jc)
        enddo                    
        return
        end
