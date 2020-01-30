C    @(#)b_redchg.f	20.3 2/13/96
C****************************************************************
C
C   	File: b_redchg.f
C
C   	Purpose: 1. Reduce the following record types:
C                    "B", "+", "X".
C                2. Convert one of the change records into an
C                   equilalent record.
C                3. Delete all other change records by setting
C                   an error flag.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: redchgs.f
C
C****************************************************************
C
      	subroutine b_redchg (nx, nxdx)
        integer nx, nxdx(*)
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/changr.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'

        common /scratch/ array(MAXCHG)
        integer array

        integer fieldptr(5), field(2,78), get_rule
        external get_rule

        data fieldptr / 1, 18, 30, 47, 59 /

        data field /
c
c           Bus types B , BE, BS, BC, BV, BQ, BG, BT, BX, BF
c
     &       2,  2,  4,  6, 19, 20, 21, 25, 26, 30, 31, 34, 35, 38,
     &      39, 42, 43, 47, 48, 52, 53, 57, 58, 61, 62, 65, 66, 73,
     &      74, 77, 78, 80,  0,  0,
c
c           Bus type BD
c
     &       2,  2,  4,  6, 19, 20, 24, 25, 26, 30, 31, 35, 36, 40,
     &      41, 45, 46, 50, 51, 58, 59, 62,  0,  0,
c
c           Bus type BM
c
     &       2,  2,  4,  6, 19, 20, 24, 25, 26, 30, 31, 35, 36, 40,
     &      41, 45, 46, 50, 51, 58, 59, 62, 63, 63, 64, 66, 67, 69, 
     &      70, 75, 76, 80,  0,  0,
c
c           Conintuation bus "+"
c
     &       2,  2,  4,  6, 19, 20, 21, 25, 26, 30, 31, 34, 35, 38,
     &      39, 42, 43, 47, 48, 52, 53, 57,  0,  0, 
c
c           X bus
c
     &       4,  6, 21, 28, 29, 32, 33, 33, 34, 38, 39, 39, 40, 44,
     &      45, 45, 46, 50, 51, 51, 52, 56, 57, 57, 58, 62, 63, 63, 
     &      64, 68, 69, 69, 70, 74, 75, 75, 76, 80,  0,  0/ 

        ic = array(nxdx(1))

        ictype = index ('B$$+X', oldchg(ic)(1:1))
        if (oldchg(ic)(1:2) .eq. 'BD') ictype = ictype + 1
        if (oldchg(ic)(1:2) .eq. 'BM') ictype = ictype + 2
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
              if (oldchg(jc)(121:121) .eq. 'P') then
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
              if (oldchg(jc)(121:121) .eq. 'P') then
                  oldchg(jc)(121:121) = oldchg(ic)(121:121)
              endif

           endif
           num_rule = get_rule(nx, nxdx, ic, jc)
        enddo                    
        return
        end
