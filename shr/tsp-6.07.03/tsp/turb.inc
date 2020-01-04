c     -- start of turb.inc
c     turbdat contains the turbine data.  See turbinp for map.
c     turbtype contains the turbine type by index: 1=TA, 2=TB, 3=TC, 4=TD
c                                                  5=TE, 6=TF, 7=TG, 8=TW
      real turbdat(MAXGEN*TURBSLOTS)
      integer turbtyp(MAXGEN), turbsource(MAXGEN), turbindx(MAXGEN),
     &        turboffset
      common /turbine/ turbdat, turbtyp, turbsource, turbindx,
     &                 turboffset
c    -- end of turb.inc
