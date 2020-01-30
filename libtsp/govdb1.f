C    %W% %G%
      subroutine govdb1(db, ep, xp, x, yp, y)
     
      implicit none

      real db, ep, xp, x, yp, y

      y = x + sign(1.0,x)*(ep-db)

      if (x.gt.-db.and.x.lt.db) then
        if (x.le.0.0) then
          if ((y.gt.0.0).or.(xp.gt.ep-db).or.(yp.eq.0.0)) y = 0.0
        else if (x.gt.0.0) then
          if ((y.lt.0.0).or.(xp.lt.db-ep).or.(yp.eq.0.0)) y = 0.0
        end if
      endif
      
      return
      end 
