C    %W% %G%
      subroutine govdb2(db, xp, x, yp, y)
     
      implicit none

      real db, xp, x, yp, y

      if (abs(yp+db-xp).lt.1.0e-6) then
c       past point was on upgoing line
        if (x.ge.xp) then
c         still going up
          y = x - db
        else
c         direction has reversed.  x is going down.
          if (xp-x.lt.2*db) then
c           in deadband
            y = yp
          else
c           past deadband.  Now on downgoing path
            y = x + db
          endif
        endif
      else if (abs(xp+db-yp).lt.1.0e-6) then
c       past point was on downgoing path
        if (x.le.xp) then
c         still going down
          y = x + db
        else
c         direction has reversed.  x is going up.
          if (x-xp.lt.2*db) then
c           in deadband
            y = yp
          else
c           past deadband.  Now on upgoing path
            y = x - db
          endif
        endif
      else
c       past point was in deadband
        if (x.ge.yp+db) then
c         on upgoing path
          y = x - db
        else if (x.le.yp-db) then
c         on downgoing path
          y = x + db
        else
c         still in deadband
          y = yp
        endif
      endif
      
      return
      end 
