C    @(#)geta.f	20.3 2/13/96
      double precision function geta(i,j)
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
      include 'ipfinc/smallp.inc'
 
 
        istart = irow(i)
        last = irow(i+1) - 1
        geta = 0.0
        do 10 look = istart,last
           jhere = jcol(look)
           if (jhere .eq. j) then
              geta = aa(look)
              go to 20
           else if (jhere .gt. j) then
              go to 20
           endif
   10   continue

   20   continue
        return
        end
