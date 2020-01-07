c
        integer MAXN, MXSIZE
        parameter (MAXN = 500)
        parameter (MXSIZE = 250)

        common /smallp/ bound(MAXN), c(MAXN), piv(MAXN),
     1                  x(MAXN), yac(MAXN), b(MAXM), s(MAXM), g(MAXM),
     2                  slack(MAXM), y(MAXM), inv(MAXM,MAXM), xr(MAXM),
     3                  yr(MAXM), gr(MAXM), big, driver, obj, r, small,
     4                  yaminc, xkpos, howneg, 
     5                  inbase(MAXN), iseff(MAXM), xbasis(MAXM), 
     6                  ybasis(MAXM), tol(8), inrev, ir, irmax, isbig, 
     7                  isbnd, isdone, istate, itr, itrmax, m, marki, 
     8                  markk, mnow, more, morepr, n, neginv, negrow, 
     9                  newx, newy, numslk, size, size1

 
        double precision bound, c, piv, x, yac, b, s, g, slack, y,
     1                   inv, xr, yr, gr, big, driver, obj, r,
     2                   small, yaminc, xkpos, howneg
c
        integer size,size1,xbasis,ybasis
        integer inbase, iseff, inrev, ir, irmax, isbig, isbnd,
     1          isdone, istate, itr, itrmax, m, marki, markk,
     2                  mnow, more, morepr, n, neginv, negrow, 
     3                  newx, newy, numslk
        real    tol

