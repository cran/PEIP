      subroutine aprod ( mode, m, n, x, y, leniw, lenrw, iw, rw )

      implicit           double precision (a-h,o-z)
      integer            mode, m, n, leniw, lenrw
      integer            iw(leniw)
      double precision   x(n), y(m), rw(lenrw)

*----------------------------------------------------------------------
*     APROD performs the following functions:
*
*     If mode = 1, set y = y + A*x
*     If mode = 2, set x = x + A(transpose)*y
*
*     where A is a matrix stored by rows in the arrays RA, JA, NA.
*     The workspace array RW contains RA. The first M components of IW
*     contain NA, and the remainder of IW contains JA.
*----------------------------------------------------------------------

*     local variables
      integer            lenja, lenra, locja

      locja = m + 1
      lenja = leniw - locja + 1
      lenra = lenrw
  100 continue

      call aprod1( mode, m, n, x, y, lenja, lenra, iw,iw(locja),rw) 
      return

*     end of aprod
      end




      subroutine aprod1 ( mode, m, n, x, y, lenja, lenra, na, ja, ra)

      integer           mode, m, n, lenja, lenra
      integer           na(m), ja(lenja)
      double precision  x(n), y(m), ra(lenra)

      integer           i,j,l,l1,l2
      real              sum,yi,zero

      zero = 0.0
      l2 = 0
      if ( mode .ne. 1 ) go to 400

*-----------------------------------------------------------------------
*     mode = 1 -- set y = y + A*x
*-----------------------------------------------------------------------

      do 200 i = 1, m
         sum = zero
         l1 = l2 + 1
         l2 = l2 + na(i)
         do 100 l = l1, l2
            j = ja(l)
            sum = sum + ra(l)*x(j)
  100    continue
         y(i) = y(i) + sum
  200 continue
      return

*-----------------------------------------------------------------------
*     mode = 2 -- set x = x + A(transpose)*y
*-----------------------------------------------------------------------

  400 do 600 i = 1, m
         yi = y(i)
         l1 = l2 + 1
         l2 = l2 + na(i)
         do 500 l = l1, l2
            j = ja(l)
            x(j) = x(j) + ra(l)*yi
  500    continue
  600 continue
      return

*     end of aprod1
      end


















