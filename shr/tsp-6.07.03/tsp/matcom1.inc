
      integer ROW_SIZE
      parameter (ROW_SIZE = 50)
      common /matrow/ ii, matrow(3*ROW_SIZE+3),
     &                inn, idell, idelh, locl(ROW_SIZE), 
     &                gil(ROW_SIZE), bil(ROW_SIZE), loch(ROW_SIZE), 
     &                gih(ROW_SIZE), bih(ROW_SIZE)
      dimension atrow(3*ROW_SIZE+3)
      equivalence (matrow,atrow)

