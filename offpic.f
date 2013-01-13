	FUNCTION OFFPIC(A, IX, IY, NFAST, NSLOW, DX, DY)
        include 'TUNEABLE'
	DIMENSION A(NPMAX)
	LOGICAL OFFPIC, NOGOOD
C:
C:	COMPUTES DISTANCES BEYOND EDGE OF PICTURE
C:
c
c  Changed indices.
c
	X = IX + A(3)
	Y = IY + A(4)
c
	IF (X .LT. 0) THEN
	  DX = -X
	ELSE IF (X .GT. NFAST) THEN
	  DX = X - NFAST
	ELSE
	  DX = 0
	END IF
	IF (Y .LT. 0) THEN
	  DY = -Y
	ELSE IF (Y .GT. NSLOW) THEN
	  DY = Y - NSLOW
	ELSE
	  DY = 0
	END IF
	NOGOOD = (DX .NE. 0) .OR. (DY .NE. 0)
c
        if(.not.fixpos)	NOGOOD = NOGOOD .OR. (A(2) .LT. 0)
        NOGOOD = NOGOOD .OR. (ABS(A(3)) .GT. IRECT(1)/2.)
        NOGOOD = NOGOOD .OR. (ABS(A(4)) .GT. IRECT(2)/2.)
c
	OFFPIC = NOGOOD
	RETURN
	END
	
