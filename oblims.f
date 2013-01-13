	SUBROUTINE OBLIMS(STARPAR, JRECT)
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C: PSEUDOGAUSSIAN EXP(-T**2) = 1/(1 + T**2 + T**4/2 + T**6/6)
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	INCLUDE 'TUNEABLE'
c        DIMENSION STARPAR(1)
	DIMENSION STARPAR(7)
	INTEGER*2 JRECT(4)
	IF (STARPAR(2) .GT. 0) THEN
	  TEMP = (STARPAR(2)/NPHOB*6)**0.33333333	
	  IF (STARPAR(5) .GT. 0) THEN		
	    FUDGEX = SQRT(TEMP*STARPAR(5)*2)		
	  ELSE						
	    FUDGEX = 10
	  END IF
	  IF (STARPAR(7) .GT. 0) THEN		
	    FUDGEY = SQRT(TEMP*STARPAR(7)*2)
	  ELSE
	    FUDGEY = 10
	  END IF
	ELSE
	  FUDGEX = 10
	  FUDGEY = 10
	END IF
	JRECT(1) = STARPAR(3) - FUDGEX
	JRECT(2) = STARPAR(3) + FUDGEX
	JRECT(3) = STARPAR(4) - FUDGEY
	JRECT(4) = STARPAR(4) + FUDGEY
	RETURN
	END
