	SUBROUTINE OBLIMS(STARPAR, JRECT)
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C: PSEUDOGAUSSIAN EXP(-T**2) = 1/(1 + T**2 + T**4/2 + T**6/6 + T**8/24)
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	INCLUDE 'TUNEABLE'
	common/HST/BETA8
	DIMENSION STARPAR(npmax)
	INTEGER*2 JRECT(4)
	IF (STARPAR(2) .GT. 0) THEN
c... Begin Beta8 modification:
          if(BETA8 .gt. 0) then
             TEMP = (24.*STARPAR(2)/NPHOB/BETA8)**0.25000000
          else if(BETA6 .gt. 0.) then
             TEMP = (6.*STARPAR(2)/NPHOB/BETA6)**0.33333333
          else if(BETA4 .gt. 0.) then
             TEMP = (2.*STARPAR(2)/NPHOB/BETA4)**0.50000000
          else
             TEMP = (STARPAR(2)/NPHOB)
          end if
c... End modification
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
