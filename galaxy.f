	FUNCTION GALAXY(A, ERR, STARPAR)
	INCLUDE 'TUNEABLE'
	COMMON /PARPRED/ PARMS(NPMAX)	
	COMMON /SEARCH/ NSTOT, THRESH
	COMMON /BYVIRTUE/ CHI
c
	DIMENSION A(NPMAX), ERR(NPMAX), STARPAR(NPMAX), B(NPMAX)
	DIMENSION CHI(4), TOT(3)
	LOGICAL GALAXY
c
	GALAXY = .FALSE.
	DO 2757 I = 1, 4
	  CHI(I) = 0
2757	CONTINUE
	IF     (A(5) .LT. 2*SQRT(ERR(5))) THEN
	ELSE IF(A(7) .LT. 2*SQRT(ERR(7))) THEN
	ELSE IF(A(2) .LT. 2*SQRT(ERR(2))) THEN		
	ELSE 
	  CALL PARINTERP(STARPAR(3), STARPAR(4), B)
          tot(1) = amax1(parms(5), (sig(1)*b(5))**2)
          tot(2) = amax1(parms(6),  sig(2)**2/b(5)/b(7))
          tot(3) = amax1(parms(7), (sig(3)*b(7))**2)
	  CHI(1) = (A(5) - B(5))**2/(TOT(1) + ERR(5))
	  CHI(2) = (A(6) - B(6))**2/(TOT(2) + ERR(6))
	  CHI(3) = (A(7) - B(7))**2/(TOT(3) + ERR(7))
	  IF (A(5) .LT. B(5)) CHI(1) = 0
	  IF (A(7) .LT. B(7)) CHI(3) = 0
	  CHI(4) = CHI(1) + CHI(3)
	  CHI(4) = CHI(4) + CHI(2)
	if(lverb.gt.20) then
	  write(6,*) ' (GALAXY...) Object at:', STARPAR(3), STARPAR(4) 
	  write(6,*) ' Chisq-X, Chisq-XY, Chisq-Y, Chisq-TOT: ', CHI    
	end if
c     
	  NSIGMA = min(SQRT(CHI(4)), 1.0E+08)
	if(lverb.gt.20) then
	  write(6,*) 'NSIGMA =',NSIGMA
	end if
	  GALAXY = CHI(4) .GE. CHICRIT
	END IF
	RETURN
	END 
	  
