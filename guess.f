	FUNCTION GUESS1 (A, STARPAR, IX, IY)	
        include 'TUNEABLE'
	COMMON /CRUDESTAT/ NPT, SUM0, SUM1, SUM2, MAXVAL, XM, YM
	COMMON /UNITIZE/ UFACTOR
	DIMENSION A(NPMAX), STARPAR(NPMAX)
	XT = IX
	YT = IY
	CALL PARINTERP( XT, YT, A)		
	GUESS1 = A(1)				
C:
C:  SERIOUS PROBLEM HERE.  I'D LIKE TO STOP MAKING GUESS1 A FUNCTION.
C:  BUT IT'S CALLED BY ISEARCH, FILLERUP AND DOPHOT.  SO I NEED TO MAKE
C:  CHANGES CONSISTENTLY
C:
	A(1) = A(1)/UFACTOR
C	A(1) = (A(1) + SUM2)/2			!CHICKEN HEARTED
	A(1) = SUM2/UFACTOR
	A(2) = MAXVAL/UFACTOR - A(1)
	A(3) = XM
	A(4) = YM
	RETURN
	ENTRY GUESS2(A, STARPAR, IX, IY)	
	IX = IFIX(STARPAR(3) + 0.5)		
	IY = IFIX(STARPAR(4) + 0.5)
	DO 2757 K = 5, NPAR
	  A(K) = STARPAR(K)
2757	CONTINUE
	A(1) = STARPAR(1)/UFACTOR
	A(2) = STARPAR(2)/UFACTOR
	A(3) = STARPAR(3) - IX
	A(4) = STARPAR(4) - IY
	GUESS2 = STARPAR(1)
	RETURN
	ENTRY GUESS3(A, STARPAR, IX, IY)	
	IX = IFIX(STARPAR(3) + 0.5)		
	IY = IFIX(STARPAR(4) + 0.5)
	CALL PARINTERP(STARPAR(3), STARPAR(4), A)
	A(1) = STARPAR(1)/UFACTOR
	A(2) = STARPAR(2)/UFACTOR
	A(3) = STARPAR(3) - IX
	A(4) = STARPAR(4) - IY
	GUESS3 = STARPAR(1)
	RETURN
	END
