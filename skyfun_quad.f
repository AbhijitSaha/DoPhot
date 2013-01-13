c
c  This is the skyfun for the quadratic law.
c  Note that we now use m to indicate whether to
c  quit at 
	FUNCTION SKYFUN(IX, A, FA, M, MMAX)
	COMMON /DRFAKE/ NEEDIT
	COMMON /UNDERGND/ NFAST, NSLOW
	LOGICAL NEEDIT
	DIMENSION A(7),FA(7)
	INTEGER*2 IX(2)
	DATA HALF, threeh / 0.5, 1.5 /
        
	hX = half*(IX(1) - HALF*NFAST)/(HALF*NFAST)
	hY = half*(IX(2) - HALF*NSLOW)/(HALF*NSLOW)
	FA(1) = 1
	FA(2) = hx
	FA(3) = hy
        fa(4) = threeh*hx*hx - half
        fa(5) = hx*hy
        fa(6) = threeh*hy*hy - half
	TEMP = 0
	DO 2757 I = 1, m
	  TEMP = TEMP + A(I)*FA(I)
2757	CONTINUE
	SKYFUN = TEMP	
	RETURN
	END
