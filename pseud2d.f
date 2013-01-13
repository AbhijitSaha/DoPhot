C:  UNTESTED
	FUNCTION PSEUD2D(IX, A, FA, M, MMAX)
	COMMON /DRFAKE/NEEDIT
        common /tune17/ beta4, beta6
	DIMENSION A(1), FA(1)
	DIMENSION PP(2)
	INTEGER*2 IX(2)
	LOGICAL NEEDIT
	DATA HALF, THIRD, SIXTH / 0.5, 0.3333333, 0.16666666/
	DATA EXPMIN / -23. /
c
c  Changed indices.
c
	X = IX(1) - A(3)
	Y = IX(2) - A(4)
c
	A5 = 1./A(5)
	A7 = 1./A(7)
	T5 = A5*X
	T6 = A(6)*Y
	T7 = A7*Y
	T1 = HALF*((T5 + 2*T6)*X + T7*Y)
	IF (T1 .GT. 0) THEN
	  T2 = T1*T1*HALF
	  DENOM = 1 + T1 + beta4*T2 + (beta6*third)*T1*T2
	  DDDT = 1 + beta4*T1 + beta6*T2
	  PEXP = 1/DENOM
	ELSE
	  T1 = AMAX1(T1,EXPMIN)
	  PEXP = EXP(-T1)
	  DENOM = 1
	  DDDT = 1
	END IF
c
c  Changed indices.
c
	FA(2) = PEXP
	PEXP = A(2)*PEXP
c
	PSEUD2D = PEXP + A(1)
	IF (NEEDIT) THEN
c
c  Changed indices.
c
	  FA(6) = PEXP*DDDT/DENOM
	  FA(3) = (T5 + T6)*FA(6)
	  FA(4) = (A(6)*X + T7)*FA(6)
	  FA(5) = HALF*T5**2*FA(6)
	  FA(7) = HALF*T7**2*FA(6)
	  FA(6) = -X*Y*FA(6)
	  FA(1) = 1
	END IF
	RETURN
c
	ENTRY PSEUD4D(IX, A, FA, M, MMAX)
	A8 = 1./A(8)
	A10 = 1./A(10)
	DO 2757 I = 1, 2
	  IOFF = (I - 1)*3
c
c  Changed indices.
c
	  X = IX(1) - A(3 + IOFF)
	  Y = IX(2) - A(4 + IOFF)
c
	  T8 = A8*X
	  T9 = A(9)*Y
	  T10 = A10*Y
	  T1 = HALF*((T8 + 2*T9)*X + T10*Y)
	  IF (T1 .GT. 0) THEN
	    T2 = T1*T1*HALF
	    DENOM = 1 + T1 + beta4*T2 + (beta6*third)*T1*T2
	    DDDT = 1 + beta4*T1 + beta6*T2
	    PP(I) = 1/DENOM
	  ELSE
	    PP(I) = EXP(-T1)
	    DENOM = 1
	    DDDT = 1
	  END IF
c
c  Changed indices and A4 to A2.
c
	  A2 = EXP(A(2 + IOFF))
	  FA(2 + IOFF) = A2*PP(I)
	  PP(I) = A2*PP(I)
c
	  FAC = PP(I)*DDDT/DENOM
c
c  Changed indices.
c
	  FA(3 + IOFF) = (A8*X + A(9)*Y)*FAC
	  FA(4 + IOFF) = (A(9)*X + A10*Y)*FAC
c
2757	CONTINUE
	FA(1) = 1
	PSEUD4D = PP(1) + PP(2) + A(1)
	RETURN
	END
