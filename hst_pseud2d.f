C:  UNTESTED
	FUNCTION PSEUD2D(IX, A, FA, M, MMAX)
	COMMON /DRFAKE/NEEDIT
        common /tune17/ beta4, beta6
	common /HST/ beta8
	DIMENSION A(1), FA(1)
	DIMENSION PP(2)
	INTEGER*2 IX(2)
	LOGICAL NEEDIT
	DATA HALF, THIRD, SIXTH / 0.5, 0.3333333, 0.16666666/
	DATA EXPMIN / -23. /
	DATA FMINCORR, FMAXCORR / -.15, 1.0 /
c	DATA beta8 / 2.5 /
	X = IX(1) - A(3)
	Y = IX(2) - A(4)
	A5 = 1./A(5)
	A7 = 1./A(7)
	T5 = A5*X
	T6 = A(6)*Y
	T7 = A7*Y
	T1 = HALF*((T5 + 2*T6)*X + T7*Y)
	IF (T1 .GT. 0) THEN
	  T2 = T1*T1*HALF
	  DENOM = 1 + T1 + beta4*T2 + (beta6*third)*T1*T2 + 
     +        (beta8/6.)*T2*T2
	  DDDT = 1 + beta4*T1 + beta6*T2 + beta8*T1*T2*third
         DBLDT = beta4 + beta6*T1 + beta8*T2
	  PEXP = 1/DENOM
	ELSE
	  T1 = AMAX1(T1,EXPMIN)
	  PEXP = EXP(-T1)
	  DENOM = 1
	  DDDT = 1
	  DBLDT = 1
	END IF
	FA(2) = PEXP
	PEXP = A(2)*PEXP
	PSEUD2D = PEXP + A(1)
	  t8 = t5 + t6          ! This is DT/DX
	  t9 = a(6)*x + t7      ! This is DT/DY
          P1 = 2.*( (DDDT/DENOM)**2 ) - (DBLDT/DENOM)
	  Q1 = DDDT/DENOM
	  corr = ( P1*(t8**2 + t9**2) - Q1*(a5+a7) )/24.
	  IF(corr .le. FMINCORR) corr = FMINCORR
	  IF(corr .ge. FMAXCORR) corr = FMAXCORR
	IF (NEEDIT) THEN
	  FA(6) = PEXP*DDDT/DENOM
	  FA(3) = (T5 + T6)*FA(6)
	  FA(4) = (A(6)*X + T7)*FA(6)
	  FA(5) = HALF*T5**2*FA(6)
	  FA(7) = HALF*T7**2*FA(6)
	  FA(6) = -X*Y*FA(6)
	  FA(1) = 1
	  if (corr .le. FMINCORR) then
	    corr = FMINCORR
	    fc3 = 0.
	    fc4 = 0.
	    fc5 = 0.
	    fc6 = 0.
	    fc7 = 0.
	  else if (corr .ge. FMAXCORR) then
	    corr = FMAXCORR
	    fc3 = 0.
	    fc4 = 0.
	    fc5 = 0.
	    fc6 = 0.
	    fc7 = 0.
	  else 
	   fc3 = -2.*(a5*t8 + a(6)*t9)/24.
	   fc4 = -2.*(a(6)*t8 + a7*t9)/24.
	   fc5 = -(2*x*t8 - 1)/24./a(5)**2
	   fc7 = -(2*y*t9 - 1)/24./a(7)**2
	   fc6 = 2*(y*t8 + x*t9)/24.
	  end if
	  fa(3) = fa(3)*(1 + corr) + PEXP*fc3
	  fa(4) = fa(4)*(1 + corr) + PEXP*fc4
	  fa(2) = fa(2)*(1 + corr)
	  fa(5) = fa(5)*(1 + corr) + PEXP*fc5
	  fa(6) = fa(6)*(1 + corr) + PEXP*fc6
	  fa(7) = fa(7)*(1 + corr) + PEXP*fc7
	END IF
	  PSEUD2D = PEXP*(1 + corr) + a(1)
	RETURN
	ENTRY PSEUD4D(IX, A, FA, M, MMAX)
	A8 = 1./A(8)
	A10 = 1./A(10)
	DO 2757 I = 1, 2
	  IOFF = (I - 1)*3
	  X = IX(1) - A(3 + IOFF)
	  Y = IX(2) - A(4 + IOFF)
	  T8 = A8*X
	  T9 = A(9)*Y
	  T10 = A10*Y
	  T1 = HALF*((T8 + 2*T9)*X + T10*Y)
	  IF (T1 .GT. 0) THEN
	    T2 = T1*T1*HALF
	    DENOM = 1 + T1 + beta4*T2 + (beta6*third)*T1*T2 +
     +            (beta8/6.)*T2*T2
	    DDDT = 1 + beta4*T1 + beta6*T2 + beta8*third*T2*T1
            DBLDT = beta4 + beta6*T1 + beta8*T2
	    PP(I) = 1/DENOM
	  ELSE
	    PP(I) = EXP(-T1)
	    DENOM = 1
	    DDDT = 1
	    DBLDT = 1
	  END IF
	  A2 = EXP(A(2 + IOFF))
	  FA(2 + IOFF) = A2*PP(I)
	  PP(I) = A2*PP(I)
	  FAC = PP(I)*DDDT/DENOM
	  FA(3 + IOFF) = (A8*X + A(9)*Y)*FAC
	  FA(4 + IOFF) = (A(9)*X + A10*Y)*FAC
	  u8 = t8 + t9	 ! This is DT1/DX
	  u9 = a(9)*x + t10	! This is DT1/DY
	  P1 = 2.*((DDDT/DENOM)**2) - (DBLDT/DENOM)
	  Q1 = DDDT/DENOM
	  corr = ( P1*(u8**2 + u9**2) - Q1*(a8 + a10) )/24.
	  if (corr .le. FMINCORR) then
	   corr = FMINCORR
	   fc3 = 0.
	   fc4 = 0.
	  else if (corr .ge. FMAXCORR) then
	   corr = FMAXCORR
	   fc3 = 0.
	   fc4 = 0.
	  else
           fc3 = -2.*(a8*u8 + a(9)*u9)/24.
	   fc4 = -2.*(a(9)*u8 + a7*u9)/24.
	  end if
	  fa(3 + ioff) = fa(3 + ioff)*(1 + corr) + PP(I)*fc3
          fa(4 + ioff) = fa(4 + ioff)*(1 + corr) + PP(I)*fc4
	  fa(2 + ioff) = fa(2 + ioff)*(1 + corr) 
	  PP(I) = PP(I)*(1 + corr)
2757	CONTINUE
	FA(1) = 1
	PSEUD4D = PP(1) + PP(2) + A(1)
	RETURN
	END
