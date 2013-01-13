	FUNCTION TWOFIT(TWOSTAR, STARPAR)
	PARAMETER (MMAX = 11)
	PARAMETER (MAXFIL = 1024)
        include 'TUNEABLE'
	COMMON /SUBRASTER/  Z(MAXFIL), XX(MAXFIL), YE(MAXFIL)
	COMMON /CRUDESTAT/ NPT, SUM1, SUMX, SUMY, MAXVAL, XM, YM
	COMMON /FITARRAYS/ A(NPMAX),FA(NPMAX),C(2*NPMAX+1,2*NPMAX+1),
     1	B(2*NPMAX),FB(2*NPMAX)
	COMMON /BYVIRTUE / CHI
	DIMENSION BACC(2*NPMAX),BLIM(2*NPMAX),STARPAR(NPMAX),CHI(4)
	EXTERNAL TWOSTAR
	LOGICAL BADFIT,CONV
	DATA IADD, ISUB / 1, -1 /
C:
C...SENSE IS THAT IF X IS LONG, ANGLE IS SMALL.
C...SENSE IS THAT IF X&Y ARE POSITIVELY CORRELATED, ANGLE IS POSITIVE.
C:
	BADFIT = .FALSE.
        CONV = .TRUE.
c
c  Changed indices.
c
	CALL PARINTERP(STARPAR(3), STARPAR(4), AVA)
	IF (B(1) .EQ. 0) THEN
	  A5 = 1/A(5)
	  A7 = 1/A(7)
	  ANGLE = ATAN2(-2*A(6), A7 - A5)/2
	  ROOT = SQRT((A5 - A7)**2 + 4*A(6)**2)
	  ROOT1 = (A5 + A7 + ROOT)/2
	  ROOT2 = ROOT1 - ROOT
	  DX2 = A(5) - AVA(5)
	  DY2 = A(7) - AVA(7)
	  DX = SQRT(AMAX1(DX2,0.))/2
	  DY = SQRT(AMAX1(DY2,0.))/2
	  BADFIT = AMAX1(DX,DY) .EQ. 0
	  DX = SIGN(DX,COS(ANGLE))		
	  DY = SIGN(DY,SIN(ANGLE))
	  GAREA = 1/SQRT(ABS(ROOT1*ROOT2))      
	  SAREA = SQRT(ABS(AVA(5)*AVA(7)))      
c
c  Changed indices.
c
	  DX74 = STARPAR(3) - A(3)		
	  DY74 = STARPAR(4) - A(4)		
c
	  DOT = DX74*DX + DY74*DY			
	  IF (DOT .GT. 0) THEN			
	    FAC1 = 0.6666666
	    FAC2 = 1.3333333
	  ELSE
	    FAC1 = 1.3333333
	    FAC2 = 0.6666666
	  END IF
c
c  Changed indices!
c
	  B(1) = A(1)
	  B(2) = (A(2)*GAREA/SAREA/2*FAC2)		
	  B(3) = A(3) - DX*FAC1
	  B(4) = A(4) - DY*FAC1
	  B(5) = (A(2)*GAREA/SAREA/2*FAC1)		
	  B(6) = A(3) + DX*FAC2
	  B(7) = A(4) + DY*FAC2
c
	END IF
c
c  Changed indices.
c
	B(2) = ALOG(B(2))
	B(5) = ALOG(B(5))
	B(8) = AVA(5)
	B(9) = AVA(6)
	B(10) = AVA(7)
c
	DO 2757 I = 1,4
	  BACC(I + 3) = ACC(I)
	  BACC(I) = ACC(I)
          BLIM(I + 3) = ALIM(I)
          BLIM(I) = ALIM(I)
2757	CONTINUE
c
c  Changed indices.
c
	BACC(2) = -.01					
	BACC(5) = -.01					
        BLIM(2) = -10.0					
        BLIM(5) = -10.0                                 
	DXMAX = AMAX1(ABS(B(3)),ABS(B(6)))
	DYMAX = AMAX1(ABS(B(4)),ABS(B(7)))
c
	BADFIT = BADFIT .OR. (DXMAX .GT. IRECT(1)/2.)
	BADFIT = BADFIT .OR. (DYMAX .GT. IRECT(2)/2.)
	IF ( .NOT. BADFIT) THEN		
      TWOFIT = CHISQ(TWOSTAR,XX,Z,YE,NPT,B,FB,C,NFIT2,BACC,BLIM,
     1	2*NIT)
          CONV = TWOFIT .LT. 1E10
	END IF
c
c  Changed indices.
c
	DXMAX = AMAX1(ABS(B(3)),ABS(B(6)))
	DYMAX = AMAX1(ABS(B(4)),ABS(B(7)))
	BADFIT = BADFIT .OR. (DXMAX .GT. IRECT(1)/2. + 1)
	BADFIT = BADFIT .OR. (DYMAX .GT. IRECT(2)/2. + 1)
      IF( CONV ) THEN
c
c  Changed indices.
c
	IF (B(2) .LT. B(5)) THEN
	  DO 2758 I = 2, 4			
	    FB(I) = B(I)
	    B(I)  = B(I + 3)
	    B(I + 3)  = FB(I)
2758	  CONTINUE
	END IF
c
c  Changed indices.
c
	B(2) = EXP(B(2))				
	B(5) = EXP(B(5))				
c
	DO 2759 I = 1, 7
	  FB(I) = B(I)					
2759	CONTINUE
	B(8) = B(1)
	B(9) = B(5)
	B(10) = B(6)
	B(11) = B(7)
	DO 2760 I = 5,7
	  B(I) = AVA(I)
	  B(I + 7) = AVA(I)
2760	CONTINUE
      END IF
      IF (.NOT. CONV .OR. BADFIT) TWOFIT = 1.0E20
	RETURN
	END
