	SUBROUTINE IMPAPER(BIG,NOISE,NFAST,NSLOW,K)
	PARAMETER (NAPPLE = 5)
        include 'TUNEABLE'
	COMMON /STARLIST/ STARPAR(NPMAX,NSMAX), IMTYPE(NSMAX), 
     1   SHADOW(NPMAX,NSMAX), SHADERR(NPMAX,NSMAX)
	COMMON /APERLIST/ APPLE(NAPPLE ,NSMAX)
	INTEGER*2 BIG(NFAST, NSLOW)
	INTEGER*4 NOISE(NFAST,NSLOW)
c        real*8 suma0,suma1,sumb0,sumb1
         real*8 suman,sumbn,vara,varb
	LOGICAL BADPIX
	DATA MAGIC / 2147483647/
	DATA IADD, ISUB / 1, -1/
c
	DO 2757 I = 1, NAPPLE
	  APPLE(I,K) = 0
2757	CONTINUE
c
c  Changed indices.
c
	XC = STARPAR(3,K)
	YC = STARPAR(4,K)		
c
	BADPIX = .FALSE.
        suman = 0.0
        sumbn = 0.0
	SUMA0 = 0.0
	SUMA1 = 0.0
	SUMB0 = 0.0
	SUMB1 = 0.0
	JHI = MIN0(IFIX(YC + ARECT(2) + 0.5), NSLOW)
	J = MAX0(IFIX(YC - ARECT(2) + 0.5), 1)		
	IHI = MIN0(IFIX(XC + ARECT(1) + 0.5), NFAST)
	ILO = MAX0(IFIX(XC - ARECT(1) + 0.5), 1)	
	TXA = ARECT(1)*0.5 + 0.5
	TXB = ARECT(1)     + 0.5
	TYA = ARECT(2)*0.5 + 0.5
	TYB = ARECT(2)     + 0.5
2758 	IF (J .LE. JHI) THEN
	  ABJYC = ABS(J - YC)
	  YA=AMIN1(1.,AMAX1(0.,TYA - ABJYC))
	  YB=AMIN1(1.,AMAX1(0.,TYB - ABJYC))
C	  YA=AMIN1(1.,AMAX1(0.,ARECT(2)/2+0.5-ABS(J-YC)))
C	  YB=AMIN1(1.,AMAX1(0.,ARECT(2)+0.5-ABS(J-YC)))
	  I = ILO
2759 	  IF (I .LE. IHI) THEN
	    ABIXC = ABS(I - XC)
	    XA=AMIN1(1.,AMAX1(0.,TXA - ABIXC))
	    XB=AMIN1(1.,AMAX1(0.,TXB - ABIXC))
C	    XA=AMIN1(1.,AMAX1(0.,ARECT(1)/2+0.5-ABS(I-XC)))
C	    XB=AMIN1(1.,AMAX1(0.,ARECT(1)+0.5-ABS(I-XC)))
	    XYA = XA*YA
	    XYB = XB*YB
	    BADPIX = NOISE(I,J) .GE. MAGIC
	    IF (XYA .GT. 0) THEN
	      IF (BADPIX) THEN
		I = IHI
		J = JHI
	      ELSE
		SUMA0 = SUMA0 + XYA
		SUMA1 = SUMA1 + XYA*BIG(I,J)
                suman = suman + xya*xya*noise(i,j)
	      END IF
	    END IF
	    IF (XYA .LT. 1) THEN
	      IF (.NOT. BADPIX) THEN
		DXY = (XYB - XYA)/NOISE(I,J)
		SUMB0 = SUMB0 + DXY
		SUMB1 = SUMB1 + DXY*BIG(I,J)
                sumbn = sumbn + dxy*dxy*noise(i,j)
	      END IF
	    END IF
	    I = I + 1
	  GO TO 2759
	  END IF
	  J = J + 1
	GO TO 2758
	END IF
	IF ( .NOT. BADPIX) THEN
	  IF (ABS(SUMA0/(ARECT(1)*ARECT(2))-1).GT.0.001) THEN
	if(lverb.gt.20) 
     *     write(6,*) 'AREAS NOT EQUAL! STAR #, SUMB1 & SUMB0 = ', 
     *     K, sumb1, SUMB0
	  ELSE
	    APPLE(1,K) = SUMA1 - SUMB1*(SUMA0/SUMB0)
	    APPLE(2,K) = SUMB1/SUMB0
            vara = suman
            varb = (suma0/sumb0)**2*sumbn
            apple(5,k) = 1.086*dsqrt(vara + varb)/apple(1,k)
	    IF (STARPAR(5,K) .GT. 0) THEN		
              starlum = elarea(starpar(5,k),starpar(6,k),starpar(7,k))
c
c  Changed indices.
c
              starlum = 6.283185*starlum*starpar(2,k)
c
c  This is an independent way of calculating the difference between the
c  aperture and fit mags.  Large values for abs(APPLE(3,i)) for STAR 
c  objects will indicate problems.  This value equals the difference 
c  between the sky and fit mags.
c
 	      RATLUM = STARLUM/APPLE(1,K)
	      IF (RATLUM .GT. 0) APPLE(3,K) = 2.5*ALOG10(RATLUM)
	    END IF
	  END IF
	END IF
	RETURN
	END
