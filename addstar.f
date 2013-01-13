	SUBROUTINE ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR,IADD)
	INCLUDE 'TUNEABLE'
c:      common block corrupts updating of a in shape
c	COMMON /FITARRAYS/ A(NPMAX), FA(NPMAX), C(NPMAX+1, NPMAX+1)
	COMMON /UNITIZE/ UFACTOR
	COMMON / DRFAKE / NEEDIT
        dimension a(npmax), fa(npmax)
	INTEGER*2 BIG(NFAST,NSLOW)
	INTEGER*4 NOISE(NFAST,NSLOW)
	INTEGER*2 IX(2), JRECT(4)
        INTEGER*4 JLO, JHI, ILO, IHI
	DIMENSION STARPAR(NPMAX),B(NPMAX)
	LOGICAL BADNEWS, NEEDIT
	EXTERNAL ONESTAR
	EQUIVALENCE (IX,R4)
	DATA MAGIC / 2147483647/
	NEEDIT = .FALSE.
	SKY = GUESS2(A, STARPAR, IXIN, IYIN)	
	SKY = GUESS2(B, STARPAR, IXIN, IYIN)
	CALL ADDLIMS(STARPAR, JRECT)
c
c  No changes here due to reordering of parameters.
c
	B(5) = A(5)*XPND**2
	B(6) = A(6)/XPND**2			
	B(7) = A(7)*XPND**2
c
	BFACTOR = UFACTOR*IADD
	BSKY = IADD*(-SKY + 0.5)
	CFACTOR = FAC*UFACTOR
	CSKY = FAC*(-SKY) + 0.5
        JLO = JRECT(3)
	JLO = MAX0(JLO, 1)
        JHI = JRECT(4)         
	JHI = MIN0(JHI, NSLOW)
	DO 2757 J = JLO, JHI
	  IX(2) = J - IYIN
          ILO = JRECT(1)
	  ILO = MAX0(ILO, 1)
          IHI = JRECT(2)
	  IHI = MIN0(IHI, NFAST)
	  DO 2758 I = ILO, IHI
	    IF (NOISE(I,J) .LT. MAGIC) THEN
	      IX(1) = I - IXIN
C	      IVAL = UFACTOR*ONESTAR(R4, A, FA, M, NPMAX) - SKY + 0.5
	      IVAL = BFACTOR*ONESTAR(R4, A, FA, M, NPMAX) + BSKY
	      ITEMP = BIG(I,J) + IVAL
	      BADNEWS = (ITEMP .GE. 32767) .OR. (ITEMP .LE. -32768)
	      IF (BADNEWS) THEN
		write(6,*) ' I, J, & BADNEWS = ', I, J, ITEMP,
     + 		' (.....routine ADDSTAR)'
      		write(6,*) ' SERIOUS problem.. do not pass go 
     +      	  Do not collect 200'     
	        BIG(I,J) = -32768
	        NOISE(I,J) = MAGIC
	      ELSE
		BIG(I,J) = ITEMP
C		IVAL=FAC*(UFACTOR*ONESTAR(R4,B,FA,M,NPMAX)-SKY)+0.5
	        IVAL = CFACTOR*ONESTAR(R4,B,FA,M,NPMAX) + CSKY
		NOISE(I,J) = NOISE(I,J) - IADD*IVAL**2
		IF (NOISE(I,J) .LE. 0) THEN
      		 write(6,*) ' I, J, NEGNOISE = ',I,J,NOISE(I,J),
     + 		 ' (.....routine ADDSTAR)'
      		 write(6,*) ' SERIOUS problem.. do not pass go 
     +      	  Do not collect 200'     
	         write(6,*)
		END IF
	      END IF
	    END IF
2758	  CONTINUE
2757	CONTINUE
	NEEDIT = .TRUE.
	RETURN
	END
