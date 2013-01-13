	SUBROUTINE VARIPARHUB(NSTOT, NFAST, NSLOW)
	PARAMETER (MAXFIL = 1024)
	INCLUDE 'TUNEABLE'
	COMMON /SUBRASTER/  XY(MAXFIL), Z(MAXFIL), E(MAXFIL)
	COMMON /STARLIST/ STARPAR(NPMAX,NSMAX),IMTYPE(NSMAX),
     1     SHADOW(NPMAX,NSMAX),SHADERR(NPMAX,NSMAX)
	COMMON /PARPRED/ PARMS(NPMAX)
	COMMON /FITARRAYS/ A(NPMAX),FA(NPMAX),C(2*NPMAX+1,2*NPMAX+1),
     1   B(2*NPMAX), FB(2*NPMAX)
        common /hubvar/ hubpar(nphub)
	DIMENSION PARWT(NPMAX),ROOTS(NPMAX)
	DIMENSION WEIGHT(NPMAX),PARVAL(NPMAX)
	DIMENSION HACC(nphub),HLIM(nphub)
        dimension HUBPARINIT(nphub)
	INTEGER K(4)
	LOGICAL POSITIVE, GOODSTAR, PERFECT, CONV, type1, type3
	INTEGER*2 IXY(2)
	EQUIVALENCE (IXY,R4)
	EXTERNAL HUBFUN
	DATA K / 1, 5, 6, 7 /
	DATA MINRMS / 5 /
	DATA MINHUB /100/
	DATA ITHUB / 100 /
	DATA HACC / .01, -0.1, -0.1, 4*0.1 /
	DATA HLIM / 4*-1.0E5, 3*-1.0E6 /
C:
C:  TWO PASSES, ONE TO COMPUTE EXPECTED VALUE AND ONE TO COMPUTE RMS
C:
        CONV = .TRUE.
	DO 2757 J = 1,4
	  PARVAL(K(J)) = 0
	  PARWT(K(J)) = 0
	  PARMS(K(J)) = 0
	  WEIGHT(K(J)) = 0
2757	CONTINUE
	NPERF = 0
	NGOOD = 0
	I = 1
	SKYMAX = -1E10
2758 	IF (I .LE. NSTOT) THEN
	  GOODSTAR = SHADOW(1,I) .GT. 0.0	
           type1 = imtype(i) .eq. 1 .or. imtype(i) .eq. 11
           type3 = imtype(i) .eq. 3 .or. imtype(i) .eq. 13
	  PERFECT = GOODSTAR .AND. type1
	  GOODSTAR = GOODSTAR .AND. (type1.or.type3)
	  IF (PERFECT) THEN				
	    PARWT(1) = PARWT(1) + 1.0
	    PARVAL(1) = PARVAL(1) + STARPAR(1, I)
	    DO 2759 J = 2, 4
	      TEMP = 1.0/SHADERR(K(J), I)
	      PARWT(K(J)) = PARWT(K(J)) + TEMP
	      PARVAL(K(J)) = PARVAL(K(J)) + TEMP*SHADOW(K(J), I)
2759	    CONTINUE
	    NPERF = NPERF + 1
	  END IF
C
	  IF (GOODSTAR) THEN
	    IF (NGOOD .lt. MAXFIL) THEN
	      IF (STARPAR(1,I) .GT. SKYMAX) THEN
	        SKYMAX = STARPAR(1,I)
c
c  Changed indices.
c
		XM = STARPAR(3,I)
		YM = STARPAR(4,I)
c
	      END IF
	      NGOOD = NGOOD + 1
c
c  Changed indices.
c
	      IXY(1) = STARPAR(3,I) + 0.5
	      IXY(2) = STARPAR(4,I) + 0.5
c
	      XY(NGOOD) = R4
	      Z(NGOOD) = STARPAR(1,I)
	      E(NGOOD) = 1
	    END IF
C
	  END IF
	  I = I + 1
	GO TO 2758
	END IF
	if(lverb.gt.10) then
	write(6,*) ' # of stars available for computing TYPICAL
     + SHAPE parameters (Nperf) = ', NPERF
	write(6,*) ' # of stars available for computing MODEL
     + SKY parameters (Ngood) = ', NGOOD
	end if
	IF (NPERF .GE. 1) THEN
	  DO 2760 J = 1, 4
	    PARVAL(K(J)) = PARVAL(K(J))/PARWT(K(J))
	    AVA(K(J)) = PARVAL(K(J))
2760	  CONTINUE
	if(lverb.gt.10) write(6,*) ' WEIGHTED MEANS: ', PARVAL
C
	  IF (NGOOD .GE. MINHUB )THEN
	    IF (HUBPAR(4) .EQ. 0) THEN
	      HUBPAR(1) = SKYguess
	      HUBPAR(2) = XM
	      HUBPAR(3) = YM
	      HUBPAR(4) = SKYMAX - HUBPAR(1)
	      HUBPAR(5) = NFAST
	      HUBPAR(6) = 0			
	      HUBPAR(7) = NSLOW
	    END IF
            DO 2761 M=1,NPHUB
              HUBPARINIT(M) = HUBPAR(M)
2761            CONTINUE
	    HUBFIT = CHISQ(HUBFUN,XY,Z,E,NGOOD,HUBPAR,FA,C,NHUBFIT,
     1         HACC,HLIM,ITHUB)
            CONV = HUBFIT .LT. 1E10 
            IF(.NOT. CONV) THEN
              DO 2762 M=1,NPHUB
                 HUBPAR(M) = HUBPARINIT(M) 
2762              CONTINUE
            END IF
	  ELSE
	    HUBPAR(1) = SKYguess
	    HUBPAR(2) = NFAST/2
	    HUBPAR(3) = NSLOW/2
	    HUBPAR(4) = 0
	    HUBPAR(5) = NFAST
	    HUBPAR(6) = 0			
	    HUBPAR(7) = NSLOW
      if(lverb.gt.10) write(6,*) 'HUBFUN PARAMETERS:'
      if(lverb.gt.10) write(6,*) (HUBPAR(kk),kk=1,nhubfit)
	  END IF
      if(lverb.gt.10) write(6,*) 'HUBFUN PARAMETERS:'
      if(lverb.gt.10) write(6,*) (HUBPAR(kk),kk=1,nhubfit)
C
	  IF (NPERF .GE. MINRMS) THEN
	    DO 2763 I = 1, NSTOT
	      PERFECT = IMTYPE(I) .EQ. 1 .or. imtype(i) .eq. 11			
	      PERFECT = PERFECT .AND. SHADOW(1,I) .NE. 0	
	      IF (PERFECT) THEN
c
c  Changed indices.
c
		CALL PARINTERP(STARPAR(3,I), STARPAR(4,I), A)
		DO 2764 J = 1, 4
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:  I HAVE CONVINCED MYSELF THAT THE UNCERTAINTY PER RESIDUAL SQUARED IS
C:  IS 2*DELTA*(O-C).  BUT IF I USE 1/THIS AS A WEIGHT, ACCIDENTAL CANCELLATIONS
C:  GIVE SCREWEY RESULTS.  SO WE'LL TAKE THE LARGER OF O-C**2 AND SHADERR.
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
		  SQ = (SHADOW(K(J),I)-A(K(J)))**2
		  WT = 1/SHADERR(K(J),I)/AMAX1(SHADERR(K(J),I),SQ)
		  WEIGHT(K(J)) = WEIGHT(K(J)) + WT
		  PARMS(K(J)) = PARMS(K(J)) + WT*SQ
2764		CONTINUE
	      END IF
2763	    CONTINUE
	    POSITIVE = .TRUE.
	    DO 2765 J = 1, 4
	      PARMS(K(J)) = PARMS(K(J))/WEIGHT(K(J))
	      POSITIVE = POSITIVE .AND. PARMS(K(J)) .GE. 0
	      IF (PARMS(K(J)) .GE. 0) THEN
		ROOTS(K(J)) = SQRT(PARMS(K(J)))
	      ELSE
      if(lverb.gt.10) write(6,*) ' NEGATIVE SCATTER: '
      if(lverb.gt.10) write(6,*) ' PARAM# & SCATTER = ' ,
     1            K(J), PARMS(K(J))
		PARMS(K(J)) = 0
		ROOTS(K(J)) = 0
	      END IF
2765	    CONTINUE
      if(lverb.gt.10) write(6,*) ' SCATTER: ', ROOTS
	  END IF
        ELSE
	    HUBPAR(1) = SKYguess
	    HUBPAR(2) = NFAST/2
	    HUBPAR(3) = NSLOW/2
	    HUBPAR(4) = 0
	    HUBPAR(5) = NFAST
	    HUBPAR(6) = 0			
	    HUBPAR(7) = NSLOW
      if(lverb.gt.10) write(6,*) 'HUBFUN PARAMETERS:'
      if(lverb.gt.10) write(6,*) (HUBPAR(kk),kk=1,nhubfit)
	END IF
	RETURN
	END
