
	SUBROUTINE PARAVG
	PARAMETER (MMAX = 11)
	INCLUDE 'TUNEABLE'
	COMMON /STARLIST/ STARPAR(NPMAX,NSMAX),IMTYPE(NSMAX),
     1  SHADOW(NPMAX,NSMAX),SHADERR(NPMAX,NSMAX)
	COMMON /SEARCH/ NSTOT,THRESH
	COMMON /FITARRAYS/ A(NPMAX),FA(NPMAX),C(2*NPMAX+1,2*NPMAX+1),
     1    B(2*NPMAX), FB(2*NPMAX)
	DIMENSION SUM(3,NPMAX)
C:
C:  RUN THROUGH STAR LIST AND COMPUTE AVERAGE VALUES FOR SKY, ETC.
C:
	N = 0
	IF (NSTOT .GE. 1) THEN
	  DO 2757 I = 1, NSTOT
	    IF (IMTYPE(I) .EQ. 1 .or. imtype(i) .eq. 11) THEN		
	      N = N + 1
	      DO 2758 J = 1, NPAR
              if(shaderr(j,i).eq.0.0) then
              write(6,*) 'shaderr = 0; i = ',i
              write(6,*) ' ignored this object '
              write(6,*)
              N = N - 1
              go to 2757
              end if
		SUM(1,J) = SUM(1,J) + 1/SHADERR(J,I)
		SUM(2,J) = SUM(2,J) + SHADOW(J,I)/SHADERR(J,I)
2758	      CONTINUE
	    END IF
2757	  CONTINUE
	  IF (N .GT. 0) THEN
	    DO 2759 J = 1, NPAR
	      AVA(J) = SUM(2,J)/SUM(1,J)
2759	    CONTINUE
	  END IF
	if(lverb.gt.10) then
	   write(6,*) 'AVERAGE values so far of 7 parameters for stars:'   
	   write(6,*) AVA
	endif
	END IF
	RETURN
	END
	
	 
