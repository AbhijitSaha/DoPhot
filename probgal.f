	FUNCTION PROBGAL(ONESTAR, XX, YY, ZZ, NPT, A, FA)
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:I THINK I WANT TO CALCULATE THE DERIVATIVE OF THE BEST FITTING CHISQUARED
C:WITH RESPECT TO A VARIATION IN A(2),A(5),A(6) AND A(7) WHICH KEEPS FLUX
C:CONSTANT.
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        include 'TUNEABLE'
	DIMENSION C(NPMAX+1,NPMAX+1), B(NPMAX+1,NPMAX+1),V(NPMAX+1)
        dimension indx(npmax+1)
	DIMENSION XX(npt), YY(npt), ZZ(npt)
	DIMENSION A(npmax), FA(npmax)
	EXTERNAL ONESTAR
	CHI1 = 0
	DO 2757 I = 1, NPMAX
	  C(I,NPMAX+1) = 0
	  DO 2758 J = 1, NPMAX
	    C(J,I) = 0
2758	  CONTINUE
2757	CONTINUE
	DO 2759 I = 1, NPT
	  F = ONESTAR(XX(I), A, FA, NPMAX, NPMAX) - YY(I)
	  CHI1 = CHI1 + F**2/ZZ(I)
	  DO 2760 J = 1, NPMAX
	    FAJ = FA(J)/ZZ(I)
	    C(J, NPMAX+1) = C(J, NPMAX+1) + FAJ*F
            v(j) = c(j, npmax+1)
	    DO 2761 K = 1,J
	      C(J,K) = C(J,K) + FAJ*FA(K)
2761	    CONTINUE
2760	  CONTINUE
2759	CONTINUE
	DO 2762 I = 1, NPMAX
	  DO 2763 J = 1, I
	    B(I,J) = C(I,J)
	    B(J,I) = C(I,J)
	    C(J,I) = C(I,J)
2763	  CONTINUE
2762	CONTINUE
	CALL LUDCMP(b,npmax,npmax+1,INDX,D)
	CALL LUBKSB(b,npmax,npmax+1,INDX,V)
	DCHI = 0
	DO 2764 I = 1, NPMAX
	  DCHI = DCHI + c(I,npmax+1)*(-V(I))
	  DO 2765 J = 1, NPMAX
	    DCHI = DCHI + V(I)*c(J,I)*V(J)/2
2765	  CONTINUE
2764	CONTINUE
	B5 = A(5) - V(5)
	B6 = A(6) - V(6)
	B7 = A(7) - V(7)
	AAREA = ELAREA (A(5),A(6),A(7))
	BAREA = ELAREA (B5  ,B6,  B7  )
	IF (DCHI .gT. 0) THEN
	if(lverb.gt.20) then
c
c  Changed indices.
c
	write(6,*) ' Object at', A(3),A(4), 'has -ve Delta Chi**2 !' 
         write(6,*) 'DCHI = ', DCHI
	end if
	  PROBGAL = 0
c	ELSE IF (AMIN1(AAREA, BAREA) .LE. 0) THEN
	ELSE IF (AAREA .LE. 0) THEN
	  PROBGAL = 0
	ELSE
	  CHIPER = CHI1/NPT				
c	  PROBGAL = SIGN(BAREA - AAREA, DCHI/CHIPER)
	  PROBGAL = SIGN(dchi/chiper, BAREA - AAREA)
	END IF
	RETURN
	END
	
