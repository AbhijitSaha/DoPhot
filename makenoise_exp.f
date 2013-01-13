	SUBROUTINE MAKENOISE(BIG, NOISE, NFAST, NSLOW, Jsig)
	INCLUDE 'TUNEABLE'
	COMMON /SEARCH/ NSTOT, THRESH
	INTEGER*2 BIG(NFAST,NSLOW), jsig(NFAST, NSLOW)
	INTEGER*4 NOISE(NFAST,NSLOW)
	LOGICAL BADLINE, BADPIX
	DATA MAGIC / 2147483647/
	JTOP = MIN0(ITOP, 32767)
	JBOT = MAX0(IBOT, -32768)
	TEMP1 = (RNOISE/EPERDN)**2		
        TEMP1 = amax1(TEMP1, 4.)
	TEMP2 = 1/EPERDN			
	DO 2757 I = 1, NSLOW
          BADLINE = (I .LE. NBADBOT .OR. I .GT. NSLOW - NBADTOP)
          DO 2758 J = 1, NFAST
	    IF (BADLINE) THEN
	      NOISE(J,I) = MAGIC
	    ELSE
	      BADPIX = (J .LE. NBADLEFT .OR. J .GT. NFAST - NBADRIGHT)
	      IF (BADPIX) THEN
		NOISE(J,I) = MAGIC
	      ELSE IF	(BIG(J,I) .GE. JTOP)THEN
		NOISE(J,I) = MAGIC
	      ELSE IF	(BIG(J,I) .LE. JBOT)THEN
		NOISE(J,I) = MAGIC
	      ELSE
                if (big(j,i) .gt. 0) then
        	  NOISE(J,I) = max1( float(jsig(j,i))*float(jsig(j,i)), 
     $  (abs(float(big(j,i)))*temp2 + temp1)  )
                else
                noise(j,i) = temp1
                end if
	      END IF
	    END IF
2758	  CONTINUE
2757	CONTINUE
c
c...  return jsig with sigma values actually used: 
c
	do 9876 i = 1, nslow
           do 9877 j = 1, nfast
             jsig(j,i) = nint( sqrt(float(noise(j,i))) )
9877	   continue
9876	continue
c
	RETURN
	END
