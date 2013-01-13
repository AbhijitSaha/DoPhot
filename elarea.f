	FUNCTION ELAREA(B5,A6,B7)
        include 'TUNEABLE'
	A5 = 1/B5
	A7 = 1/B7
C	ANGLE = ATAN2(-2*A6, A7 - A5)/2
	ROOT = SQRT((A5 - A7)**2 + 4*A6**2)	
	ROOT1 = (A5 + A7 + ROOT)/2
	ROOT2 = ROOT1 - ROOT			
	IF (ROOT2 .GT. 0) THEN
	  ELAREA = 1/SQRT(ROOT1*ROOT2)
	ELSE
        if(lverb.gt.20) then
         write(6,*) ' ROOTS 1 & 2 = ', ROOT1, ROOT2, '(....ELAREA)'         
	end if
	  ELAREA = 0
	END IF
	RETURN
	END
