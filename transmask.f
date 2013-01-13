	FUNCTION TRANSMASK(BIG,NOISE,NFAST,NSLOW,IX,IY,SKY)
	INCLUDE 'TUNEABLE'
	COMMON /PASSMASK/ STARMASK(-8:8, -8:8)	
        common /trans7/ test7 
	INTEGER*2 BIG(NFAST,NSLOW)
	INTEGER*4 NOISE(NFAST,NSLOW)
	LOGICAL TRANSMASK, test7
	DATA MAGIC / 2147483647/
	TRANSMASK = .FALSE.
        if (test7) then
          hump2 = crit7
        else
          hump2 = bumpcrit**2
        end if
	SUM0 = 0
	SUM1 = 0			
	SUM2 = 0
	DO 2757 J = -IYBY2, IYBY2
	  JJ = IY + J
	  IF (JJ .LT. 1) THEN
	  ELSE IF (JJ .GT. NSLOW) THEN
	  ELSE
	  DO 2758 I = -IXBY2, IXBY2
	    II = IX + I
	    IF (II .LT. 1) THEN
	    ELSE IF (II .GT. NFAST) THEN
	    ELSE
	    IF (NOISE(II,JJ) .NE. MAGIC) THEN
	      SUM0 = SUM0 + 1
	      TEMP = STARMASK(I,J)/NOISE(II,JJ)
	      SUM1 = SUM1 + STARMASK(I,J)*TEMP		
	      SUM2 = SUM2 + TEMP*(BIG(II,JJ)-SKY)
	    END IF
	    END IF
2758	  CONTINUE
	  END IF
2757	CONTINUE
	IF (SUM2 .GT. 0) THEN
	  RAT = SUM2**2/SUM1
	  IF (RAT .GT. hUMP2) THEN
	    TRANSMASK = .TRUE.
	    IF (RAT .LT. 1.1*hUMP2) THEN
	 if(lverb.gt.30) then
	    write(6,*) 'MARGINAL: (S/N)**2 through Mask = ', RAT
	 end if
	    END IF
	  END IF
	END IF
	RETURN
	END
