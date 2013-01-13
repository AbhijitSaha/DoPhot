 	SUBROUTINE BADOTPT(I,ITYPE,STPARR,NPMAX,APPARR,NAPPLE,
     +    PROBG, OUTSTRING) 
	REAL*4 STPARR(NPMAX), APPARR(NAPPLE)
	CHARACTER*(*) OUTSTRING 
C 
c... Get image shape and area and calc. Fit magnitude:
	IF(ITYPE .NE. 8) THEN
	  CALL ELLIPSE(STPARR(5),STPARR(6),STPARR(7),AREA,AMAJOR,
     +      AMINOR, TILT)
c
c  Changed index.
c
	  FMAG = AREA*STPARR(2)
c
	  IF(FMAG .LE. 0.0) THEN
	    FMAG = 99.999
	  ELSE 
	    FMAG = -2.5 * ALOG10( FMAG )
	  END IF
	  TILT = 57.29578 * TILT
	ELSE
	  IF(STPARR(6) .EQ. -1.) THEN
	     FMAG = 99.999
	  ELSE  
             FMAG = -99.999
	  END IF
	  IF(STPARR(5) .GE. STPARR(7)) THEN
	    AMAJOR = STPARR(5)
	    AMINOR = STPARR(7)
	    TILT = 0.0
	  ELSE 
	    AMAJOR = STPARR(7)
	    AMINOR = STPARR(5)
	    TILT = 90.0
	  END IF
        END IF
C
c... Convert aperture flux to magnitudes: 
	IF(APPARR(1) .LE. 0.0) THEN
	  APMAG = 99.999
	ELSE 
	  APMAG = -2.5 * ALOG10( APPARR(1) )
	END IF
C
c... If available, get uncertainty in aperture magnitudes:
	APERUNC = 99.999
	IF( NAPPLE .GE. 5) APERUNC = APPARR(5)
C
C..  Convert co-ordinates so that internal representation where 
c... center of 1st pixel is 1.0 is changed so that center of 1st
c... pixel is 0.5
c
c  Changed indices.
c
	XC = STPARR(3) -0.5
	YC = STPARR(4) -0.5
c
	write(OUTSTRING, 130) I, XC, YC, FMAG, APPARR(4),
     +     STPARR(1), float(ITYPE), PROBG, APPARR(3)
c
130	format(I6, 2F9.2, 3F9.3, f9.0, f9.2,f9.3)
	RETURN
	END
