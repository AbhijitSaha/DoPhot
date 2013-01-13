 	SUBROUTINE STDOTPT(I,ITYPE,STPARR,NPAR,APPARR,NAPPLE,
     +    PROBG, OUTSTRING) 
	REAL*4 STPARR(NPAR), APPARR(NAPPLE)
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
C
C 
C
c....  Fix the co-ordinates so that the center of the first pixel is 
c....   0.5 and not 1.0 as in the internal representation
c
	XC = STPARR(3) - 0.5
	YC = STPARR(4) - 0.5
c
c...   limit value of APPARR(4), the fit mag error to within +/-9.99 to prevent
c        format overflow
c
	APPARR(4) = amax1( amin1(APPARR(4), 9.99) , -99.99)
c
	WRITE(OUTSTRING, 120) I,ITYPE,XC,YC,FMAG,
     +    APPARR(4), STPARR(1), AMAJOR, AMINOR, TILT, PROBG, 
     +    APMAG, APERUNC, APPARR(2), APPARR(3)
c....... Order is:  No., obtype, xpos, ypos, fitmag, err_fitmag,
c........           fitsky, FWHM_major, FWHM_minor, Tilt, probgal,
c.......            apmag, err_apmag, apsky, diff_fit_ap
C
C
120     FORMAT(I6,I3,2F9.2,F9.3,F7.3, F10.2,2F10.3,F8.2,1PE11.3,
     +     0PF9.3, F7.3, F10.2, F8.3)
C
	RETURN
	END
