	SUBROUTINE STDINPT( I1, I2, STPARR, NPAR, INSTR)
	real*4 STPARR(NPAR)
	character*(*) INSTR
c
	if(NPAR .LT. 7) PRINT *, ' Less than 7 parameters allocated in 
     +     STDINPT routine...  Dire warning !!! '
c
	read(INSTR, 120) I1, I2, XC, YC, FMAG, dummy,
     +    STPARR(1), AMAJOR, AMINOR, TILT, dummy, dummy, dummy,
     +    dummy, dummy
c
c...... Convert position centers so that the center of the 1st pixel is 1.0 
c...... ( as required in the internal representation) rather than
c...... 0.5 as used in the COMPLETE style output
c
c  Changed indices.
c
	STPARR(3) = XC + 0.5
	STPARR(4) = YC + 0.5
c
	IF( I2 .eq. 8) THEN
	  IF( TILT .GT. 89.5 .AND. TILT .LT. 90.5) THEN
	    STPARR(5) = AMINOR
	    STPARR(7) = AMAJOR
	    STPARR(6) = 0.0
c
c  Changed index.
c
	    STPARR(2) = 0.0
c
	  ELSE
            STPARR(5) = AMAJOR
	    STPARR(7) = AMINOR
	    STPARR(6) = 0.0
c
c  Changed index.
c
	    STPARR(2) = 0.0
c
	  END IF
	ELSE
	  CALL ELLIPINT(AMAJOR,AMINOR,TILT,AREA,STPARR(5),
     +    STPARR(6), STPARR(7) )
	  IF(FMAG .GT. 0.0) THEN
c
c  Changed index.
c
	     STPARR(2) = 0.0
	  ELSE
c
c  Changed indices.
c
	     STPARR(2) = 10.**(-0.4*FMAG)
	     STPARR(2) = STPARR(2) / AREA
c
	  END IF
        END IF
c
120	FORMAT(I6,I3,2F9.2,F9.3,F7.3,F10.2,2F10.3,F8.2,1PE11.3,
     +     0PF9.3, F7.3, F10.2, F8.3)
c
	RETURN
	END
