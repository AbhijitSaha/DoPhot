	FUNCTION cosprob(A, ERR, STARPAR)
	INCLUDE 'TUNEABLE'
	COMMON /PARPRED / PARMS(NPMAX)	
	COMMON /SEARCH/ NSTOT, THRESH
	DIMENSION A(NPMAX), ERR(NPMAX), STARPAR(NPMAX), B(NPMAX)
	LOGICAL cosprob

C
	cosprob = .FALSE.
	  CALL PARINTERP(STARPAR(3), STARPAR(4), B)
	  IF (A(5) .LT. B(5)/2.) COSPROB = .TRUE.
	  IF (A(7) .LT. B(7)/2.) COSPROB = .TRUE.
	if(lverb.gt.20) then
	  write(6,*) ' (cosprob...) Object at:', STARPAR(3), STARPAR(4)  
	  if(COSPROB) then 
             write(6,*) 'Narrow profile: Probably Cosmic Ray'
          else 
             write(6,*) ' OK for type 1 '
          end if
	end if
c	END IF
	RETURN
	END 
