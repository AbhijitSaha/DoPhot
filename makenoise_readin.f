	SUBROUTINE MAKENOISE(BIG, NOISE, NFAST, NSLOW, JSIGMA)
	INCLUDE 'TUNEABLE'
	COMMON /SEARCH/ NSTOT, THRESH
	INTEGER*2 BIG(NFAST,NSLOW), JSIGMA(NFAST,NSLOW)
        common /DDD1/ ksigma(ncmax*nrmax/2)
	INTEGER*4 NOISE(NFAST,NSLOW)
        Character*80 sigfilename
	LOGICAL BADLINE, BADPIX
        LOGICAL INP_NOISE
	DATA MAGIC / 2147483647/
c
c
c
        INP_NOISE = .false.
c
	write(*,*) ' Enter filename of integerized sigma image: '
        write(*,*) ' enter null to calc noise ' 
	read(*,198) sigfilename
 198	format(A)  
c
        if(sigfilename .ne. '') then
          nhead = 250
          call rffile(nhead, header, nx, ny, ksigma, sigfilename)
          print *, ' Igot here '
          inp_noise = .true.
        end if 
c                    
	JTOP = MIN0(ITOP, 32767)
	JBOT = MAX0(IBOT, -32768)
	TEMP1 = (RNOISE/EPERDN)**2		
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
		  NOISE(J,I) = BIG(J,I)*TEMP2 + TEMP1
                else
                  noise(j,i) = temp1
                end if
                if(inp_noise) then 
                  NOISE(J,I) = jsigma(j,i)*jsigma(j,i)
                end if
	      END IF
	    END IF
2758	  CONTINUE
2757	CONTINUE
c
        if(inp_noise) then 
	 call wffile(nhead, header, nx, ny, noise, 'NOISE.fit')
        endif
c
	RETURN
	END
