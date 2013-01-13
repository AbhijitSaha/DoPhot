	SUBROUTINE WARMSTART(ONESTAR,BIG,NOISE,NFAST,NSLOW,filename)
        include 'TUNEABLE'
c
	COMMON /SEARCH/ NSTOT, THRESH
	COMMON /STARLIST/ STARPAR(NPMAX,NSMAX), IMTYPE(NSMAX),
     1  SHADOW(NPMAX,NSMAX),ERRSHAD(NPMAX,NSMAX)
c
	CHARACTER*80 FMT1,prompt
        character*131 inputline
	CHARACTER*(*) filename
c
	INTEGER*2 BIG(NFAST,NSLOW) 
        INTEGER*4 NOISE(NFAST,NSLOW)
c
	real SUM(NPMAX),ss(npmax)
c
	LOGICAL DONE, OBLIT
c
	EXTERNAL ONESTAR
c
	DATA IADD, ISUB /1 , -1/
c
        call opena(11,files(3),0,ierr)
        if(ierr.eq.1) then
        print *,'Input object file doesn''t exist on warmstart!'
        prompt = 'Enter input object file name'
        call opens(11,prompt,0)
        end if
c
        if(flags(7)(1:1).eq.'Y') then
          call opena(12,files(7),0,ierr)
          if(ierr.eq.1) then
            print *,
     *   'Desired input shadow file doesn''t exist on warmstart!'
            prompt = 'Enter input shadow file name'
            call opens(12,prompt,0)
          end if
        end if
c
	IOS = 0
	I = 1
	NSTAR = 0
2757 	IF (IOS .EQ. 0) THEN
          if(flags(8)(1:5).eq.'INTER') then
c
	    READ(11, 151, IOSTAT=IOS) I1,I2,(ss(j),j=1,npar)
 151        format(I5, I3, 7F11.3)
c
            starpar(1,i) = ss(1)
            starpar(2,i) = ss(4)
            starpar(3,i) = ss(2)
            starpar(4,i) = ss(3)
            starpar(5,i) = ss(5)
            starpar(6,i) = ss(6)
            starpar(7,i) = ss(7)
          else if(flags(8)(1:5).eq.'COMPL') then
            read(11,'(a)',iostat=ios) inputline(1:131)
            call stdinpt(i1,i2,starpar(1,i),npar,inputline)
          end if
	  IF (IOS .EQ. 0) THEN
c
            if(flags(7)(1:1).eq.'Y') then
c
	      READ (12,151,IOSTAT=IO)  J1,J2,(ss(j),J=1,NPAR)
c
              shadow(1,i) = ss(1)
              shadow(2,i) = ss(4)
              shadow(3,i) = ss(2)
              shadow(4,i) = ss(3)
              shadow(5,i) = ss(5)
              shadow(6,i) = ss(6)
              shadow(7,i) = ss(7)
c
	      SKY = SHADOW(1,I)				
c
c  Changed indices.
c
	      IF (SHADOW(2,I) .GT. 0) THEN
		TEMP = (SHADOW(2,I) + SKY)/SHADOW(2,I)**2
c
	      ELSE
	        TEMP = 1E10
	      END IF
	      DO 2758 J = 1, NPAR
		ERRSHAD(J,I) = TEMP
2758	      CONTINUE
	      IF (J1 .NE. I1 .OR. J2 .NE. I2) THEN
	        write(6,*) ' Trouble in WARMSTART:'
	        write(6,*) ' Object_input and Shadow_input files
     + have discrepant data!'
	        write(6,*) ' Forcing STOP!'
		write(6,*) 'J1.NE.I1 OR J2.NE.I2 ', J1,I1,J2,I2
	        STOP 66666
	      END IF
            end if
	    NSTOT = I
	    IMTYPE(I) = I2
	    IF (IMTYPE(I) .EQ. 1 .or. imtype(i) .eq. 11) THEN
	      NSTAR = NSTAR + 1
	      DO 2759 J = 1, NPAR
	        SUM(J) = SUM(J) + STARPAR(J,I)
2759	      CONTINUE
	    ELSE IF (IMTYPE(I) .EQ. 8) THEN		
	 DONE=OBLIT(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I))
	    END IF
	    IF     (IMTYPE(I) .EQ. 6) THEN			
	    ELSE IF(IMTYPE(I) .EQ. 8) THEN
	    ELSE
	CALL ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),ISUB)
	    END IF
	  END IF	
	  I = I + 1
	GO TO 2757
	END IF
      if(lverb.gt.10) then
        write(6,*)
	write(6,*) NSTOT, ' objects subtracted from image 
     + at WARMSTART'
      end if
	IF (NSTAR .NE. 0) THEN
	  DO 2760 I = 1, NPAR
	    AVA(I) = SUM(I)/NSTAR
2760	  CONTINUE
	END IF
	RETURN
	END
