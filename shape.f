	SUBROUTINE SHAPE(ONESTAR, TWOSTAR, BIG, NOISE, NFAST, NSLOW)
	PARAMETER (MMAX = 11)
	PARAMETER ( MAXFIL = 1024)
	INCLUDE 'TUNEABLE'
	COMMON /STARLIST/ STARPAR(NPMAX,NSMAX), IMTYPE(NSMAX),
     1  SHADOW(NPMAX,NSMAX),SHADERR(NPMAX,NSMAX)
	COMMON /SEARCH/ NSTOT, THRESH
	COMMON /SUBRASTER/  Z(MAXFIL), XX(MAXFIL), YE(MAXFIL)
	COMMON /CRUDESTAT/ NPT, SUM1, SUMX, SUMY, MAXVAL
	COMMON /FITARRAYS/ A(NPMAX),FA(NPMAX),C(2*NPMAX+1,2*NPMAX+1),
     1	B(2*NPMAX),FB(2*NPMAX)
	COMMON /BYVIRTUE / CHI(4)
        common /trans7/ test7
        common /fixpass/ fixxy
c
	REAL TWOFPAR(NPMAX,NSMAX)
	INTEGER*2 BIG(NFAST,NSLOW), NOISE(NFAST,NSLOW)
	EXTERNAL ONESTAR,TWOSTAR
	LOGICAL VERYBIG, GALAXY, OFFP, OFFPIC, VFAINT, TOOFAINT
	LOGICAL CONVERGE, test7
        logical fixxy, transmask, notnuff, gotfaint
	REAL ERR(NPMAX)
	DATA IADD, ISUB / 1, -1 /
	DATA IZERO / 0 /
C:
C:  NEXT WE GO THROUGH PREVIOUSLY IDENTIFIED STARS AND FIT AGAIN
C:
C:  91-Oct-27: We've introduced jmtype which is the "reduced" imtype of
C:  objects for which the positions are fixed (fixxy = .true.).  At the end
C:  of loop 2757 we increment the jmtype for such objects and assign
C:  its value to imtype(i).  -PLS
c
        test7 = .true.
c
	NSPREV = NSTOT
	I = 1
2757 	IF (I .LE. NSPREV) THEN
        gotfaint = .false.
	    if(lverb .gt. 20) then
	      write(6,*) ' Determining SHAPE for object No.', I 
	    end if
           fixxy = imtype(i) - (mod(imtype(i),10)) .eq. 10
           if (fixxy) then
              jmtype = imtype(i) - 10
           else
              jmtype = imtype(i)
           end if
	  VFAINT = JMTYPE .EQ. 7
	  IF (SHADOW(1,I) .EQ. 0) THEN
	    SKY = GUESS2 (A, STARPAR(1, I), IX, IY)
	  ELSE
	    SKY = GUESS2 (A, SHADOW(1, I), IX, IY)
	  END IF
	  IF (VFAINT) THEN			
	  if(lverb.gt.20) then
c
c  Changed indices.
c
	     write(6,*) 'TOO FAINT, SKIPPING Object #', I, ' at:',
     +          STARPAR(3,I),STARPAR(4,I)
c
	  end if
	  ELSE IF (JMTYPE .EQ. 4 .or. jmtype .eq. 9) THEN
	  if(lverb.gt.20) then
c
c  Changed loop indices.
c
	      write(6,*) ' SKIPPING NONCONVERGER.. Obj#: ', I,
     *            '  AT: ',(STARPAR(J,I),J=3,4)
c
	  end if
	  ELSE IF (JMTYPE .EQ. 6) THEN	
	  ELSE IF (JMTYPE .EQ. 8) THEN	
	  ELSE
	    CALLADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),IADD)
	    CALL FILLERUP (BIG, NOISE, IX, IY, NFAST, NSLOW)
            notnuff = (NPT .LT. ENUFF7*IRECT(1)*IRECT(2))
	    if (notnuff) then		
      		if(lverb.gt.20) then 
		   write(6,*) 
     *     ' Obj#, NPTS, IX & IY = ', I, NPT,IX,IY,
     +        '   .... SKIPPIN STAR: not enough pixels for 7-parm fit '
	         end if
	      IF (JMTYPE .NE. 2) JMTYPE = 5		
            endif
            if (fixxy) then
              gotfaint = .not.TRANSMASK(BIG,NOISE,NFAST,NSLOW,IX,IY,SKY)
              if (gotfaint .and. (jmtype .ne. 3)) jmtype = 7
            end if
            if (notnuff .or. gotfaint) then
                if(lverb.gt.20) then
                   write(6,*)
     *             ' Obj#, NPTS, IX & IY = ', I, NPT,IX,IY,
     *             '  .... skipping star: ought to be type 7'
                end if
	      CALL ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),ISUB)
	    ELSE
	      if(lverb.gt.20) then
	       write(6,*) ' Obj#, #-PTS FIT, X, Y = ', I, NPT, IX, IY
	      end if
	      GALCHI=CHISQ(ONESTAR,XX,Z,YE,NPT,A,FA,C,NFIT2,ACC,
     *             ALIM,NIT)
	      CALL PARUPD (A, SHADOW(1,I), IX, IY)
	      CALL ERRUPD (C, SHADERR(1,I), NFIT2)
	      VERYBIG = GALAXY(A, SHADERR(1,I), STARPAR(1,I))
	      IF(JMTYPE.EQ.3)VERYBIG=VERYBIG.AND.(CHI(4).GT.XTRA)
C	      CONVERGE = GALCHI .LT. 1E5
	      CONVERGE = GALCHI .LT. 1E10
	      OFFP = OFFPIC(A,IX,IY,NFAST,NSLOW,DX,DY)
	      VERYBIG = VERYBIG .AND. (.NOT. OFFP)	
	      VERYBIG = VERYBIG .AND. CONVERGE
c
C: 91-Oct-27 If the object position if fixed then we don't test
C: for duplicity.  If it failed to converge, it is so flagged,
C: and it's original status as type 3 is preserved.		
c
	      IF ((.NOT. VERYBIG) .or. fixxy) THEN
	        IF (JMTYPE .NE. 3) JMTYPE = 1	
		IF (.NOT. CONVERGE) THEN
		  IF (JMTYPE .NE. 3) JMTYPE = 9
	          SHADOW(1,I) = 0			
      		if(lverb.gt.20) then
c
c  Changed indices.
c
        write(6,*) 'Obj #', I, ' at', STARPAR(3,I), STARPAR(4,I),
     +   'FAILED TO CONVERGE! '	
c
	        end if
		ELSE IF (OFFP) THEN
		  JMTYPE = 4				
	          SHADOW(1,I) = 0			
      	    if(lverb.gt.20) then
c
c  Changed indices.
c
	   write(6,*) ' ABSURD SHAPE VALUES for Obj #', I, 'at:', 
     +     STARPAR(3,I), STARPAR(4,I)
c
	   write(6,*) ' Fit center outside fit subraster 
     +            .... DISCARD SOLUTION!'
	    end if
	        END IF
		CALL ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),ISUB)
	      ELSE
      		if(lverb.gt.20) then
c
c  Changed indices.
c
	         write(6,*) ' Obj #', I, ' at:', STARPAR(3,I),
     +            STARPAR(4,I),  '  is VERY BIG.... '
c
                 write(6,*) ' . .. Testing GALAXY vs. DBLE-STAR '
	        end if
		CALL TWOUPD(TWOFPAR(1,I), B, -IX, -IY)
		STARCHI = TWOFIT(TWOSTAR, STARPAR(1,I))
        	IF (STARCHI/GALCHI .LT. STOGRAT) THEN	
      		if(lverb.gt.20) then
	          write(6,*) ' Result-> A SPLIT STAR:',
     + ' GAL-CHI:', GALCHI, ' STAR-CHI:', STARCHI
	        end if
		  JMTYPE = 3
		  CALL PARUPD (B, SHADOW(1,I), IX, IY)	
		  CALL PARUPD (B, STARPAR(1,I), IX, IY)
		  CALL ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),ISUB)
		  NSTOT = NSTOT + 1
		  IMTYPE(NSTOT) = 3		
		  CALL PARUPD (TWOFPAR(1,NSTOT),TWOFPAR(1,I),IZERO,IZERO)
		  CALL PARUPD (B(NFIT2+1), STARPAR(1,NSTOT), IX, IY)
		  CALLADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,NSTOT),
     1	  		ISUB)
		ELSE
              if(lverb.gt.20) then
	        write(6,*) ' Result-> A GALAXY:', 
     + ' GAL-CHI:', GALCHI, ' STAR-CHI:', STARCHI
	      end if
		  JMTYPE = 2
		  CALL PARUPD (A, STARPAR(1,I), IX, IY)
		  CALL TWOUPD (FB, TWOFPAR(1,I), IX, IY)
		  CALLADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),ISUB)
		END IF
	      END IF
	    END IF
	  END IF
          imtype(i) = jmtype
          if (fixxy) imtype(i) = jmtype + 10
          fixxy = .false.
	  I = I + 1
	GO TO 2757
	END IF
        test7 = .false.
	RETURN
	END
