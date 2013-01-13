	SUBROUTINE IMPROVE(ONESTAR,BIG,NOISE,NFAST,NSLOW)
	PARAMETER (MMAX = 11)
        include 'TUNEABLE'
	PARAMETER (MAXFIL = 1024)
	PARAMETER (NAPPLE = 5)
c
	COMMON /APERLIST/ APPLE(NAPPLE ,NSMAX)
	COMMON /PROBPASS/ PROBG(NSMAX)
	COMMON /STARLIST/ STARPAR(NPMAX,NSMAX), IMTYPE(NSMAX), 
     1   SHADOW(NPMAX,NSMAX), SHADERR(NPMAX,NSMAX)
	COMMON /SEARCH/ NSTOT,THRESH
	COMMON /SUBRASTER/  Z(MAXFIL), XX(MAXFIL), YE(MAXFIL)
	COMMON /CRUDESTAT/ NPT, SUM1, SUMX, SUMY, MAXVAL, XM,YM
	COMMON /FITARRAYS/ A(NPMAX),FA(NPMAX),C(2*NPMAX+1,2*NPMAX+1),
     1    B(2*NPMAX), FB(2*NPMAX)
	COMMON /CTIMES / CHIIMP, APERTIME, FILLTIME, ADDTIME
        common /fixpass/ fixxy
c
	REAL*4 CC(5,5)
        real*4 ccc(3,3), err(npmax)
c
	INTEGER*2 BIG(NFAST,NSLOW)
        INTEGER*4 NOISE(NFAST,NSLOW)
c
	LOGICAL OFFPIC, SKIP, CONVERGE, SNOK, TRANSMASK
        logical fixxy, toofaint
c
	EQUIVALENCE (C, CC)
        equivalence (c, ccc)
c
	EXTERNAL ONESTAR
c
	DATA IADD, ISUB / 1, -1 /
        data pi / 3.14159265 /
        data nfit0 / 2 /
C:
C:  NEXT WE GO THROUGH PREVIOUSLY IDENTIFIED STARS AND FIT AGAIN
C:  91-Oct-27: We've introduced jmtype which is the "reduced" imtype of
C:  objects for which the positions are fixed (fixxy = .true.).  At the end
C:  of loop 2757 we increment the jmtype for such objects and assign
C:  its value to imtype(i).  -PLS
c
c   In addition, FIXPOS must be .true. as well;  this way one can have an
c   input file with imtype > 10, but still do 4 parameter fits in IMPROVE.
c   - MLM
c
c        NFIT0 = 2
	I = 1
2757 	IF (I .LE. NSTOT) THEN
c
c  Test if positions should be fixed for this object.
c
           fixxy = imtype(i) - (mod(imtype(i),10)) .eq. 10
           fixxy = fixxy .and. fixpos
c           
           if (fixxy) then
              jmtype = imtype(i) - 10
           else
              jmtype = imtype(i)
           end if
c
	  IF 		(JMTYPE .EQ. 0) THEN
C	  ELSE IF	(JMTYPE .EQ. 2) THEN
	  ELSE IF	(JMTYPE .EQ. 6) THEN
	  ELSE IF	(JMTYPE .EQ. 8) THEN
	  ELSE
	    CALL ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),IADD)
	    SKY = GUESS3(A, STARPAR(1, I), IX, IY)
	 if(lverb.gt.20) then
	     write(6,*) ' IMPROVING STAR #', I, ' AT ', IX, IY
	 end if
	    CALL FILLERUP (BIG, NOISE, IX, IY, NFAST, NSLOW)
            if (fixxy) then
               snok = .not. OFFPIC(A,IX,IY,NFAST,NSLOW,DX,DY)
               snok = snok .and. (npt .ge. ENUFF4*IRECT(1)*IRECT(2))
               nfit = nfit0
               iit = 2
            else
 	      SNOK = TRANSMASK(BIG,NOISE,NFAST,NSLOW,IX,IY,SKY)
              nfit = nfit1
              iit = nit
            end if
	    IF (SNOK) THEN
	       starchi=chisq(onestar,xx,z,ye,npt,a,fa,c,nfit,acc,
     *               alim,iit)
	    ELSE
	  if(lverb.gt.20) write(6,*) ' STAR #,NPT,IX,IY = ', I,NPT,IX,IY
	    END IF
	    SKIP=.NOT.SNOK.OR.OFFPIC(A,IX,IY,NFAST,NSLOW,DX,DY)
	    IF (.NOT. SKIP) THEN
c	      CONVERGE = STARCHI .LT. 9e9
              CONVERGE = STARCHI .lt. 9e10
	      IF (CONVERGE) THEN			
	        IF (JMTYPE .NE. 2) THEN
		  CALL PARUPD (A, STARPAR(1,I), IX, IY)
                  call errupd(C, err, NFIT)
                  if (jmtype .ne. 3) then
                    if (toofaint(STARPAR(1,I), ERR)) jmtype=7
                  end if
c
c  Changed index.
c
                  totstar = 2*pi*starpar(2,i)*elarea(a(5),a(6),a(7))
                  varstar = totstar*eperdn
                  varsky = arect(1)*arect(2)*(sky*eperdn+rnoise**2)
                  if (varstar + varsky .gt. 0) then
                  errelec = sqrt(varstar + varsky)
                  errdn = errelec/eperdn
                  snpred = 1.086*errdn/totstar
                  if (snpred .lt. apmagmaxerr) then
		    CALL IMPAPER(BIG,NOISE,NFAST,NSLOW,I)
                  end if
                  end if
c
c  Changed index.
c
		  APPLE(4,I) = 1.086*CC(2,2)/A(2)
                  if (fixxy) apple(4,i) = 1.086*ccc(2,2)/a(2)
	        END IF
c  It doesn't make much sense to ask if it's bigger than a star if the
c  central intensity is
                if (a(2) .gt. 0) then
	        PROBG(I) = PROBGAL(ONESTAR, XX, Z, YE, NPT, A, FA)
                else
                probg(i) = -9999
                end if
	      ELSE
	        IF (JMTYPE .NE. 3) JMTYPE = 4	
	      END IF
	     CALL ADDSTAR(ONESTAR,BIG,NOISE,NFAST,NSLOW,STARPAR(1,I),ISUB)
	    ELSE
	      JMTYPE = 6	
	if(lverb.gt.20) then
	 write(6,*) ' DEACTIVATING STAR #', I, ' AT IX,IY = ',IX,IY
	end if
	    END IF
	  END IF
          imtype(i) = jmtype
          if (fixxy) imtype(i) = jmtype + 10
          fixxy = .false.
	  I = I + 1
	GO TO 2757
	END IF
	RETURN
	END
