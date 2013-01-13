c This is still called variparplane but it allows for a variable
c PSF.  You must use it with parinterp_psf.f and skyfun_quad.f
c and TUNEABLE2_9 or greater.  This was cribbed in part from the
c Schechter/Preston Bulge BHB star survey but the prefix "wid" was changed
c to "psf" throughout.  "quot" was changed to 3.
c Needs doing:  put npsffit into param_default file
c Needs doing:  add verbosity checks to the new write statements.
c
c  PKPRFMN  parameter introduced  April 29, 1994 (AS)
c  PKPRFHI and LO modification made June 5, 1994 (AS)
c
      SUBROUTINE VARIPARPLANE(NSTOT, NFAST, NSLOW)
      PARAMETER (MAXFIL = 1024)
      parameter (maxfit = 128)
      parameter (quot   = 3)
      include 'TUNEABLE'
      real*4 pkprflo, pkprfhi
      COMMON /SUBRASTER/  XY(MAXFIL), Z(MAXFIL), E(MAXFIL)
      COMMON /STARLIST/ STARPAR(NPMAX,NSMAX),IMTYPE(NSMAX),
     1     SHADOW(NPMAX,NSMAX),SHADERR(NPMAX,NSMAX)
      COMMON /PARPRED/ PARMS(npmax)
      COMMON /FITARRAYS/ A(NPMAX),FA(NPMAX),C(2*NPMAX+1,2*NPMAX+1),
     2  B(2*NPMAX), FB(2*NPMAX)
      common /skyvar/ skypar(npsky)
      common /psfvar/ psfpar(nppsf,quot)
      common /perfs/ pkprflo, pkprfhi
      dimension zz(maxfit,quot)
c     yy's are errors here
      dimension yy(maxfit,quot)		
      dimension xx(maxfit,quot)
      DIMENSION PARWT(NPMAX),ROOTS(NPMAX)
      DIMENSION WEIGHT(NPMAX),PARVAL(NPMAX),npsf(quot)
      DIMENSION SKYACC(npsky),SKYLIM(npsky)
      dimension psfacc(nppsf),psflim(nppsf)
      INTEGER K(4)
      EXTERNAL SKYFUN
      LOGICAL POSITIVE, GOODSTAR, PERFECT, type1, type3
      INTEGER*2 IXY(2)
      EQUIVALENCE (IXY,R4)
      data itpsf / 3 /
      DATA K / 1, 5, 6, 7 /
      DATA MINRMS / 5 /
      DATA MINSKY /25/
      DATA ITSKY / 100 /
      DATA SKYACC / npsky*0.001 /
      DATA SKYLIM / npsky*0.0 /
      data psfacc, psflim /  nppsf*0.001, nppsf*-10.0 /
C:    
c
      write(6,*) ' PKPRFLO: ', pkprflo, '     PKPRFHI: ', pkprfhi
c
C:    TWO PASSES, ONE TO COMPUTE EXPECTED VALUE AND ONE TO COMPUTE RMS
C: 
      DO 2757 J = 1,7
         PARVAL(j) = 0
         PARWT(j) = 0
         PARMS(j) = 0
         WEIGHT(j) = 0
         if (j .le. quot) npsf(j) = 0
 2757 CONTINUE
      NPERF = 0
      NGOOD = 0
      I = 1
      SKYMAX = -1E10
 2758 IF (I .LE. NSTOT) THEN
         GOODSTAR = SHADOW(1,I) .GT. 0
         type1 = imtype(i) .eq. 1 .or. imtype(i) .eq. 11
         type3 = imtype(i) .eq. 3 .or. imtype(i) .eq. 13
         PERFECT = GOODSTAR .AND. type1 .and. 
     $ (STARPAR(2,I) .ge. pkprflo .and. STARPAR(2,I) .le. PKPRFhi)       
         GOODSTAR = GOODSTAR .AND. (type1 .OR. type3)
C     "if perfect" loop used to be here in varipar_plane
         IF (GOODSTAR) THEN
	    IF (NGOOD .lt. MAXFIL) THEN
               IF (STARPAR(1,I) .GT. SKYMAX) THEN
                  SKYMAX = STARPAR(1,I)
c     
c     Changed indices.
c     The following look like fossils, but perhaps they're in a common block:
                  XM = STARPAR(3,I)
                  YM = STARPAR(4,I)
c     
               END IF
               NGOOD = NGOOD + 1
c     
c     Changed indices.
c     
               IXY(1) = STARPAR(3,I) + 0.5
               IXY(2) = STARPAR(4,I) + 0.5
c     
               XY(NGOOD) = R4
               Z(NGOOD) = STARPAR(1,I)
               E(NGOOD) = 1
	      parval(1) = parval(1) + starpar(1,i)
	      parwt(1) = parwt(1) + 1
	    END IF
  	    if (parwt(1) .ne. 0) ava(1) = parval(1)/parwt(1)
            if (perfect) then
               nperf = nperf + 1
               do 1000 j = 5, 7
                  if (shadow(j,i) .le. 0 .and. j .ne. 6) then
                     write(6,*) 
     1               ' WARNING: Negative value for a(j) at j,i = ', j,i
                  else if (shaderr(j,i) .le. 0) then
                     write(6,*) ' WARNING: Negative value for shaderr(j)
     1                   at j,i = ', j,i
                  else if (npsf(j-4) .ge. maxfit) then
                  else
                     npsf(j-4) = npsf(j-4) + 1
                     if (j .eq. 6) then
                        zz(npsf(j-4),j-4) = shadow(j,i)
                        yy(npsf(j-4),j-4) = shaderr(j,i)
                     else
                        zz(npsf(j-4),j-4) = alog(shadow(j,i))
c:                      errln = sqrt(shaderr(j,i))/shadow(j,i)
                        errln2 = shaderr(j,i)/shadow(j,i)**2
                        yy(npsf(j-4),j-4) = errln2
                     end if
                     wt = 1/shaderr(j,i)
                     parval(j) = parval(j) + wt*shadow(j,i)
                     parwt(j) = parwt(j) + wt
c                    xx(npsf(j),j) = (shadow(2,i) - 0.5*nfast)/nfast
c                    it's perfect so it must have passed through good
                     xx(npsf(j-4),j-4) = r4
                  end if
 1000          continue
            end if
C     
         END IF
         I = I + 1
         GO TO 2758
      END IF
      if(lverb.gt.10) then
         write(6,*) '# of stars available for computing typical 
     +        SHAPE (Nperf) = ', NPERF
         write(6,*) '# of stars available for computing MODEL 
     +        SKY (Ngood) = ', NGOOD
      end if
c     note nothing gets computed if there are no perfect stars
      IF (NPERF .GE. 1) THEN
         DO 2760 J = 1, 4
	    PARVAL(K(J)) = PARVAL(K(J))/PARWT(K(J))
	    AVA(K(J)) = PARVAL(K(J))
 2760    CONTINUE
         if(lverb.gt.10) write(6,*) 'WEIGHTED MEANS: ', PARVAL
c     
         write(6,*) ' Parameter # and coefficients :'
         do 1100 j = 5, 7
            if (npsf(j-4) .ge. 2*npsffit) then
               call chisq(skyfun,xx(1,j-4),zz(1,j-4),yy(1,j-4),
     1         npsf(j-4),psfpar(1,j-4),fa,c,npsffit,psfacc,psflim,itpsf)
            else
               psfpar(1,j-4) = ava(j)
               if (j .ne. 6) psfpar(1,j-4) = alog(ava(j))
               do 1200 l = 2,nppsf
                  psfpar(l,j-4) = 0
 1200          continue
            end if
            write(6,*) j, (psfpar(l,j-4),l=1,nppsf)
 1100    continue
C     
         IF (NGOOD .GE. MINSKY )THEN
	    IF (SKYPAR(1) .EQ. 0) THEN
               SKYPAR(1) = AVA(1)
               DO 2761 L = 2, NPMAX
                  SKYPAR(L) = 0
 2761          CONTINUE
	    END IF
c     
c     Call the appropriate sky function.
c     
            CALL CHISQ(SKYFUN,XY,Z,E,NGOOD,
     *           SKYPAR,FA,C,NSKYFIT,SKYACC,SKYLIM,ITSKY)
c     
         ELSE
	    SKYPAR(1) = AVA(1)
	    DO 2762 L = 2, NPMAX
               SKYPAR(L) = 0
 2762       CONTINUE
         END IF
         if(lverb.gt.10) write(6,*) 'SKYFUN PARAMETERS: '
         if(lverb.gt.10) write(6,*) (SKYPAR(I),I=1,NSKYFIT)
C     
         IF (NPERF .GE. MINRMS) THEN
	    DO 2763 I = 1, NSTOT
               PERFECT = IMTYPE(I) .EQ. 1 .or. imtype(i) .eq. 11		
               PERFECT = PERFECT .AND. SHADOW(1,I) .NE. 0	
               PERFECT = PERFECT .and. (STARPAR(2,I) .ge. PKPRFlo
     $         .and. STARPAR(2,I) .le. PKPRFhi)
               IF (PERFECT) THEN
c     
c     Changed indices.
c     
                  CALL PARINTERP(STARPAR(3,I),STARPAR(4,I),A)
c     
                  DO 2764 J = 1, 4
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:    I HAVE CONVINCED MYSELF THAT THE UNCERTAINTY PER RESIDUAL SQUARED IS
C:    IS 2*DELTA*(O-C).  BUT IF I USE 1/THIS AS A WEIGHT, ACCIDENTAL CANCELLATIONS
C:    GIVE SCREWEY RESULTS.  SO WE'LL TAKE THE LARGER OF O-C**2 AND SHADERR.
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                     SQ = (SHADOW(K(J),I)-A(K(J)))**2
                     WT = 1/SHADERR(K(J),I)/AMAX1(SHADERR(K(J),I),SQ)
                     WEIGHT(K(J)) = WEIGHT(K(J)) + WT
                     PARMS(K(J)) = PARMS(K(J)) + WT*SQ
 2764             CONTINUE
               END IF
 2763       CONTINUE
	    POSITIVE = .TRUE.
	    DO 2765 J = 1, 4
               PARMS(K(J)) = PARMS(K(J))/WEIGHT(K(J))
               POSITIVE = POSITIVE .AND. PARMS(K(J)) .GE. 0
               IF (PARMS(K(J)) .GE. 0) THEN
                  ROOTS(K(J)) = SQRT(PARMS(K(J)))
               ELSE
                  if(lverb.gt.10) write(6,*) 'NEGATIVE SCATTER :'
                  if(lverb.gt.10) write(6,*) 'PARAM# & SCATTER = ' ,
     1                 K(J), PARMS(K(J))
                  PARMS(K(J)) = 0
                  ROOTS(K(J)) = 0
               END IF
 2765       CONTINUE
            if(lverb.gt.10) write(6,*) 'SCATTER: ', ROOTS
         END IF
      ELSE
         SKYPAR(1) = AVA(1)
         DO 2766 L = 2, NPMAX
	    SKYPAR(L) = 0
 2766    CONTINUE
         if(lverb.gt.10) write(6,*) 'SKYFUN PARAMETERS: '
         if(lverb.gt.10) write(6,*) (SKYPAR(I),I=1,NSKYFIT)
      END IF
c     
      RETURN
      END

