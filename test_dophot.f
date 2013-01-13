	PROGRAM DOPHOT
	INCLUDE 'TUNEABLE'
c
c
	PARAMETER (NAPPLE = 5)
c
        COMMON /STARLIST/ STARPAR(NPMAX,NSMAX), IMTYPE(NSMAX),
     1     SHADOW(NPMAX,NSMAX),ERRSHAD(NPMAX,NSMAX)
	COMMON /APERLIST/ APPLE(NAPPLE ,NSMAX)
	COMMON /UNITIZE/ UFACTOR
	COMMON /UNDERGND/ NFAST, NSLOW
	COMMON /CTIMES/ CHIIMP, APERTIME, FILLTIME, ADDTIME
	COMMON /DRFAKE/ NEEDIT
        common /search/ nstot,thresh
	common /probpass/ probg(nsmax)
	COMMON /ORIGINAL/ NOISE(nrmax,ncmax)
	common / / ibig(ncmax*nrmax/2)
        common /median/ MEDPIX 
        common /trans7/ test7
        common /fixpass/ fixxy
c
	LOGICAL needit, first, warm, test7, fixxy
c
	character*80 header(100)
        character*128 outputline
c
        integer*2 MEDPIX(nrmax,ncmax)
        integer*4 jhxwid,jhywid,PASSCNT,NPPP,mprec
	INTEGER*4 IMG(4), NOI(4)
c
	EXTERNAL PSEUD2D, PSEUD4D
c
	DATA IMG(3), NOI(3) / 98, 99 /		
	data needit,fixxy / .TRUE. , .false. /
	data first, test7 / .true. , .false. /
c
        logv = 100
        ufactor = 100
c
        CALL TUNEUP(files,flags)
c
c  For debugging.
c      call printparams
c
c  Open log file if desired.
c
      if(lverb.gt.0.and.files(6)(1:4).ne.'TERM') 
     *      call opena(6,files(6),2,ierr)
      lverb = lverb*10 + 1
c
c  Open input picture.
c
      nhead = 250
      if(flags(9)(1:3).eq.'FIT') 
     *     call rffile(nhead,header,nx,ny,ibig,files(1))
c      if(flags(9)(1:3).eq.'PDM') 
c     *      call readpdmf(nhead,header,nx,ny,ibig,files(1))
c      if(flags(9)(1:3).eq.'DTA') 
c     *      call readdtaf(nhead,header,nx,ny,ibig,files(1))
c      if(flags(9)(1:3).eq.'LOC') 
c     *      call readlocalf(nhead,header,nx,ny,ibig,files(1))
      if(flags(9)(1:3) .ne. 'FIT') then 
        write(6,*) ' only FITS format available in UNIX... stopping'
        stop
      end if 
      nfast = nx
      nslow = ny
c
      if(nthpix .le. 1) nthpix = sqrt(float(nfast))
c
      if(flags(2)(1:5) .eq. 'MEDIA') then
        if(lverb.ge.10) then
          write(6,*) ' Median box half-size (X,Y): ', jhxwid, jhywid
          write(6,*) ' Median precison in DN :', mprec
        end if
        PASSCNT = 0
      end if
c
      if(lverb .ge. 10) then 
          write(6,*) ' NTHPIX (initial sky update freq. in pixels) :',
     *       nthpix
      end if
c
	WARM = (flags(6)(1:1).eq.'Y')
	CALL MAKENOISE(ibig,noise,NFAST,NSLOW)
	IF (WARM) CALL WARMSTART(PSEUD2D,ibig,noise,NFAST,
     *     NSLOW,files(3))
        if(WARM) call wffile(nhead,header,nx,ny,ibig,'firstsub.fit') 
c
	THRESH = TMIN
c
        NPPP = 1
2757 	IF (THRESH*2**TFAC .LE. TMAX) THEN
          NPPP = NPPP + 1
	  THRESH = THRESH*2**TFAC
	GO TO 2757
	END IF
c
2758 	IF (THRESH/TMIN .GE. 0. 999) THEN
c
c	  SKY = GUESS1(A, STARPAR, IX, IY)			!FOSSIL?
c
	  if(lverb.gt.10) then
            write(6,*)
            write(6,*) 
     *        'Starting loop at threshold level ', THRESH
          end if
c
      if(flags(2)(1:5) .eq. 'MEDIA') then
        if(PASSCNT .eq. 0 .or. PASSCNT .eq. (NPPP-4) ) then
c
          if(lverb.ge.10) then
            write(6,*) ' (re-)Making median background picture '
          end if
c
          call MEDFIL(nfast,nslow,ibig,MEDPIX,ibot,itop,jhxwid,
     *                 jhywid,nrmax,ncmax,mprec)
        else 
          if(lverb.ge.10) then
            write(6,*) ' Skipping median filtering on this pass'
          end if
        end if
        PASSCNT = PASSCNT + 1
        call VARIPARPLANE(NSTOT,NFAST,NSLOW)
      end if
      if(flags(2)(1:5).eq.'PLANE') 
     *     CALL VARIPARPLANE(NSTOT,NFAST,NSLOW)
      if(flags(2)(1:5).eq.'HUBBL') 
     *     CALL VARIPARHUB(NSTOT,NFAST,NSLOW)
c
c: need varipar because improve calls guess3 which calls skyfun
c: need makemask because improve calls snok which needs mask
c
          call makemask(pseud2d)
          if (fixpos .and. first) call
     *      IMPROVE(PSEUD2D,IBIG,NOISE,NFAST,NSLOW)
          first = .false.
c
	  NSTAR = ISEARCH(PSEUD2D,IBIG,noise,NFAST,NSLOW)
C	  IF (NSTAR .NE. 0) THEN
	    CALL SHAPE(PSEUD2D,PSEUD4D,IBIG,NOISE,NFAST,NSLOW)
	    CALL PARAVG					
	    IF (NSTOT .GE. 1) then
      if(flags(2)(1:5) .eq. 'MEDIA')
     *     CALL VARIPARPLANE(NSTOT,NFAST,NSLOW)
      if(flags(2)(1:5).eq.'PLANE') 
     *     CALL VARIPARPLANE(NSTOT,NFAST,NSLOW)
      if(flags(2)(1:5).eq.'HUBBL') 
     *     CALL VARIPARHUB(NSTOT,NFAST,NSLOW)
            END IF
	    CALL IMPROVE(PSEUD2D,IBIG,NOISE,NFAST,NSLOW)
C	  END IF
c
	  if(lverb.gt.10) then
          write(6,*) ' Ending loop at threshold level ',THRESH
          write(6,*) 
     *    ' Number of new objects found on this threshold = ',NSTAR
          write(6,*)
     *    ' Total number of objects found so far = ',NSTOT
          end if
c
	  if(PASSCNT .ne. NPPP .and. PASSCNT .ne. NPPP+1) then 
                  THRESH = THRESH/2**TFAC
          end if
c..........  allows 2 cycles of iteration at lowest threshold ........
c
          call opena(3,files(4),2,ierr)
          if(flags(4)(1:1).eq.'Y') call opena(4,files(5),2,ierr)
c
	  DO 2759 I = 1, NSTOT
c
c  Decide what kind of output is needed.
c
          if(flags(3)(1:5).eq.'INTER') then
            call sumout(i,imtype(i),starpar(1,i),npar,apple(1,i),
     *      napple,probg(i))
          else if(flags(3)(1:5).eq.'COMPL') then
            call stdotpt(i,imtype(i),starpar(1,i),npar,apple(1,i),
     *      napple,probg(i),outputline)
            write(3,999) outputline(1:128)
          else if(flags(3)(1:5).eq.'INCOM') then
            call badotpt(i,imtype(i),starpar(1,i),npar,apple(1,i),
     *      napple,probg(i),outputline)
            write(3,999) outputline(1:80)
          end if
c
 999      format(a)
c
          if(flags(4)(1:1) .eq. 'Y') then
	    IF (SHADOW(1,I) .NE. 0) THEN
              call shdout(i,imtype(i),shadow(1,i),npar)
	    ELSE
              call shdout(i,imtype(i),starpar(1,i),npar)
	    END IF
          end if
2759	  continue
c
          close(unit=3)
          if(flags(4)(1:1).eq.'Y') close(unit=4)
c
      if(flags(5)(1:1).eq.'Y') then
      if(flags(9)(1:3).eq.'FIT') 
     *     call wffile(nhead,header,nx,ny,ibig,files(2))
c      if(flags(9)(1:3).eq.'PDM') 
c     *     call writepdmf(nhead,header,nx,ny,ibig,files(2))
c      if(flags(9)(1:3).eq.'DTA') 
c     *     call writedtaf(nhead,header,nx,ny,ibig,files(2))
c      if(flags(9)(1:3).eq.'LOC') 
c     *     call writelocalf(nhead,header,nx,ny,ibig,files(2))
      end if
c
	GO TO 2758
	END IF
c
c.........................................................
c       Do curve of growth aperture correction.
c       
        if(flags(10)(1:1).eq.'Y') then 
          write(6,*) ' doing 1st set of apcorr measures: '
          call apcorr2(ibig,noise,pseud2d)
          write(6,*) ' doing 2nd set of apcorr measures: '
          call apcorrxtra(ibig,noise,pseud2d)
        end if
c.........................................................
c       
c
	CALL EXIT
	END
c
c  Subroutine to write INTERNAL output for a summary file.
c
        subroutine sumout(i,imtype,starpar,npar,apple,napple,probg)
        real starpar(npar),apple(napple)
c
        write(3,100) i,imtype,starpar(1),starpar(3),starpar(4),
     *       starpar(2),(starpar(k),k=5,npar),
     *       (apple(kk),kk=1,4),probg
 100    format(i5,i3,1pe11.4,0p2f11.2,1p6e11.3,0p2f8.3,1pe10.3)
c
        return
        end
c
c  Subroutine to write INTERNAL output for a shadow file.
c
        subroutine shdout(i,imtype,starpar,npar)
        real starpar(npar)
c
        write(4,100) i,imtype,starpar(1),starpar(3),starpar(4),
     *       starpar(2),(starpar(k),k=5,npar)
 100    format(i5,i3,1pe11.4,0p2f11.2,1p6e11.3)
c
        return
        end









