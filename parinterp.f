	FUNCTION PARINTERP(X, Y, STARPAR)
        include 'TUNEABLE'
        common /skyvar/ skypar(npsky)
        common /hubvar/ hubpar(nphub)
	COMMON /UNDERGND/ NFAST, NSLOW
	COMMON /DRFAKE/ NEEDIT
        common /median/ MEDPIX
	LOGICAL NEEDIT
	DIMENSION STARPAR(NPMAX), DUMMY(NPMAX)
	INTEGER K(4)
	INTEGER*2 IXY(2)
        integer*2 MEDPIX(nrmax,ncmax)
c
	IXY(1) = X + 0.5
	IXY(2) = Y + 0.5
	DATA K / 1, 5, 6, 7 /
c
	DO 2757 I = 2, 4
	  STARPAR(K(I)) = AVA(K(I))
2757	CONTINUE
	NEEDIT = .FALSE.
c
c  Decide which sky function to use.
c
      if(flags(2)(1:5).eq.'PLANE') STARPAR(1) = 
     *     SKYFUN(IXY, SKYPAR, DUMMY, NPSKY, NPSKY)
      if(flags(2)(1:5).eq.'HUBBL') STARPAR(1) = 
     *     HUBFUN(IXY, HUBPAR, DUMMY, NPHUB, NPHUB)
      if(flags(2)(1:5) .eq. 'MEDIA') STARPAR(1) = 
     *   MEDPIX( min(max(nint(X),1),NFAST), min(max(nint(Y),1),NSLOW) )
c
	NEEDIT = .TRUE.
	PARINTERP = STARPAR(1)
	RETURN
	END
