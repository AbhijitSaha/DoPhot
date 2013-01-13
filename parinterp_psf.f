	FUNCTION PARINTERP(X, Y, STARPAR)
        include 'TUNEABLE'
        common /skyvar/ skypar(npsky)
        common /hubvar/ hubpar(nphub)
        common /psfvar/ psfpar(nppsf,3)
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
c klugy check for zero leading term on psfpar
	DO 2757 I = 2, 4
          if (psfpar(1,i-1) .ne. 0) then
	  STARPAR(K(I)) =
     &    skyfun(ixy,psfpar(1,i-1),dummy,npsffit,nppsf)
          if (i .ne. 3) starpar(k(i)) = exp(starpar(k(i)))
          else
          starpar(k(i)) = ava(k(i))
          end if
2757	CONTINUE
	NEEDIT = .FALSE.
c
c  Decide which sky function to use.
c
      if(flags(2)(1:5).eq.'PLANE') STARPAR(1) = 
     *     SKYFUN(IXY, SKYPAR, DUMMY, NSKYfit, NPSKY)
      if(flags(2)(1:5).eq.'HUBBL') STARPAR(1) = 
     *     HUBFUN(IXY, HUBPAR, DUMMY, NPHUB, NPHUB)
      if(flags(2)(1:5) .eq. 'MEDIA') STARPAR(1) = 
     *     MEDPIX( nint(X), nint(Y) )
c
	NEEDIT = .TRUE.
	PARINTERP = STARPAR(1)
	RETURN
	END
