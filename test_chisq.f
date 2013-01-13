      FUNCTION CHISQ (FUNCTN,X,Y,YE,N,A,FA,C,M,ACC,ALIM,IT)
      PARAMETER (MMAX = 11)
      common /tune14/ lverb
      DIMENSION A(m+1), FA(m+1), C(M+1,M+1), ACC(m+1), ALIM(m+1)
      CHARACTER*80 FMT1
      EXTERNAL FUNCTN
      LOGICAL CONV, MARQ, LIMIT
c      real*8 chi,chiold
      DIMENSION X(n), Y(n), YE(n)
      DIMENSION V(MMAX), B(MMAX,MMAX), INDX(MMAX)
      DATA FMT1 /'(I3,I2,1P11E10.2)'/
      DATA IZ /0/
c     
      IF(M.GT.MMAX-1) RETURN
      CONV = .FALSE.
      LIMIT = .FALSE.
      DO 2757 J=1,M
C     
C     CHECK IF PARAMETERS EXCEEDS LIMITS AT THE START.  CHECK ONLY
C     ABSOLUTE PARAMETERS (ALIM(J) < 0).
C     
         IF (ALIM(J) .LT. 0.0) THEN
            LIMIT = ABS(A(J)).GT.ABS(ALIM(J))
            IF(LIMIT) then
            if(lverb.gt.20) 
     *  write(6,*) 'SELF-DECEPTION HAS OCCURED: INITIAL LIMITS.'
            go to 9876
            end if
         END IF
 2757 CONTINUE
 9876 continue
      if(lverb.gt.30) write(6, FMT1) IZ,IZ,(A(KK),KK=1,M)
      IFACT = 0
      I = 1
 2758 IF ((I .LE. IT) .AND. (.NOT. CONV) .AND. (.NOT. LIMIT)) THEN
         CHI = 0.0
         DO 2759 J=1,M
	    B(J,M+1)=0
	    DO 2760 KK=1,M			
               B(J,KK)=0
 2760       CONTINUE
 2759    CONTINUE
         DO 2761 J=1,N
	    F = FUNCTN(X(J), A, FA, M, MMAX) - Y(J)
            F2 = F**2
            YE1 = 1.0/YE(J)
 	    CHI = CHI + F2*YE1
	    DO 2762 KK=1,M
               IF (FA(KK) .NE. 0) THEN
                  FAKK = FA(KK)/YE(J)
                  B(KK,M+1)=B(KK,M+1)+FAKK*F
                  DO 2763 L=1,KK
                     IF (FA(L) .NE. 0)
     1                    B(KK,L)=B(KK,L)+FAKK*FA(L)	
 2763             CONTINUE
               END IF
 2762       CONTINUE
 2761    CONTINUE
         CHIOLD = CHI
         K = 1
         MARQ = .FALSE.
 2764    IF ((K .LE. 10) .AND. (.NOT. MARQ) .AND. (.NOT. LIMIT)) THEN
	    CONV = (K .EQ. 1)
	    IF (K .EQ. 1)  FACT = 0.
	    IF (K .NE. 1)  FACT = 2.**IFACT
	    DO 2765 J=1,M
               L = 1
 2766          IF (L .LT. J) THEN
                  C(J,L)=B(J,L)
                  C(L,J)=B(J,L)
                  L = L + 1
                  GO TO 2766
               END IF
               C(J,J)=(1+FACT)*B(J,J)
               V(J) = B(J,M+1)
 2765       CONTINUE
	    CALL LUDCMP(C,M,M+1,INDX,D)
	    CALL LUBKSB(C,M,M+1,INDX,V)
	    DO 2767 J=1,M
               A(J)=A(J)-V(J)
C     
C     CHECK IF CHANGE IN PARAMETERS EXCEEDS LIMITS.  IF ALIM(J) > 0, THEN
C     CONSIDER FRACTIONAL CHANGES.  IF ALIM(J) < 0, CONSIDER ABSOLUTE
C     CHANGES.  IF ALIM(J) = 0, IGNORE THIS TEST.
C     
               IF (ALIM(J) .GT. 0.0) THEN
                  LIMIT = LIMIT.OR.(ABS(V(J)/A(J)).GT.ALIM(J))
                  IF(LIMIT) then
                  if(lverb.gt.20) 
     *    write(6,*) 'SELF-DECEPTION HAS OCCURED: FRAC LIMITS.'
                  go to 9875
                  end if
               ELSE IF (ALIM(J) .LT. 0.0) THEN
                  LIMIT = LIMIT.OR.(ABS(A(J)).GT.ABS(ALIM(J)))
                  IF(LIMIT) then
                  if(lverb.gt.20)
     *    write(6,*) 'SELF-DECEPTION HAS OCCURED: ABS LIMITS.'
                  go to 9875
                  end if
               END IF
C     
               IF (ACC(J) .GE. 0) THEN
                  CONV=CONV.AND.(ABS(V(J)/A(J)).LE.ACC(J))
               ELSE
                  CONV=CONV.AND.(ABS(V(J)).LE.ABS(ACC(J)))
               END IF	  
 2767       CONTINUE
 9875       continue
c     
            if(lverb.gt.30) write(6, FMT1) I,K,(A(KK),KK=1,M)
	    IF (CONV) THEN		
               MARQ = .TRUE.
	    ELSE IF (.NOT. LIMIT) THEN
               CHI = 0.0
               DO 2768 J = 1, N
                  F = FUNCTN(X(J), A, FA, m, MMAX) - Y(J)
                  F2 = F**2
                  YE1 = 1.0/YE(J)
                  CHI = CHI + F2*YE1
 2768          CONTINUE
               IF (K .EQ. 2) IFACT = IFACT - 1
               IF (CHI .LT. (1.0001 * CHIOLD)) THEN
                  MARQ = .TRUE.
               ELSE
                  IF(K .EQ. 2)  IFACT = IFACT + 1
                  IF (K .GE. 2) IFACT = IFACT + 1
                  IF(IFACT.GT.10)GO TO 50
                  DO 2769 J=1,M
                     A(J)=A(J)+V(J)
 2769             CONTINUE
               END IF
	    END IF
	    K = K + 1
            GO TO 2764
         END IF
         I = I + 1
         GO TO 2758
      END IF
 50   CONTINUE
C     
      IF(.NOT. LIMIT) THEN
         DO 2770 J = 1, M			
            DO 2771 I = 1, M
               B(I,J) = 0
 2771       CONTINUE
            B(J,J) = 1
            CALL LUBKSB(C,M,M+1,INDX,B(1,J))
 2770    CONTINUE
      END IF
C     
      IF (CONV .AND. (.NOT. LIMIT)) THEN
         PERDEG = SQRT(CHI/MAX0(N-M,1))
         DO 2772 I=1,M
	    IF (B(I,I) .GT. 0) THEN
               SAVE=SQRT(B(I,I))
	    ELSE
               if(lverb.gt.20) write(6,*) 
     *  'TROUBLE: NEGATIVE AUTOVARIANCE FOR I, B(I,I) = ',I,B(I,I)
               SAVE = 1E10
	    END IF
	    DO 2773 J=1,M
               C(I,J)=B(I,J)/SAVE
               C(J,I)=B(J,I)/SAVE
 2773       CONTINUE
	    C(I,I)=SAVE*PERDEG
 2772    CONTINUE
         CHISQ=CHIOLD
      ELSE
         CHISQ = 1E10
      END IF
      RETURN
      END
