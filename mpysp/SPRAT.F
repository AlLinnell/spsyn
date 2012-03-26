      SUBROUTINE SPRAT(N1,N2,RF,KK,KEY)
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C   
      COMMON /CA/OFL(300000),OWL(300000),FL(300000),WL(300000),
     $CFL(300000),WLB(300000),FLB(300000)
C     Scale second synthetic spectrum by factor RF
C     N2 = number of entries in array WL
      DO J=1,N2
         FL(J)=FL(J)*RF
      END DO
C     N1 = number of data values in array OWL
C     Find second spectrum wavelength ordinal position closest to
C     given entry in first spectrum.
C     Both spectra must have wavelengths in ascending order.
      KK=0
      DO 2 K=1,N1
	WLV=OWL(K)
	IF (WLV.LT.WL(1)) THEN
		IF (KEY.EQ.1) THEN
			GO TO 3
		ELSE
			GO TO 2
		END IF
	END IF
c	IF (WLV.GT.WL(N2)) RETURN
	IF (WLV.GT.WL(N2)) THEN
		IF (KEY.EQ.1) THEN
			GO TO 3
		ELSE
			GO TO 2
		END IF
	END IF
	KK=KK+1 
      DO 1 J=1,N2
		IF (WL(J).EQ.WLV) GO TO 4
		IF ((WL(J+1).GT.WLV).AND.(WL(J).LE.WLV)) THEN
			DF=WL(J+1)-WL(J)
			CF=WLV-WL(J)
			DFLX=FL(J+1)-FL(J)
			WLB(KK)=OWL(K)
			FLB(KK)=FL(J)+CF/DF*DFLX
			CFL(KK)=FLB(KK)
			FLB(KK)=OFL(K)*FLB(KK)
			GO TO 2
		END IF
    1 END DO
      IF (WL(N2).EQ.OWL(N1)) THEN
		GO TO 2
	ELSE
		CALL PMDSTOP
	END IF
    3 KK=KK+1
      WLB(KK)=WLV
	FLB(KK)=OFL(K)
    2 END DO
      RETURN
    4 WLB(KK)=WLV
	CFL(KK)=FL(J)
	FLB(KK)=FL(J)*OFL(K)
	GO TO 2
      END
