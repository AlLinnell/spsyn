      SUBROUTINE TPWL(FL,WL,RWL,LWL,NWL,FTP)
C     Subroutine to produce an interpolated flux value at wavelength
C        RWL
C     FL=array of flux values of length NWL
C     WL=corresponding array of wavelengths in ascending order
C     RWL=reference wavelength for which an interpolated flux value
C        is needed
C     LWL=ordinal value of WL entry immediately preceding the RWL of
C        the last previous interpolation
C     NWL=number of entries in arrays FL and WL
C     FTP=interpolated flux value
      DIMENSION FL(NWL),WL(NWL)
      FTP=0.0
      WL1=WL(1)
      WL2=WL(NWL)
      IF ((RWL.LT.WL1).OR.(RWL.GT.WL2)) GO TO 99
      IF ((LWL.LE.1).OR.(LWL.GE.NWL)) LWL=1
      IF (RWL.GE.WL(LWL)) THEN
         DO J=1,NWL
            IF (RWL.EQ.WL(LWL+J)) GO TO 30
            IF (J.EQ.NWL) GO TO 30
            IF (LWL+J.EQ.NWL) GO TO 30
            IF (LWL+J.GT.NWL) CALL PMDSTOP
            IF ((WL(LWL+J-1).LE.RWL).AND.(WL(LWL+J).GT.RWL)) GO TO 10
         END DO
         CALL PMDSTOP
   10    LWL=LWL+J-1
         CALL LNTER1(WL(LWL),FL(LWL),WL(LWL+1),FL(LWL+1),RWL,FTP)
         GO TO 99
      ELSE
         DO J=1,NWL
            IF (RWL.EQ.WL(LWL-J)) GO TO 50
            IF (LWL-J.EQ.1) GO TO 40
            IF (LWL-J.LT.1) CALL PMDSTOP
            IF ((WL(LWL-J).LE.RWL).AND.(WL(LWL-J+1).GT.RWL)) GO TO 20
         END DO
         CALL PMDSTOP
   20    LWL=LWL-J
         CALL LNTER1(WL(LWL),FL(LWL),WL(LWL+1),FL(LWL+1),RWL,FTP)
      END IF
      GO TO 99
   30 FTP=FL(LWL+J)
      GO TO 99
   40 FTP=FL(1)
      GO TO 99
   50 FTP=FL(LWL-J)
   99 RETURN
      END
