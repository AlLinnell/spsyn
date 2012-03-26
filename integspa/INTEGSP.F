      PROGRAM INTEGSP
C     Program to multiply a spectrum by a second spectrum and integrate the product
C     spectrum
C     The program reads in the first spectrum to OWL1,OFL1
C     The program reads in the second spectrum to OWL2,OFL2
C     It reads NSD, an integer specifying the number of subdivisions over which
C     integration will take place.
C     It then interpolates from the first spectrum to NSD+1 points in OINTW1, OINTF1
C     It interpolates from the second spectrum to NSD+1 points in OINTW2, OINTF2
C     It calculates a product function and corresponding wavelengths in CWL, CONV
C     Finally, it integrates the product function in CONV and writes the output to
C        PROD.DAT
C     The control file is INTEGSP.INF in directory /ia
C     The output file is PROD.DAT in directory /oa
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION OFL1(200000),OWL1(200000),OFL2(200000),OWL2(200000),
     $ OINTW1(200000),OINTF1(200000),CWL(200000),CONV(200000),
     $OINTW2(200000),OINTF2(200000)
      DIMENSION FL1(10)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 48 FL1
      CHARACTER * 7 LABEL
      CHARACTER * 80 FMT1,FMT2
      DATA IN1,IN2,IN3,NOUT1,NOUT2,NOUT3,NOUT4/1,2,3,4,55,6,7/
      DATA ER/1.D-8/
  100 FORMAT (A48)
C  101 FORMAT (F15.6,E15.6)
  102 FORMAT (F15.8,2X,E18.8)
  103 FORMAT (I5,F15.8,2X,E18.8)
  105 FORMAT (' ')
C  103 FORMAT (3F15.8)
  104 FORMAT ('Value of VMG=',3X,F15.10,/,'Value of SM1=',3X,E15.10,/,
     $'Value of SM2=',3X,E15.10)
C*********************************************************************
      OPEN (UNIT = 10,     FILE ='INTEGSP.ILS',
     $STATUS = 'OLD')
      READ ( 10, 100)       (FL1(J), J=1,6)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT3, FILE = FL1(6),  STATUS = 'REPLACE')
C      OPEN ( UNIT = NOUT4, FILE = FL1(7),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '**INTSP'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read first data file format
      CALL CREAD ( FMT1)
      CALL DUMMY(1)
C     Read second data file format
      CALL CREAD ( FMT2)
      CALL DUMMY(1)
C     Read starting wavelength
      CALL DREAD (WST,  'F8.1' )
      WST=WST*1.0D-8
      CALL DUMMY(1)
C     Read ending wavelength
      CALL DREAD (WND,  'F8.1' )
      WND=WND*1.0D-8
      CALL DUMMY(1)
C     Read number of subintervals
      CALL IREAD (NSD)
C      CALL DUMMY(1)
      CALL IREAD (KEY)
C      KEY=1
C     Be sure NSD is even number
      NSDA=NSD/2
      NSD=2*NSDA
C
      RG=WND-WST
      K=NSD
      KCT=0
      PI=3.1415926535898D0
      DO J=1,200000
          READ ( IN2, FMT1, END=1, ERR=90) X,Y
          X=X*1.0D-8
          IF ((X.GE.WST).AND.(X.LE.WND)) THEN
             KCT=KCT+1
             OWL1(KCT)=X
             IF (KEY.EQ.0) OFL1(KCT)=Y
             IF (KEY.EQ.1) OFL1(KCT)=Y*X
C             WRITE (NOUT2,102) OWL1(KCT)*1.0D8,OFL1(KCT)
             zzz=0.d0
          END IF
      END DO
      CALL PMDSTOP
C     NOWL1=Number of wavelengths
    1 NOWL1=KCT
C      NVL=NSD
C      DO J=1,NVL+1
C        A=PI*(J-1)/NVL
C        B=1.D0
C        OWL1(J)=A
C        OFL1(J)=B
C        WRITE (NOUT1,103) J,A,B 
C      END DO
C      WRITE (NOUT1,105)
C      NOWL1=J-1
C
      KCT=0
      DO J=1,200000
          READ ( IN3, FMT2, END=2, ERR=90) X,Y
          X=X*1.0D-8
          IF ((X.GE.WST).AND.(X.LE.WND)) THEN
             KCT=KCT+1
             OWL2(KCT)=X
             IF (KEY.EQ.0) OFL2(KCT)=Y
             IF (KEY.EQ.1) OFL2(KCT)=Y*X
             zzz=0.d0
          END IF
      END DO
      CALL PMDSTOP
    2 NOWL2=KCT
C      DO J=1,NVL+1
C        A=PI*(J-1)/NVL
C        B=DSIN(A)
C        OWL2(J)=A
C        OFL2(J)=B
C        WRITE (NOUT1,103) J,A,B 
C      END DO
C      WRITE (NOUT1,105)
C      NOWL2=J-1
C
C     Interpolate to equal spacings in wavelength,
C     first spectrum
      IF (K.LT.NOWL1-1) CALL PMDSTOP
      DO L=1,K+1
         A=RG*(L-1)/K+WST
C         A=PI*(L-1)/K
         OINTW1(L)=A
         zzz=0.d0
      END DO
      KKST=1
      DO L=1,K+1
        A=OINTW1(L)
       DO KK=KKST,NOWL1-1
      RWL1=OWL1(KK)
      RWL2=OWL1(KK+1)
        IF ((A.LT.RWL1).AND.(A.LT.RWL2)) THEN
           OINTF1(L)=0.D0
           GO TO 10
        END IF
        IF (DABS(A-RWL1).LE.ER) THEN
           OINTF1(L)=OFL1(KK)
           GO TO 10
        END IF
        IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
           DF=RWL2-RWL1
           DFA=OINTW1(L)-RWL1
           H=DFA/DF
           C=OFL1(KK)
           D=OFL1(KK+1)
           DLT=D-C
           DLTA=DLT*H
           OINTF1(L)=C+DLTA
           GO TO 10
        END IF
        IF (DABS(A-RWL2).LE.ER) THEN
           OINTF1(L)=OFL1(KK+1)
        END IF
      END DO
   10 CONTINUE
      WRITE (NOUT2,102) OINTW1(L)*1.0D8,OINTF1(L)
      zzz=0.d0
C     Set scan to start just before last scanned point.
      IF (KK.GT.2) KKST=KK-1
      END DO
C    Interpolate second function to equal wavelength spacings,
C    second spectrum
      IF (K.LT.NOWL2-2) CALL PMDSTOP
      DO L=1,K+1
         A=RG*(L-1)/K+WST
C         A=PI*(L-1)/K
         OINTW2(L)=A
         zzz=0.d0
      END DO
      KKST=1
      DO L=1,K+1
        A=OINTW2(L)
       DO KK=KKST,NOWL2
      RWL1=OWL2(KK)
      RWL2=OWL2(KK+1)
        IF ((A.LT.RWL1).AND.(A.LT.RWL2)) THEN
           OINTF2(L)=0.D0
           GO TO 20
        END IF
        IF (DABS(A-RWL1).LE.ER) THEN
           OINTF2(L)=OFL2(KK)
           GO TO 20
        END IF
        IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
           DF=RWL2-RWL1
           DFA=OINTW2(L)-RWL1
           H=DFA/DF
           C=OFL2(KK)
           D=OFL2(KK+1)
           DLT=D-C
           DLTA=DLT*H
           OINTF2(L)=C+DLTA
           GO TO 20
        END IF
        IF (DABS(A-RWL2).LE.ER) THEN
           OINTF2(L)=OFL2(KK+1)
        END IF
      END DO
   20 CONTINUE
      WRITE (NOUT3,102) OINTW2(L)*1.0D8,OINTF2(L)/1.0D-9
      zzz=0.d0
C     Set scan to start just before last scanned point.
      IF (KK.GT.2)KKST=KK-1 
      END DO
C     Form product function
C
      DO N=1,K+1
          AA=OINTW1(N)
          BB=OINTF1(N)
          CC=OINTF2(N)
          CWL(N)=AA
          CONV(N)=BB*CC
C          WRITE (NOUT1, 103) N,CWL(N),CONV(N)
      END DO
C     Integrate product function by Simpson's rule
      SM1=0.D0
      H=CWL(2)-CWL(1)
      CF=H/3.D0
      LM=(K+1)/2
      JPT=1
      DO L=1,LM
            A=CONV(JPT)
            B=CONV(JPT+1)
            C=CONV(JPT+2)
            SM1=SM1+A+4.D0*B+C
            JPT=JPT+2
            zzz=0.d0
      END DO
      SM1=SM1*CF
C      WRITE (NOUT1,104) SM1 
C     Integrate filter passband by Simpson's rule
      SM2=0.D0
      H=OINTW1(2)-OINTW1(1)
      CF=H/3.D0
      LM=(K+1)/2
      JPT=1
      DO L=1,LM
            A=OINTF1(JPT)
            B=OINTF1(JPT+1)
            C=OINTF1(JPT+2)
            SM2=SM2+A+4.D0*B+C
            JPT=JPT+2
            zzz=0.d0
      END DO
      SM2=SM2*CF
C      WRITE (NOUT1,104) SM2
      SM=SM1/SM2
      VMG=-2.5*DLOG10(SM)-21.1D0
      WRITE (NOUT1,104) VMG,SM1,SM2       
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT2,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT3,    STATUS = 'KEEP')
C      CLOSE ( UNIT = NOUT4,    STATUS = 'KEEP')
C*********************************************************************
      STOP 'INTEGSP completed successfully'
   90 CALL PMDSTOP
      END
