      Program PHIST
C      Program to produce histogram plot of residuals
C      of file D:\cvsystems\V3885Sgr\RES1.DAT
      DIMENSION WL(50000),RES(50000)
      DIMENSION H(2,50),DIV(50),BIN(50)
  100 FORMAT(F15.6,E15.6)
  101 FORMAT(F6.1,2X,F6.1)
	OPEN (5,FILE='D:\CVSYSTEMS\V3885SGR\RES1.DAT',STATUS='OLD')
	OPEN (6,FILE='D:\OA2\HV.DAT',STATUS='REPLACE')
	DO J=1,50000
		READ (5,100,END=1,ERR=5) WL(J),RES(J)
	END DO
	CALL PMDSTOP
    1 NWL=J-1
      SG=3.1167E-13
	DO J=1,NWL
		RES(J)=RES(J)/SG
	END DO
	DO K=1,49
		DIV(K)=-5.2+K*0.2
		BIN(K)=0.0
	END DO
	NCT=0
	DO J=1,NWL
		DO K=1,49
			IF ((RES(J).GE.DIV(K)).AND.(RES(J).LT.DIV(K+1))) THEN
			BIN(K)=BIN(K)+1.0
			NCT=NCT+1
			END IF
		END DO
	END DO
	DO K=1,50
		H(1,K)=DIV(K)
		H(2,K)=BIN(K)
	END DO
	DO K=1,50
		WRITE (6,101) H(1,K),H(2,K)
	END DO
	STOP
    5 CALL PMDSTOP
      END