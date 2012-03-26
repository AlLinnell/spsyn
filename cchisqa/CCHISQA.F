      Program cchisqa
C     Program to calculate values of chi^2 for a range of values
C     of mass transfer rates and white dwarf Teff values.
C     List of CALPT input files is in 
C		D:\CVSYSTEMS\RWSEX\CALPTAB.DAT
C     List of PGC input files is in
C		D:\CVSYSTEMS\RWSEX\ADRPGCAA.DAT
C     List of ACPGC input files is in
C		D:\CVSYSTEMS\RWSEX\ADRACPGC.DAT
C     List of storage addresses for output synthetic spectra is in
C		D:\CVSYSTEMS\RWSEX\MODSTRA.DAT
C     List of addresses of ACP6 files is in 
C         D:\CVSYSTEMS\RWSEX\ACP6ADRA.DAT
C
      DATA NEXP/11/
	DATA NTEMP/1/
C	DATA NMTR/3/
	INTEGER*4 RESULT
	CHARACTER*72 adracp,adrpgc,progname,adrp6,adrmod,calptadr
	DATA IN1,IN2,IN3,IN4,IN5/5,6,7,8,9/
C     I is index of EXP values
	DATA IST/1/
	DATA IND/11/
C     J is temperature index
	DATA JST/1/
	DATA JND/1/
  100 FORMAT (A72)
      result=setexitqq(qwin$exitnopersist)
C     open list of PGC input files
	OPEN (UNIT=IN1, FILE='~/cvsystems/rwsex/adrpgcaa.dat', 
     $STATUS='OLD')
C     open list of ACPGC input files
	OPEN (UNIT=IN2, FILE='~/cvsystems/rwsex/adracpgc.dat', 
     $STATUS='OLD')
C     open list of ACP6 input files
	OPEN (UNIT=IN3, FILE='~/cvsystems/rwsex/acp6adra.dat',
     $STATUS='OLD')
C     open list of storage addresses for output synthetic spectra
	OPEN (UNIT=IN4, FILE='~/cvsystems/rwsex/modstra.dat',
     $STATUS = 'OLD')
C     open list of CALPT input files
      OPEN (UNIT=IN5, FILE='~/cvsystems/rwsex/calptab.dat',
     $STATUS = 'OLD')
C
	IF (IND.GT.NEXP) GO TO 99
	IF (JND.GT.NTEMP) GO TO 99
   	DO j=1,ntemp
			IF (J.LT.JST) GO TO 20
			IF (J.GT.JND) GO TO 20
C     Read address of CALPT file
			READ (IN5,100) calptadr
			progname=calptadr
			result=systemqq(progname)
C     Run CALPT
			.//calpt
C     Run TRNSF1
			.//TRNSF1
C     Run REPL4
			.//REPL4
C     Run PGA
			.//PGA
C     Read address of PGC file
			READ(IN1,100) adrpgc
C     Set control file for PGC
			progname=adrpgc
			result=systemqq(progname)
C     Run PGC
			.//PGC
C     Run PGD
			.//PGD
C     Read address of ACP6 file
			READ(IN3,100) adrp6
C     Set control file ACP6
			progname=adrp6
			result=systemqq(progname)
C
		DO i=1, NEXP
			IF (I.GT.IND) GO TO 20
			IF (I.LT.IST) GO TO 20
C     Read address of ACPGC file
			READ(IN2,100) adracp
C     Set control file for ACPGC
			progname=adracp
			result=systemqq(progname)
C     Run ACPGC
			.//ACPGC
C     Run ACPGD
			.//ACPGD
C     Run RDVEL2
			.//RDVEL2
C     Run ACPGF6
			.//ACPGF6
C     Run DATCONV
			.//DATCONV
C     Read address of storage for SYSSM.DAT
			READ(IN4,100) adrmod
			progname=adrmod
C     Store SYSSM.DAT
			result=systemqq(progname)
C     Read address of storage for SYSC1.DAT
c			READ(IN4,100) adrmod
c			progname=adrmod
C     Store SYSC1.DAT
c			result=systemqq(progname)
C     Read address of storage for SYSC2.DAT
c			READ(IN4,100) adrmod
c			progname=adrmod
C     Store SYSC2.DAT
c			result=systemqq(progname)
C     Read address of storage for SYSFC.DAT
c			READ(IN4,100) adrmod
c			progname=adrmod
C     Store SYSFC.DAT
c			result=systemqq(progname)
      	END DO
	REWIND IN2
   20 CONTINUE
      END DO
   99 STOP
	END Program cchisqa
