      SUBROUTINE  EREAD ( EGET, FORM )

C     This subroutine reads in variables with variable Format
C     Version  Feb. 1, 1986 by Josef Kallrath


      CHARACTER * 64  DUMMY
      CHARACTER * (*) FORM
      CHARACTER * 80  F0

      COMMON  /NUNIT/ NU, NUOUT, IFOUT

      F0 = '( A64, ' // FORM // ')'

      READ ( NU, F0 ) DUMMY, EGET

      IF (IFOUT .EQ. 1)  WRITE ( NUOUT, F0 ) DUMMY, EGET

      RETURN

      END
