      SUBROUTINE ISORT (LIST)
!
!     This routine bubble-sorts an integer list of length N
!     into increasing order.
!-----------------------------------------------------------------------
!     input:   LIST   integer list
!
!     output:  LIST   modified into increasing order
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     declare dummy variables
!
      INTEGER :: LIST(:)
!
!     declare local variables
!
      INTEGER :: N, I, J, LST
!
!     end of declarations
!=======================================================================
!
      N = SIZE(LIST)
!
      DO I = 1, N - 1
        DO J = 2, N - I + 1
          IF (LIST(J-1) .GT. LIST(J)) THEN
            LST = LIST(J-1)
            LIST(J-1) = LIST(J)
            LIST(J) = LST
          ENDIF
        END DO
      END DO
C
      RETURN
      END SUBROUTINE ISORT

      MODULE ISORT_I
        INTERFACE
          SUBROUTINE ISORT (LIST)
C
          IMPLICIT NONE
C
C         declare dummy variables
C
          INTEGER :: LIST(:)
C
          END SUBROUTINE ISORT
        END INTERFACE
      END MODULE ISORT_I
