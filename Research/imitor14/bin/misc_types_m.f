      MODULE MISC_TYPES_M
!
!     This module contains miscellaneous type definitions needed
!     by other modules.  All of them are "simple," in the sense that
!     none of them have type extensions.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      IMPLICIT NONE
!
!=======================================================================
!     derived-type definitions
!=======================================================================
!
      TYPE FILES
        CHARACTER (LEN = 40) :: ROOT_NAME
        INTEGER :: LN = 0, N_UNIT = 7
!
        CONTAINS
!
        PROCEDURE, PRIVATE :: GET_ROOT_NAME
        PROCEDURE :: OPEN_FILE
      END TYPE FILES
!
!-----------------------------------------------------------------------
!
      TYPE GLOBAL_SIM_VARS
        INTEGER :: NCOORD, ANALYSIS_TYPE
        LOGICAL :: FINITE_DEF, THERMAL
      END TYPE GLOBAL_SIM_VARS
!
!-----------------------------------------------------------------------
!
      TYPE MATERIAL
        INTEGER :: MAT_TYPE
        REAL (DBL), ALLOCATABLE :: PROPS(:)
      END TYPE MATERIAL
!
!-----------------------------------------------------------------------
!
      TYPE NODAL_BC
        INTEGER :: ND, DIR
        CHARACTER (LEN = 4) :: NBC_TYPE
        REAL (DBL) :: AMP
        TYPE (TIME_FN), POINTER :: TF => NULL ()
      END TYPE NODAL_BC
!
!-----------------------------------------------------------------------
!
      TYPE FACET_BC
        CHARACTER (LEN = 5) :: FBC_TYPE
        REAL (DBL) :: TRAC(3), PRES
        TYPE (TIME_FN), POINTER :: TF => NULL ()
      END TYPE FACET_BC
!
!-----------------------------------------------------------------------
!
      TYPE TIME_FN
        REAL (DBL), ALLOCATABLE :: T_F(:,:)
!
        CONTAINS
!
        PROCEDURE :: EVAL => TIME_FUNC_EVAL
      END TYPE TIME_FN
!
!-----------------------------------------------------------------------
!
      TYPE STEP_TIME
        REAL (DBL) :: TM
        INTEGER :: PRINT_CODE
      END TYPE STEP_TIME
!
!-----------------------------------------------------------------------
!
      TYPE CONVRG_CONTROL
        REAL (DBL) :: STRESS_TOL
        INTEGER :: MAX_NR_ITER, MAX_PASS
      END TYPE CONVRG_CONTROL
!
!-----------------------------------------------------------------------
!
      TYPE SOLN_STAT
        REAL (DBL) :: T_BEG, T_END
        INTEGER :: ITER, STEP_NUM
        LOGICAL :: CONVERGED, ABORT
      END TYPE SOLN_STAT
!
!-----------------------------------------------------------------------
!
      TYPE LCL_NODAL_DATA
        INTEGER :: NND
        INTEGER, ALLOCATABLE :: NUM_NODAL_DOF(:)
        REAL (DBL), ALLOCATABLE :: XYZ(:,:), U_INC(:,:), U_TOT(:,:)
!
        CONTAINS
!
        PROCEDURE :: DELETE => DEL_LCL_NODAL_DATA
      END TYPE LCL_NODAL_DATA
!
!-----------------------------------------------------------------------
!
      PRIVATE :: DEL_LCL_NODAL_DATA, TIME_FUNC_EVAL, GET_ROOT_NAME,     &
     &           OPEN_FILE
!
!=======================================================================
      CONTAINS
!=======================================================================
!     type-bound procedures on the above derived types
!=======================================================================
!
      SUBROUTINE DEL_LCL_NODAL_DATA (LM_DOF)
!
        CLASS (LCL_NODAL_DATA), INTENT (INOUT) :: LM_DOF
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(LM_DOF%NUM_NODAL_DOF)) THEN
          DEALLOCATE (LM_DOF%NUM_NODAL_DOF)
        END IF
        IF (ALLOCATED(LM_DOF%U_INC)) DEALLOCATE (LM_DOF%U_INC)
        IF (ALLOCATED(LM_DOF%U_TOT)) DEALLOCATE (LM_DOF%U_TOT)
!
        RETURN
      END SUBROUTINE DEL_LCL_NODAL_DATA
!
!=======================================================================
!
      FUNCTION TIME_FUNC_EVAL (TF, TIME)
!
        CLASS (TIME_FN), INTENT (IN) :: TF
        REAL (DBL), INTENT (IN) :: TIME
        REAL (DBL) :: TIME_FUNC_EVAL
!
!       declare local variables
!
        INTEGER :: N, I
        REAL (DBL) :: T1, F1, T0, F0, FRAC
!
!-----------------------------------------------------------------------
!
        N = SIZE(TF%T_F, 2)
        DO I = 2, N
          T1 = TF%T_F(1,I)
          F1 = TF%T_F(2,I)
          IF (T1 .GE. TIME .OR. I .EQ. N) THEN
            T0 = TF%T_F(1,I-1)
            F0 = TF%T_F(2,I-1)
            FRAC = (TIME - T0) / (T1 - T0)
            TIME_FUNC_EVAL = F0 + FRAC * (F1 - F0)
            EXIT
          END IF
        END DO
!
        RETURN
      END FUNCTION TIME_FUNC_EVAL
!
!=======================================================================
!
      SUBROUTINE GET_ROOT_NAME (FN_OBJ)
!
        CLASS (FILES), INTENT (INOUT) :: FN_OBJ
!
!       declare local variables
!
        INTEGER :: STAT, I, NF, NL
        CHARACTER (LEN = 50) :: FNAME
!
!-----------------------------------------------------------------------
!
        CALL GET_COMMAND_ARGUMENT (1, FNAME, STATUS = STAT)
!
        IF (STAT .NE. 0) THEN
          PRINT *, 'error reading root filename from command line'
          PRINT *, 'aborting'
          STOP
        END IF
!
        NF = 0
        DO I = 1, 50
          IF (NF .EQ. 0) THEN
            IF (FNAME(I:I) .NE. ' ') NF = I
          ELSE
            IF (FNAME(I:I) .EQ. ' ') THEN
              NL = I - 1
              EXIT
            END IF
          END IF
        END DO
        FN_OBJ%LN = NL - NF + 1
        FN_OBJ%ROOT_NAME(1:FN_OBJ%LN) = FNAME(NF:NL)
!
        RETURN
      END SUBROUTINE GET_ROOT_NAME
!
!=======================================================================
!
      SUBROUTINE OPEN_FILE (FN_OBJ, F_TYPE, F_UNIT)
!
        CLASS (FILES), INTENT (INOUT) :: FN_OBJ
        CHARACTER (LEN = 4), INTENT (IN) :: F_TYPE
        INTEGER, INTENT (OUT) :: F_UNIT
!
!       declare local variables
!
        INTEGER :: IOS
!
!-----------------------------------------------------------------------
!
        IF (FN_OBJ%LN .EQ. 0) CALL FN_OBJ%GET_ROOT_NAME
!
        IF (F_TYPE .EQ. 'MESH') THEN
!
          F_UNIT = FN_OBJ%N_UNIT + 1
          OPEN (UNIT = F_UNIT, ACCESS = 'SEQUENTIAL',                   &
     &          FILE = FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.m',             &
     &          FORM = 'FORMATTED', STATUS = 'OLD', IOSTAT = IOS)
          IF (IOS .NE. 0) THEN
            PRINT *, 'error opening file ',                             &
     &                FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.m'
            PRINT *, 'aborting'
            STOP
          END IF
          FN_OBJ%N_UNIT = F_UNIT
!
        ELSE IF (F_TYPE .EQ. 'INPT') THEN
!
          F_UNIT = FN_OBJ%N_UNIT + 1
          OPEN (UNIT = F_UNIT, ACCESS = 'SEQUENTIAL',                   &
     &          FILE = FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.i',             &
     &          FORM = 'FORMATTED', STATUS = 'OLD', IOSTAT = IOS)
          IF (IOS .NE. 0) THEN
            PRINT *, 'error opening file ',                             &
     &                FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.i'
            PRINT *, 'aborting'
            STOP
          END IF
          FN_OBJ%N_UNIT = F_UNIT
!
        ELSE IF (F_TYPE .EQ. 'OUTP') THEN
!
          F_UNIT = FN_OBJ%N_UNIT + 1
          OPEN (UNIT = F_UNIT, ACCESS = 'SEQUENTIAL',                   &
     &          FILE = FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.e',             &
     &          FORM = 'FORMATTED', STATUS = 'UNKNOWN', IOSTAT = IOS)
          IF (IOS .NE. 0) THEN
            PRINT *, 'error opening file ',                             &
     &                FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.e'
            PRINT *, 'aborting'
            STOP
          END IF
          FN_OBJ%N_UNIT = F_UNIT
!
        END IF
!
        RETURN
      END SUBROUTINE OPEN_FILE
!
!=======================================================================
!
      END MODULE MISC_TYPES_M
