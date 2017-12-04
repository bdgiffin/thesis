      MODULE LIN_SOLVER_M
!
!     This module defines the LIN_SOLVER derived type.  This type
!     holds data that is relevant to the solution of a global system
!     of linear equations.  The base type LIN_SOLVER is ABSTRACT,
!     with type extensions defining specific solver implementations.
!     At present, only a simple compacted-column solver is 
!     implemented, as extended type LIN_SOLVER_CC.  TBPs are defined
!     which assemble the equation system, and solve the system.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE ELEMS_NODES_M
      IMPLICIT NONE
!
!=======================================================================
!     derived-type definitions
!=======================================================================
!
      TYPE, ABSTRACT :: LIN_SOLVER_DAT
!
        CONTAINS
!
        PROCEDURE (DEL_LIN_SOLVER_DAT), DEFERRED :: DELETE
      END TYPE LIN_SOLVER_DAT
!
!-----------------------------------------------------------------------
!
      ABSTRACT INTERFACE 
        SUBROUTINE DEL_LIN_SOLVER_DAT (LS_DAT)
          IMPORT :: LIN_SOLVER_DAT
          CLASS (LIN_SOLVER_DAT), INTENT (INOUT) :: LS_DAT
        END SUBROUTINE DEL_LIN_SOLVER_DAT
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (LIN_SOLVER_DAT) :: LIN_SOLVER_DAT_CC
        INTEGER, ALLOCATABLE :: IDGL(:,:)
!
        CONTAINS
!
        PROCEDURE :: DELETE => DELETE_LS_DAT_CC
      END TYPE LIN_SOLVER_DAT_CC
!
!-----------------------------------------------------------------------
!
      TYPE :: LIN_SOLVER
        INTEGER :: NUM_DOF
        INTEGER, ALLOCATABLE :: ID(:,:)
        REAL (DBL), ALLOCATABLE :: U(:), STF(:), FRC(:)
        CLASS (LIN_SOLVER_DAT), ALLOCATABLE :: LS_DAT
!
        CONTAINS
!
        PROCEDURE :: ASM_LIN_EQ
        PROCEDURE :: LIN_SOLVE
        PROCEDURE :: DELETE => DELETE_LIN_SOLVER
        PROCEDURE :: ZERO_SOLVER_DATA
        PROCEDURE, PRIVATE :: ASM_LIN_EQ_CC
        PROCEDURE, PRIVATE :: LIN_SOLVE_CC
!
      END TYPE LIN_SOLVER
!
!-----------------------------------------------------------------------
!
      PRIVATE :: ASM_LIN_EQ, LIN_SOLVE, ASM_LIN_EQ_CC, LIN_SOLVE_CC,    &
     &           DELETE_LS_DAT_CC, DELETE_LIN_SOLVER,                   &
     &           ZERO_SOLVER_DATA, FACTOR, FORWARD, BACK_SUB
!
!========================================================================
      CONTAINS
!========================================================================
!
      SUBROUTINE ZERO_SOLVER_DATA (LIN_EQ_OBJ)
!
        CLASS (LIN_SOLVER), INTENT (INOUT) :: LIN_EQ_OBJ
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(LIN_EQ_OBJ%U)) LIN_EQ_OBJ%U = 0.0_DBL
        IF (ALLOCATED(LIN_EQ_OBJ%FRC)) LIN_EQ_OBJ%FRC = 0.0_DBL
        IF (ALLOCATED(LIN_EQ_OBJ%STF)) LIN_EQ_OBJ%STF = 0.0_DBL
!
        RETURN
      END SUBROUTINE ZERO_SOLVER_DATA
!
!=======================================================================
!
      SUBROUTINE DELETE_LIN_SOLVER (LIN_EQ_OBJ)
!
        CLASS (LIN_SOLVER), INTENT (INOUT) :: LIN_EQ_OBJ
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(LIN_EQ_OBJ%ID)) DEALLOCATE (LIN_EQ_OBJ%ID)
        IF (ALLOCATED(LIN_EQ_OBJ%U)) DEALLOCATE (LIN_EQ_OBJ%U)
        IF (ALLOCATED(LIN_EQ_OBJ%FRC)) DEALLOCATE (LIN_EQ_OBJ%FRC)
        IF (ALLOCATED(LIN_EQ_OBJ%STF)) DEALLOCATE (LIN_EQ_OBJ%STF)
        IF (ALLOCATED(LIN_EQ_OBJ%LS_DAT)) THEN
          CALL LIN_EQ_OBJ%LS_DAT%DELETE
          DEALLOCATE (LIN_EQ_OBJ%LS_DAT)
        END IF
!
        RETURN
      END SUBROUTINE DELETE_LIN_SOLVER
!
!=======================================================================
!
      SUBROUTINE DELETE_LS_DAT_CC (LS_DAT)
!
        CLASS (LIN_SOLVER_DAT_CC), INTENT (INOUT) :: LS_DAT
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(LS_DAT%IDGL)) DEALLOCATE (LS_DAT%IDGL)
!
        RETURN
      END SUBROUTINE DELETE_LS_DAT_CC
!
!=======================================================================
!
      SUBROUTINE ASM_LIN_EQ (LIN_EQ_OBJ, ELEM, ELEM_DOF)
!
        CLASS (LIN_SOLVER), INTENT (INOUT) :: LIN_EQ_OBJ
        TYPE (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA) :: ELEM_DOF
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => LIN_EQ_OBJ%LS_DAT)
        TYPE IS (LIN_SOLVER_DAT_CC)
          CALL LIN_EQ_OBJ%ASM_LIN_EQ_CC (ELEM, ELEM_DOF)
        END SELECT
        CALL ELEM%DELETE_FRC_STF
!
        RETURN
      END SUBROUTINE ASM_LIN_EQ
!
!========================================================================
!
      SUBROUTINE LIN_SOLVE (LIN_EQ_OBJ)
!
        CLASS (LIN_SOLVER), INTENT (INOUT) :: LIN_EQ_OBJ
!
!------------------------------------------------------------------------
!
        SELECT TYPE (DAT => LIN_EQ_OBJ%LS_DAT)
        TYPE IS (LIN_SOLVER_DAT_CC)
          CALL LIN_EQ_OBJ%LIN_SOLVE_CC 
        END SELECT
!
        RETURN
      END SUBROUTINE LIN_SOLVE
!
!========================================================================
!
      SUBROUTINE ASM_LIN_EQ_CC (LIN_EQ_OBJ, ELEM, ELEM_DOF)
!
!       declare dummy arguments
!
        CLASS (LIN_SOLVER), INTENT (INOUT) :: LIN_EQ_OBJ
        TYPE (ELEM_CONTAINER), INTENT (IN) :: ELEM
        TYPE (LCL_NODAL_DATA) :: ELEM_DOF
!
!       declare local variables
!
        INTEGER :: I, J, K, NN, LOC, IDOF, JDOF
        INTEGER, ALLOCATABLE :: CN(:,:)
!
!-----------------------------------------------------------------------
!
!       only do something if the LS_DAT component of LIN_EQ_OBJ is of
!       the right type
!
        SELECT TYPE (DAT => LIN_EQ_OBJ%LS_DAT)
        TYPE IS (LIN_SOLVER_DAT_CC)
!
!         get the total number of dof/eqns for the current element
!
          NN = SUM(ELEM_DOF%NUM_NODAL_DOF(:))
!
!         fill a local array with a (component, global node) pair for
!         each entry in the element residual vector
!
          ALLOCATE (CN(2,NN))
          K = 0
          DO I = 1, SIZE(ELEM%NODE_IDS)
            DO J = 1, ELEM_DOF%NUM_NODAL_DOF(I)
              K = K + 1
              CN(1,K) = J
              CN(2,K) = ELEM%NODE_IDS(I)
            END DO
          END DO
!
!         assemble the current element contribution into the global
!         force vector
!
          IF (ALLOCATED(ELEM%FRC)) THEN
            DO I = 1, NN
              LOC = LIN_EQ_OBJ%ID(CN(1,I),CN(2,I))
              IF (LOC .GT. 0) LIN_EQ_OBJ%FRC(LOC) = LIN_EQ_OBJ%FRC(LOC) &
     &                                              + ELEM%FRC(I)
            END DO
          END IF
!
!         assemble the current element contribution into the global
!         stiffness matrix
!
          IF (ALLOCATED(ELEM%STF)) THEN
            DO I = 1, NN
              IDOF = LIN_EQ_OBJ%ID(CN(1,I),CN(2,I))
              DO J = 1, NN
                JDOF = LIN_EQ_OBJ%ID(CN(1,J),CN(2,J))
                IF (IDOF .GT. 0 .AND. JDOF .GT. 0) THEN
                  LOC = DAT%IDGL(2,JDOF) + IDOF - DAT%IDGL(1,JDOF)
                  LIN_EQ_OBJ%STF(LOC) = LIN_EQ_OBJ%STF(LOC) +           &
     &                                  ELEM%STF(I,J)
                END IF
              END DO
            END DO
          END IF
!
          DEALLOCATE (CN)
!
        END SELECT
!
        RETURN
      END SUBROUTINE ASM_LIN_EQ_CC
!
!=======================================================================
!
      SUBROUTINE LIN_SOLVE_CC (LIN_EQ_OBJ)
!
        CLASS (LIN_SOLVER), INTENT (INOUT) :: LIN_EQ_OBJ
!
!       declare local variables
!
        INTEGER :: NDOF
!
!-----------------------------------------------------------------------
!
!       only do someting if the LS_DAT component of LN_EQ_OBJ is of
!       the right type
!
        SELECT TYPE (DAT => LIN_EQ_OBJ%LS_DAT)
        TYPE IS (LIN_SOLVER_DAT_CC)
!
!         get the number of unknowns (i.e. the size of the linear system)
!
          NDOF = LIN_EQ_OBJ%NUM_DOF
!
!         factor the stiffness matrix
!
          CALL FACTOR (NDOF, DAT%IDGL, LIN_EQ_OBJ%STF)
!
!         perform forward reduction
!
          CALL FORWARD (NDOF, DAT%IDGL, LIN_EQ_OBJ%STF,                 &
     &                  LIN_EQ_OBJ%FRC, LIN_EQ_OBJ%U)
!
!         perform back-substitution
!
          CALL BACK_SUB (NDOF, DAT%IDGL, LIN_EQ_OBJ%STF,                &
     &                   LIN_EQ_OBJ%U)
!
        END SELECT
!
      RETURN
      END SUBROUTINE LIN_SOLVE_CC
!
!=======================================================================
!     PRIVATE procedures called by the above TBPs
!=======================================================================
!
      SUBROUTINE FACTOR (NUNWN, IDGL, STF)
!
!       declare dummy arguments
!
        INTEGER :: NUNWN, IDGL(:,:)
        REAL (DBL) :: STF(:)
!
!       declare local variables
!
        INTEGER :: L, MF, ML, I, IL, J, NL, IJ, JL, II
!
!-----------------------------------------------------------------------
!
!       loop over columns in the actual stiffness matrix
!
        DO L = 1, NUNWN
!
!         get the first and last nonzero rows in the current column
!
          MF = IDGL(1,L)
          ML = MF + IDGL(2,L+1) - IDGL(2,L) - 1
!
!         U-L factorization of STF:
!         replace the upper part of the current column of STF with the
!         corresponding entries in U
!
          DO I = MF, L - 1
            IL = IDGL(2,L) + I - IDGL(1,L)
            DO J = MF, I - 1
              NL = IDGL(1,J) + IDGL(2,J+1) - IDGL(2,J) - 1
              IF (I .GE. J .AND. I .LE. NL) THEN
                IJ = IDGL(2,J) + I - IDGL(1,J)
                JL = IDGL(2,L) + J - IDGL(1,L)
                STF(IL) = STF(IL) - STF(IJ) * STF(JL)
              END IF
            END DO
            II = IDGL(2,I) + I - IDGL(1,I)
            STF(IL) = STF(IL) / STF(II)
          END DO
!
!         U-L factorization of STF:
!         replace the lower part of the current column of STF with the
!         corresponding entries in L
!
          DO I = L, ML
            IL = IDGL(2,L) + I - IDGL(1,L)
            DO J = MF, L - 1
              NL = IDGL(1,J) + IDGL(2,J+1) - IDGL(2,J) - 1
              IF (I .GE. J .AND. I .LE. NL) THEN
                IJ = IDGL(2,J) + I - IDGL(1,J)
                JL = IDGL(2,L) + J - IDGL(1,L)
                STF(IL) = STF(IL) - STF(IJ) * STF(JL)
              END IF
            END DO
          END DO
!
        END DO
!
        RETURN
      END SUBROUTINE FACTOR
!
!=======================================================================
!
      SUBROUTINE FORWARD (NUNWN, IDGL, STF, FRC, U)
!
!       declare dummy arguments
!
        INTEGER :: NUNWN, IDGL(:,:)
        REAL (DBL) :: STF(:), FRC(:), U(:)
!
!       declare local variables
!
        INTEGER :: I, J, ML, IJ, II
!
!-----------------------------------------------------------------------
!
        IF (SIZE(U) .EQ. 0) RETURN
!
        U(1) = FRC(1) / STF(1)
        DO I = 2, NUNWN
          DO J = 1, I - 1
            ML = IDGL(1,J) + IDGL(2,J+1) - IDGL(2,J) - 1
            IF (I .LE. ML) THEN
              IJ = IDGL(2,J) + I - IDGL(1,J)
              FRC(I) = FRC(I) - STF(IJ) * U(J)
            END IF
          END DO
          II = IDGL(2,I) + I - IDGL(1,I)
          U(I) = FRC(I) / STF(II)
        END DO
!
        RETURN
      END SUBROUTINE FORWARD
!
!=======================================================================
!
      SUBROUTINE BACK_SUB (NUNWN, IDGL, STF, U)
!
!       declare dummy arguments
!
        INTEGER :: NUNWN, IDGL(:,:)
        REAL (DBL) :: STF(:), U(:)
!
!       declare local variables
!
        INTEGER :: I, J, MF, IJ
!
!-----------------------------------------------------------------------
!
        IF (SIZE(U) .EQ. 0) RETURN
!
        DO I = NUNWN, 1, -1
          DO J = I + 1, NUNWN
            MF = IDGL(1,J)
            IF (I .GE. MF) THEN
              IJ = IDGL(2,J) + I - IDGL(1,J)
              U(I) = U(I) - STF(IJ) * U(J)
            END IF
          END DO
        END DO
!
        RETURN
      END SUBROUTINE BACK_SUB
!
      END MODULE LIN_SOLVER_M
