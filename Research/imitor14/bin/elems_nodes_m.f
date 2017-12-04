      MODULE ELEMS_NODES_M
!
!     This module defines the derived types and their type-bound
!     procedures relevant to elements and nodes.  The basic types are
!     NODE_CONTAINER and ELEM_CONTAINER, which hold the data for a 
!     single node and a single element, respectively.
!     Two module variables, CUR_GLOBAL_SIM_VARS and CUR_SOLN_STAT, are
!     also defined herein.  The values of these variables are needed
!     by some of the routines defined in this module.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE MISC_TYPES_M
      USE IP_FRC_STF_M
      USE IP_STRAIN_INC_M
      USE CONSTITUTIVE_MODELS_M
      USE MATRIX_M
      USE SHAPE_FNS_M
      IMPLICIT NONE
!
!=======================================================================
!     Module data.  These variables are set by a TBP on type SIM_MESH,
!     which is called at the beginning of SIM_MESH's STEP_SOLN TBP.
!     This ensures that the variables below are available to the 
!     procedures defined in this module in the course of execution of
!     STEP_SOLN.
!=======================================================================
!
!
!=======================================================================
!     derived-type definitions
!=======================================================================
!     Type NODE_CONTAINER has a single component, which is a 
!     polymorphic variable that holds the data for a node.  Type
!     extensions from the empty base type NODE_DAT define the various
!     types of nodes.
!-----------------------------------------------------------------------
!
      TYPE, ABSTRACT :: NODE_DAT
      END TYPE NODE_DAT
!
!-----------------------------------------------------------------------
!     extensions of type NODE_DAT:  one each for 2D and 3D nodes,
!     with and without temperature dof alongside the displacement dof
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (NODE_DAT) :: NODE_2D_DISP
        REAL (DBL) :: XYZ(2), U_TOT(2), U_INC(2), RESID(2), RESID_TOL(2)
      END TYPE NODE_2D_DISP
!
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (NODE_DAT) :: NODE_3D_DISP
        REAL (DBL) :: XYZ(3), U_TOT(3), U_INC(3), RESID(3), RESID_TOL(3)
      END TYPE NODE_3D_DISP
!
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (NODE_DAT) :: NODE_2D_DISP_TEMP
        REAL (DBL) :: XYZ(2), U_TOT(3), U_INC(3), RESID(3), RESID_TOL(3)
      END TYPE NODE_2D_DISP_TEMP
!
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (NODE_DAT) :: NODE_3D_DISP_TEMP
        REAL (DBL) :: XYZ(3), U_TOT(4), U_INC(4), RESID(4), RESID_TOL(4)
      END TYPE NODE_3D_DISP_TEMP
!
!-----------------------------------------------------------------------
!
      TYPE :: NODE_CONTAINER
        CLASS (NODE_DAT), ALLOCATABLE :: ND
!
!       NOTE: Added allocatable ELEM_IDS(:) component on node container
!       -BDG
!
        INTEGER, ALLOCATABLE :: ELEM_IDS(:)
!
        CONTAINS
!
        PROCEDURE :: RES_COMP
        PROCEDURE :: SUM_RESID
        PROCEDURE :: SUM_RESID_TOL
        PROCEDURE :: CORRECT_DOF
        PROCEDURE :: CORRECT_DOF_GUESS
        PROCEDURE :: ADVANCE_DOF
        PROCEDURE :: GET_NUM_DOF
        PROCEDURE :: GET_DIM
        PROCEDURE :: GET_NODAL_VALS
        PROCEDURE :: SET_NODAL_VALS
        PROCEDURE :: ZERO_NODAL_VALS
      END TYPE NODE_CONTAINER
!
!-----------------------------------------------------------------------
!     Type ELEM_CONTAINER has four components that are relevant to both
!     continuum and facet elements, plus a polymorphic component that
!     holds the data that is specific to these two basic element types.
!     Type extensions from the abstract base type ELEM_DAT define the
!     element-type-specific data.
!-----------------------------------------------------------------------
!
      TYPE, ABSTRACT :: ELEM_DAT
!
        CONTAINS 
!
        PROCEDURE (DEL_LM_DAT), DEFERRED :: DELETE
      END TYPE ELEM_DAT
!
!-----------------------------------------------------------------------
!     interface for the deferred TBP on type ELEM_DAT
!-----------------------------------------------------------------------
!
      ABSTRACT INTERFACE
        SUBROUTINE DEL_LM_DAT (LM_DAT)
          IMPORT :: ELEM_DAT
          CLASS (ELEM_DAT), INTENT (INOUT) :: LM_DAT
        END SUBROUTINE DEL_LM_DAT
      END INTERFACE
!
!-----------------------------------------------------------------------
!     extensions of type ELEM_DAT (one for continuum elements, and
!     one for facet elements)
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (ELEM_DAT) :: ELEM_DAT_CONTINUUM
        TYPE (MATERIAL), POINTER :: MAT
        REAL (DBL), ALLOCATABLE :: IP_VARS_0(:,:), IP_VARS_1(:,:)
        REAL (DBL), ALLOCATABLE :: ELEM_VARS_0(:), ELEM_VARS_1(:)
        REAL (DBL), ALLOCATABLE :: SF_GRADS(:,:,:), SF_VALS(:,:), WTS(:)
        REAL (DBL), ALLOCATABLE :: STR_INC(:,:), ROT_INC(:,:,:)
!
        CONTAINS
!
        GENERIC :: ASSIGNMENT (=) => ASSIGN_ELEM_DAT_CONT
        PROCEDURE :: DELETE => DELETE_ELEM_CONT_DAT
        PROCEDURE, PRIVATE :: ASSIGN_ELEM_DAT_CONT
      END TYPE ELEM_DAT_CONTINUUM
!
!-----------------------------------------------------------------------
!
      TYPE, EXTENDS (ELEM_DAT) :: ELEM_DAT_FACET
        TYPE (FACET_BC), POINTER :: BC_DAT
        REAL (DBL), ALLOCATABLE :: SF_GRADS(:,:,:), SF_VALS(:,:),       &
     &                             WTS(:), NRML(:,:), RN(:,:,:)
!
        CONTAINS
!
        GENERIC :: ASSIGNMENT (=) => ASSIGN_ELEM_DAT_FACET
        PROCEDURE :: DELETE => DELETE_ELEM_FACET_DAT
        PROCEDURE, PRIVATE :: ASSIGN_ELEM_DAT_FACET
      END TYPE ELEM_DAT_FACET
!
!-----------------------------------------------------------------------
!
      TYPE :: ELEM_CONTAINER
        LOGICAL :: GOT_SF_DAT = .FALSE.
        INTEGER :: LM_TYPE
        INTEGER, ALLOCATABLE :: NODE_IDS(:)
        REAL (DBL), ALLOCATABLE :: FRC(:), STF(:,:)
        CLASS (ELEM_DAT), ALLOCATABLE :: LM_DAT
!
        CONTAINS
!
        GENERIC :: ASSIGNMENT (=) => ASSIGN_ELEM_CONTAINER
        PROCEDURE :: DELETE_FRC_STF
        PROCEDURE :: DELETE => DELETE_ELEM
        PROCEDURE :: GET_SF_DAT
        PROCEDURE :: GET_RESID_STIFFNESS
        PROCEDURE :: GET_RESID_TOL
        PROCEDURE :: GET_RESID_STIFF_GUESS
        PROCEDURE, PRIVATE :: GET_SF_DAT_CONT
        PROCEDURE, PRIVATE :: GET_SF_DAT_FACET
        PROCEDURE, PRIVATE :: GET_RESID_STIFF_CONT
        PROCEDURE, PRIVATE :: GET_RESID_STIFF_FACET
        PROCEDURE, PRIVATE :: GET_RESID_TOL_CONT
        PROCEDURE, PRIVATE :: ASSIGN_ELEM_CONTAINER
      END TYPE ELEM_CONTAINER
!
!-----------------------------------------------------------------------
!
      PRIVATE :: RES_COMP, SUM_RESID, CORRECT_DOF, ADVANCE_DOF,         &
     &      GET_NUM_DOF, GET_NODAL_VALS, SET_NODAL_VALS,                &
     &      ZERO_NODAL_VALS, DELETE_FRC_STF, DELETE_ELEM, GET_SF_DAT,   &
     &      GET_RESID_STIFFNESS, GET_SF_DAT_CONT, GET_RESID_STIFF_CONT, &
     &      DELETE_ELEM_CONT_DAT, GET_SF_DAT_FACET,                     &
     &      GET_RESID_STIFF_FACET, DELETE_ELEM_FACET_DAT, GET_DIM,      &
     &      GET_RESID_TOL, GET_RESID_TOL_CONT, SUM_RESID_TOL,           &
     &      ASSIGN_ELEM_CONTAINER,                                      &
     &      ASSIGN_ELEM_DAT_CONT, ASSIGN_ELEM_DAT_FACET
!
!=======================================================================
      CONTAINS
!=======================================================================
!     TBPs on type NODE_CONTAINER
!=======================================================================
!
      FUNCTION RES_COMP (NODE)
!
        CLASS (NODE_CONTAINER), INTENT (IN) :: NODE
        REAL (DBL) :: RES_COMP
!
!       declare local variables
!
        INTEGER :: N
        REAL (DBL) :: RC(4)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          N = 2
          WHERE (DAT%RESID_TOL(1:2) .GT. 0.0_DBL)
            RC(1:2) = ABS(DAT%RESID) / DAT%RESID_TOL
          ELSEWHERE
            RC(1:2) = -1.0_DBL
          END WHERE
        TYPE IS (NODE_3D_DISP)
          N = 3
          WHERE (DAT%RESID_TOL(1:3) .GT. 0.0_DBL)
            RC(1:3) = ABS(DAT%RESID) / DAT%RESID_TOL
          ELSEWHERE
            RC(1:3) = -1.0_DBL
          END WHERE
        TYPE IS (NODE_2D_DISP_TEMP)
          N = 3
          WHERE (DAT%RESID_TOL(1:3) .GT. 0.0_DBL)
            RC(1:3) = ABS(DAT%RESID) / DAT%RESID_TOL
          ELSEWHERE
            RC(1:3) = -1.0_DBL
          END WHERE
        TYPE IS (NODE_3D_DISP_TEMP)
          N = 4
          WHERE (DAT%RESID_TOL(1:4) .GT. 0.0_DBL)
            RC(1:4) = ABS(DAT%RESID) / DAT%RESID_TOL
          ELSEWHERE
            RC(1:4) = -1.0_DBL
          END WHERE
        END SELECT
!
        IF (ANY(RC(1:N) .LT. 0.0_DBL)) THEN
          RES_COMP = -1.0_DBL
        ELSE
          RES_COMP = MAXVAL(RC(1:N))
        END IF
!
        RETURN
      END FUNCTION RES_COMP
!
!=======================================================================
!
      SUBROUTINE SUM_RESID (NODE, RSD)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        REAL (DBL), INTENT (IN) :: RSD(:)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          DAT%RESID(1:2) = DAT%RESID(1:2) + RSD(1:2)
        TYPE IS (NODE_3D_DISP)
          DAT%RESID(1:3) = DAT%RESID(1:3) + RSD(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          DAT%RESID(1:3) = DAT%RESID(1:3) + RSD(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          DAT%RESID(1:4) = DAT%RESID(1:4) + RSD(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE SUM_RESID
!
!=======================================================================
!
      SUBROUTINE SUM_RESID_TOL (NODE, RSD)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        REAL (DBL), INTENT (IN) :: RSD(:)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          DAT%RESID_TOL(1:2) = DAT%RESID_TOL(1:2) + RSD(1:2)
        TYPE IS (NODE_3D_DISP)
          DAT%RESID_TOL(1:3) = DAT%RESID_TOL(1:3) + RSD(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          DAT%RESID_TOL(1:3) = DAT%RESID_TOL(1:3) + RSD(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          DAT%RESID_TOL(1:4) = DAT%RESID_TOL(1:4) + RSD(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE SUM_RESID_TOL
!
!=======================================================================
!
      SUBROUTINE CORRECT_DOF (NODE, DU)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        REAL (DBL), INTENT (IN) :: DU(:)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          DAT%U_INC(1:2) = DAT%U_INC(1:2) - DU(1:2)
        TYPE IS (NODE_3D_DISP)
          DAT%U_INC(1:3) = DAT%U_INC(1:3) - DU(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          DAT%U_INC(1:3) = DAT%U_INC(1:3) - DU(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          DAT%U_INC(1:4) = DAT%U_INC(1:4) - DU(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE CORRECT_DOF
!
!=======================================================================
!
      SUBROUTINE CORRECT_DOF_GUESS (NODE, DU)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        REAL (DBL), INTENT (IN) :: DU(:)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          DAT%U_INC(1:2) = DU(1:2) - DAT%U_INC(1:2)
        TYPE IS (NODE_3D_DISP)
          DAT%U_INC(1:3) = DU(1:3) - DAT%U_INC(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          DAT%U_INC(1:3) = DU(1:3) - DAT%U_INC(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          DAT%U_INC(1:4) = DU(1:4) - DAT%U_INC(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE CORRECT_DOF_GUESS
!
!=======================================================================
!
      SUBROUTINE ADVANCE_DOF (NODE)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          DAT%U_TOT(1:2) = DAT%U_TOT(1:2) + DAT%U_INC(1:2)
        TYPE IS (NODE_3D_DISP)
          DAT%U_TOT(1:3) = DAT%U_TOT(1:3) + DAT%U_INC(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          DAT%U_TOT(1:3) = DAT%U_TOT(1:3) + DAT%U_INC(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          DAT%U_TOT(1:4) = DAT%U_TOT(1:4) + DAT%U_INC(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE ADVANCE_DOF
!
!=======================================================================
!
      FUNCTION GET_DIM (NODE)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        INTEGER :: GET_DIM
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          GET_DIM = 2
        TYPE IS (NODE_3D_DISP)
          GET_DIM = 3
        TYPE IS (NODE_2D_DISP_TEMP)
          GET_DIM = 2
        TYPE IS (NODE_3D_DISP_TEMP)
          GET_DIM = 3
        END SELECT
!
        RETURN
      END FUNCTION GET_DIM
!
!=======================================================================
!
      FUNCTION GET_NUM_DOF (NODE)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        INTEGER :: GET_NUM_DOF
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          GET_NUM_DOF = 2
        TYPE IS (NODE_3D_DISP)
          GET_NUM_DOF = 3
        TYPE IS (NODE_2D_DISP_TEMP)
          GET_NUM_DOF = 3
        TYPE IS (NODE_3D_DISP_TEMP)
          GET_NUM_DOF = 4
        END SELECT
!
        RETURN
      END FUNCTION GET_NUM_DOF
!
!=======================================================================
!
      SUBROUTINE GET_NODAL_VALS (NODE, XYZ, U_TOT, U_INC, RESID,        &
     &                           RESID_TOL)
!
        CLASS (NODE_CONTAINER), INTENT (IN) :: NODE
        REAL (DBL), INTENT (OUT), OPTIONAL :: XYZ(:), U_TOT(:),         &
     &                            U_INC(:), RESID(:), RESID_TOL(:)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          IF (PRESENT(XYZ)) XYZ(1:2) = DAT%XYZ(1:2)
          IF (PRESENT(U_TOT)) U_TOT(1:2) = DAT%U_TOT(1:2)
          IF (PRESENT(U_INC)) U_INC(1:2) = DAT%U_INC(1:2)
          IF (PRESENT(RESID)) RESID(1:2) = DAT%RESID(1:2)
          IF (PRESENT(RESID_TOL)) RESID_TOL(1:2) = DAT%RESID_TOL(1:2)
        TYPE IS (NODE_3D_DISP)
          IF (PRESENT(XYZ)) XYZ(1:3) = DAT%XYZ(1:3)
          IF (PRESENT(U_TOT)) U_TOT(1:3) = DAT%U_TOT(1:3)
          IF (PRESENT(U_INC)) U_INC(1:3) = DAT%U_INC(1:3)
          IF (PRESENT(RESID)) RESID(1:3) = DAT%RESID(1:3)
          IF (PRESENT(RESID_TOL)) RESID_TOL(1:3) = DAT%RESID_TOL(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          IF (PRESENT(XYZ)) XYZ(1:2) = DAT%XYZ(1:2)
          IF (PRESENT(U_TOT)) U_TOT(1:3) = DAT%U_TOT(1:3)
          IF (PRESENT(U_INC)) U_INC(1:3) = DAT%U_INC(1:3)
          IF (PRESENT(RESID)) RESID(1:3) = DAT%RESID(1:3)
          IF (PRESENT(RESID_TOL)) RESID_TOL(1:3) = DAT%RESID_TOL(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          IF (PRESENT(XYZ)) XYZ(1:3) = DAT%XYZ(1:3)
          IF (PRESENT(U_TOT)) U_TOT(1:4) = DAT%U_TOT(1:4)
          IF (PRESENT(U_INC)) U_INC(1:4) = DAT%U_INC(1:4)
          IF (PRESENT(RESID)) RESID(1:4) = DAT%RESID(1:4)
          IF (PRESENT(RESID_TOL)) RESID_TOL(1:4) = DAT%RESID_TOL(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE GET_NODAL_VALS
!
!=======================================================================
!
      SUBROUTINE ZERO_NODAL_VALS (NODE, U_TOT, U_INC, RESID, RESID_TOL)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        LOGICAL, INTENT (IN), OPTIONAL :: U_TOT, U_INC, RESID, RESID_TOL
!
!       declare local variables
!
        INTEGER :: NN
        REAL (DBL) :: ZERO(20)
!
!-----------------------------------------------------------------------
!
        NN = NODE%GET_NUM_DOF()
        ZERO = 0.0_DBL
!
        IF (PRESENT(U_TOT)) THEN
          IF (U_TOT) CALL NODE%SET_NODAL_VALS (U_TOT = ZERO(1:NN))
        END IF
!
        IF (PRESENT(U_INC)) THEN
          IF (U_INC) CALL NODE%SET_NODAL_VALS (U_INC = ZERO(1:NN))
        END IF
!
        IF (PRESENT(RESID)) THEN
          IF (RESID) CALL NODE%SET_NODAL_VALS (RESID = ZERO(1:NN))
        END IF
!
        IF (PRESENT(RESID_TOL)) THEN
          IF (RESID_TOL) CALL NODE%SET_NODAL_VALS (RESID_TOL =          &
     &                                             ZERO(1:NN))
        END IF
!
        RETURN
      END SUBROUTINE ZERO_NODAL_VALS
!
!=======================================================================
!
      SUBROUTINE SET_NODAL_VALS (NODE, XYZ, U_TOT, U_INC, RESID,        &
     &                           RESID_TOL)
!
        CLASS (NODE_CONTAINER), INTENT (INOUT) :: NODE
        REAL (DBL), INTENT (IN), OPTIONAL :: XYZ(:), U_TOT(:),          &
     &              U_INC(:), RESID(:), RESID_TOL(:)
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => NODE%ND)
        TYPE IS (NODE_2D_DISP)
          IF (PRESENT(XYZ)) DAT%XYZ(1:2) = XYZ(1:2)
          IF (PRESENT(U_TOT)) DAT%U_TOT(1:2) = U_TOT(1:2)
          IF (PRESENT(U_INC)) DAT%U_INC(1:2) = U_INC(1:2)
          IF (PRESENT(RESID)) DAT%RESID(1:2) = RESID(1:2)
          IF (PRESENT(RESID_TOL)) DAT%RESID_TOL(1:2) = RESID_TOL(1:2)
        TYPE IS (NODE_3D_DISP)
          IF (PRESENT(XYZ)) DAT%XYZ(1:3) = XYZ(1:3)
          IF (PRESENT(U_TOT)) DAT%U_TOT(1:3) = U_TOT(1:3)
          IF (PRESENT(U_INC)) DAT%U_INC(1:3) = U_INC(1:3)
          IF (PRESENT(RESID)) DAT%RESID(1:3) = RESID(1:3)
          IF (PRESENT(RESID_TOL)) DAT%RESID_TOL(1:3) = RESID_TOL(1:3)
        TYPE IS (NODE_2D_DISP_TEMP)
          IF (PRESENT(XYZ)) DAT%XYZ(1:2) = XYZ(1:2)
          IF (PRESENT(U_TOT)) DAT%U_TOT(1:3) = U_TOT(1:3)
          IF (PRESENT(U_INC)) DAT%U_INC(1:3) = U_INC(1:3)
          IF (PRESENT(RESID)) DAT%RESID(1:3) = RESID(1:3)
          IF (PRESENT(RESID_TOL)) DAT%RESID_TOL(1:3) = RESID_TOL(1:3)
        TYPE IS (NODE_3D_DISP_TEMP)
          IF (PRESENT(XYZ)) DAT%XYZ(1:3) = XYZ(1:3)
          IF (PRESENT(U_TOT)) DAT%U_TOT(1:4) = U_TOT(1:4)
          IF (PRESENT(U_INC)) DAT%U_INC(1:4) = U_INC(1:4)
          IF (PRESENT(RESID)) DAT%RESID(1:4) = RESID(1:4)
          IF (PRESENT(RESID_TOL)) DAT%RESID_TOL(1:4) = RESID_TOL(1:4)
        END SELECT
!
        RETURN
      END SUBROUTINE SET_NODAL_VALS
!
!=======================================================================
!     TBPs on type ELEM_CONTAINER
!=======================================================================
!
      SUBROUTINE ASSIGN_ELEM_CONTAINER (LM_1, LM_2)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: LM_1
        TYPE (ELEM_CONTAINER), INTENT (IN) :: LM_2
!
!-----------------------------------------------------------------------
!
        LM_1%GOT_SF_DAT = LM_2%GOT_SF_DAT
        LM_1%LM_TYPE = LM_2%LM_TYPE
!
        IF (ALLOCATED(LM_1%NODE_IDS)) DEALLOCATE (LM_1%NODE_IDS)
        IF (ALLOCATED(LM_2%NODE_IDS)) THEN
          ALLOCATE (LM_1%NODE_IDS(SIZE(LM_2%NODE_IDS)))
          LM_1%NODE_IDS = LM_2%NODE_IDS
        END IF
!
        IF (ALLOCATED(LM_1%FRC)) DEALLOCATE (LM_1%FRC)
        IF (ALLOCATED(LM_2%FRC)) THEN
          ALLOCATE (LM_1%FRC(SIZE(LM_2%FRC)))
          LM_1%FRC = LM_2%FRC
        END IF
!
        IF (ALLOCATED(LM_1%STF)) DEALLOCATE (LM_1%STF)
        IF (ALLOCATED(LM_2%STF)) THEN
          ALLOCATE (LM_1%STF(SIZE(LM_2%STF, 1),SIZE(LM_2%STF, 2)))
          LM_1%STF = LM_2%STF
        END IF
!
        IF (ALLOCATED(LM_1%LM_DAT)) THEN
          CALL LM_1%LM_DAT%DELETE
          DEALLOCATE (LM_1%LM_DAT)
        END IF
        IF (ALLOCATED(LM_2%LM_DAT)) THEN
          SELECT TYPE (DAT_2 => LM_2%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            ALLOCATE (ELEM_DAT_CONTINUUM :: LM_1%LM_DAT)
            SELECT TYPE (DAT_1 => LM_1%LM_DAT)
            TYPE IS (ELEM_DAT_CONTINUUM)
              DAT_1 = DAT_2
            END SELECT
          TYPE IS (ELEM_DAT_FACET) 
            ALLOCATE (ELEM_DAT_FACET :: LM_1%LM_DAT)
            SELECT TYPE (DAT_1 => LM_1%LM_DAT)
            TYPE IS (ELEM_DAT_FACET)
              DAT_1 = DAT_2
            END SELECT
          END SELECT
        END IF
!
        RETURN
      END SUBROUTINE ASSIGN_ELEM_CONTAINER
!
!=======================================================================
!
      SUBROUTINE ASSIGN_ELEM_DAT_CONT (DAT_1, DAT_2)
!
        CLASS (ELEM_DAT_CONTINUUM), INTENT (INOUT) :: DAT_1
        TYPE (ELEM_DAT_CONTINUUM), INTENT (IN) :: DAT_2
!
!-----------------------------------------------------------------------
!
        IF (ASSOCIATED(DAT_2%MAT)) THEN
          DAT_1%MAT => DAT_2%MAT
        ELSE
          DAT_1%MAT => NULL()
        END IF
!
        IF (ALLOCATED(DAT_1%IP_VARS_0)) DEALLOCATE (DAT_1%IP_VARS_0)
        IF (ALLOCATED(DAT_2%IP_VARS_0)) THEN
          ALLOCATE (DAT_1%IP_VARS_0(SIZE(DAT_2%IP_VARS_0, 1),           &
     &                              SIZE(DAT_2%IP_VARS_0, 2)))
          DAT_1%IP_VARS_0 = DAT_2%IP_VARS_0
        END IF
!
        IF (ALLOCATED(DAT_1%IP_VARS_1)) DEALLOCATE (DAT_1%IP_VARS_1)
        IF (ALLOCATED(DAT_2%IP_VARS_1)) THEN
          ALLOCATE (DAT_1%IP_VARS_1(SIZE(DAT_2%IP_VARS_1, 1),           &
     &                              SIZE(DAT_2%IP_VARS_1, 2)))
          DAT_1%IP_VARS_1 = DAT_2%IP_VARS_1
        END IF
!
        IF (ALLOCATED(DAT_1%ELEM_VARS_0))                               &
     &                DEALLOCATE (DAT_1%ELEM_VARS_0)
        IF (ALLOCATED(DAT_2%ELEM_VARS_0)) THEN
          ALLOCATE (DAT_1%ELEM_VARS_0(SIZE(DAT_2%ELEM_VARS_0)))
          DAT_1%ELEM_VARS_0 = DAT_2%ELEM_VARS_0
        END IF
!
        IF (ALLOCATED(DAT_1%ELEM_VARS_1))                               &
     &                DEALLOCATE (DAT_1%ELEM_VARS_1)
        IF (ALLOCATED(DAT_2%ELEM_VARS_1)) THEN
          ALLOCATE (DAT_1%ELEM_VARS_1(SIZE(DAT_2%ELEM_VARS_1)))
          DAT_1%ELEM_VARS_1 = DAT_2%ELEM_VARS_1
        END IF
!
        IF (ALLOCATED(DAT_1%SF_GRADS)) DEALLOCATE (DAT_1%SF_GRADS)
        IF (ALLOCATED(DAT_2%SF_GRADS)) THEN
          ALLOCATE (DAT_1%SF_GRADS(SIZE(DAT_2%SF_GRADS, 1),             &
     &                             SIZE(DAT_2%SF_GRADS, 2),             &
     &                             SIZE(DAT_2%SF_GRADS, 3)))
          DAT_1%SF_GRADS = DAT_2%SF_GRADS
        END IF
!
        IF (ALLOCATED(DAT_1%SF_VALS)) DEALLOCATE (DAT_1%SF_VALS)
        IF (ALLOCATED(DAT_2%SF_VALS)) THEN
          ALLOCATE (DAT_1%SF_VALS(SIZE(DAT_2%SF_VALS, 1),               &
     &                            SIZE(DAT_2%SF_VALS, 2)))
          DAT_1%SF_VALS = DAT_2%SF_VALS
        END IF
!
        IF (ALLOCATED(DAT_1%WTS)) DEALLOCATE (DAT_1%WTS)
        IF (ALLOCATED(DAT_2%WTS)) THEN
          ALLOCATE (DAT_1%WTS(SIZE(DAT_2%WTS)))
          DAT_1%WTS = DAT_2%WTS
        END IF
!         
        IF (ALLOCATED(DAT_1%STR_INC)) DEALLOCATE (DAT_1%STR_INC)
        IF (ALLOCATED(DAT_2%STR_INC)) THEN
          ALLOCATE (DAT_1%STR_INC(SIZE(DAT_2%STR_INC, 1),               &
     &                            SIZE(DAT_2%STR_INC, 2)))
          DAT_1%STR_INC = DAT_2%STR_INC
        END IF
!         
        IF (ALLOCATED(DAT_1%ROT_INC)) DEALLOCATE (DAT_1%ROT_INC)
        IF (ALLOCATED(DAT_2%ROT_INC)) THEN
          ALLOCATE (DAT_1%ROT_INC(SIZE(DAT_2%ROT_INC, 1),               &
     &                             SIZE(DAT_2%ROT_INC, 2),              &
     &                             SIZE(DAT_2%ROT_INC, 3)))
          DAT_1%ROT_INC = DAT_2%ROT_INC
        END IF
!
        RETURN
      END SUBROUTINE ASSIGN_ELEM_DAT_CONT
!
!=======================================================================
!
      SUBROUTINE ASSIGN_ELEM_DAT_FACET (DAT_1, DAT_2)
!
        CLASS (ELEM_DAT_FACET), INTENT (INOUT) :: DAT_1
        TYPE (ELEM_DAT_FACET), INTENT (IN) :: DAT_2
!
!-----------------------------------------------------------------------
!
        IF (ASSOCIATED(DAT_2%BC_DAT)) THEN
          DAT_1%BC_DAT => DAT_2%BC_DAT
        ELSE
          DAT_1%BC_DAT => NULL()
        END IF
!
        IF (ALLOCATED(DAT_1%SF_GRADS)) DEALLOCATE (DAT_1%SF_GRADS)
        IF (ALLOCATED(DAT_2%SF_GRADS)) THEN
          ALLOCATE (DAT_1%SF_GRADS(SIZE(DAT_2%SF_GRADS, 1),             &
     &                             SIZE(DAT_2%SF_GRADS, 2),             &
     &                             SIZE(DAT_2%SF_GRADS, 3)))
          DAT_1%SF_GRADS = DAT_2%SF_GRADS
        END IF
!
        IF (ALLOCATED(DAT_1%SF_VALS)) DEALLOCATE (DAT_1%SF_VALS)
        IF (ALLOCATED(DAT_2%SF_VALS)) THEN
          ALLOCATE (DAT_1%SF_VALS(SIZE(DAT_2%SF_VALS, 1),               &
     &                            SIZE(DAT_2%SF_VALS, 2)))
          DAT_1%SF_VALS = DAT_2%SF_VALS
        END IF
!
        IF (ALLOCATED(DAT_1%WTS)) DEALLOCATE (DAT_1%WTS)
        IF (ALLOCATED(DAT_2%WTS)) THEN
          ALLOCATE (DAT_1%WTS(SIZE(DAT_2%WTS)))
          DAT_1%WTS = DAT_2%WTS
        END IF
!
        IF (ALLOCATED(DAT_1%NRML)) DEALLOCATE (DAT_1%NRML)
        IF (ALLOCATED(DAT_2%NRML)) THEN
          ALLOCATE (DAT_1%NRML(SIZE(DAT_2%NRML, 1),                     &
     &                       SIZE(DAT_2%NRML, 2)))
          DAT_1%NRML = DAT_2%NRML
        END IF
!
        RETURN
      END SUBROUTINE ASSIGN_ELEM_DAT_FACET
!
!=======================================================================
!
      SUBROUTINE DELETE_FRC_STF (ELEM)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(ELEM%FRC)) DEALLOCATE (ELEM%FRC)
        IF (ALLOCATED(ELEM%STF)) DEALLOCATE (ELEM%STF)
!
      RETURN
      END SUBROUTINE DELETE_FRC_STF
!
!=======================================================================
!
      SUBROUTINE DELETE_ELEM (ELEM)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
!
!-----------------------------------------------------------------------
!
        CALL ELEM%DELETE_FRC_STF
        CALL ELEM%LM_DAT%DELETE
        IF (ALLOCATED(ELEM%NODE_IDS)) DEALLOCATE (ELEM%NODE_IDS)
        ELEM%GOT_SF_DAT = .FALSE.
!
        RETURN
      END SUBROUTINE DELETE_ELEM
!
!=======================================================================
!
      SUBROUTINE GET_SF_DAT (ELEM, ELEM_DOF)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
          CALL ELEM%GET_SF_DAT_CONT (ELEM_DOF)
        TYPE IS (ELEM_DAT_FACET)
          CALL ELEM%GET_SF_DAT_FACET (ELEM_DOF)
        END SELECT
!
        RETURN
      END SUBROUTINE GET_SF_DAT
!
!=======================================================================
!
      SUBROUTINE GET_RESID_STIFFNESS (ELEM, ELEM_DOF, NEED_STF)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        LOGICAL, INTENT (IN) :: NEED_STF
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
          CALL ELEM%GET_RESID_STIFF_CONT (ELEM_DOF, NEED_STF)
        TYPE IS (ELEM_DAT_FACET)
          CALL ELEM%GET_RESID_STIFF_FACET (ELEM_DOF, NEED_STF)
        END SELECT
!
        RETURN
      END SUBROUTINE GET_RESID_STIFFNESS
!
!=======================================================================
!
      SUBROUTINE GET_RESID_STIFF_GUESS (ELEM, ELEM_DOF, DISP_BCS,       &
     &                                  T0, T1, T2)
!
!       This routine computes the element contribution to the
!       residual, and, if indicated, to the tangent stiffness,
!       for a single continuum element.
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        REAL (DBL), INTENT (IN) :: DISP_BCS(:)
        REAL (DBL), INTENT (IN) :: T0, T1, T2
!
!       declare local variables
!
        INTEGER :: K, NIP, NX, NC, NND
        REAL (DBL), ALLOCATABLE :: FRC_IP(:), STF_IP(:,:)
        LOGICAL :: TEMPER
!
!-----------------------------------------------------------------------
!
!       only do something if the LM_DAT component of ELEM is of the 
!       right type
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
!
!         set number of IPs for the current element
!
          NIP = SIZE(DAT%WTS)
!
!         set the spatial dimension of the problem
!
          NC = SIZE(DAT%SF_GRADS, 1)
!
!         set number of nodes for current element
! 
          NND = SIZE(ELEM_DOF%NUM_NODAL_DOF, 1)
!           
          ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,                  &
     &               NODE_IDS => ELEM%NODE_IDS)
!           
!         delete FRC and STF, if they currently exist
!
          CALL ELEM%DELETE_FRC_STF 
!
!         determine if a temperature dof is present at any node of the
!         element
!
          IF (SIZE(ELEM_DOF%XYZ, 1) .EQ. 2) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 3) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          ELSE IF (SIZE(ELEM_DOF%XYZ, 1) .EQ. 3) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 4) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          END IF
!
!         Get the total number of dof for the element,
!         allocate and initialize element FRC and STF, and allocate
!         local variable DE_DU if necessary, which holds the derivative
!         of the strain increment WRT incr. nodal displacements.  Also,
!         allocate the local IP contributions to FRC and STF.
!         Note that the constitutive update and material tangent modulus
!         calculations are always done in 3D, even for 2D problems.
!
          NX = SUM(ELEM_DOF%NUM_NODAL_DOF(:))
          ALLOCATE (ELEM%FRC(NX), FRC_IP(NX))
          ELEM%FRC = 0.0_DBL
          ALLOCATE (ELEM%STF(NX,NX), STF_IP(NX,NX))
          ELEM%STF = 0.0_DBL
!           
          DO K = 1, NIP
            frc_ip = 0.0_dbl
            stf_ip = 0.0_dbl
!             
            CALL IP_CONT_GUESS (ELEM_DOF, DAT%SF_GRADS(:,:,K),          &
     &                          DAT%STR_INC(:,K), DAT%ROT_INC(:,:,K),   &
     &                          FRC_IP, STF_IP, T0, T1, T2)
!
!           accumulate IP contributions to the element FRC and STF
! 
            ELEM%FRC = ELEM%FRC + DAT%WTS(K) * FRC_IP
            ELEM%STF = ELEM%STF + DAT%WTS(K) * STF_IP
!
!         close loop over IPs
!
          END DO
!           
!         apply essential boundary conditions
! 
          ELEM%FRC = ELEM%FRC - M_V(ELEM%STF, DISP_BCS)
!
          DEALLOCATE (FRC_IP, STF_IP)
          END ASSOCIATE
!
        END SELECT
!
      RETURN
      END SUBROUTINE GET_RESID_STIFF_GUESS
!
!=======================================================================
!
      SUBROUTINE GET_RESID_TOL (ELEM, ELEM_DOF)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
!
!-----------------------------------------------------------------------
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
          CALL ELEM%GET_RESID_TOL_CONT (ELEM_DOF)
        END SELECT
!
        RETURN
      END SUBROUTINE GET_RESID_TOL
!
!=======================================================================
!
      SUBROUTINE GET_SF_DAT_CONT (ELEM, ELEM_DOF)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
!
!       declare local variables
!
        INTEGER :: NND, NIP, I, J, K, N, NC
        REAL (DBL) :: DET
        REAL (DBL), ALLOCATABLE :: SF(:,:), RN(:,:,:), RJ(:,:),         &
     &                             RJ_INV(:,:), PARENT_WTS(:)
!
!-----------------------------------------------------------------------
!
!       only do something if the LM_DAT component of ELEM is of the
!       right type
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
!
!         set the number of coordinates, i.e. the dimensionality of the
!         problem; and allocate the Jacobian and its inverse
!
          NC = SIZE(ELEM_DOF%XYZ, 1)
          ALLOCATE (RJ(NC,NC), RJ_INV(NC,NC))
!
!         Set nodal and IP coordinates, and SF values and parent-element
!         gradients, depending on the LM_TYPE code.  Further LM_TYPEs
!         can be accommodated by adding more CASEs here.
!
          SELECT CASE (ELEM%LM_TYPE) 
          CASE (1)
!
!           standard isoparametric 2D 4-node quad element
!
            CALL SF_ELEM_1 (NIP, NND, SF, RN, PARENT_WTS)
!
          CASE (2)
!           
!           standard isoparametric 3D 8-node quad element
! 
            CALL SF_ELEM_2 (NIP, NND, SF, RN, PARENT_WTS)
!             
          CASE (10)
!           
!           standard isoparametric 2D 9-node Lagrange element
! 
            CALL SF_ELEM_10 (NIP, NND, SF, RN, PARENT_WTS)
!             
          CASE DEFAULT
!
            PRINT *, 'continuum element type code ', ELEM%LM_TYPE,      &
     &               ' is currently not supported'
            PRINT *, 'aborting'
            STOP 
!
          END SELECT
!
!         allocate the element's SF data objects, if not already
!
          IF (.NOT. ALLOCATED(DAT%SF_GRADS)) THEN
            ALLOCATE (DAT%SF_GRADS(NC,NND,NIP))
          END IF
          IF (.NOT. ALLOCATED(DAT%SF_VALS)) THEN
            ALLOCATE (DAT%SF_VALS(NND,NIP))
          END IF
          IF (.NOT. ALLOCATED(DAT%WTS)) THEN
            ALLOCATE (DAT%WTS(NIP))
          END IF
!
!         compute and store SF values, gradients, and weights for the 
!         continuum element
!
          DO K = 1, NIP
!
!           set SF gradients and values, and weights, at IP number K
!
            FORALL (J = 1 : NND) DAT%SF_VALS(J,K) = SF(J,K)
            RJ = 0.0_DBL
            DO N = 1, NND
              FORALL (I = 1 : NC, J = 1 : NC)
                RJ(I,J) = RJ(I,J) + ELEM_DOF%XYZ(I,N) * RN(J,N,K)
              END FORALL
            END DO
            IF (NC .EQ. 2) THEN
              DET = RJ(1,1) * RJ(2,2) - RJ(1,2) * RJ(2,1)
            ELSE
              DET = DETM(RJ, 3)
            END IF
            DAT%WTS(K) = PARENT_WTS(K) * DET
            CALL INVERSE (RJ, RJ_INV)
            FORALL (I = 1 : NC, N = 1 : NND)
              DAT%SF_GRADS(I,N,K) = SUM(RJ_INV(1:NC,I) * RN(1:NC,N,K))
            END FORALL
!
          END DO
!
          ELEM%GOT_SF_DAT = .TRUE.
!
          DEALLOCATE (SF, RN, RJ, RJ_INV, PARENT_WTS)
!
        END SELECT
!
      RETURN
      END SUBROUTINE GET_SF_DAT_CONT
!
!=======================================================================
!
      SUBROUTINE GET_RESID_STIFF_CONT (ELEM, ELEM_DOF, NEED_STF)
!
!       This routine computes the element contribution to the
!       residual, and, if indicated, to the tangent stiffness,
!       for a single continuum element.
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        LOGICAL, INTENT (IN) :: NEED_STF
!
!       declare local variables
!
        INTEGER :: K, NIP, NX, NC
        REAL (DBL), ALLOCATABLE :: DE_DU(:,:),                          &   
     &                             TAN_MOD(:,:), FRC_IP(:), STF_IP(:,:),&
     &                             F0(:,:), F1(:,:)
        LOGICAL :: TEMPER
!
!-----------------------------------------------------------------------
!
!       only do something if the LM_DAT component of ELEM is of the 
!       right type
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
!
!         set number of IPs for the current element
!
          NIP = SIZE(DAT%WTS)
!
!         set the spatial dimension of the problem
!
          NC = SIZE(DAT%SF_GRADS, 1)
!
!         delete FRC and STF, if they currently exist
!
          CALL ELEM%DELETE_FRC_STF 
!
!         determine if a temperature dof is present at any node of the
!         element
!
          IF (SIZE(ELEM_DOF%XYZ, 1) .EQ. 2) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 3) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          ELSE IF (SIZE(ELEM_DOF%XYZ, 1) .EQ. 3) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 4) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          END IF
!
!         Get the total number of dof for the element,
!         allocate and initialize element FRC and STF, and allocate
!         local variable DE_DU if necessary, which holds the derivative
!         of the strain increment WRT incr. nodal displacements.  Also,
!         allocate the local IP contributions to FRC and STF.
!         Note that the constitutive update and material tangent modulus
!         calculations are always done in 3D, even for 2D problems.
!
          NX = SUM(ELEM_DOF%NUM_NODAL_DOF(:))
          ALLOCATE (ELEM%FRC(NX), FRC_IP(NX))
          ELEM%FRC = 0.0_DBL
          IF (NEED_STF) THEN
            ALLOCATE (ELEM%STF(NX,NX), STF_IP(NX,NX))
            ELEM%STF = 0.0_DBL
            IF (TEMPER) THEN
              ALLOCATE (DE_DU(9,NX))
            ELSE
              ALLOCATE (DE_DU(6,NX))
            END IF
          END IF
!
!         allocate the strain increment and the tangent modulus
!
          IF (TEMPER) THEN
            ALLOCATE (TAN_MOD(9,9))
            IF (.NOT. ALLOCATED(DAT%STR_INC)) THEN
              ALLOCATE (DAT%STR_INC(9,NIP))
            END IF     
          ELSE
            ALLOCATE (TAN_MOD(6,6))
            IF (.NOT. ALLOCATED(DAT%STR_INC)) THEN
              ALLOCATE (DAT%STR_INC(6,NIP))
            END IF              
          END IF
!
!         allocate rotation increment and derivative of rotation
!         increment WRT incr. nodal displacements
!
          IF (CUR_GLOBAL_SIM_VARS%FINITE_DEF) THEN
            IF (.NOT. ALLOCATED(DAT%ROT_INC)) THEN
              ALLOCATE (DAT%ROT_INC(3,3,NIP))
            END IF  
          END IF
!
          DO K = 1, NIP
!
!           form a strain increment at the current IP
!
            IF (CUR_GLOBAL_SIM_VARS%FINITE_DEF) THEN
               CALL STRAIN_INC_LRG_DEF (ELEM_DOF, DAT%SF_GRADS(:,:,K),  &
     &               DAT%STR_INC(:,K), DAT%ROT_INC(:,:,K))
            ELSE
              IF (NEED_STF) THEN
                CALL STRAIN_INC_SML_DEF (ELEM_DOF, DAT%SF_GRADS(:,:,K), &
     &               DAT%STR_INC(:,K), DE_DU)
              ELSE
                CALL STRAIN_INC_SML_DEF (ELEM_DOF, DAT%SF_GRADS(:,:,K), &
     &               DAT%STR_INC(:,K))
              END IF
            END IF
!
!           Update the stress (and heat flux vector for coupled
!           thermal materials), and get the material's tangent modulus
!           with a call to the constitutive model.  If any of the
!           current element's nodes have temperature dof, then it is 
!           assumed that the called constitutive model is a coupled 
!           one.  In such cases, TAN_MOD should come back 9x9. 
!           Otherwise, TAN_MOD should be 6x6.
!
            SELECT CASE (DAT%MAT%MAT_TYPE)
            CASE (1)
              CALL CM_1 (DAT%MAT%PROPS, DAT%IP_VARS_0(:,K),             &
     &                   DAT%STR_INC(:,K), DAT%IP_VARS_1(:,K), TAN_MOD)
            CASE (2)
              ALLOCATE(F0(3,3), F1(3,3))
              CALL GET_DEF_GRAD(ELEM_DOF, DAT%SF_GRADS(:,:,K), F0)
              IF (NEED_STF) THEN
                CALL CM_2 (DAT%MAT%PROPS, F0, F1, DAT%STR_INC(:,K),     &
     &                     DAT%IP_VARS_1(:,K), TAN_MOD)
              ELSE
                CALL CM_2 (DAT%MAT%PROPS, F0, F1, DAT%STR_INC(:,K),     &
     &                     DAT%IP_VARS_1(:,K))  
              END IF
              DEALLOCATE(F0, F1)
            CASE DEFAULT
              PRINT *, 'invalid material ID specified'
              PRINT *, 'aborting'
              STOP
            END SELECT
!             
!           Rotate the stress update from the constitutive model by the
!           appropriate rotation increment
!
            IF (CUR_GLOBAL_SIM_VARS%FINITE_DEF) THEN
              CALL FWD_ROT (DAT%IP_VARS_1(:,K), DAT%ROT_INC(:,:,K))
            END IF   
!
!           form contributions to the element residual vector and, 
!           optionally, the element stiffness matrix for the current IP
!
            IF (CUR_GLOBAL_SIM_VARS%FINITE_DEF) THEN
              IF (NEED_STF) THEN
               CALL IP_CONT_LRG_DEF (ELEM_DOF, DAT%ROT_INC(:,:,K),      &
     &              DAT%SF_GRADS(:,:,K), DAT%IP_VARS_1(:,K), FRC_IP,    &
     &              TAN_MOD = TAN_MOD, STF = STF_IP)
              ELSE
               CALL IP_CONT_LRG_DEF (ELEM_DOF, DAT%ROT_INC(:,:,K),      &
     &              DAT%SF_GRADS(:,:,K), DAT%IP_VARS_1(:,K), FRC_IP)
              END IF
            ELSE
              IF (NEED_STF) THEN
                CALL IP_CONT_SML_DEF (ELEM_DOF%NUM_NODAL_DOF,           &
     &              DAT%SF_GRADS(:,:,K), DAT%IP_VARS_1(:,K), FRC_IP,    &
     &              DE_DU, TAN_MOD = TAN_MOD, STF = STF_IP)
              ELSE
                CALL IP_CONT_SML_DEF (ELEM_DOF%NUM_NODAL_DOF,           &
     &              DAT%SF_GRADS(:,:,K), DAT%IP_VARS_1(:,K), FRC_IP)
              END IF
            END IF
!
!           accumulate IP contributions to the element FRC and STF
!
            ELEM%FRC = ELEM%FRC + DAT%WTS(K) * FRC_IP
            IF (NEED_STF) ELEM%STF = ELEM%STF + DAT%WTS(K) * STF_IP
!
!           close loop over IPs
!
          END DO
!
          DEALLOCATE (FRC_IP, TAN_MOD)
          IF (NEED_STF) DEALLOCATE (DE_DU, STF_IP)
!
        END SELECT
!
      RETURN
      END SUBROUTINE GET_RESID_STIFF_CONT
!
!=======================================================================
!
      SUBROUTINE GET_RESID_TOL_CONT (ELEM, ELEM_DOF)
!
!       This routine computes the element contribution to the
!       residual tolerance vector for a single continuum element.
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
!
!       declare local variables
!
        INTEGER :: K, NIP, NX, NC, NND, I, A, N
        REAL (DBL), ALLOCATABLE :: FRC_IP(:)
        LOGICAL :: TEMPER
!
!-----------------------------------------------------------------------
!
!       only do something if the LM_DAT component of ELEM is of the 
!       right type
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
!
!         set number of IPs for the current element
!
          NIP = SIZE(DAT%WTS)
!
!         set the number of nodes, and the spatial dimension
!
          NND = SIZE(ELEM_DOF%NUM_NODAL_DOF)
          NC = SIZE(DAT%SF_GRADS, 1)
!
!         delete FRC and STF, if they currently exist
!
          CALL ELEM%DELETE_FRC_STF 
!
!         determine if a temperature dof is present at any node of the
!         element
!
          IF (SIZE(ELEM_DOF%XYZ, 1) .EQ. 2) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 3) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          ELSE IF (SIZE(ELEM_DOF%XYZ, 1) .EQ. 3) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 4) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          END IF
!
!         Get the total number of dof for the element,
!         and allocate and initialize element FRC.  Also,
!         allocate the local IP contribution to FRC.
!
          NX = SUM(ELEM_DOF%NUM_NODAL_DOF(:))
          ALLOCATE (ELEM%FRC(NX), FRC_IP(NX))
          ELEM%FRC = 0.0_DBL
!
!         loop over integration points
!
          DO K = 1, NIP
!
!           form the IP contribution to the element residual-tolerance
!           vector
!
            N = 0
            DO A = 1, NND
              FORALL (I = 1 : NC) FRC_IP(N+I) =                         &
     &           CONVRG_PARAMS%STRESS_TOL * ABS(DAT%SF_GRADS(I,A,K))
              N = N + ELEM_DOF%NUM_NODAL_DOF(A)
            END DO
!
!           accumulate IP contributions to the element FRC 
!
            ELEM%FRC = ELEM%FRC + DAT%WTS(K) * FRC_IP
!
!           close loop over IPs
!
          END DO
!
          DEALLOCATE (FRC_IP)
!
        END SELECT
!
      RETURN
      END SUBROUTINE GET_RESID_TOL_CONT
!
!
!=======================================================================
!
      SUBROUTINE DELETE_ELEM_CONT_DAT (LM_DAT)
!
        CLASS (ELEM_DAT_CONTINUUM), INTENT (INOUT) :: LM_DAT
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(LM_DAT%IP_VARS_0)) DEALLOCATE(LM_DAT%IP_VARS_0)
        IF (ALLOCATED(LM_DAT%IP_VARS_1)) DEALLOCATE(LM_DAT%IP_VARS_1)
        IF (ALLOCATED(LM_DAT%ELEM_VARS_0))                              &
     &                DEALLOCATE(LM_DAT%ELEM_VARS_0)
        IF (ALLOCATED(LM_DAT%ELEM_VARS_1))                              &
     &                DEALLOCATE(LM_DAT%ELEM_VARS_1)
        IF (ALLOCATED(LM_DAT%SF_GRADS)) DEALLOCATE(LM_DAT%SF_GRADS)
        IF (ALLOCATED(LM_DAT%SF_VALS)) DEALLOCATE(LM_DAT%SF_VALS)
        IF (ALLOCATED(LM_DAT%WTS)) DEALLOCATE(LM_DAT%WTS)
        IF (ALLOCATED(LM_DAT%STR_INC)) DEALLOCATE(LM_DAT%STR_INC)
        IF (ALLOCATED(LM_DAT%ROT_INC)) DEALLOCATE(LM_DAT%ROT_INC)
        NULLIFY (LM_DAT%MAT)
!
      RETURN
      END SUBROUTINE DELETE_ELEM_CONT_DAT
!
!=======================================================================
!
      SUBROUTINE GET_SF_DAT_FACET (ELEM, ELEM_DOF)
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
!
!       declare local variables
!
        INTEGER :: NND, NIP, I, J, K, N, NC, NCF
        REAL (DBL) :: RL, EP(3), NRML(3)
        REAL (DBL) :: DX(3,2), H(2,2), H_INV(2,2), M(2,3)
        REAL (DBL), ALLOCATABLE :: SF(:,:), RN(:,:,:), PARENT_WTS(:),   &
     &                             HA(:,:)
!
!-----------------------------------------------------------------------
!
!       only do something if the LM_DAT component of ELEM is of the 
!       right type
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_FACET)
!
!         set the number of coordinates, i.e. the dimensionality of the
!         problem; and the dimensionality of the facet
!
          NC = SIZE(ELEM_DOF%XYZ, 1)
          NCF = NC - 1
!
!         Set nodal and IP coordinates, and SF values and parent-element
!         gradients, depending on the LM_TYPE code.  Further LM_TYPEs
!         can be accommodated by adding more CASEs here.
!
          SELECT CASE (ELEM%LM_TYPE) 
          CASE (101)
!
!           standard isoparametric 1D 2-point linear element:  this is
!           a facet element for the 2D 4-node quad (LM_TYPE = 1)
!
            CALL SF_ELEM_101 (NIP, NND, SF, RN, PARENT_WTS)
!
          CASE (102)
!
!           standard isoparametric 2D 4-point bilinear element:  this is
!           a facet element for the 3D 8-node quad (LM_TYPE = 2)
!
            CALL SF_ELEM_102 (NIP, NND, SF, RN, PARENT_WTS)
!
          CASE (110)
!
!           standard isoparametric 1D 3-point quadratic element:  this is
!           a facet element for the 2D 9-node Lagrange (LM_TYPE = 10)
!
            CALL SF_ELEM_110 (NIP, NND, SF, RN, PARENT_WTS)
!
          CASE DEFAULT
!
            PRINT *, 'facet element type code ', ELEM%LM_TYPE,          &
     &               ' is currently not supported'
            PRINT *, 'aborting'
            STOP 
!
          END SELECT
!
!         allocate the element's SF data objects, if not already
!
          IF (.NOT. ALLOCATED(DAT%SF_GRADS)) THEN
            ALLOCATE (DAT%SF_GRADS(NC,NND,NIP))
          END IF
          IF (.NOT. ALLOCATED(DAT%SF_VALS)) THEN
            ALLOCATE (DAT%SF_VALS(NND,NIP))
          END IF
          IF (.NOT. ALLOCATED(DAT%WTS)) THEN
            ALLOCATE (DAT%WTS(NIP))
          END IF
          IF (.NOT. ALLOCATED(DAT%NRML)) THEN
            ALLOCATE (DAT%NRML(NC,NIP))
          END IF
!           
          ALLOCATE (HA(NCF,NND))
!           
!
!         Compute and store SF values, SF gradients, and weights for the 
!         facet element.  Note that, in contrast to the situation with
!         continuum elements, the SF_GRADS here are gradients of the
!         SFs WRT parent-element surface coordinates (not physical 
!         coordinates).  The WTS, however, correspond to integration
!         in the physical reference configuration; i.e. they incorporate
!         the area stretch in the isoparametric transformation to the
!         reference configuration.
!  
          DO K = 1, NIP
!
!           Set SF gradients and values, weights, and outward unit 
!           normal (all relative to the global ref. config.)   
!           at IP number K.  Note that the orientation of the outward
!           normal will depend on the ordering of the facet nodes.
!           For 2D elements, the calculation below assumes that facet
!           nodes occur in counterclockwise order around the element.
!           For 3D elements, it should be assumed that the facet-node
!           ordering corresponds to the element-outward direction by
!           the right-hand rule.
!
!           Modifications herein made on 03.04.2015
!           -BDG
!
            DO J = 1, NND
              DAT%SF_VALS(J,K) = SF(J,K)
            END DO
            DX = 0.0_DBL
            DO J = 1, NCF
              DO I = 1, NC
                DX(I,J) = SUM(ELEM_DOF%XYZ(I,1:NND) * RN(J,1:NND,K))
              END DO
            END DO
            IF (NCF .EQ. 1) DX(3,2) = 1.0_DBL
            NRML(1:3) = CROSS_PR(DX(1:3,1), DX(1:3,2))
            RL = SQRT(SUM(NRML(1:NC) * NRML(1:NC)))
            DAT%WTS(K) = PARENT_WTS(K) * RL
            DAT%NRML(1:NC,K) = NRML(1:NC) / RL
            EP = 0.0_DBL
            EP(MINLOC(ABS(NRML(1:NC)))) = 1.0_DBL
            M(1,1:NC) = EP(1:NC) - SUM(NRML(1:NC) * EP(1:NC))           &
     &                           * NRML(1:NC)
            M(1,1:NC) = M(1,1:NC) / SQRT(SUM(M(1,1:NC) * M(1,1:NC)))
            IF (NCF .EQ. 2) M(2,1:NC) = CROSS_PR(NRML,M(1,1:NC))
            DO J = 1, NCF
              DO I = 1, NCF 
                H(I,J) = SUM(DX(1:NC,I) * M(J,1:NC))
              END DO
            END DO
            CALL INVERSE (H(1:NCF,1:NCF), H_INV(1:NCF,1:NCF))
            DO N = 1, NND
              DO I = 1, NCF
                HA(I,N) = SUM(H_INV(I,1:NCF) * RN(1:NCF,N,K))
              END DO
            END DO
            DO N = 1, NND
              DO I = 1, NC
                DAT%SF_GRADS(I,N,K) = SUM(HA(1:NCF,N) * M(1:NCF,I))
              END DO
            END DO
!           
          END DO
!           
          DEALLOCATE (SF, RN, PARENT_WTS, HA)
!
        END SELECT
!
      RETURN
      END SUBROUTINE GET_SF_DAT_FACET
!
!=======================================================================
!
      SUBROUTINE GET_RESID_STIFF_FACET (ELEM, ELEM_DOF, NEED_STF)
!
!       This routine computes the element contribution to the
!       residual, and, if indicated, to the tangent stiffness,
!       for a single facet element.
!
        CLASS (ELEM_CONTAINER), INTENT (INOUT) :: ELEM
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        LOGICAL, INTENT (IN) :: NEED_STF
!
!       declare local variables
!
        INTEGER :: NC, NIP, NX, K
        REAL (DBL) :: TF_VAL, PRES
        REAL (DBL), ALLOCATABLE :: FRC_IP(:), STF_IP(:,:), TRAC_VEC(:)
        LOGICAL :: TEMPER
!
!-----------------------------------------------------------------------
!
!       only do something if the LM_DAT component of ELEM is of the 
!       right type
!
        SELECT TYPE (DAT => ELEM%LM_DAT)
        TYPE IS (ELEM_DAT_FACET)
!
!         set the number of IPs for the current facet element
!
          NIP = SIZE(DAT%WTS)
!
!         set the number of spatial dimensions
!
          NC = SIZE(ELEM_DOF%XYZ, 1)
!
!         determine if a temperature dof is present at any node of the
!         element
!
          IF (NC .EQ. 2) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 3) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          ELSE IF (NC .EQ. 3) THEN
            IF (SIZE(ELEM_DOF%U_INC, 1) .EQ. 4) THEN
              TEMPER = .TRUE.
            ELSE
              TEMPER = .FALSE.
            END IF
          END IF
!
!         Get the total number of dof for the element,
!         allocate and initialize element FRC and STF, and allocate
!         local variable DE_DU if necessary, which holds the derivative
!         of the strain increment WRT nodal displacements.  Also,
!         allocate the local IP contributions to FRC and STF.
!         Note that the constitutive update and material tangent modulus
!         calculations are always done in 3D, even for 2D problems.
!
          NX = SUM(ELEM_DOF%NUM_NODAL_DOF(:))
          ALLOCATE (ELEM%FRC(NX), FRC_IP(NX), TRAC_VEC(NC))
          ELEM%FRC = 0.0_DBL
          IF (NEED_STF) THEN
            ALLOCATE (ELEM%STF(NX,NX), STF_IP(NX,NX))
            ELEM%STF = 0.0_DBL
          END IF
!
!         form the Cauchy traction vector or pressure that acts on the 
!         facet at time T_END
!
          ASSOCIATE (BC => DAT%BC_DAT)
          TF_VAL = BC%TF%EVAL(CUR_SOLN_STAT%T_END)
          IF (BC%FBC_TYPE .EQ. 'CTRAC' .OR. BC%FBC_TYPE .EQ. 'PTRAC')   &
     &     THEN
            TRAC_VEC(1:NC) = TF_VAL * BC%TRAC(1:NC)
            PRES = 0.0_DBL
          ELSE IF (BC%FBC_TYPE .EQ. 'CPRES' .OR. BC%FBC_TYPE .EQ.       &
     &     'PPRES') THEN
            PRES = TF_VAL * BC%PRES
            TRAC_VEC = 0.0_DBL
          END IF
!           END ASSOCIATE
!
!         loop over the facet's IPs, and form and assemble IP
!         contributions to the facet's residual vector and stiffness
!         matrix
!
          DO K = 1, NIP
!
            IF (CUR_GLOBAL_SIM_VARS%FINITE_DEF) THEN
!
!             finite-deformation case:  an IP stiffness contribution
!             must be computed, if requested
! 
              CALL IP_FCT_LRG_DEF (ELEM_DOF, DAT%SF_GRADS(:,:,K),       &
     &             DAT%SF_VALS(:,K), FRC_IP, STF_IP, DAT%BC_DAT,        &
     &             NEED_STF, DAT%NRML(:,K))   
!      
            ELSE
!
!             small-deformation case:  no IP stiffness contribution
!             exists
!
              SELECT CASE (DAT%BC_DAT%FBC_TYPE)
              CASE ('CTRAC', 'PTRAC')
                CALL IP_FCT_SML_DEF (ELEM_DOF%NUM_NODAL_DOF,            &
     &               DAT%SF_VALS(:,K), FRC_IP, TRAC_VEC = TRAC_VEC)
              CASE ('CPRES', 'PPRES')
                CALL IP_FCT_SML_DEF (ELEM_DOF%NUM_NODAL_DOF,            &
     &               DAT%SF_VALS(:,K), FRC_IP, PRES = PRES,             &
     &               NRML = DAT%NRML(:,K))
              END SELECT
            END IF
!
!           accumulate contributions to the element FRC and STF
!
            ELEM%FRC = ELEM%FRC - DAT%WTS(K) * FRC_IP
            IF (NEED_STF .AND. CUR_GLOBAL_SIM_VARS%FINITE_DEF) ELEM%STF &
     &                         = ELEM%STF - DAT%WTS(K) * STF_IP
!
!           close loop over IPs
!
          END DO

          END ASSOCIATE          
!
          DEALLOCATE (FRC_IP, TRAC_VEC)
          IF (ALLOCATED(STF_IP)) DEALLOCATE (STF_IP)
!
        END SELECT
!
      RETURN
      END SUBROUTINE GET_RESID_STIFF_FACET
!
!=======================================================================
!
      SUBROUTINE DELETE_ELEM_FACET_DAT (LM_DAT)
!
        CLASS (ELEM_DAT_FACET), INTENT (INOUT) :: LM_DAT
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(LM_DAT%SF_GRADS)) DEALLOCATE(LM_DAT%SF_GRADS)
        IF (ALLOCATED(LM_DAT%SF_VALS)) DEALLOCATE(LM_DAT%SF_VALS)
        IF (ALLOCATED(LM_DAT%WTS)) DEALLOCATE(LM_DAT%WTS)
        NULLIFY (LM_DAT%BC_DAT)
!
      RETURN
      END SUBROUTINE DELETE_ELEM_FACET_DAT
!
!=======================================================================
!
      END MODULE ELEMS_NODES_M
