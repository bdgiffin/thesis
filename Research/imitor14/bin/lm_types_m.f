      MODULE LM_TYPES_M
!
!     This module holds module data which specifies the spatial
!     dimension (2 or 3) and the number of nodes for each element
!     type.  In addition, for continuum elements, the local node
!     numbers corresponding to each facet are defined here.
!     The module data is private; it is used only by the two module
!     procedures defined herein.  The module data must be added to
!     when new element types are implemented.
!
!     There are two module procedures defined herein.
!     
!-----------------------------------------------------------------------
!
      USE KINDS_M
      IMPLICIT NONE
!
!=======================================================================
!     Module data.
!=======================================================================
!
!     continuum element type IDs, and dimensions,
!     node counts, IP counts, element-variable counts, and facet counts 
!     for each type
!
      INTEGER, PRIVATE :: CONT_LM_TYPES(3) = (/ 1, 2, 10 /)
      INTEGER, PRIVATE :: LM_DIM(3) = (/ 2, 3, 2 /)
      INTEGER, PRIVATE :: CONT_LM_NUM_NDS(3) = (/ 4, 8, 9 /)
      INTEGER, PRIVATE :: CONT_LM_NUM_IPS(3) = (/ 4, 8, 9 /)
      INTEGER, PRIVATE :: CONT_LM_NUM_E_VARS(3) = (/ 0, 0, 0 /)
      INTEGER, PRIVATE :: CONT_LM_NUM_FCTS(3) = (/ 4, 6, 4 /)
!
!     facet element type IDs, and node and IP counts for each type
!
      INTEGER, PRIVATE :: FCT_LM_TYPES(3) = (/ 101, 102, 110 /)
      INTEGER, PRIVATE :: FCT_LM_NUM_NDS(3) = (/ 2, 4, 3 /)
      INTEGER, PRIVATE :: FCT_LM_NUM_IPS(3) = (/ 2, 4, 3 /)
!
!     continuum-element type 1 (4 node quad):
!     facet type IDs, and local node numbers for each facet
!
      INTEGER, TARGET, PRIVATE :: FCT_TYPES_1(4) =                      &
     &                    (/ 101, 101, 101, 101 /)
      INTEGER, TARGET, PRIVATE :: FCT_NODES_1(2, 4) =                   &
     &      RESHAPE ( (/ 1, 2,   2, 3,   3, 4,   4, 1 /),               &
     &                SHAPE = (/ 2, 4 /) )
!
!     continuum-element type 2 (8 node brick):
!     facet type IDs, and local node numbers for each facet
!
      INTEGER, TARGET, PRIVATE :: FCT_TYPES_2(6) =                      &
     &                    (/ 102, 102, 102, 102, 102, 102 /)
      INTEGER, TARGET, PRIVATE :: FCT_NODES_2(4,6) =                    &
     &      RESHAPE ( (/ 1, 4, 3, 2,   2, 3, 7, 6,   3, 4, 8, 7,        &
     &                   4, 1, 5, 8,   1, 2, 6, 5,   5, 6, 7, 8 /),     &
     &                SHAPE = (/ 4, 6 /) )     
!
!     continuum-element type 3 (9 node Lagrange) with 3x3 integration:
!     facet type IDs, and local node numbers for each facet
!
      INTEGER, TARGET, PRIVATE :: FCT_TYPES_10(4) =                     &
     &                    (/ 110, 110, 110, 110 /)
      INTEGER, TARGET, PRIVATE :: FCT_NODES_10(3, 4) =                  &
     &      RESHAPE ( (/ 1, 6, 2,   2, 7, 3,   3, 8, 4,   4, 5, 1 /),   &
     &                SHAPE = (/ 3, 4 /) ) 
!
!-----------------------------------------------------------------------
!
      PUBLIC :: NUM_ELEM_NDS, NUM_ELEM_IPS, NUM_ELEM_VARS, GET_FCT_NDS
!
!=======================================================================
      CONTAINS
!=======================================================================
!     Module procedures.
!=======================================================================
!
      FUNCTION NUM_ELEM_NDS (LM_TYPE)
!
!       returns the number of nodes for element type LM_TYPE
!
        INTEGER, INTENT (IN) :: LM_TYPE
        INTEGER :: NUM_ELEM_NDS
!
!       declare local variables
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        NUM_ELEM_NDS = 0
!
        DO I = 1, SIZE(CONT_LM_TYPES)
          IF (CONT_LM_TYPES(I) .EQ. LM_TYPE) THEN
            NUM_ELEM_NDS = CONT_LM_NUM_NDS(I)
            RETURN
          END IF
        END DO
!
        DO I = 1, SIZE(FCT_LM_TYPES)
          IF (FCT_LM_TYPES(I) .EQ. LM_TYPE) THEN
            NUM_ELEM_NDS = FCT_LM_NUM_NDS(I)
            RETURN
          END IF
        END DO
!
        RETURN
      END FUNCTION NUM_ELEM_NDS
!
!=======================================================================
!
      FUNCTION NUM_ELEM_IPS (LM_TYPE)
!
!       returns the number of integration points for element type 
!       LM_TYPE
!
        INTEGER, INTENT (IN) :: LM_TYPE
        INTEGER :: NUM_ELEM_IPS
!
!       declare local variables
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        NUM_ELEM_IPS = 0
!
        DO I = 1, SIZE(CONT_LM_TYPES)
          IF (CONT_LM_TYPES(I) .EQ. LM_TYPE) THEN
            NUM_ELEM_IPS = CONT_LM_NUM_IPS(I)
            RETURN
          END IF
        END DO
!
        DO I = 1, SIZE(FCT_LM_TYPES)
          IF (FCT_LM_TYPES(I) .EQ. LM_TYPE) THEN
            NUM_ELEM_IPS = FCT_LM_NUM_IPS(I)
            RETURN
          END IF
        END DO
!
        RETURN
      END FUNCTION NUM_ELEM_IPS
!
!=======================================================================
!
      FUNCTION NUM_ELEM_VARS (LM_TYPE)
!
!       returns the number of element variables for (continuum) element
!       type LM_TYPE
!
        INTEGER, INTENT (IN) :: LM_TYPE
        INTEGER :: NUM_ELEM_VARS
!
!       declare local variables
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        NUM_ELEM_VARS = 0
!
        DO I = 1, SIZE(CONT_LM_TYPES)
          IF (CONT_LM_TYPES(I) .EQ. LM_TYPE) THEN
            NUM_ELEM_VARS = CONT_LM_NUM_E_VARS(I)
            RETURN
          END IF
        END DO
!
        RETURN
      END FUNCTION NUM_ELEM_VARS
!
!=======================================================================
!
      SUBROUTINE GET_FCT_NDS (ND_LIST, CONT_LM_NDS, CONT_LM_TYPE,       &
     &                        LCL_FCT, FCT_LM_TYPE)
!
!       This routine takes as input an unordered list of global node 
!       IDs, an ordered list of global node IDs that define a single
!       continuum element, and the element type of the continuum
!       element.  If the unordered list of nodes corresponds
!       to a single facet of the continuum element, then the local facet
!       number and the facet-element type are returned.  In addition,
!       the input unordered node list is put in the correct order 
!       for the facet.  If the input node list does not correspond to
!       any facet of the continuum element, then LCL_FCT is returned 
!       as 0.
!
        INTEGER, INTENT (INOUT) :: ND_LIST(:)
        INTEGER, INTENT (OUT) :: LCL_FCT, FCT_LM_TYPE
        INTEGER, INTENT (IN) :: CONT_LM_NDS(:), CONT_LM_TYPE
!
!       declare local variables
!
        INTEGER, POINTER :: FCT_TYPES(:), FCT_NODES(:,:)
        INTEGER :: N, I, J, KN, LMI, M, FTI
!
!-----------------------------------------------------------------------
!
!       set the number of nodes in ND_LIST
!
        N = SIZE(ND_LIST)
!
!       get the index of the continuum element type
!
        LMI = MINLOC(ABS(CONT_LM_TYPES(:) - CONT_LM_TYPE), 1)
!
!       set pointers to the lists of facet types and local facet nodes
!       for the continuum element type
!
        SELECT CASE (CONT_LM_TYPE)
        CASE (1)
          FCT_TYPES => FCT_TYPES_1
          FCT_NODES => FCT_NODES_1
        CASE (2)
          FCT_TYPES => FCT_TYPES_2
          FCT_NODES => FCT_NODES_2       
        CASE (10)
          FCT_TYPES => FCT_TYPES_10
          FCT_NODES => FCT_NODES_10          
        END SELECT
!
!       loop over facets of the continuum element type
!
        LCL_FCT = 0
        DO M = 1, CONT_LM_NUM_FCTS(LMI)
!
!         get the index of the current facet type
!
          FTI = MINLOC(ABS(FCT_LM_TYPES(:) - FCT_TYPES(M)), 1)
!
!         if the current facet type has the same number of nodes as
!         does ND_LIST, then check for a match
!
          IF (FCT_LM_NUM_NDS(FTI) .EQ. N) THEN      
            KN = 0
            OUTER: DO I = 1, N
              DO J = 1, N
                IF (ND_LIST(I) .EQ. CONT_LM_NDS(FCT_NODES(J,M))) THEN
                  KN = KN + 1
                  CYCLE OUTER
                END IF
              END DO
            END DO OUTER
            IF (KN .EQ. N) THEN
              ND_LIST(1:N) = CONT_LM_NDS(FCT_NODES(1:N,M))
              LCL_FCT = M
              FCT_LM_TYPE = FCT_TYPES(M)
              EXIT
            END IF
          END IF
!
        END DO
!
        RETURN
      END SUBROUTINE GET_FCT_NDS
!
!=======================================================================
!
      END MODULE LM_TYPES_M
