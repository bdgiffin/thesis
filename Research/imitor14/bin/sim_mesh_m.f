      MODULE SIM_MESH_M
!
!     This module defines the SIM_MESH derived type.
!     A SIM_MESH object contains all of the data
!     that defines a finite element simulation as it exists at a 
!     particular point during a simulation:  the mesh, the material
!     types and properties, boundary-condition data, etc.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE MISC_TYPES_M
      USE ELEMS_NODES_M
      USE LIN_SOLVER_M
      USE LM_TYPES_M
      IMPLICIT NONE
!
!=======================================================================
!     SIM_MESH derived-type definition
!=======================================================================
!
      TYPE SIM_MESH
        TYPE (FILES) :: FILE_OBJ
        TYPE (ELEM_CONTAINER), ALLOCATABLE :: ELEMS(:)
        TYPE (NODE_CONTAINER), ALLOCATABLE :: NODES(:)
        TYPE (MATERIAL), POINTER :: MATERIALS(:)
        TYPE (NODAL_BC), ALLOCATABLE :: NODAL_BCS(:)
        TYPE (FACET_BC), POINTER :: FACET_BCS(:)
        TYPE (TIME_FN), POINTER :: TIME_FNS(:)
        TYPE (CONVRG_CONTROL) :: CONVRG_PARAMS
        TYPE (SOLN_STAT) :: SOLN_STATUS
        TYPE (GLOBAL_SIM_VARS) :: SIM_VARS
        TYPE (STEP_TIME), ALLOCATABLE :: STEPS(:)
        CLASS (LIN_SOLVER), POINTER :: LIN_EQ_OBJECT
!
        CONTAINS
!
        PROCEDURE :: DELETE_ELEMS
        PROCEDURE :: DELETE_NODES
        PROCEDURE :: DELETE_MATERIALS
        PROCEDURE :: DELETE_NODAL_BCS 
        PROCEDURE :: DELETE_FACET_BCS
        PROCEDURE :: DELETE_TIME_FNS
        PROCEDURE :: DELETE_LIN_EQ_OBJECT
        PROCEDURE :: DELETE_SIM_MESH
        PROCEDURE :: STEP_SOLN
        PROCEDURE :: UPD_U_INC
        PROCEDURE :: SET_U_INC_GUESS
        PROCEDURE :: UPD_GLB_VALS
        PROCEDURE :: CONVRG
        PROCEDURE :: ASSEMBLE_RESID
        PROCEDURE :: ASSEMBLE_RESID_TOL
        PROCEDURE :: GET_LCL_NODAL_DATA
        PROCEDURE :: SET_CUR_GLOBAL_VALS
        PROCEDURE :: PROB_SETUP
        PROCEDURE :: INIT_LIN_EQ
        PROCEDURE :: OUTPUT
        PROCEDURE :: INIT_INC_DOF
        PROCEDURE :: INIT_GUESS
        PROCEDURE :: ZERO_NODAL_VALS
      END TYPE SIM_MESH
!
!-----------------------------------------------------------------------
!
      PRIVATE :: DELETE_ELEMS, DELETE_NODES, DELETE_MATERIALS,          &
     &           DELETE_NODAL_BCS, DELETE_FACET_BCS, DELETE_TIME_FNS,   &
     &           DELETE_LIN_EQ_OBJECT, DELETE_SIM_MESH, INIT_LIN_EQ,    &
     &           STEP_SOLN, UPD_U_INC, UPD_GLB_VALS, CONVRG,            &
     &           ASSEMBLE_RESID, GET_LCL_NODAL_DATA, ASSEMBLE_RESID_TOL,&
     &           SET_CUR_GLOBAL_VALS, PROB_SETUP, OUTPUT, INIT_INC_DOF, &
     &           ZERO_NODAL_VALS
!
!=======================================================================
      CONTAINS
!=======================================================================
!     TBPs on type SIM_MESH
!=======================================================================
!
      SUBROUTINE STEP_SOLN (SM)
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
!
!       declare local variables
!
        INTEGER :: SOLN_PASS, MODE, NR_COUNT, NUM_ELEM, L
        TYPE (LCL_NODAL_DATA) :: ELEM_DOF
!
!-----------------------------------------------------------------------
!
!       associate variable names for simplicity
!
        ASSOCIATE (MAX_NR_ITER => SM%CONVRG_PARAMS%MAX_NR_ITER,         &
     &             MAX_PASS => SM%CONVRG_PARAMS%MAX_PASS,               &
     &             STEP_ID => SM%SOLN_STATUS%STEP_NUM)
!
!       set the initial solution status for this step
!
        SM%SOLN_STATUS%CONVERGED = .FALSE.
        SM%SOLN_STATUS%ABORT = .FALSE.
        SM%SOLN_STATUS%ITER = 0
        IF (STEP_ID .EQ. 1) THEN
          SM%SOLN_STATUS%T_BEG = 0.0_DBL
        ELSE
          SM%SOLN_STATUS%T_BEG = SM%STEPS(STEP_ID-1)%TM
        END IF
        SM%SOLN_STATUS%T_END = SM%STEPS(STEP_ID)%TM
!
!       set global values (module data in module ELEMS_NODES_M), so
!       that the procedures in ELEMS_NODES_M will have them when
!       they are called from routine STEP_SOLN
!
        CALL SM%SET_CUR_GLOBAL_VALS
!
!       initialize the nodal dof:  set U_INC where they
!       are prescribed by BC, and make an initial guess at U_INC
!       for the free dof
!
        CALL SM%INIT_INC_DOF
!
!       set solution mode to 1 and newton-raphson counter to zero,
!       and loop over the solution passes
!
        MODE = 1
        NR_COUNT = 0
!
        NUM_ELEM = SIZE(SM%ELEMS)
!
        DO SOLN_PASS = 1, MAX_PASS
!
!         zero the global residual vector if on a MODE = 1 pass, or
!         zero the global equation system if on a MODE = 2 pass
!
          IF (MODE .EQ. 1) THEN
            CALL SM%ZERO_NODAL_VALS (RESID = .TRUE.)
          ELSE IF (MODE .EQ. 2) THEN
            CALL SM%LIN_EQ_OBJECT%ZERO_SOLVER_DATA
          END IF
!
!         inner loop over elements to assemble the global residual
!         vector (MODE = 1) or the linear equation system (MODE = 2)
!
          DO L = 1, NUM_ELEM
!
!           copy nodal data for the current element into local
!           variable ELEM_DOF
!
            CALL SM%GET_LCL_NODAL_DATA (L, ELEM_DOF)
!
!           before trying to compute an element residual and/or stiffness,
!           check that the current element already has its shape-function
!           data in place (and create it if not)
!
            IF (.NOT. SM%ELEMS(L)%GOT_SF_DAT) THEN
              CALL SM%ELEMS(L)%GET_SF_DAT (ELEM_DOF)
            END IF
!
!           if we are on the first solution pass for the current
!           time step, assemble the residual-tolerance vector
!
            IF (SOLN_PASS .EQ. 1) THEN
              CALL SM%ELEMS(L)%GET_RESID_TOL (ELEM_DOF)
              CALL SM%ASSEMBLE_RESID_TOL (L)
            END IF
!
!           Evaluate the element residual force vector and/or 
!           tangent stiffness based on the solution mode. This 
!           subroutine resides in the ELEMS_NODES_M module.
!           (Note that an element, broadly defined, may be a facet.)
!
            IF (MODE .EQ. 1) THEN
              CALL SM%ELEMS(L)%GET_RESID_STIFFNESS (ELEM_DOF, .FALSE.)
            ELSE IF (MODE .EQ. 2) THEN
              CALL SM%ELEMS(L)%GET_RESID_STIFFNESS (ELEM_DOF, .TRUE.)
            END IF
!
!           assemble the element contribution to the global residual
!           vector, or the global linear system of equations
!
            IF (MODE .EQ. 1) THEN
              CALL SM%ASSEMBLE_RESID (L)
            ELSE IF (MODE .EQ. 2) THEN
              CALL SM%LIN_EQ_OBJECT%ASM_LIN_EQ (SM%ELEMS(L), ELEM_DOF)
            END IF
!
            CALL ELEM_DOF%DELETE 
!
          END DO
!
!         apply nodal force boundary conditions to the global residual
!         vector
!
!          CALL NOD_RESID (SM%NODAL_BCS(L), SM%LIN_EQ_OBJECT)
!
!         Solution mode 1 checks convergence and if not satisfied, sets
!         solution mode to 2 for a full Newton-Raphson iteration on the
!         next solution step.  Solution mode 2 solves the linear system
!         of equations and updates the incremental nodal displacements
!         and then sets the solution mode to 1 for a residual only 
!         evaluation on the next solution step.  If convergence is 
!         obtained on a mode 1 pass, global updates are made.
!
          IF (MODE .EQ. 1) THEN
            CALL SM%CONVRG
            IF (SM%SOLN_STATUS%CONVERGED) THEN
!
!             update the global variables necessary to complete the full
!             solution step from t_n to t_n+1
!
              CALL SM%UPD_GLB_VALS
!
              EXIT
            ELSE
              MODE = 2
            END IF
!
          ELSE IF (MODE .EQ. 2) THEN
!
!           solve the linear system of equations.
!
            CALL SM%LIN_EQ_OBJECT%LIN_SOLVE
!
!           update the incremental nodal displacement
!
            CALL SM%UPD_U_INC
!
!           set MODE for an evaluate-residual pass on the next pass
!
            MODE = 1
!
!           increment the NR iteration counter
!
            NR_COUNT = NR_COUNT + 1
            SM%SOLN_STATUS%ITER = NR_COUNT
!
!           check to see if the newton-raphson counter exceeds the
!           user specified maximum
!
            IF (NR_COUNT .GT. MAX_NR_ITER) THEN
              SM%SOLN_STATUS%ABORT = .TRUE.
              PRINT *, 'maximum newton iterations reached without',     &
     &                  ' reaching equilibrium convergence'
              EXIT
            END IF
            IF (SOLN_PASS .EQ. MAX_PASS) THEN
              SM%SOLN_STATUS%ABORT = .TRUE.
              PRINT *, 'maximum solution passes reached without',       &
     &                ' equilibrium convergence'
              EXIT
            END IF
!
          END IF
!
        END DO
!
        IF (SM%SOLN_STATUS%ABORT) STOP 'aborting'
!
      END ASSOCIATE
!
      RETURN
      END SUBROUTINE STEP_SOLN
!
!=======================================================================
!
      SUBROUTINE CONVRG (SM, MAX_RESID)
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
        REAL (DBL), INTENT (OUT), OPTIONAL :: MAX_RESID
!
!       declare local variables
!
        INTEGER :: I, NN
        REAL (DBL) :: XR(20), RSD(20), RSD_TOL(20), MR
        LOGICAL :: MR_PRES
!
!------------------------------------------------------------------------
!
        MR_PRES = PRESENT(MAX_RESID)
        MR = -1.0_DBL
        SM%SOLN_STATUS%CONVERGED = .TRUE.
!
        DO I = 1, SIZE(SM%NODES)
          ASSOCIATE (NODE => SM%NODES(I))
          NN = NODE%GET_NUM_DOF()
          CALL NODE%GET_NODAL_VALS (RESID = RSD(1:NN),                  &
     &                              RESID_TOL = RSD_TOL(1:NN))
          WHERE (SM%LIN_EQ_OBJECT%ID(1:NN,I) .GT. 0)
            XR(1:NN) = ABS(RSD(1:NN)) / RSD_TOL(1:NN)
          ELSEWHERE
            XR(1:NN) = -1.0_DBL
          END WHERE
          MR = MAX(MR, MAXVAL(XR(1:NN)))
          IF (.NOT. MR_PRES) THEN
            IF (MR .GT. 1.0_DBL) THEN
              SM%SOLN_STATUS%CONVERGED = .FALSE.
              RETURN
            END IF
          END IF
          END ASSOCIATE
        END DO
!
        IF (MR .GT. 1.0_DBL) SM%SOLN_STATUS%CONVERGED = .FALSE.
        IF (MR_PRES) MAX_RESID = MR
!
      RETURN
      END SUBROUTINE CONVRG
!
!=======================================================================
!
      SUBROUTINE ASSEMBLE_RESID (SM, L)
!
!       This routine sums element-L contributions to the residual
!       into element L's global residual entries.
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
        INTEGER, INTENT (IN) :: L
!
!       declare local variables
!
        INTEGER :: K, I
!
!------------------------------------------------------------------------
!
        ASSOCIATE (NIDS => SM%ELEMS(L)%NODE_IDS, ELEM => SM%ELEMS(L))
!
        K = 0
        DO I = 1, SIZE(NIDS)
          ASSOCIATE (NODE => SM%NODES(NIDS(I)))
          CALL NODE%SUM_RESID (ELEM%FRC(K+1:))
          K = K + NODE%GET_NUM_DOF()
          END ASSOCIATE
        END DO
        CALL ELEM%DELETE_FRC_STF
!
        END ASSOCIATE
!
        RETURN
      END SUBROUTINE ASSEMBLE_RESID
!
!=======================================================================
!
      SUBROUTINE ASSEMBLE_RESID_TOL (SM, L)
!
!       This routine sums element-L contributions to the residual
!       tolerance into the nodal residual-tolerance  entries for
!       element L.
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
        INTEGER, INTENT (IN) :: L
!
!       declare local variables
!
        INTEGER :: K, I
!
!------------------------------------------------------------------------
!
        SELECT TYPE (DAT => SM%ELEMS(L)%LM_DAT)
        TYPE IS (ELEM_DAT_CONTINUUM)
!
          ASSOCIATE (NIDS => SM%ELEMS(L)%NODE_IDS, ELEM => SM%ELEMS(L))
!
          K = 0
          DO I = 1, SIZE(NIDS)
            ASSOCIATE (NODE => SM%NODES(NIDS(I)))
            CALL NODE%SUM_RESID_TOL (ELEM%FRC(K+1:))
            K = K + NODE%GET_NUM_DOF()
            END ASSOCIATE
          END DO
          CALL ELEM%DELETE_FRC_STF
!
          END ASSOCIATE
!
        END SELECT
!
        RETURN
      END SUBROUTINE ASSEMBLE_RESID_TOL
!
!=======================================================================
!
      SUBROUTINE UPD_U_INC (SM)
!
!       This routine corrects the incremental nodal displacements for
!       the SM object with the current solution in SM's LIN_EQ_OBJECT
!       component.
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: I, NC
        REAL (DBL) :: UC(10)
!
!------------------------------------------------------------------------
!
        ASSOCIATE (ID => SM%LIN_EQ_OBJECT%ID)
!
        DO I = 1, SIZE(SM%NODES)
          ASSOCIATE (NODE => SM%NODES(I))
          NC = NODE%GET_NUM_DOF()
          WHERE (ID(1:NC,I) .NE. 0)
            UC(1:NC) = SM%LIN_EQ_OBJECT%U(ID(1:NC,I))
          ELSEWHERE
            UC(1:NC) = 0.0_DBL
          END WHERE
          CALL NODE%CORRECT_DOF (UC(1:NC))
          END ASSOCIATE
        END DO
!
        END ASSOCIATE
!
      RETURN
      END SUBROUTINE UPD_U_INC
!
!=======================================================================
!
      SUBROUTINE SET_U_INC_GUESS (SM)
!
!       This routine sets the incremental nodal displacements for
!       the SM object with the solution in SM's LIN_EQ_OBJECT
!       component for the initial guess. 
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: I, NC
        REAL (DBL) :: UC(10)
!
!------------------------------------------------------------------------
!
        ASSOCIATE (ID => SM%LIN_EQ_OBJECT%ID)
!
        DO I = 1, SIZE(SM%NODES)
          ASSOCIATE (NODE => SM%NODES(I))
          NC = NODE%GET_NUM_DOF()
          WHERE (ID(1:NC,I) .NE. 0)
            UC(1:NC) = SM%LIN_EQ_OBJECT%U(ID(1:NC,I))
          ELSEWHERE
            UC(1:NC) = 0.0_DBL
          END WHERE
          CALL NODE%CORRECT_DOF_GUESS (UC(1:NC))     
!           
          END ASSOCIATE        
        END DO
!
        END ASSOCIATE
!
      RETURN
      END SUBROUTINE SET_U_INC_GUESS
!
!=======================================================================
!
      SUBROUTINE UPD_GLB_VALS (SM)
!
!       This routine sums the incremental nodal displacements into the
!       total nodal displacements, for all nodes in the SM object.
!       It also overwrites beginning-step element-variable and 
!       IP-variable values with their end-step values.
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: I, L
!
!------------------------------------------------------------------------
!
        DO I = 1, SIZE(SM%NODES)
          CALL SM%NODES(I)%ADVANCE_DOF
        END DO
!
        DO L = 1, SIZE(SM%ELEMS)
          SELECT TYPE (DAT => SM%ELEMS(L)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            IF (ALLOCATED(DAT%IP_VARS_0)) DAT%IP_VARS_0 = DAT%IP_VARS_1
            IF (ALLOCATED(DAT%ELEM_VARS_0)) DAT%ELEM_VARS_0 =           &
     &                                      DAT%ELEM_VARS_1
          END SELECT
        END DO
!
        RETURN
      END SUBROUTINE UPD_GLB_VALS
!
!=======================================================================
!
      SUBROUTINE DELETE_ELEMS (SM)
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: L
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(SM%ELEMS)) THEN
          DO L = 1, SIZE(SM%ELEMS)
            CALL SM%ELEMS(L)%DELETE
          END DO
          DEALLOCATE (SM%ELEMS)
        END IF
!
      RETURN
      END SUBROUTINE DELETE_ELEMS
!
!=======================================================================
!
      SUBROUTINE DELETE_NODES (SM)
!
        CLASS (SIM_MESH) :: SM
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(SM%NODES)) THEN
          DEALLOCATE (SM%NODES)
        END IF
!
      RETURN
      END SUBROUTINE DELETE_NODES
!
!-----------------------------------------------------------------------
!
      SUBROUTINE DELETE_MATERIALS (SM)
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: L
!
!-----------------------------------------------------------------------
!
        IF (ASSOCIATED(SM%MATERIALS)) THEN
          DO L = 1, SIZE(SM%MATERIALS)
            IF (ALLOCATED(SM%MATERIALS(L)%PROPS)) THEN
              DEALLOCATE (SM%MATERIALS(L)%PROPS)
            END IF
          END DO
          DEALLOCATE (SM%MATERIALS)
        END IF
!
      RETURN
      END SUBROUTINE DELETE_MATERIALS
!
!=======================================================================
!
      SUBROUTINE DELETE_NODAL_BCS (SM)
!
        CLASS (SIM_MESH) :: SM
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(SM%NODAL_BCS)) THEN
          DO I = 1, SIZE(SM%NODAL_BCS)
            SM%NODAL_BCS(I)%TF => NULL()
          END DO
          DEALLOCATE (SM%NODAL_BCS)
        END IF
!
      RETURN
      END SUBROUTINE DELETE_NODAL_BCS
!
!=======================================================================
!
      SUBROUTINE DELETE_FACET_BCS (SM)
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        IF (ASSOCIATED(SM%FACET_BCS)) THEN
          DO I = 1, SIZE(SM%FACET_BCS)
            SM%FACET_BCS(I)%TF => NULL()
          END DO
          DEALLOCATE (SM%FACET_BCS)
        END IF
!
      RETURN
      END SUBROUTINE DELETE_FACET_BCS
!
!=======================================================================
!
      SUBROUTINE DELETE_TIME_FNS (SM)
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: L
!
!-----------------------------------------------------------------------
!
        IF (ASSOCIATED(SM%TIME_FNS)) THEN
          DO L = 1, SIZE(SM%TIME_FNS)
            IF (ALLOCATED(SM%TIME_FNS(L)%T_F)) THEN
              DEALLOCATE (SM%TIME_FNS(L)%T_F)
            END IF
          END DO
          DEALLOCATE (SM%TIME_FNS)
        END IF
!
      RETURN
      END SUBROUTINE DELETE_TIME_FNS
!
!=======================================================================
!
      SUBROUTINE DELETE_LIN_EQ_OBJECT (SM)
!
        CLASS (SIM_MESH) :: SM
!
!-----------------------------------------------------------------------
!
        IF (ASSOCIATED(SM%LIN_EQ_OBJECT)) THEN
          CALL SM%LIN_EQ_OBJECT%DELETE
          SM%LIN_EQ_OBJECT => NULL()
        END IF
!
      RETURN
      END SUBROUTINE DELETE_LIN_EQ_OBJECT
!
!=======================================================================
!
      SUBROUTINE DELETE_SIM_MESH (SM)
!
        CLASS (SIM_MESH) :: SM
!
!-----------------------------------------------------------------------
!
        CALL SM%DELETE_ELEMS
        CALL SM%DELETE_NODES
        CALL SM%DELETE_MATERIALS
        CALL SM%DELETE_NODAL_BCS 
        CALL SM%DELETE_FACET_BCS
        CALL SM%DELETE_TIME_FNS
        CALL SM%DELETE_LIN_EQ_OBJECT
!
      RETURN
      END SUBROUTINE DELETE_SIM_MESH
!
!=======================================================================
!
      SUBROUTINE GET_LCL_NODAL_DATA (SM, ELEM_ID, ELEM_DOF)
!
        CLASS (SIM_MESH) :: SM
        INTEGER :: ELEM_ID
        TYPE (LCL_NODAL_DATA), INTENT (OUT) :: ELEM_DOF
!
!       declare local variables
!
        INTEGER :: NND, N, NC, NM
!
!-----------------------------------------------------------------------
!
!       associate a name to the current element
!
        ASSOCIATE (ELEM => SM%ELEMS(ELEM_ID))
!
!       set number of nodes for the current element
!
        NND = SIZE(ELEM%NODE_IDS)
        ELEM_DOF%NND = NND
!
!       copy beginning-step and incremental nodal dof into the
!       local object, and create a local list of the numbers of
!       dof at each of the element's nodes
!
        ALLOCATE (ELEM_DOF%NUM_NODAL_DOF(NND))
        ELEM_DOF%NUM_NODAL_DOF = 0
        NC = 0
        DO N = 1, NND
          ASSOCIATE (NOD => SM%NODES(ELEM%NODE_IDS(N)))
          ELEM_DOF%NUM_NODAL_DOF(N) = NOD%GET_NUM_DOF()
          NC = MAX(NC, ELEM_DOF%NUM_NODAL_DOF(N))
          END ASSOCIATE
        END DO
!
        ALLOCATE (ELEM_DOF%U_TOT(NC,NND), ELEM_DOF%U_INC(NC,NND),       &
     &            ELEM_DOF%XYZ(SM%SIM_VARS%NCOORD,NND))
        ELEM_DOF%U_TOT = 0.0_DBL
        ELEM_DOF%U_INC = 0.0_DBL
        DO N = 1, NND
          ASSOCIATE (NOD => SM%NODES(ELEM%NODE_IDS(N)))
          NC = NOD%GET_NUM_DOF()
          NM = NOD%GET_DIM()
          CALL NOD%GET_NODAL_VALS (XYZ = ELEM_DOF%XYZ(1:NM,N),          &
     &                             U_TOT = ELEM_DOF%U_TOT(1:NC,N),      &
     &                             U_INC = ELEM_DOF%U_INC(1:NC,N))
          END ASSOCIATE
        END DO
!
        END ASSOCIATE
!
        RETURN
      END SUBROUTINE GET_LCL_NODAL_DATA
!
!=======================================================================
!
      SUBROUTINE SET_CUR_GLOBAL_VALS (SM)
!
        CLASS (SIM_MESH) :: SM
!
!-----------------------------------------------------------------------
!
        CUR_GLOBAL_SIM_VARS = SM%SIM_VARS
        CUR_SOLN_STAT = SM%SOLN_STATUS
        CONVRG_PARAMS = SM%CONVRG_PARAMS
!
        RETURN
      END SUBROUTINE SET_CUR_GLOBAL_VALS
!
!=======================================================================
!
      SUBROUTINE PROB_SETUP (SM)
!
!       This routine reads the input files, and sets up the problem
!       by filling the element and node data in the calling object.
!       It also fills the required linear-solver data.
!
        CLASS (SIM_MESH) :: SM
!
!       declare local variables
!
        INTEGER :: MESH_UNIT, PD_UNIT, PD_REC, NCOORD, NTC, NUM_NODES,  &
     &      NUM_ELEMS, I, NDOF, N, PD_REC_1, NUM_MATS, M, L, LMT, MAT,  &
     &      NODS(200), NND, NUM_VS, MIX, NI, NUM_TF, NUM_TP, J,         &
     &      NUM_NBC, ND, VS, TF, ID, NUM_FACET_BCS, NUM_FCTS,           &
     &      NUM_FCT_ELEMS, NLM, LM, IOS, NUM_STEPS, LCL_FCT, MAT_TYPE,  &
     &      LM_TYPE, NEV, NIP
        INTEGER, ALLOCATABLE :: VS_DIR(:)
        TYPE (ELEM_CONTAINER), ALLOCATABLE :: TMP_ELEMS(:)
        REAL (DBL) :: XYZ(3), AMP
        REAL (DBL), ALLOCATABLE :: VEC_SETS(:,:)
        CHARACTER (LEN = 20) :: PHYS_TYPE
        CHARACTER (LEN = 4) :: NBC_TYPE
! 
        CHARACTER (LEN = 5) :: FBC_TYPE        
        LOGICAL :: MSK(3)
!
!-----------------------------------------------------------------------
!
!       open mesh and problem-data files
!
        CALL SM%FILE_OBJ%OPEN_FILE ('MESH', MESH_UNIT)
        CALL SM%FILE_OBJ%OPEN_FILE ('INPT', PD_UNIT)
!
!       zero the number of records read so far from the PD_UNIT file
!
        PD_REC = 0
!
!       read problem physical dimension, and physics type
!
        READ (PD_UNIT, *) NCOORD, PHYS_TYPE
        PD_REC = PD_REC + 1
        IF (NCOORD .EQ. 2 .OR. NCOORD .EQ. 3) THEN
          SM%SIM_VARS%NCOORD = NCOORD
        ELSE
          PRINT *, 'error in problem-data input file:  problem ',       &
     &             'physical dimension must be 2 or 3'
          PRINT *, 'aborting'
          STOP
        END IF
!
!       Set a node-type code. Note that, at present, all nodes in the
!       mesh must be of the same type.  This restriction should be
!       removed.  Most of the code can support heterogeneous node
!       types.  The assumption of homogeneous node types is made only
!       in this routine, and in the routine that sets up the 
!       linear-equation solver data.
!
        NTC = 0
        IF (PHYS_TYPE(1:9) .EQ. 'SMALL_DEF' .OR.                        &
     &      PHYS_TYPE(1:10) .EQ. 'FINITE_DEF') THEN
          IF (NCOORD .EQ. 2) THEN
            NTC = 1
          ELSE IF (NCOORD .EQ. 3) THEN
            NTC = 2
          END IF
        ELSE IF (PHYS_TYPE(1:17) .EQ. 'SMALL_DEF_THERMAL' .OR.          &
     &           PHYS_TYPE(1:18) .EQ. 'FINITE_DEF_THERMAL') THEN
          IF (NCOORD .EQ. 2) THEN
            NTC = 3
          ELSE IF (NCOORD .EQ. 3) THEN
            NTC = 4
          END IF
        ELSE
          PRINT *, 'unsupported physics type and/or problem dimension'
          PRINT *, 'aborting'
          STOP
        END IF
!
        SM%SIM_VARS%FINITE_DEF = .FALSE.
        IF (PHYS_TYPE(1:9) .EQ. 'SMALL_DEF') THEN
          SM%SIM_VARS%FINITE_DEF = .FALSE.
        ELSE IF (PHYS_TYPE(1:10) .EQ. 'FINITE_DEF') THEN
          SM%SIM_VARS%FINITE_DEF = .TRUE.
        END IF
!
!       read mesh data:  get numbers of nodes and (continuum) elements
!
        READ (MESH_UNIT, *) NUM_NODES, NUM_ELEMS
!
!       allocate the node-container array in SM, and also a temporary
!       array of element containers (the element-container array in SM
!       will be allocated after the total number of continuum and
!       facet elements is known, below)
!
        ALLOCATE (SM%NODES(NUM_NODES), TMP_ELEMS(NUM_ELEMS))
!
!       allocate nodes, set their coordinates, and zero their other
!       components
!
        DO I = 1, NUM_NODES
          SELECT CASE (NTC)
          CASE (1)
            ALLOCATE (NODE_2D_DISP :: SM%NODES(I)%ND)
            NDOF = 2
          CASE (2)
            ALLOCATE (NODE_3D_DISP :: SM%NODES(I)%ND)
            NDOF = 3
          CASE (3)
            ALLOCATE (NODE_2D_DISP_TEMP :: SM%NODES(I)%ND)
            NDOF = 3
          CASE (4)
            ALLOCATE (NODE_3D_DISP_TEMP :: SM%NODES(I)%ND)
            NDOF = 4
          END SELECT
!
          READ (MESH_UNIT, *, IOSTAT = IOS) N, XYZ(1:NCOORD)
!
          CALL SM%NODES(N)%SET_NODAL_VALS (XYZ = XYZ(1:NCOORD))
          CALL SM%NODES(N)%ZERO_NODAL_VALS (U_TOT = .TRUE.,             &
     &         U_INC = .TRUE., RESID = .TRUE., RESID_TOL = .TRUE.)
!
        END DO
!
!       read convergence tolerance on the stress, maximum number of
!       NR iterations, and the maximum number of "solution passes"
!       per time step
!
        READ (PD_UNIT, *) SM%CONVRG_PARAMS%STRESS_TOL,                  &
     &        SM%CONVRG_PARAMS%MAX_NR_ITER, SM%CONVRG_PARAMS%MAX_PASS
        PD_REC = PD_REC + 1
!
!       read time steps for the problem
!
        READ (PD_UNIT, *) NUM_STEPS
        PD_REC = PD_REC + 1
        ALLOCATE (SM%STEPS(NUM_STEPS))
        DO I = 1, NUM_STEPS
          READ (PD_UNIT, *) N, SM%STEPS(I)%TM, SM%STEPS(I)%PRINT_CODE
          PD_REC = PD_REC + 1
        END DO
!
!       read the number of material types defined, and the material
!       type codes
!
        READ (PD_UNIT, *) NUM_MATS
        PD_REC = PD_REC + 1
        PD_REC_1 = PD_REC
        ALLOCATE (SM%MATERIALS(NUM_MATS))
        DO I = 1, NUM_MATS
          READ (PD_UNIT, *) SM%MATERIALS(I)%MAT_TYPE
        END DO
!
!       read the material properties for each material defined in the
!       input
!
        REWIND (UNIT = PD_UNIT)
        DO I = 1, PD_REC_1
          READ (PD_UNIT, *)
        END DO
        DO I = 1, NUM_MATS
          ASSOCIATE (MAT => SM%MATERIALS(I))
          ALLOCATE (MAT%PROPS(MAT_SZ(1,MAT%MAT_TYPE)))
          READ (PD_UNIT, *) M, MAT%PROPS(:)
          PD_REC = PD_REC + 1
          END ASSOCIATE
        END DO
!
!       read the element data from the mesh, and allocate the 
!       element's allocatable components
!
        DO L = 1, NUM_ELEMS
          READ (MESH_UNIT, *) N, LMT, MAT, NODS(1:NUM_ELEM_NDS(LMT))
          NND = NUM_ELEM_NDS(LMT)
          NIP = NUM_ELEM_IPS(LMT)
          NEV = NUM_ELEM_VARS(LMT)
          ALLOCATE (TMP_ELEMS(L)%NODE_IDS(1:NND))
          ALLOCATE (ELEM_DAT_CONTINUUM :: TMP_ELEMS(L)%LM_DAT)
          TMP_ELEMS(L)%NODE_IDS = NODS(1:NND)
          TMP_ELEMS(L)%LM_TYPE = LMT
          SELECT TYPE (DAT => TMP_ELEMS(L)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            MAT_TYPE = SM%MATERIALS(MAT)%MAT_TYPE
            DAT%MAT => SM%MATERIALS(MAT)
            ALLOCATE (DAT%IP_VARS_0(MAT_SZ(2,MAT_TYPE),NIP),            &
     &                DAT%IP_VARS_1(MAT_SZ(2,MAT_TYPE),NIP),            &
     &           DAT%SF_GRADS(NCOORD,NND,NIP), DAT%SF_VALS(NND,NIP),    &
     &           DAT%WTS(NIP))
            DAT%IP_VARS_0 = 0.0_DBL
            IF (NEV .GT. 0) THEN
              ALLOCATE (DAT%ELEM_VARS_0(NEV), DAT%ELEM_VARS_1(NEV))
              DAT%ELEM_VARS_0 = 0.0_DBL
            END IF
          END SELECT
        END DO
!
!       read vector sets, and a direction index for each
!
        READ (PD_UNIT, *) NUM_VS
        PD_REC = PD_REC + 1
        ALLOCATE (VEC_SETS(NCOORD,NUM_VS), VS_DIR(NUM_VS))
        DO I = 1, NUM_VS
          READ (PD_UNIT, *) N, VEC_SETS(:,N)
          PD_REC = PD_REC + 1
          VEC_SETS(1:NCOORD,N) = VEC_SETS(1:NCOORD,N) /                 &
     &                           SQRT(SUM(VEC_SETS(1:NCOORD,N)**2))
          MIX = MAXLOC(ABS(VEC_SETS(1:NCOORD,N)), 1)
          IF (VEC_SETS(MIX,N) .LT. 0.D0) VEC_SETS(:,N) = -VEC_SETS(:,N)
          MSK = .TRUE.
          MSK(MIX) = .FALSE.
          NI = MAXLOC(ABS(VEC_SETS(1:NCOORD,N)), 1,                     &
     &                MASK = MSK(1:NCOORD))
          IF (ABS(VEC_SETS(NI,N)) / VEC_SETS(MIX,N) .LT. 1.D-9) THEN
            VS_DIR(N) = MIX
          ELSE
            VS_DIR(N) = 0
          END IF
        END DO
!
!       read time functions
!
        READ (PD_UNIT, *) NUM_TF
        PD_REC = PD_REC + 1
        ALLOCATE (SM%TIME_FNS(NUM_TF))
        DO I = 1, NUM_TF
          READ (PD_UNIT, *) NUM_TP
          PD_REC = PD_REC + 1
          ALLOCATE (SM%TIME_FNS(I)%T_F(2,NUM_TP))
          DO J = 1, NUM_TP
            READ (PD_UNIT, *) SM%TIME_FNS(I)%T_F(1:2,J)
            PD_REC = PD_REC + 1
          END DO
        END DO
!
!       allocate the LIN_EQ_OBJECT, and initialize its ID component
!
        ALLOCATE (SM%LIN_EQ_OBJECT)
        ALLOCATE (LIN_SOLVER_DAT_CC :: SM%LIN_EQ_OBJECT%LS_DAT)
        ALLOCATE (SM%LIN_EQ_OBJECT%ID(NDOF,NUM_NODES))
        SM%LIN_EQ_OBJECT%ID = 0
!
!       read nodal BCs
!
        READ (PD_UNIT, *) NUM_NBC
        PD_REC = PD_REC + 1
        ALLOCATE (SM%NODAL_BCS(NUM_NBC))
        DO I = 1, NUM_NBC
          READ (PD_UNIT, *) NBC_TYPE, ND, VS, TF, AMP
          PD_REC = PD_REC + 1
          IF (NBC_TYPE .NE. 'DSPL') THEN
            PRINT *, 'error reading nodal BCs:  only displacement BCs ',&
     &               'are implemented'
            PRINT *, 'aborting'
            STOP
          END IF
          IF (VS_DIR(VS) .EQ. 0) THEN
            PRINT *, 'error reading nodal BCs:  displacement BCs in ',  &
     &               'skew directions are not implemented'
            PRINT *, 'aborting'
            STOP
          END IF
          SM%NODAL_BCS(I)%ND = ND
          SM%NODAL_BCS(I)%DIR = VS_DIR(VS)
          SM%NODAL_BCS(I)%NBC_TYPE = 'DSPL'
          SM%NODAL_BCS(I)%AMP = AMP
          SM%NODAL_BCS(I)%TF => SM%TIME_FNS(TF)
          SM%LIN_EQ_OBJECT%ID(VS_DIR(VS),ND) = -1
        END DO
!
!       fill the ID array to reflect essential BCs
!
        ID = 0
        DO N = 1, NUM_NODES
          DO J = 1, NDOF
            IF (SM%LIN_EQ_OBJECT%ID(J,N) .EQ. 0) THEN
              ID = ID + 1
              SM%LIN_EQ_OBJECT%ID(J,N) = ID
            ELSE
              SM%LIN_EQ_OBJECT%ID(J,N) = 0
            END IF
          END DO
        END DO
        SM%LIN_EQ_OBJECT%NUM_DOF = ID
!
!       count facet elements
!
        READ (PD_UNIT, *) NUM_FACET_BCS
        NUM_FCT_ELEMS = 0
        DO I = 1, NUM_FACET_BCS
          READ (PD_UNIT, *) FBC_TYPE, NUM_FCTS, VS, TF, AMP
          DO J = 1, NUM_FCTS
            READ (PD_UNIT, *)
            NUM_FCT_ELEMS = NUM_FCT_ELEMS + 1
          END DO
        END DO
!
!       allocate the SM%ELEMS component,
!       copy the continuum elements into it, and read the
!       facet BCs and facet elements for real
!
        ALLOCATE (SM%ELEMS(NUM_ELEMS+NUM_FCT_ELEMS))
        DO I = 1, NUM_ELEMS
          SM%ELEMS(I) = TMP_ELEMS(I)
          CALL TMP_ELEMS(I)%DELETE
        END DO
        DEALLOCATE (TMP_ELEMS)
!
        REWIND (UNIT = PD_UNIT)
        DO I = 1, PD_REC
          READ (PD_UNIT, *)
        END DO
!
        READ (PD_UNIT, *) NUM_FACET_BCS
        PD_REC = PD_REC + 1
        ALLOCATE (SM%FACET_BCS(NUM_FACET_BCS))
!
        NLM = NUM_ELEMS
        DO I = 1, NUM_FACET_BCS
!
          READ (PD_UNIT, *) FBC_TYPE, NUM_FCTS, VS, TF, AMP
          PD_REC = PD_REC + 1
! 
          IF ((FBC_TYPE .EQ. 'PTRAC')                                   &
     &        .OR. (FBC_TYPE .EQ. 'CTRAC')                              &
     &        .OR. (FBC_TYPE .EQ. 'PFOLL')                              &
     &        .OR. (FBC_TYPE .EQ. 'CFOLL')) THEN
            SM%FACET_BCS(I)%FBC_TYPE = FBC_TYPE     
            SM%FACET_BCS(I)%TRAC(1:NCOORD) = VEC_SETS(1:NCOORD,VS) *    &
     &                                       AMP
            SM%FACET_BCS(I)%PRES = 0.0_DBL
          ELSE IF ((FBC_TYPE .EQ. 'PPRES')                              &
     &        .OR. (FBC_TYPE .EQ. 'CPRES')) THEN
            SM%FACET_BCS(I)%FBC_TYPE = FBC_TYPE     
            SM%FACET_BCS(I)%TRAC(1:NCOORD) = 0.0_DBL
            SM%FACET_BCS(I)%PRES = AMP
          ELSE
            PRINT *, 'invalid facet BC type, must be one of the ',      &
     &               'following:'
            PRINT *, 'if finite deformation: CTRAC, PTRAC, CPRES, ',    &
     &               'PPRES, CFOLL, PFOLL'
            PRINT *, 'if small deformation: CTRAC/PTRAC, CPRES/PPRES'       
            PRINT *, 'aborting'
            STOP            
          END IF
          SM%FACET_BCS(I)%TF => SM%TIME_FNS(TF)
!
          DO J = 1, NUM_FCTS
            READ (PD_UNIT, *) LM, NND, NODS(1:NND)
            PD_REC = PD_REC + 1
            ASSOCIATE (CONT_ELEM => SM%ELEMS(LM))
            NLM = NLM + 1
            ALLOCATE (ELEM_DAT_FACET :: SM%ELEMS(NLM)%LM_DAT)
            SELECT TYPE (DAT => SM%ELEMS(NLM)%LM_DAT)
            TYPE IS (ELEM_DAT_FACET)
              CALL GET_FCT_NDS (NODS(1:NND), CONT_ELEM%NODE_IDS,        &
     &           CONT_ELEM%LM_TYPE, LCL_FCT, LM_TYPE)
              ALLOCATE (SM%ELEMS(NLM)%NODE_IDS(1:NND),                  &
     &                  SOURCE = NODS(1:NND))
              DAT%BC_DAT => SM%FACET_BCS(I)
              SM%ELEMS(NLM)%LM_TYPE = LM_TYPE
              NND = NUM_ELEM_NDS(LM_TYPE)
              NIP = NUM_ELEM_IPS(LM_TYPE)
              ALLOCATE (DAT%SF_GRADS(NCOORD,NND,NIP),                   &
     &         DAT%SF_VALS(NND,NIP), DAT%WTS(NIP), DAT%NRML(NCOORD,NIP))
            END SELECT
            END ASSOCIATE
          END DO
!
        END DO
!
!       finish initializing the LIN_EQ_OBJECT (this call ultimately 
!       fills the DIAG component of the LIN_EQ_OBJECT)
!
        CALL SM%INIT_LIN_EQ
!
!       add any future PD_UNIT input reads here
!
!
!       close input files
!
        CLOSE (UNIT = PD_UNIT)
        CLOSE (UNIT = MESH_UNIT)
!
!       set the initial solution step to 1
!
        SM%SOLN_STATUS%STEP_NUM = 1
!
        RETURN
      END SUBROUTINE PROB_SETUP
!
!=======================================================================
!
      SUBROUTINE INIT_LIN_EQ (SM)
!
!       This routine initializes the linear equation solver 
!       (specifically, the LIN_EQ_OBJECT component of SM) by filling
!       whatever integer arrays appear within this object, and 
!       allocating the %U, %FRC, and %STF components.
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
!
!       declare local variables
!
        INTEGER :: NUM_NODES, NUM_ELEMS, NDOF, LM_TOT, L, I, NOD, LAST, &
     &             NEXT, LOC, IDX, COMP, J, IDM, LM, ND, K, KD, NSTF,   &
     &             NLEN
        INTEGER, ALLOCATABLE :: LMND(:), IMND(:)
!
!-----------------------------------------------------------------------
!
!       set the numbers of nodes and elements, and the number of dof
!       at each node
!
        NUM_NODES = SIZE(SM%NODES)
        NUM_ELEMS = SIZE(SM%ELEMS)
        ALLOCATE (LMND(NUM_NODES+1))
!
!       set the number of dof per node (note that currently, this
!       is assumed to be the same for all elements in the mesh;
!       here, the determination is made based on node 1)
!
        NDOF = SM%NODES(1)%GET_NUM_DOF()
!
!       get the number of elements attached to each node
!
        LMND = 0
        LM_TOT = 0
        DO L = 1, NUM_ELEMS
          DO I = 1, SIZE(SM%ELEMS(L)%NODE_IDS)
            NOD = SM%ELEMS(L)%NODE_IDS(I)
            LMND(NOD) = LMND(NOD) + 1
            LM_TOT = LM_TOT + 1
          END DO
        END DO
!
        ALLOCATE (IMND(LM_TOT+1))
!
        LAST = LMND(1)
        LMND(1) = 1
        DO I = 2, NUM_NODES
          NEXT = LMND(I)
          LMND(I) = LMND(I-1) + LAST
          LAST = NEXT
        END DO
        DO L = 1, NUM_ELEMS
          DO I = 1, SIZE(SM%ELEMS(L)%NODE_IDS)
            NOD = SM%ELEMS(L)%NODE_IDS(I)
            LOC = LMND(NOD)
            IMND(LOC) = L
            LMND(NOD) = LOC + 1
          END DO
        END DO
        DO I = NUM_NODES + 1, 2, -1
          LMND(I) = LMND(I-1)
        END DO
        LMND(1) = 1
!
!       for the compacted-column solver, form IDGL(2,NUM_DOF+1):  
!         (1,.) = first row of the active part of the argument column
!         (2,.) = first entry in component STF(.) corresponding to
!                 the argument column
!
        SELECT TYPE (DAT => SM%LIN_EQ_OBJECT%LS_DAT)
        TYPE IS (LIN_SOLVER_DAT_CC)
          ASSOCIATE (NUM_DOF => SM%LIN_EQ_OBJECT%NUM_DOF)
!
          ALLOCATE (DAT%IDGL(2,NUM_DOF+1), SM%LIN_EQ_OBJECT%U(NUM_DOF), &
     &              SM%LIN_EQ_OBJECT%FRC(NUM_DOF))
!
!         temporarily put first/last row numbers for the active part
!         of each column in IDGL
!
          IDX = -1
          NOD = 1
          COMP = 1
          DO I = 1, NUM_DOF
!
!           get the node number for dof number I
!
            J = SM%LIN_EQ_OBJECT%ID(COMP,NOD)
            DO WHILE (J .LT. I)
              IF (COMP .LT. NDOF) THEN
                COMP = COMP + 1
              ELSE
                COMP = 1
                NOD = NOD + 1
              END IF
              J = SM%LIN_EQ_OBJECT%ID(COMP,NOD)
            END DO
!
            IDM = NUM_DOF + 1
            DO L = LMND(NOD), LMND(NOD+1) - 1
              LM = IMND(L)
              DO J = 1, SIZE(SM%ELEMS(LM)%NODE_IDS)
                ND = SM%ELEMS(LM)%NODE_IDS(J)
                DO K = 1, NDOF
                  KD = SM%LIN_EQ_OBJECT%ID(K,ND)
                  IF (KD .NE. 0) THEN
                    IDX = MAX(IDX, KD)
                    IDM = MIN(IDM, KD)
                  END IF
                END DO
              END DO
            END DO
            DAT%IDGL(1,I) = IDM
            DAT%IDGL(2,I) = IDX
!
          END DO
!
!         Store the first entry in STF associated with each column of
!         the square stiffness matrix.  Note that %IDGL(2,NUM_DOF+1) - 1
!         is equal to the total length of STF.
!
          NSTF = 0
          NEXT = 1
          DO I = 1, NUM_DOF
            NLEN = DAT%IDGL(2,I) - DAT%IDGL(1,I) + 1
            NSTF = NSTF + NLEN
            DAT%IDGL(2,I) = NEXT
            NEXT = NSTF + 1
          END DO
          DAT%IDGL(2,NUM_DOF+1) = NEXT
!
!         allocate the %STF component
!
          ALLOCATE (SM%LIN_EQ_OBJECT%STF(NEXT-1))
!
          END ASSOCIATE
        END SELECT
!
        DEALLOCATE (LMND, IMND)
!
        RETURN
      END SUBROUTINE INIT_LIN_EQ
!
!=======================================================================
!
      SUBROUTINE OUTPUT (SM)
!
!       This routine writes requested output to the output file.
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
!
!       declare local variables
!
        INTEGER :: OP_UNIT, I, J, NC, NDOF
        REAL (DBL) :: XYZ(3), U_TOT(20)
        LOGICAL :: OPN
        CHARACTER (LEN = 73) :: SEP
!
!-----------------------------------------------------------------------
!
        ASSOCIATE (STEP_ID => SM%SOLN_STATUS%STEP_NUM)
!
!       if no output was requested, then just update the step number and
!       return
!
        IF (SM%STEPS(STEP_ID)%PRINT_CODE .EQ. 0) THEN
          STEP_ID = STEP_ID + 1
          RETURN
        END IF
!
!       check that the output file is open - if not, then open it
!
        INQUIRE (FILE = SM%FILE_OBJ%ROOT_NAME(1:SM%FILE_OBJ%LN)//'.e',  &
     &           OPENED = OPN)
        IF (OPN) THEN
          INQUIRE (FILE = SM%FILE_OBJ%ROOT_NAME(1:SM%FILE_OBJ%LN)//'.e',&
     &             NUMBER = OP_UNIT)
        ELSE
          CALL SM%FILE_OBJ%OPEN_FILE ('OUTP', OP_UNIT)
        END IF
!
!       print the step header information
!
        FORALL (I = 1 : 73) SEP(I:I) = '='
        WRITE (OP_UNIT, 100) SEP
        WRITE (OP_UNIT, 102) STEP_ID,                                   &
     &                       SM%SOLN_STATUS%T_END, SM%SOLN_STATUS%ITER
        WRITE (OP_UNIT, 100) SEP
  100   FORMAT (A73)
  102   FORMAT ('STEP NUMBER ', I4, ' :: END TIME ', E12.5,             &
     &          ' :: CONVERGED IN ', I4, ' ITERATIONS')
!
!       write nodal data to the output file
!
        WRITE (OP_UNIT, 100)
        WRITE (OP_UNIT, 104) 
  104   FORMAT ('NODAL COORDINATES AND END-STEP DOF VALUES: ')
        WRITE (OP_UNIT, 100)
        NC = SM%SIM_VARS%NCOORD
        DO I = 1, SIZE(SM%NODES)
          NDOF = SM%NODES(I)%GET_NUM_DOF()
          CALL SM%NODES(I)%GET_NODAL_VALS (XYZ = XYZ(1:NC),             &
     &                                     U_TOT = U_TOT(1:NDOF))
          WRITE (OP_UNIT, 106) I, XYZ(1:NC), U_TOT(1:NDOF)
  106     FORMAT (I5, 2X, 6(E13.6, 2X))
        END DO
!
!       write element IP variables to the output file
!
        WRITE (OP_UNIT, 100)
        WRITE (OP_UNIT, 108) 
  108   FORMAT ('INTEGRATION-POINT VARIABLES AT EACH IP: ')
        WRITE (OP_UNIT, 100)
        DO I = 1, SIZE(SM%ELEMS)
          SELECT TYPE (DAT => SM%ELEMS(I)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            WRITE (OP_UNIT, 110) I
  110       FORMAT ('    element: ', I5)
            WRITE (OP_UNIT, 112) SM%ELEMS(I)%NODE_IDS(:)
  112       FORMAT ('      nodes: ', 20(I5, 2X))
            WRITE (OP_UNIT, 100)
            DO J = 1, SIZE(DAT%IP_VARS_0, 2)
              WRITE (OP_UNIT, 114) J, DAT%IP_VARS_0(:,J)
  114         FORMAT (I2, 4X, 10(E13.6, 2X))
            END DO
            WRITE (OP_UNIT, 100)
          END SELECT
        END DO
!
!       if this is the last output request for the run, close the
!       output file
!
        IF (ALL(SM%STEPS(STEP_ID+1:)%PRINT_CODE .EQ. 0)) THEN
          CLOSE (UNIT = OP_UNIT)
        END IF
!
!       update the step number
!
        STEP_ID = STEP_ID + 1
!
        END ASSOCIATE
!
        RETURN
      END SUBROUTINE OUTPUT
!
!=======================================================================
!
      SUBROUTINE INIT_INC_DOF (SM)
!
!       This routine initializes the incremental nodal dof:  
!       it sets U_INC where displacements are prescribed by BC,
!       and makes an initial guess at U_INC for the free dof.
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
!
!       declare local variables
!
        INTEGER :: I, NN
        REAL (DBL) :: U_INC(20), U_TOT(20), U_TOT_BC   
!
!-----------------------------------------------------------------------
!         

!       initial guess for U_INC
!
        CALL INIT_GUESS(SM)
!
!       set U_INC in accord with essential BCs
!
        DO I = 1, SIZE(SM%NODAL_BCS)
          ASSOCIATE (ND => SM%NODAL_BCS(I)%ND,                          &
     &               DIR => SM%NODAL_BCS(I)%DIR)
          NN = SM%NODES(ND)%GET_NUM_DOF ()
          CALL SM%NODES(ND)%GET_NODAL_VALS (U_INC = U_INC(1:NN),        &
     &                                      U_TOT = U_TOT(1:NN))
          U_TOT_BC = SM%NODAL_BCS(I)%TF%EVAL(SM%SOLN_STATUS%T_END) *    &
     &               SM%NODAL_BCS(I)%AMP
          U_INC(DIR) = U_TOT_BC - U_TOT(DIR)
          CALL SM%NODES(ND)%SET_NODAL_VALS (U_INC = U_INC(1:NN))
          END ASSOCIATE
        END DO
!
        RETURN
      END SUBROUTINE INIT_INC_DOF
!
!=======================================================================
!
      SUBROUTINE INIT_GUESS (SM)
!
!       This routine initializes the incremental nodal dof:  
!       IT makes an initial guess at U_INC for the free dof based on
!       Rashid 1995
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
!
!       declare local variables
!
        INTEGER :: I, NN, STEP, NUM_ELEM, L, NX, A, B, NA
        REAL (DBL) :: DENOM, SCALE_FAC, U_INC(20), U_TOT(20), U_TOT_BC
        REAL (DBL) :: T0, T1, T2, ZERO
        REAL (DBL), ALLOCATABLE :: DISP_BCS(:)
        TYPE (LCL_NODAL_DATA) :: ELEM_DOF
!         
!-----------------------------------------------------------------------
!
!       initial guess for U_INC based either on material velocity
!       field extrapolation or on extrapolated stretch and rotation
!       tensors as performed in Rashid 1995
!       (note that all dof are initialized to zero ahead of the first 
!       solution step in PROB_SETUP)
!
        STEP = SM%SOLN_STATUS%STEP_NUM
        NUM_ELEM = SIZE(SM%ELEMS)
        ZERO = 0.0_DBL    
!         
        IF (STEP .GT. 1) THEN          
          IF (STEP .EQ. 2) THEN
            T0 = ZERO
          ELSE
            T0 = SM%STEPS(STEP - 2)%TM
          END IF
          T1 = SM%STEPS(STEP - 1)%TM
          T2 = SM%STEPS(STEP)%TM
!           
          IF (CUR_GLOBAL_SIM_VARS%FINITE_DEF) THEN
!             
!           zero the global equation system
! 
            CALL SM%LIN_EQ_OBJECT%ZERO_SOLVER_DATA
!
!           inner loop over elements to assemble the linear equation
!           system
!
            DO L = 1, NUM_ELEM
              SELECT TYPE (DAT => SM%ELEMS(L)%LM_DAT)
              TYPE IS (ELEM_DAT_CONTINUUM)
!
!               copy nodal data for the current element into local
!               variable ELEM_DOF
!
                CALL SM%GET_LCL_NODAL_DATA (L, ELEM_DOF)
!  
!               before trying to compute an element residual and/or stiffness,
!               check that the current element already has its shape-function
!               data in place (and create it if not)
!
                IF (.NOT. SM%ELEMS(L)%GOT_SF_DAT) THEN
                  CALL SM%ELEMS(L)%GET_SF_DAT (ELEM_DOF)
                END IF
!               
                NX = SUM(ELEM_DOF%NUM_NODAL_DOF(:))
                ALLOCATE (DISP_BCS(NX)) 
                DISP_BCS = 0.0_DBL
!           
                ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,            &
     &                     NODE_IDS => SM%ELEMS(L)%NODE_IDS)
! 
!               Determine incremental nodal displacments due to 
!               essential BCS for this particular element
! 
                OUTER: DO I = 1, SIZE(SM%NODAL_BCS, 1)
                  DO A = 1, SIZE(NODE_IDS, 1)               
                    IF (NODE_IDS(A) .EQ. SM%NODAL_BCS(I)%ND) THEN            
                      ASSOCIATE (ND => SM%NODAL_BCS(I)%ND,              &
     &                           DIR => SM%NODAL_BCS(I)%DIR)
                      NN = SM%NODES(ND)%GET_NUM_DOF()
                      CALL SM%NODES(ND)%GET_NODAL_VALS (U_INC           & 
     &                              = U_INC(1:NN), U_TOT = U_TOT(1:NN))
                      U_TOT_BC = SM%NODAL_BCS(I)%TF%EVAL                &
     &                                         (CUR_SOLN_STAT%T_END) *  &
     &                                         SM%NODAL_BCS(I)%AMP
!
                      NA = 0
                      B = 1
                      DO B = 1, A - 1
                        NA = NA + NN_DOF(B)
                      END DO
!                 
                      DISP_BCS(NA + DIR) = U_INC(DIR) +                 &
     &                                     U_TOT_BC - U_TOT(DIR)
                      END ASSOCIATE                
                      CYCLE OUTER
                    END IF
                  END DO
                END DO OUTER
!                 
                END ASSOCIATE
! 
!               Evaluate the element residual force vector and 
!               tangent stiffness for the linear system of equations in 
!               solving for the initial guess for incr. nodal disp. This
!               subroutine resides in the ELEMS_NODES_M module.             
! 
                CALL SM%ELEMS(L)%GET_RESID_STIFF_GUESS(ELEM_DOF,        &
     &                                             DISP_BCS, T0, T1, T2)
!
!               assemble the element contribution to the global residual
!               vector, or the global linear system of equations
!
                CALL SM%LIN_EQ_OBJECT%ASM_LIN_EQ (SM%ELEMS(L), ELEM_DOF)
!  
                CALL ELEM_DOF%DELETE
                DEALLOCATE (DISP_BCS)
!
              END SELECT  
            END DO
!
!           solve the linear system of equations.
!
            CALL SM%LIN_EQ_OBJECT%LIN_SOLVE    
!             
!           update the incremental nodal displacement 
!             
            CALL SM%SET_U_INC_GUESS            
!                  
          ELSE
!       
!           small def: use standard material velocity field extrapolation
! 
            DENOM = T1 - T0
            SCALE_FAC = (T2 - T1) / DENOM
            DO I = 1, SIZE(SM%NODES)
              NN = SM%NODES(I)%GET_NUM_DOF ()
              CALL SM%NODES(I)%GET_NODAL_VALS (U_INC = U_INC(1:NN))
              CALL SM%NODES(I)%SET_NODAL_VALS (U_INC =                  &
     &                                         SCALE_FAC * U_INC(1:NN))          
            END DO
          END IF
!           
        END IF
!
        RETURN
      END SUBROUTINE INIT_GUESS
!
!=======================================================================
!
      SUBROUTINE ZERO_NODAL_VALS (SM, U_TOT, U_INC, RESID, RESID_TOL)
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
        LOGICAL, OPTIONAL :: U_TOT, U_INC, RESID, RESID_TOL
!
!       declare local variables
!
        INTEGER :: I
!
!-----------------------------------------------------------------------
!
        IF (.NOT. ALLOCATED(SM%NODES)) RETURN
!
        IF (PRESENT(U_TOT)) THEN
          IF (U_TOT) THEN
            DO I = 1, SIZE(SM%NODES)
              CALL SM%NODES(I)%ZERO_NODAL_VALS (U_TOT = .TRUE.)
            END DO
          END IF
        END IF
!
        IF (PRESENT(U_INC)) THEN
          IF (U_INC) THEN
            DO I = 1, SIZE(SM%NODES)
              CALL SM%NODES(I)%ZERO_NODAL_VALS (U_INC = .TRUE.)
            END DO
          END IF
        END IF
!
        IF (PRESENT(RESID)) THEN
          IF (RESID) THEN
            DO I = 1, SIZE(SM%NODES)
              CALL SM%NODES(I)%ZERO_NODAL_VALS (RESID = .TRUE.)
            END DO
          END IF
        END IF
!
        IF (PRESENT(RESID_TOL)) THEN
          IF (RESID_TOL) THEN
            DO I = 1, SIZE(SM%NODES)
              CALL SM%NODES(I)%ZERO_NODAL_VALS (RESID_TOL = .TRUE.)
            END DO
          END IF
        END IF
!
        RETURN
      END SUBROUTINE ZERO_NODAL_VALS
!
!=======================================================================
!
      END MODULE SIM_MESH_M
