      MODULE IP_FRC_STF_M
!
!     This module contains procedures that compute integration-point
!     contributions to element force vectors and stiffness matrices.
!     It is USEd by ELEMS_NODES_M.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE MISC_TYPES_M
      USE MATRIX_M
      IMPLICIT NONE
!
      TYPE (GLOBAL_SIM_VARS) :: CUR_GLOBAL_SIM_VARS
      TYPE (SOLN_STAT) :: CUR_SOLN_STAT
      TYPE (CONVRG_CONTROL) :: CONVRG_PARAMS
!       
      TYPE, PUBLIC :: IP_KINEM
        REAL (DBL) :: F(3,3), AL
        REAL (DBL), ALLOCATABLE :: DF_DV(:,:,:,:), DAL_DV(:,:),         &
     &                             CN(:), DCN_DV(:,:,:), ALCN(:),       &
     &                             DALCN_DV(:,:,:)
!      
        CONTAINS
!         
        PROCEDURE :: DEL_IP_KINEM
      END TYPE IP_KINEM
!     
      PUBLIC :: IP_CONT_SML_DEF, IP_FCT_SML_DEF, FWD_ROT,               &
     &          IP_CONT_LRG_DEF, IP_FCT_LRG_DEF, GET_DEF_VARS,          &
     &          IP_CONT_GUESS, GET_DEF_GRAD
!
!=======================================================================
      CONTAINS
!=======================================================================
!
      SUBROUTINE DEL_IP_KINEM (DFVARS)
!
        CLASS (IP_KINEM), INTENT (INOUT) :: DFVARS
!
!-----------------------------------------------------------------------
!
        IF (ALLOCATED(DFVARS%DF_DV)) DEALLOCATE (DFVARS%DF_DV)
        IF (ALLOCATED(DFVARS%DAL_DV)) DEALLOCATE (DFVARS%DAL_DV)
        IF (ALLOCATED(DFVARS%ALCN)) DEALLOCATE (DFVARS%ALCN)
        IF (ALLOCATED(DFVARS%DALCN_DV)) DEALLOCATE (DFVARS%DALCN_DV)
        IF (ALLOCATED(DFVARS%CN)) DEALLOCATE (DFVARS%CN)        
        IF (ALLOCATED(DFVARS%DCN_DV)) DEALLOCATE (DFVARS%DCN_DV)        
!
        RETURN
      END SUBROUTINE DEL_IP_KINEM
!      
!=======================================================================
!
      SUBROUTINE IP_CONT_SML_DEF (NN_DOF, SF_GRADS, ELEM_VARS, FRC,     &
     &                            DE_DU, TAN_MOD, STF)
!
        INTEGER :: NN_DOF(:)
        REAL (DBL), INTENT (IN) :: SF_GRADS(:,:), ELEM_VARS(:)
        REAL (DBL), INTENT (IN), OPTIONAL :: DE_DU(:,:), TAN_MOD(:,:)
        REAL (DBL), INTENT (OUT) :: FRC(:)
        REAL (DBL), INTENT (OUT), OPTIONAL :: STF(:,:)
!
!       declare local variables
!
        INTEGER :: NND, A, B, NC, NU, NX, IJ(3,3), N, I, K, NA, NB
        REAL (DBL), ALLOCATABLE :: DT_DU(:,:,:)
!
!-----------------------------------------------------------------------
!
!       set number of nodes for the current element, the 
!       dimensionality of the problem, and the total number of
!       incremental nodal displacements for the element
!
        NND = SIZE(NN_DOF)
        NC = SIZE(SF_GRADS, 1)
        NU = NC * NND
!
!       get the largest number of dof for any node in the element
!
        NX = MAXVAL(NN_DOF(:))
!
!       set a map from tensor indices to column-vector indices
!
        IJ(1:3,1) = (/ 1, 6, 5 /)
        IJ(1:3,2) = (/ 6, 2, 4 /)
        IJ(1:3,3) = (/ 5, 4, 3 /)
!
!       form the end-step stress-divergence residual contribution
!       (an energy-equation residual contribution would go in this 
!       loop as well, in the "third position" for each node that has
!       a temperature dof)
!
        N = 0
        FRC = 0.0_DBL
        DO A = 1, NND
          FORALL (I = 1 : NC) FRC(N+I) =                                &
     &            SUM(ELEM_VARS(IJ(I,1:NC)) * SF_GRADS(1:NC,A))
          N = N + NN_DOF(A)
        END DO
!
!       if indicated, form and store the IP stiffness contribution
!
        IF (PRESENT(STF)) THEN
!
!         first, form the derivatives of the end-step stress WRT
!         incremental nodal displacements
!
          ALLOCATE (DT_DU(6,NX,NND))
          DT_DU = 0.0_DBL
          N = 0
          DO A = 1, NND
            FORALL (I = 1 : 6, K = 1 : NC)
              DT_DU(I,K,A) =                                            &
     &                      SUM(TAN_MOD(I,1:3) * DE_DU(1:3,N+K)) +      &
     &            2.0_DBL * SUM(TAN_MOD(I,4:6) * DE_DU(4:6,N+K))
            END FORALL
            N = N + NN_DOF(A)
          END DO
!
!         now form the IP contribution to the element tangent
!         stiffness, which is the derivative of the end-step
!         residual contribution WRT incremental nodal displacements
!
          STF = 0.0_DBL
          NA = 0
          DO A = 1, NND
            NB = 0
            DO B = 1, NND
              FORALL (I = 1 : NC, K = 1 : NC)
                STF(NA+I,NB+K) =                                        &
     &                 SUM(DT_DU(IJ(I,1:NC),K,B) * SF_GRADS(1:NC,A))
              END FORALL
              NB = NB + NN_DOF(B)
            END DO
            NA = NA + NN_DOF(A)
          END DO
!
        END IF
!
        IF (ALLOCATED (DT_DU)) DEALLOCATE (DT_DU)
!
        RETURN
      END SUBROUTINE IP_CONT_SML_DEF
!
!=======================================================================
!
      SUBROUTINE IP_FCT_SML_DEF (NN_DOF, SF_VALS, FRC, TRAC_VEC, PRES,  &
     &                           NRML)
!
        INTEGER :: NN_DOF(:)
        REAL (DBL), INTENT (IN) :: SF_VALS(:)
        REAL (DBL), INTENT (IN), OPTIONAL :: TRAC_VEC(:), NRML(:), PRES
        REAL (DBL), INTENT (OUT) :: FRC(:)
!
!       declare local variables
!
        INTEGER :: N, A, I, NC
!
!-----------------------------------------------------------------------
!
        FRC = 0.0_DBL
        N = 0
!
        IF (PRESENT(TRAC_VEC)) THEN
          NC = SIZE(TRAC_VEC)
        ELSE IF (PRESENT(NRML)) THEN
          NC = SIZE(NRML)
        ELSE
          NC = 0
        END IF
!
        IF (PRESENT(TRAC_VEC)) THEN
          N = 0
          DO A = 1, SIZE(NN_DOF)
            FORALL (I = 1 : NC) FRC(N+I) = SF_VALS(A) * TRAC_VEC(I)
            N = N + NN_DOF(A)
          END DO
        ELSE IF (PRESENT(PRES)) THEN
          N = 0
          DO A = 1, SIZE(NN_DOF)
            FORALL (I = 1 : NC) FRC(N+I) = FRC(N+I) -                   &
     &                                      SF_VALS(A) * PRES * NRML(I)
            N = N + NN_DOF(A)
          END DO
        END IF
!
        RETURN
      END SUBROUTINE IP_FCT_SML_DEF
!
!=======================================================================
!
      SUBROUTINE FWD_ROT (T_VEC, ROT_INC)
!
        REAL (DBL), INTENT (INOUT) :: T_VEC(:)
        REAL (DBL), INTENT (IN) :: ROT_INC(:,:)
!
!       declare local variables
!
        INTEGER :: I, J, IJ(3,3)
        REAL (DBL) :: R_T_HAT(3,3)
!
!-----------------------------------------------------------------------
!
!       set a map from tensor indices to column-vector indices
!
        IJ(1:3,1) = (/ 1, 6, 5 /)
        IJ(1:3,2) = (/ 6, 2, 4 /)
        IJ(1:3,3) = (/ 5, 4, 3 /)
! 
!       calculate forward rotated stress T = R_HAT * T_HAT * R_HAT_TRANS
! 
        R_T_HAT = 0.0_DBL
        FORALL (I = 1 : 3, J = 1 : 3)
          R_T_HAT(I,J) = SUM(ROT_INC(I,:) * T_VEC(IJ(:,J)))
        END FORALL
        DO I = 1, 3
          DO J = 1, I
            T_VEC(IJ(I,J)) = SUM(R_T_HAT(I,:) * ROT_INC(J,:))
          END DO
        END DO
! 
        RETURN
      END SUBROUTINE FWD_ROT
!
!=======================================================================
!
      SUBROUTINE IP_CONT_LRG_DEF (ELEM_DOF, ROT_INC, SF_GRADS,          &
     &                            ELEM_VARS, FRC, TAN_MOD, STF)
!
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        REAL (DBL), INTENT (IN) :: ROT_INC(:,:), SF_GRADS(:,:),         &
     &                             ELEM_VARS(:)
        REAL (DBL), INTENT (OUT) :: FRC(:)     
        REAL (DBL), INTENT (IN), OPTIONAL :: TAN_MOD(:,:)
        REAL (DBL), INTENT (OUT), OPTIONAL :: STF(:,:)
!
!       declare local variables
!
        INTEGER :: A, B, I, J, L, IJ(3,3), K, N, NA, NB, NC, NND
        REAL (DBL) :: ATSR(3,3), DELTA(3,3), F(3,3),                    &
     &                FBAR(3,3), FHAT_INV(3,3), FINV(3,3), JDET,        &
     &                P(3,3), RTRAN_T(3,3), UINC_B(3,3), C(3,3),        &
     &                DC_DA(3,3,3,3)
        REAL (DBL), ALLOCATABLE :: DA_DU(:,:,:,:), DE_DA(:,:,:,:),      &
     &                             DE_DU(:,:,:), DFINV_DU(:,:,:,:),     &
     &                             DR_DU(:,:,:,:), FINVT_B(:,:),        &
     &                             DJ_DU(:,:), DT_DU(:,:,:),            &
     &                             DT_DU_1(:,:,:), DT_DU_2(:,:,:),      &
     &                             DT_DU_3(:,:,:), DT_HAT_DU(:,:,:),    &
     &                             DP_DU(:,:,:,:), DP_DU_1(:,:,:,:),    &
     &                             DP_DU_2(:,:,:,:), DP_DU_3(:,:,:,:),  &
     &                             R_DTH_DU(:,:,:,:), R_FHAT_INV(:,:),  &
     &                             RT_FINVT_B(:,:)
!
!-----------------------------------------------------------------------
!
!       associate local variable names
!
        ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,                    &
     &             U_INC => ELEM_DOF%U_INC, U_TOT => ELEM_DOF%U_TOT,    &
     &             T => ELEM_VARS)
!
!       set number of nodes for the current element, the 
!       dimensionality of the problem, and the total number of
!       incremental nodal displacements for the element
!
        NND = SIZE(NN_DOF)
        NC = SIZE(SF_GRADS, 1)
!
!       define Kronecker delta and alternator symbol
!
        DELTA = 0.0_DBL
        DELTA(1,1) = 1.0_DBL
        DELTA(2,2) = 1.0_DBL
        DELTA(3,3) = 1.0_DBL
!
!       form deformation gradient F = del(x)/del(X) and Fbar = del(xbar)/del(X)
!       as well as Fhat inverse
!
        F = DELTA
        FBAR = DELTA
        FORALL(I = 1 : NC, J = 1 : NC)
          FBAR(I,J) = DELTA(I,J) + SUM(U_TOT(I,:) * SF_GRADS(J,:))
          F(I,J) = FBAR(I,J) + SUM(U_INC(I,:) * SF_GRADS(J,:))          
        END FORALL             
!
!       calculate J = determinant of F
!         
        JDET = DETM(F,NC)
!
!       calculate Finverse, Fhatinverse, A, Cinv
!
        FINV = DELTA
        FHAT_INV = DELTA
        CALL INV3 (F, JDET**(-1), NC, FINV)
        FHAT_INV = M_M(FBAR, FINV)
!         
!       set a map from tensor indices to column-vector indices
!
        IJ(1:3,1) = (/ 1, 6, 5 /)
        IJ(1:3,2) = (/ 6, 2, 4 /)
        IJ(1:3,3) = (/ 5, 4, 3 /)
! 
!       calculate first Piola-Kirchhoff stress
! 
        P = 0.0_DBL
        FORALL (I = 1 : 3, J = 1 : 3)
          P(I,J)  = SUM(T(IJ(I,:)) * FINV(J,:))
        END FORALL
        P = JDET * P
! 
!       form the end-step stress-divergence residual contribution   
! 
        N = 0
        FRC = 0.0_DBL
        DO A = 1, NND
          FORALL (I = 1 : NC) FRC(N+I) =                                &
     &            SUM(P(I,1:NC) * SF_GRADS(1:NC,A))
          N = N + NN_DOF(A)
        END DO
! 
!
!       if indicated, form and store the IP stiffness contribution
!
        IF (PRESENT(STF)) THEN
! 
!         make necessary array allocations
! 
          ALLOCATE (DA_DU(3,3,3,NND), DE_DA(3,3,3,3),                   &
     &              DE_DU(6,3,NND), DFINV_DU(3,3,3,NND),                &
     &              DR_DU(3,3,3,NND), FINVT_B(3,NND), DJ_DU(3,NND),     &
     &              DT_DU(6,3,NND), DT_DU_1(6,3,NND), DT_DU_2(6,3,NND), &
     &              DT_DU_3(6,3,NND), DT_HAT_DU(6,3,NND),               &
     &              DP_DU(3,3,3,NND), DP_DU_1(3,3,3,NND),               &
     &              DP_DU_2(3,3,3,NND), DP_DU_3(3,3,3,NND),             &               
     &              R_DTH_DU(3,3,3,NND), R_FHAT_INV(3,3),               &
     &              RT_FINVT_B(3,NND))
! 
!         calculate intermediate quantities
!  
          FINVT_B = 0.0_DBL
          FORALL (I = 1 : 3, A = 1 : NND)
            FINVT_B(I,A) = SUM(FINV(1:NC,I) * SF_GRADS(1:NC,A))
          END FORALL
!
          DA_DU = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            DA_DU(I,J,K,A) = FHAT_INV(I,K) * FINVT_B(J,A)
          END FORALL          
          
          RT_FINVT_B = MT_M(ROT_INC, FINVT_B)
! 
!         calculate derivative of strain increment with respect to 
!         incremental nodal displacements
! 
          ATSR = 0.0_DBL
          UINC_B = 0.0_DBL
          FORALL (I = 1 : NC, J = 1 : NC)
            UINC_B(I,J) = SUM(U_INC(I,:) * SF_GRADS(J,:))
          END FORALL
          FORALL (I = 1 : NC, J = 1 : NC)
            ATSR(I,J) = SUM(UINC_B(I,:) * FINV(:,J))
          END FORALL    
!           
          C = M_MT(ATSR,ATSR) - ATSR - TRANSPOSE(ATSR)          
!           
          DC_DA = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, L = 1 : 3)
            DC_DA(I,J,K,L) = DC_DA(I,J,K,L) - DELTA(I,K)*DELTA(J,L) -   &
     &       DELTA(J,K)*DELTA(I,L) + DELTA(I,K)*ATSR(J,L)+              &
     &       ATSR(I,L)*DELTA(J,K)
          END FORALL          
!           
          DE_DA = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, L = 1 : 3)
            DE_DA(I,J,K,L) = DE_DA(I,J,K,L) - 0.5_DBL * DC_DA(I,J,K,L)  &
     &                       + 0.25_DBL * SUM(C(I,:) * DC_DA(:,J,K,L))  &
     &                       + 0.25_DBL * SUM(DC_DA(I,:,K,L) * C(:,J))
          END FORALL    
!          
          DE_DU = 0.0_DBL
          DO L = 1, 3
            DO I = 1, 3
              DO J = 1, I
                FORALL (K = 1 : 3, A = 1 : NND)
                  DE_DU(IJ(I,J),K,A) = DE_DU(IJ(I,J),K,A) +             &
     &                      SUM(DE_DA(I,J,L,1:3) * DA_DU(L,1:3,K,A))
                END FORALL
              END DO
            END DO      
          END DO 
! 
!         calculate derivative of rotation increment with respect to
!         incremental nodal displacements
! 
          DR_DU = 0.0_DBL
          R_FHAT_INV = M_M(ROT_INC, FHAT_INV)
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            DR_DU(I,J,K,A) = 0.5_DBL *                                  &
     &                          (-FHAT_INV(J,K) * FINVT_B(I,A) +        &
     &                           R_FHAT_INV(I,K) * RT_FINVT_B(J,A))
          END FORALL
! 
!         calculate derivative of unrotated stress T_hat WRT incremental
!         nodal displacements
! 
          DT_HAT_DU = 0.0_DBL
          FORALL (I = 1 : 6, K = 1 : 3, A = 1 : NND)
            DT_HAT_DU(I,K,A) = SUM(TAN_MOD(I,1:3) * DE_DU(1:3,K,A))     &
     &             + 2.0_DBL * SUM(TAN_MOD(I,4:6) * DE_DU(4:6,K,A))
          END FORALL
!           
!         calculate intermediate quantitiy R_HAT_TRANS * T
! 
          RTRAN_T = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3)
            RTRAN_T(I,J) = SUM(ROT_INC(:,I) * T(IJ(:,J)))
          END FORALL
! 
!         calculate derivative of rotated stress T WRT incremental nodal
!         displacements
! 
          DT_DU_1 = 0.0_DBL
          DO I = 1, 3
            DO J = 1, I
              FORALL (K = 1 : 3, A = 1 : NND)
                DT_DU_1(IJ(I,J),K,A) = SUM(DR_DU(I,:,K,A) *             &
     &                                     RTRAN_T(:,J))
              END FORALL
            END DO
          END DO
! 
          R_DTH_DU = 0.0_DBL
          DT_DU_2 = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            R_DTH_DU(I,J,K,A) = SUM(ROT_INC(I,:) *                      &
     &                               DT_HAT_DU(IJ(:,J),K,A))
            DT_DU_2(IJ(I,J),K,A) = SUM(R_DTH_DU(I,:,K,A)*ROT_INC(J,:))
          END FORALL
!           
          DT_DU_3 = 0.0_DBL
          DO I = 1, 3
            DO J = 1, I
              FORALL (K = 1 : 3, A = 1 : NND)
                DT_DU_3(IJ(I,J),K,A) = SUM(RTRAN_T(:,I) *               &
     &                                 DR_DU(J,:,K,A))
              END FORALL
            END DO
          END DO
! 
          DT_DU = DT_DU_1 + DT_DU_2 + DT_DU_3
! 
!         calculate derivative of detF WRT incr. nodal displ.
! 
          DJ_DU = JDET * FINVT_B
!           
!         calculate derivative of FINV WRT incr. nodal displacements
! 
          DFINV_DU = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            DFINV_DU(I,J,K,A) = -FINV(I,K) * FINVT_B(J,A)
          END FORALL
! 
!         calculate derivative of first Piola Kirchhoff stress WRT
!         incremental nodal displacements
! 
          DP_DU_1 = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            DP_DU_1(I,J,K,A) = DJ_DU(K,A) * SUM(T(IJ(I,:))*FINV(J,:))
          END FORALL
!           
          DP_DU_2 = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            DP_DU_2(I,J,K,A) = SUM(DT_DU(IJ(I,:),K,A) * FINV(J,:))
          END FORALL
          DP_DU_2 = JDET * DP_DU_2
!           
          DP_DU_3 = 0.0_DBL
          FORALL (I = 1 : 3, J = 1 : 3, K = 1 : 3, A = 1 : NND)
            DP_DU_3(I,J,K,A) = SUM(T(IJ(I,:)) * DFINV_DU(J,:,K,A))
          END FORALL
          DP_DU_3 = JDET * DP_DU_3
!      
          DP_DU = DP_DU_1 + DP_DU_2 + DP_DU_3
! 
!         now form the IP contribution to the element tangent
!         stiffness, which is the derivative of the end-step
!         residual contribution WRT incremental nodal displacements       
! 
          STF = 0.0_DBL
          NA = 0
          DO A = 1, NND
            NB = 0
            DO B = 1, NND
              FORALL (I = 1:NC, K = 1:NC)
                STF(NA+I,NB+K) =                                        &
     &                 SUM(DP_DU(I,1:NC,K,B) * SF_GRADS(1:NC,A))
              END FORALL
              NB = NB + NN_DOF(B)
            END DO
            NA = NA + NN_DOF(A)
          END DO
!           
        DEALLOCATE (DE_DU, DFINV_DU, DR_DU, FINVT_B, DJ_DU, DT_DU,      &
     &              DT_DU_1, DT_DU_2, DT_DU_3, DT_HAT_DU, DP_DU,        &
     &              DP_DU_1, DP_DU_2, DP_DU_3, R_DTH_DU, R_FHAT_INV,    & 
     &              RT_FINVT_B)     
!      
        END IF
! 
        END ASSOCIATE
!         
        RETURN
      END SUBROUTINE IP_CONT_LRG_DEF
!
!=======================================================================
! 
      SUBROUTINE IP_FCT_LRG_DEF (ELEM_DOF, SF_GRAD,                     &
     &                 SF, FRC, STF, BC, NEED_STF,                      &
     &                 NRML)
!
!       This routine computes the contributions from a single 
!       integration point to a facet-element traction boundary
!       residual, and, optionally, to the tangent stiffness matrix 
!       corresponding to this residual.  The IP contributions are 
!       assembled into the correct places in the element residual
!       vector (and stiffness matrix), which must be provided.
!       EVAL_CODE indicates which values to use:
!
!       EVAL_CODE = 1:  use end-step values
!       EVAL_CODE = 0:  use beginning-step values; do not compute STF
!
!-----------------------------------------------------------------------
!
!       declare dummy variables
! 
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        REAL (DBL), INTENT (IN) :: SF_GRAD(:,:), SF(:)
        REAL (DBL), INTENT (INOUT) :: FRC(:)       
        REAL (DBL), INTENT (INOUT) :: STF(:,:)      
        TYPE (FACET_BC), POINTER, INTENT (IN) :: BC
        LOGICAL, INTENT (IN) :: NEED_STF  
        REAL (DBL), INTENT (IN) :: NRML(:)    
!
!       declare local variables
! 
        TYPE (IP_KINEM) :: DFVARS
        REAL (DBL) :: ZERO, TF_VAL, TAU, PRES, BETA, BETACS(3), ONE, TWO
        REAL (DBL) :: PSHR(3), CSHR(3), TAUS(3), ALCS(3), TRAC_VEC(3)
        INTEGER :: A, B, I, J, K, M, NC
        LOGICAL :: TANGENT, RESID
        REAL(DBL), ALLOCATABLE :: DTRAC_DV(:,:,:), DCS_DV(:,:,:),       &
     &                            DALCS_DV(:,:,:), DB_DV(:,:)
!         REAL(DBL), POINTER :: PHI_VALS(:)
        INTEGER :: NND
!         REAL (DBL) :: ALPHA, DELTA(3,3), F(3,3), FBAR(3,3), FINV(3,3),  &
!      &                FINVT_N(3), JDET, NORM_FINVT_N
!         REAL (DBL), ALLOCATABLE :: DAL_DU(:,:), DFINV_DU(:,:,:,:),      &
!      &                             DJ_DU(:,:), DPBAR_DU(:,:,:),         &
!      &                             FINVT_B(:,:), N_DOT_DFINV_DU(:,:,:)          
!
!       associate local variable names
!
!         ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,                    &
!      &             U_INC => ELEM_DOF%U_INC, U_TOT => ELEM_DOF%U_TOT)
!
!       set number of nodes for the current element, the 
!       dimensionality of the problem, and the total number of
!       incremental nodal displacements for the element
!  
        ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF)  
        NND = SIZE(ELEM_DOF%NUM_NODAL_DOF)
        NC = SIZE(SF_GRAD, 1)      
!          
!-----------------------------------------------------------------------
!
        ZERO = 0.0_DBL 
        ONE = 1.0_DBL
        TWO = 2.0_DBL
!
!       set flags indicating the residual vector and/or the stiffness
!       matrix are to be computed
!   
        RESID = .TRUE.
        TANGENT = .FALSE.
        IF (NEED_STF) TANGENT = .TRUE.        
!
!       enforce that no STF is to be computed if this is a 
!       beginning-step residual evaluation
!
!         IF (EVAL_CODE .EQ. 0) TANGENT = .FALSE.
!
!
!       set the values of the basis functions 
!
!         ASSOCIATE (PHI_VALS => SF)
!
!       form the Piola traction vector at the current IP at time T_END
! 
!         ASSOCIATE (BC => ELEM%BC_DAT)
! 
        TRAC_VEC = ZERO
!         IF (EVAL_CODE .EQ. 0) THEN
!           TF_VAL = BC%TF%EVAL(CUR_SOLN_STAT%T_BEG)
!         ELSE      
            TF_VAL = BC%TF%EVAL(CUR_SOLN_STAT%T_END)
!         END IF
!         
!       allocate memory if necessary 
! 
        IF (TANGENT) THEN
          IF (.NOT. ALLOCATED(DTRAC_DV)) THEN
            ALLOCATE (DTRAC_DV(3,3,1:NND))
          END IF
          IF (.NOT. ALLOCATED(DCS_DV)) THEN 
            ALLOCATE (DCS_DV(3,3,1:NND))
          END IF
          IF (.NOT. ALLOCATED(DALCS_DV)) THEN
            ALLOCATE (DALCS_DV(3,3,1:NND))
          END IF
          IF (.NOT. ALLOCATED(DB_DV)) THEN
            ALLOCATE (DB_DV(3,1:NND))
          END IF
        END IF 
! 
!       Determine which type of facet BC is present and evaluate
!       necessary quantities depending on the presence of RESID
!       and/or TANGENT. Plese refer to theory document for details
!       of calculation
! 
        SELECT CASE (BC%FBC_TYPE)
!         
        CASE ('CTRAC')
          CALL GET_DEF_VARS (ELEM_DOF, 2, RESID, TANGENT, DFVARS, NRML, &
     &                       SF_GRAD)
          IF (RESID) TRAC_VEC = TF_VAL * BC%TRAC * DFVARS%AL
          IF (TANGENT) THEN
            DO I = 1, 3
              DTRAC_DV(I,:,:) = TF_VAL * BC%TRAC(I) * DFVARS%DAL_DV
            END DO          
          END IF
!         
        CASE ('PTRAC') 
          IF (RESID) TRAC_VEC = TF_VAL * BC%TRAC
          IF (TANGENT) DTRAC_DV = ZERO
!           
        CASE ('CPRES')
          CALL GET_DEF_VARS (ELEM_DOF, 1, RESID, TANGENT, DFVARS, NRML, &
     &                       SF_GRAD)
          IF (RESID) TRAC_VEC = -TF_VAL * BC%PRES * DFVARS%ALCN
          IF (TANGENT) DTRAC_DV = -TF_VAL * BC%PRES * DFVARS%DALCN_DV
!         
        CASE ('PPRES')
          CALL GET_DEF_VARS (ELEM_DOF, 3, RESID, TANGENT, DFVARS, NRML, &
     &                       SF_GRAD)
          IF (RESID) TRAC_VEC = -TF_VAL * BC%PRES * DFVARS%CN
          IF (TANGENT) DTRAC_DV = -TF_VAL * BC%PRES * DFVARS%DCN_DV
!           
        CASE ('CFOLL')
          CALL GET_DEF_VARS (ELEM_DOF, 3, RESID, TANGENT, DFVARS, NRML, &
     &                       SF_GRAD)
          PRES = -SUM(BC%TRAC(1:NC) * NRML(1:NC))
          TAUS = BC%TRAC + PRES * NRML(:)
          TAU = SQRT(SUM(TAUS(1:NC) * TAUS(1:NC)))
          IF (TAU .LT. 1.0D-10) THEN
            IF (RESID) TRAC_VEC = TF_VAL * (-PRES * DFVARS%ALCN)      
            IF (TANGENT) DTRAC_DV = TF_VAL * (-PRES * DFVARS%DALCN_DV)     
          ELSE
            PSHR = (TAU**(-ONE)) * TAUS
            BETACS = M_V(DFVARS%F, PSHR)          
            BETA = SQRT(SUM(BETACS * BETACS))  
            IF (RESID) THEN
              ALCS = DFVARS%AL * BETA**(-ONE) * BETACS
              TRAC_VEC = TF_VAL * (TAU * ALCS - PRES * DFVARS%ALCN)          
            END IF
            IF (TANGENT) THEN
              DB_DV = ZERO
              DALCS_DV = ZERO
              DO K = 1, 3
                DO A = 1, NND
                  DB_DV(K,A) = BETA**(-ONE) *                           &
     &                 SUM(M_V(DFVARS%DF_DV(:,:,K,A), PSHR) * BETACS)
                  DALCS_DV(:,K,A) =                                     &
     &               DFVARS%DAL_DV(K,A) * BETA**(-ONE) * BETACS         &
     &               - DFVARS%AL * BETA**(-TWO) * DB_DV(K,A) * BETACS   &
     &               + DFVARS%AL * BETA**(-ONE) *                       &
     &                 M_V(DFVARS%DF_DV(:,:,K,A), PSHR)
                END DO
              END DO
              DTRAC_DV = TF_VAL * (TAU * DALCS_DV                       &
     &                        - PRES * DFVARS%DALCN_DV)    
            END IF
          END IF
!         
        CASE ('PFOLL')
          CALL GET_DEF_VARS (ELEM_DOF, 3, RESID, TANGENT, DFVARS, NRML, &
     &                       SF_GRAD)
          PRES = -SUM(BC%TRAC(1:NC) * NRML(1:NC))
          TAUS = BC%TRAC + PRES * NRML(:)
          TAU = SQRT(SUM(TAUS(1:NC) * TAUS(1:NC)))
          IF (TAU .LT. 1.0D-10) THEN
            IF (RESID) TRAC_VEC = TF_VAL * (- PRES * DFVARS%CN)     
            IF (TANGENT) DTRAC_DV = TF_VAL * (-PRES * DFVARS%DCN_DV)     
          ELSE          
            PSHR = (TAU**(-ONE)) * TAUS
            BETACS = M_V(DFVARS%F, PSHR)          
            BETA = SQRT(SUM(BETACS * BETACS))
            IF (RESID) THEN
              CSHR = BETA**(-ONE) * BETACS          
              TRAC_VEC = TF_VAL * (TAU * CSHR - PRES * DFVARS%CN)
            END IF
            IF (TANGENT) THEN
              DB_DV = ZERO
              DCS_DV = ZERO
              DO K = 1, 3
                DO A = 1, NND
                  DB_DV(K,A) = BETA**(-ONE) *                           &
     &                 SUM(M_V(DFVARS%DF_DV(:,:,K,A), PSHR) * BETACS)
                  DCS_DV(:,K,A) = -BETA**(-TWO) * DB_DV(K,A) * BETACS   &
     &                            + BETA**(-ONE) *                      &
     &                            M_V(DFVARS%DF_DV(:,:,K,A), PSHR)
                END DO
              END DO
              DTRAC_DV = TF_VAL * (TAU * DCS_DV - PRES * DFVARS%DCN_DV)            
            END IF
          END IF
!           
        CASE DEFAULT
          PRINT *, 'facet BC type code ', BC%FBC_TYPE,                  &
     &               ' is currently not supported'
          PRINT *, 'aborting'
          STOP
        END SELECT
!
!       form the force-vector contribution if needed
!
        IF (RESID) THEN
          FRC = ZERO
          K = 0
          DO A = 1, NND
            DO I = 1, NC
!               IF (ELEM%ACTIVE(I,A) .EQ. 'Y') THEN
                K = K + 1
!                 IF (I .LE. 3) THEN
                  FRC(K) = TRAC_VEC(I) * SF(A)
!                 END IF
!               END IF
            END DO
          END DO
        END IF
!
!       form the derivative of FRC with respect to incremental nodal 
!       dof, if indicated
!
        
        IF (TANGENT) THEN
          STF = ZERO
          K = 0
          DO A = 1, NND
            DO I = 1, NC
!               IF (ELEM%ACTIVE(I,A) .EQ. 'Y') THEN
                K = K + 1
!                 IF (I .GT. 3) CYCLE
                M = 0
                DO B = 1, NND
                  DO J = 1, NC
!                     IF (ELEM%ACTIVE(J,B) .EQ. 'Y') THEN
                      M = M + 1
!                       IF (J .GT. 3) CYCLE
                      STF(K,M) = DTRAC_DV(I,J,B) * SF(A)
!                     END IF
                  END DO
                END DO
!               END IF
            END DO
          END DO
!
        END IF
!         
        END ASSOCIATE
        CALL DFVARS%DEL_IP_KINEM
!
        END SUBROUTINE IP_FCT_LRG_DEF
!
!=======================================================================
! 
      SUBROUTINE GET_DEF_VARS (ELEM_DOF, KINEM_CODE, RESID, TANGENT,    &
     &                         DFVARS, NRML, SF_GRAD)

!
!       This routine takes as input the data for a single facet element, 
!       as represented by a LCL_ELEM_FACET object, and an 
!       integration-point number. It computes the end-step total 
!       deformation gradient F, along with other kinematic quantities
!       of interest
!       
!       KINEM_CODE indicates which kinematic quantities are desired
!       KINEM_CODE = 1: calculate (alpha * n_i), where alpha is the area
!                       stretch of current configuration with respect to
!                       reference config. and n_i are the components of 
!                       the normal in the current configuration
!       KINEM_CODE = 2: calculate alpha
!       KINEM_CODE = 3: calculate n_i
! 
!       If the logical flag TANGENT is .TRUE., then the derivative of F 
!       with respect to all incremental nodal dof for the element are 
!       also computed. Additionally, derivatives of the above quantities 
!       delineated by KINEM_CODE wrt incr. nodal displ. are computed.
!       Results are stored in object DFVARS.
!
!-----------------------------------------------------------------------
!
!       declare dummy variables
!
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        INTEGER, INTENT (IN) :: KINEM_CODE        
        LOGICAL, INTENT (IN) :: TANGENT, RESID
        TYPE (IP_KINEM), INTENT (OUT) :: DFVARS
        REAL (DBL), INTENT (IN) :: NRML(:)  
        REAL (DBL), INTENT (IN) :: SF_GRAD(:,:)             
!g
!       declare local variables
!
        REAL (DBL) :: UB_GRAD(3,3), UH_GRAD(3,3), F_BAR(3,3), NCF
        REAL (DBL) :: FM1(3), FM2(3), M1(3), M2(3), EP(3)
        REAL (DBL) :: ZERO, ONE, TWO
!         REAL (DBL), POINTER :: SF_GRAD(:,:)
        REAL (DBL), ALLOCATABLE :: DAL2_DV(:,:)
        INTEGER :: A, I, J, K, NC, NND
!
!-----------------------------------------------------------------------
!
!       simplify the names of the variables in ELEM
!
        ASSC1 : ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,            &
     &             U_H => ELEM_DOF%U_INC, U_B => ELEM_DOF%U_TOT)
!
!         SF_GRAD => ELEM%SF_GRAD(:,:,IPN)
        NND = SIZE(NN_DOF, 1)
!
        ZERO = 0.0_DBL
        ONE = 1.0_DBL
        TWO = 2.0_DBL
!
!       get the spatial dimension of the problem
!
        NC = SIZE(SF_GRAD, 1)
        NCF = NC - 1
!
!       form displacement gradients
!
        UB_GRAD = 0.0_DBL
        UH_GRAD = 0.0_DBL
        DO I = 1, NC
          DO J = 1, NC
            UB_GRAD(I,J) = SUM(U_B(I,:) * SF_GRAD(J,:))
            UH_GRAD(I,J) = SUM(U_H(I,:) * SF_GRAD(J,:))
          END DO
        END DO
!
!       form the beginning-step and end-step deformation gradients
!
        F_BAR = UB_GRAD
        DO I = 1, 3
          F_BAR(I,I) = F_BAR(I,I) + ONE
        END DO
        DFVARS%F = F_BAR + UH_GRAD
!
!       form derivative of F wrt incremental nodal dof, if indicated
!
        IF (TANGENT) THEN
!
!         allocate local scratch space and allocatable components of
!         DFVARS, if neccessary
!
          IF (.NOT. ALLOCATED(DFVARS%DF_DV)) THEN
            ALLOCATE (DFVARS%DF_DV(3,3,3,NND))
          END IF        
!
!         set the derivative of F wrt incremental nodal dof
!           
          DFVARS%DF_DV = ZERO
          DO I = 1, NC
            DO J = 1, NC
              DO A = 1, NND
                DFVARS%DF_DV(I,J,I,A) = SF_GRAD(J,A)
              END DO
            END DO
          END DO
! 
        END IF
        ASSC2 : ASSOCIATE (F => DFVARS%F, DF_DV => DFVARS%DF_DV) 
!         
!       calculate material line vectors M_1, M_2
! 
        IF (NCF .EQ. 1) THEN
          M2 = (/ ZERO, ZERO, ONE /)
          M1 = CROSS_PR(M2, NRML)
        ELSE IF (NCF .EQ. 2) THEN
          EP = 0.0_DBL
          EP(MINLOC(ABS(NRML(1:NC)))) = 1.0_DBL
          M1 = ZERO
          M1 = EP(1:NC) - DOT_PRODUCT(NRML(1:NC),EP(1:NC))              &
     &                         * NRML(1:NC)
          M1 = M1 / SQRT(SUM(M1 * M1))        
          M2 = CROSS_PR(NRML,M1)
        END IF
! 
        FM1 = M_V(F, M1)
        FM2 = M_V(F, M2)
! 
!       determine which kinematic quantities are to be computed
! 
        IF (KINEM_CODE .GE. 1) THEN
!         
          IF (RESID .OR. TANGENT) THEN
! 
            IF (.NOT. ALLOCATED(DFVARS%ALCN)) ALLOCATE (DFVARS%ALCN(3))
!           
!           compute alpha * n
! 
            DFVARS%ALCN = ZERO
            DFVARS%ALCN = CROSS_PR(FM1, FM2)
!             
          END IF
!      
!         check if tangent stiffness required
! 
          IF (TANGENT) THEN
!           
            IF (.NOT. ALLOCATED(DFVARS%DALCN_DV)) THEN
              ALLOCATE (DFVARS%DALCN_DV(3,3,NND))
            END IF
!
!           compute derivative of alpha * n wrt. incr. nodal disp.
! 
            DFVARS%DALCN_DV = ZERO
            DO K = 1, 3
              DO A = 1, NND
                DFVARS%DALCN_DV(:,K,A) = CROSS_PR(M_V(                  &
     &                                DF_DV(:,:,K,A),M1), FM2) +        &
     &                                CROSS_PR(FM1,M_V(                 &
     &                                DF_DV(:,:,K,A), M2))   
              END DO
            END DO
!             
          END IF
        END IF
        ASSC3 : ASSOCIATE (ALCN => DFVARS%ALCN,                         &
     &                     DALCN_DV => DFVARS%DALCN_DV)        
!         
!       if KINEM_CODE > 1, continue with computations
! 
        IF (KINEM_CODE .GE. 2) THEN
!         
          IF (RESID .OR. TANGENT) THEN
!         
!           compute alpha
!     
            DFVARS%AL = ZERO
            DFVARS%AL = SQRT(SUM(ALCN * ALCN))
!             
          END IF
!           
!         check if tangent stiffness required
! 
          IF (TANGENT) THEN
!           
            IF (.NOT. ALLOCATED(DFVARS%DAL_DV)) THEN
              ALLOCATE (DFVARS%DAL_DV(3,NND))
            END IF
            ALLOCATE (DAL2_DV(3,NND))
!
!           compute derivative of alpha wrt. incr. nodal disp.
! 
            DAL2_DV = ZERO
            DFVARS%DAL_DV = ZERO            
            DO K = 1, 3
              DO A = 1, NND
                DAL2_DV(K,A) = SUM(DALCN_DV(:,K,A) * ALCN)
                DFVARS%DAL_DV(K,A) = DFVARS%AL**(-ONE) * DAL2_DV(K,A)
              END DO
            END DO
!            
          END IF
        END IF
        ASSC4 : ASSOCIATE (AL => DFVARS%AL, DAL_DV => DFVARS%DAL_DV)        
!         
!       if KINEM_CODE > 2, continue with computations
! 
        IF (KINEM_CODE .GE. 3) THEN
!         
          IF (RESID) THEN
!         
            IF (.NOT. ALLOCATED(DFVARS%CN)) ALLOCATE (DFVARS%CN(3))
!           
!           compute n
! 
            DFVARS%CN = ZERO
            DFVARS%CN = AL**(-ONE) * ALCN
!           
          END IF
!      
!         check if tangent stiffness required
! 
          IF (TANGENT) THEN
            IF (.NOT. ALLOCATED(DFVARS%DCN_DV)) THEN
              ALLOCATE (DFVARS%DCN_DV(3,3,NND))
            END IF
!             
!           compute derivative of n wrt incr. nodal disp. 
! 
            DFVARS%DCN_DV = ZERO
            DO K = 1, 3
              DO A = 1, NND
                DFVARS%DCN_DV(:,K,A) = -AL**(-TWO) * DAL_DV(K,A) *      &
     &                           ALCN(:) + AL**(-ONE) * DALCN_DV(:,K,A)         
              END DO
            END DO
! 
          END IF
        END IF
! 
!       end all associate constructs
! 
        END ASSOCIATE ASSC4
        END ASSOCIATE ASSC3
        END ASSOCIATE ASSC2
        END ASSOCIATE ASSC1
!
        RETURN
      END SUBROUTINE GET_DEF_VARS
!       
!=======================================================================
!
      SUBROUTINE IP_CONT_GUESS (ELEM_DOF, SF_GRADS, STR_INC, ROT_INC,   &
     &                          FRC, STF, T0, T1, T2)
!
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        REAL (DBL), INTENT (INout) :: SF_GRADS(:,:), ROT_INC(:,:),      &
     &                             STR_INC(:)
        REAL (DBL), INTENT (OUT) :: FRC(:)     
        REAL (DBL), INTENT (OUT) :: STF(:,:)
        REAL (DBL), INTENT (IN) :: T0, T1, T2
!
!       declare local variables
!
        INTEGER :: A, B, I, J, IJ(3,3), K, N, NA, NB, NC, NND   
        REAL (DBL) :: FK(3,3), FKM1(3,3), FHAT_INV(3,3), FKINV(3,3),    &
     &                JKDET, JBDET, EPSI(3,3,3), FBAR(3,3), THETAB,     &
     &                RBAR(3,3), FBINV(3,3), PI, FKM1INV(3,3),          &
     &                RR1(3,3), RR2(3,3), R, S, RHO, JKM1DET, TRF,      &
     &                TRR1, TRR2, SINTH, MU(3), DSQ(6), DCUB(6),        &
     &                UBMSQ(6), UBAR(6),UBM1(6), STR_RATE(6), DELTA(3,3)
        REAL (DBL) :: ZERO, ONE, TWO, HALF, QTR, THREE, THETA
        REAL (DBL), ALLOCATABLE :: AMAT(:,:), BMAT(:,:)
!
!-----------------------------------------------------------------------
!
!       associate local variable names
!
        ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,                    &
     &             U_INC => ELEM_DOF%U_INC, U_TOT => ELEM_DOF%U_TOT)
!
!       set number of nodes for the current element, the 
!       dimensionality of the problem, and the total number of
!       incremental nodal displacements for the element
! 
        NND = SIZE(NN_DOF)
        NC = SIZE(SF_GRADS, 1)
!
        ZERO = 0.0_DBL
        ONE = 1.0_DBL
        TWO = 2.0_DBL
        THREE = 3.0_DBL
        HALF = 0.5_DBL
        QTR = 0.25_DBL
        PI = 3.14159265358979324_DBL
!
!       define Kronecker delta and alternator symbols
!
        DELTA = 0.0_DBL
        DELTA(1,1) = 1.0_DBL
        DELTA(2,2) = 1.0_DBL
        DELTA(3,3) = 1.0_DBL
!
        EPSI = 0.0_DBL
        EPSI(1,2,3) = 1.0_DBL
        EPSI(1,3,2) = -1.0_DBL        
        EPSI(2,3,1) = 1.0_DBL
        EPSI(2,1,3) = -1.0_DBL        
        EPSI(3,1,2) = 1.0_DBL
        EPSI(3,2,1) = -1.0_DBL
!         
!       set a map from tensor indices to column-vector indices
!
        IJ(1:3,1) = (/ 1, 6, 5 /)
        IJ(1:3,2) = (/ 6, 2, 4 /)
        IJ(1:3,3) = (/ 5, 4, 3 /)
!
!       form deformation gradient FK = del(xK)/del(X) and FKM1 = 
!       del(xbar)/del(X) as well as Fhat inverse FOR THE PREVIOUS
!       TIME STEP
!
        FK = DELTA
        FKM1 = DELTA        
!
        FORALL(I = 1 : NC, J = 1 : NC)
          FK(I,J) = DELTA(I,J) + SUM(U_TOT(I,:) * SF_GRADS(J,:))
          FKM1(I,J) = FK(I,J) - SUM(U_INC(I,:) * SF_GRADS(J,:))   
        END FORALL 
!
!       calculate J = determinant of Fk
!         
        JKDET = DETM(FK,NC)
        JKM1DET = DETM(FKM1, NC)
!
!       calculate Finverse, Fhatinverse
!
        FKINV = DELTA
        FKM1INV = DELTA
        FHAT_INV = DELTA
        CALL INV3 (FK, JKDET**(-1), NC, FKINV)
        CALL INV3 (FKM1, JKM1DET**(-1), NC, FKM1INV)
        FHAT_INV = M_M(FKM1, FKINV)
! 
!       compute shape function gradients wrt to kappa k configuration
! 
        ALLOCATE (BMAT(NC, NND))
        BMAT = ZERO
        DO I = 1, NC
          DO A =  1, NND
            BMAT(I,A) = SUM(SF_GRADS(1:NC, A) * FKM1INV(1:NC, I))            
          END DO
        END DO
!         
!       compute intermediate values needed for rotation angle of
!       previous time step
! 
        RR1 = M_MT(ROT_INC, ROT_INC)     
        RR2 = M_M(ROT_INC, ROT_INC)
        TRR1 = RR1(1,1) + RR1(2,2) + RR1(3,3)
        TRR2 = RR2(1,1) + RR2(2,2) + RR2(3,3) 
!             
!       assume sine is always positive, direction of axis of
!       rotation mu will handle sign issues
! 
        SINTH = HALF * SQRT(TRR1 - TRR2)        
        IF (ABS(SINTH) .LT. 1.0D-8) THEN
          MU = ZERO
        ELSE
          MU(1) = ROT_INC(2,3) - ROT_INC(3,2)
          MU(2) = ROT_INC(3,1) - ROT_INC(1,3)
          MU(3) = ROT_INC(1,2) - ROT_INC(2,1)
          MU = -HALF / SINTH * MU
        END IF
        THETA = ASIN(SINTH)
!         
!       determine sign of cos(theta)
! 
        TRF = FHAT_INV(1,1) + FHAT_INV(2,2) + FHAT_INV(3,3)
        TRF = TRF - ONE        
        IF (SIGN(ONE, TRF) .LT. ZERO) THETA = PI - THETA
!         
!       extrapolate angle of rotation
! 
        THETAB = (T2 - T0) / (T1 - T0) * THETA
!         
!       intermediate quantities
! 
        R = ONE - COS(THETAB)
!         
!       form Rbar
! 
        DO I = 1, 3
          DO J = 1, 3
            RBAR(I,J) = DELTA(I,J) + R * (MU(I) * MU(J) - DELTA(I,J))   &
     &                  - SIN(THETAB) * SUM(EPSI(I,J,:) * MU)
          END DO
        END DO
!         
!       compute Ub minus 1 and Ub
! 
        STR_RATE = (T1 - T0)**(-1) * STR_INC
        DO I = 1, 3
          DO J = 1, I
            DSQ(IJ(I,J)) = SUM(STR_RATE(IJ(I,:)) * STR_RATE(IJ(:,J)))                  
          END DO
        END DO
!         
        DO I = 1, 3
          DO J = 1, I
            DCUB(IJ(I,J)) = SUM(STR_RATE(IJ(I,:)) * DSQ(IJ(:,J)))                  
          END DO
        END DO        
! 
        UBM1 = (T2 - T0) * STR_RATE + HALF * (T2 - T0)**2 * DSQ
     &         + 1.0_DBL / 6.0_DBL * (T2 - T0)**3 * DCUB
        UBAR = UBM1
        DO I = 1, 3
          UBAR(I) = UBAR(I) + ONE
        END DO
!         
!       compute Fbar
! 
        DO I = 1, 3
          DO J = 1, 3
            FBAR(I,J) = SUM(RBAR(I,:) * UBAR(IJ(:,J)))
          END DO
        END DO
! 
        JBDET = DETM(FBAR, NC)
        FBINV = DELTA
        CALL INV3(FBAR, JBDET**(-1), NC, FBINV)
!         
!       determine A vector for each node, stored in AMAT
! 
        ALLOCATE (AMAT(NC, NND))
        AMAT = ZERO
        DO I = 1, NC
          DO A = 1, NND
            AMAT(I,A) = SUM(FBINV(1:NC,I) * BMAT(1:NC, A))                
          END DO
        END DO
!         
!       determine relative stretch to total stretch and rotation rho 
! 
        DO I = 1, 3
          DO J = 1, I
            UBMSQ(IJ(I,J)) = SUM(UBM1(IJ(I,:)) * UBM1(IJ(:,J)))                  
          END DO
        END DO        
        S = UBMSQ(1) + UBMSQ(2) + UBMSQ(3)
!         
        RHO = TWO / PI * ASIN(S / SQRT(R**2 + S**2))
        IF (RHO .LT. 0.1_DBL) THEN
          RHO = 0.1_DBL
        ELSE IF (RHO .GT. 0.9_DBL) THEN
          RHO = 0.9_DBL
        END IF
! 
!       form the end-step residual contribution   
! 
        N = 0
        FRC = 0.0_DBL
        DO B = 1, NND
          FORALL (I = 1 : NC) FRC(N+I) =                                &
     &          (TWO * RHO * AMAT(I, B) - (TWO - RHO) *                 &
     &          SUM(FBINV(I,1:NC) * AMAT(:,B)) - (THREE * RHO - TWO) *  &
     &          SUM(FBINV(1:NC,I) * AMAT(:,B))) * JKM1DET
          N = N + NN_DOF(B)
        END DO
! 
!       now form the IP contribution to the element tangent
!       stiffness, which is the derivative of the end-step
!       residual contribution WRT incremental nodal displacements       
! 
        STF = 0.0_DBL
        NB = 0
        DO B = 1, NND
          NA = 0
          DO A = 1, NND
            FORALL (I = 1:NC, K = 1:NC)
              STF(NB+I,NA+K) = ((TWO - RHO) * SUM(AMAT(:,A) * AMAT(:,B))&
     &                      * DELTA(I,K) +                              &
     &                      (THREE * RHO - TWO) * AMAT(I,A) * AMAT(K,B))&
     &                      * JKM1DET
            END FORALL
            NA = NA + NN_DOF(A)
          END DO
          NB = NB + NN_DOF(B)
        END DO
! 
        DEALLOCATE (AMAT)
        END ASSOCIATE
!         
        RETURN
      END SUBROUTINE IP_CONT_GUESS
!
!=======================================================================
!
      SUBROUTINE GET_DEF_GRAD (ELEM_DOF, SF_GRADS, F0)
!
        TYPE (LCL_NODAL_DATA), INTENT (IN) :: ELEM_DOF
        REAL (DBL), INTENT (IN) :: SF_GRADS(:,:)
        REAL (DBL), INTENT (OUT) :: F0(3,3)
!
!       declare local variables
!
        INTEGER :: I, J, NC
        REAL (DBL) :: ZERO, ONE
!
!-----------------------------------------------------------------------
!         
!       associate local variable names
!
        ASSOCIATE (U_TOT => ELEM_DOF%U_TOT)
!
!       set number of nodes for the current element, the 
!       dimensionality of the problem, and the total number of
!       incremental nodal displacements for the element
!
        NC = SIZE(SF_GRADS, 1)
! 
        ZERO = 0.0_DBL
        ONE = 1.0_DBL
!         
!       form deformation gradient F0 = del(xbar)/del(X)
!
        F0 = ZERO
        DO I = 1, NC
          DO J = 1, NC
            F0(I,J) = SUM(U_TOT(I,:) * SF_GRADS(J,:))         
          END DO
        END DO
        DO I = 1, 3
          F0(I,I) = F0(I,I) + ONE
        END DO
        
        END ASSOCIATE
! 
        RETURN
      END SUBROUTINE GET_DEF_GRAD
!
!=======================================================================
!
      END MODULE IP_FRC_STF_M
