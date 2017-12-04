      MODULE IP_STRAIN_INC_M
!
!     This module contains procedures that compute integration-point
!     strain-increment values.  It is USEd by ELEMS_NODES_M...
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE MISC_TYPES_M
      USE MATRIX_M
      IMPLICIT NONE
!
      PUBLIC :: STRAIN_INC_SML_DEF, STRAIN_INC_LRG_DEF
!
!=======================================================================
      CONTAINS
!=======================================================================
!
      SUBROUTINE STRAIN_INC_SML_DEF (ELEM_DOF, SF_GRADS, STR_INC,       &
     &                               DE_DU)
!
        TYPE (LCL_NODAL_DATA) :: ELEM_DOF
        REAL (DBL), INTENT (IN) :: SF_GRADS(:,:)
        REAL (DBL), INTENT (OUT), OPTIONAL :: DE_DU(:,:)
        REAL (DBL), INTENT (OUT) :: STR_INC(:)
!
!       declare local variables
!
        INTEGER :: I, J, K, N, NND, NC, NT
        LOGICAL :: TEMP_DOF
!
!-----------------------------------------------------------------------
!
!       associate local variable names
!
        ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,                    &
     &             U_INC => ELEM_DOF%U_INC)
!
!       set number of nodes for the current element
!
        NND = SIZE(NN_DOF)
!
!       set the spatial dimension of the problem
!
        NC = SIZE(SF_GRADS, 1)
!
!       Set a logical flag indicating that temperature dof are present
!       at any node of the element.  If TEMP_DOF is .TRUE., then
!       STRIN_INC comes in with length 9, and 3 temperature-gradient
!       increment components are appended to the 6 kinematic strain
!       increments.  Also, DE_DU comes in with dimension (9,NND).
!
        IF (NC .EQ. 2) THEN
          NT = 3
        ELSE
          NT = 4
        END IF
        IF (ANY(NN_DOF(1:NND) .EQ. NT)) THEN
          TEMP_DOF = .TRUE.
        ELSE
          TEMP_DOF = .FALSE.
        END IF
!
!       Form a "strain increment" for the step.  The strain increment
!       has 6 components for the kinematic strain, because even
!       for 2D problems, the constitutive update is done in 3D.
!       If any of the element's nodes has a temperature dof,
!       3 additional components, corresponding to an increment of
!       temperature gradient, are appended to the 6 kinematic strains.
!
        STR_INC = 0.0_DBL
        DO I = 1, NND
          FORALL (J = 1 : NC) STR_INC(J) = STR_INC(J) + U_INC(J,I) *    &
     &                                      SF_GRADS(J,I)
          STR_INC(6) = STR_INC(6) + 0.5_DBL *                           &
     &         (U_INC(1,I) * SF_GRADS(2,I) + U_INC(2,I) * SF_GRADS(1,I))
        END DO
        IF (NC .EQ. 3) THEN
          DO I = 1, NND
            STR_INC(4) = STR_INC(4) + 0.5_DBL *                         &
     &           (U_INC(2,I) * SF_GRADS(3,I) +                          &
     &            U_INC(3,I) * SF_GRADS(2,I))
            STR_INC(5) = STR_INC(5) + 0.5_DBL *                         &
     &           (U_INC(1,I) * SF_GRADS(3,I) +                          &
     &            U_INC(3,I) * SF_GRADS(1,I))
          END DO
        END IF
        IF (TEMP_DOF) THEN
          DO I = 1, NND
            IF (NN_DOF(I) .EQ. NT - 1) CYCLE
            STR_INC(7:8) = STR_INC(7:8) +                               &
     &                      U_INC(NT,I) * SF_GRADS(1:2,I)
          END DO
          IF (NC .EQ. 3) THEN
            DO I = 1, NND
              IF (NN_DOF(I) .EQ. NT - 1) CYCLE
              STR_INC(9) = STR_INC(9) + U_INC(NT,I) * SF_GRADS(3,I)
            END DO
          END IF
        END IF
!
!       if indicated, compute the derivative of the strain increment
!       WRT nodal dof
!
        IF (PRESENT(DE_DU)) THEN
          DE_DU = 0.0_DBL
          K = 0
          IF (NC .EQ. 2) THEN
            DO N = 1, NND
              DO I = 1, NN_DOF(N)
                K = K + 1
                SELECT CASE (I)
                CASE (1)
                  DE_DU(1,K) = SF_GRADS(1,N)
                  DE_DU(6,K) = 0.5_DBL * SF_GRADS(2,N)
                CASE (2)
                  DE_DU(2,K) = SF_GRADS(2,N)
                  DE_DU(6,K) = 0.5_DBL * SF_GRADS(1,N)
                CASE (3)
                  DE_DU(7,K) = SF_GRADS(1,N)
                  DE_DU(8,K) = SF_GRADS(2,N)
                END SELECT
              END DO
            END DO
          ELSE IF (NC .EQ. 3) THEN
            DO N = 1, NND
              DO I = 1, NN_DOF(N)
                K = K + 1
                SELECT CASE (I)
                CASE (1)
                  DE_DU(1,K) = SF_GRADS(1,N)
                  DE_DU(5,K) = 0.5_DBL * SF_GRADS(3,N)
                  DE_DU(6,K) = 0.5_DBL * SF_GRADS(2,N)
                CASE (2)
                  DE_DU(2,K) = SF_GRADS(2,N)
                  DE_DU(4,K) = 0.5_DBL * SF_GRADS(3,N)
                  DE_DU(6,K) = 0.5_DBL * SF_GRADS(1,N)
                CASE (3)
                  DE_DU(3,K) = SF_GRADS(3,N)
                  DE_DU(4,K) = 0.5_DBL * SF_GRADS(2,N)
                  DE_DU(5,K) = 0.5_DBL * SF_GRADS(1,N)
                CASE (4)
                  DE_DU(7:9,K) = SF_GRADS(1:3,N)
                END SELECT
              END DO
            END DO
          END IF
        END IF
!
        END ASSOCIATE
!
        RETURN
      END SUBROUTINE STRAIN_INC_SML_DEF
!
!=======================================================================
!
      SUBROUTINE STRAIN_INC_LRG_DEF (ELEM_DOF, SF_GRADS, STR_INC,       &
     &                               ROT_INC)
!
        TYPE (LCL_NODAL_DATA) :: ELEM_DOF
        REAL (DBL), INTENT (IN) :: SF_GRADS(:,:)
        REAL (DBL), INTENT (OUT) :: STR_INC(:)
        REAL (DBL), INTENT (OUT) :: ROT_INC(:,:)
!
!       declare local variables
!
        INTEGER :: I, J, NC, NND
        REAL (DBL) :: A(3,3), ALPHA(3), CINV_MIN_I(3,3), COS_COEF,      &
     &                COS_THETA, DELTA(3,3), EPSI(3,3,3), F(3,3),       &
     &                FBAR(3,3), FHAT_INV(3,3), FINV(3,3), JDET, P, Q,  &
     &                SIN_COEF,  STR_INC_MAT(3,3), TOL, TR_FHAT_INV,    &
     &                TR_FHAT_INV_MIN_1, UINC_B(3,3)
!
!-----------------------------------------------------------------------
!
!       associate local variable names
!
        ASSOCIATE (NN_DOF => ELEM_DOF%NUM_NODAL_DOF,                    &
     &             U_INC => ELEM_DOF%U_INC, U_TOT => ELEM_DOF%U_TOT)
!
!       set number of nodes for the current element
!
        NND = SIZE(NN_DOF)
!
!       set the spatial dimension of the problem
!
        NC = SIZE(SF_GRADS, 1)
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
!       define tolerance for which Q in algorithm is determined close 
!       to zero
! 
        TOL = 0.01_DBL
!
!       form deformation gradient F = del(x)/del(X) and 
!       Fbar = del(xbar)/del(X) as well as inverse of Fhat
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
!       calculate inverse of F, inverse of Fhat, A, inverse of C
!
        FINV = DELTA
        FHAT_INV = DELTA
        CALL INV3 (F, JDET**(-1), NC, FINV)
        FHAT_INV = M_M(FBAR, FINV)    
!         
        A = 0.0_DBL
        UINC_B = 0.0_DBL
        FORALL (I = 1 : NC, J = 1 : NC)
          UINC_B(I,J) = SUM(U_INC(I,:) * SF_GRADS(J,:))
        END FORALL
        FORALL (I = 1 : NC, J = 1 : NC)
          A(I,J) = SUM(UINC_B(I,:) * FINV(:,J))
        END FORALL    
!         
        CINV_MIN_I = 0.0_DBL
        CINV_MIN_I = M_MT(A,A) - A - TRANSPOSE(A)
!
!       calculate strain increment as a matrix
!
        STR_INC_MAT = 0.0_DBL
        STR_INC_MAT = -0.5_DBL * CINV_MIN_I                             &
     &                  + 0.25_DBL * M_M(CINV_MIN_I, CINV_MIN_I)
!
!       flatten strain increment into vector
! 
        STR_INC = 0.0_DBL
        STR_INC(1) = STR_INC_MAT(1,1)
        STR_INC(2) = STR_INC_MAT(2,2)
        STR_INC(6) = STR_INC_MAT(1,2)
        IF (NC .EQ. 3) THEN
          STR_INC(3) = STR_INC_MAT(3,3)
          STR_INC(4) = STR_INC_MAT(2,3)
          STR_INC(5) = STR_INC_MAT(1,3)
        END IF
!
!       compute parameters needed for rotation increment
! 
        ALPHA(1) = -A(2,3) + A(3,2)
        ALPHA(2) = -A(3,1) + A(1,3)
        ALPHA(3) = -A(1,2) + A(2,1)
!
        Q = 0.25_DBL * SUM(ALPHA * ALPHA)
!         
        TR_FHAT_INV = FHAT_INV(1,1) + FHAT_INV(2,2) + FHAT_INV(3,3)
        TR_FHAT_INV_MIN_1 = TR_FHAT_INV - 1.0_DBL
        P = 0.25_DBL * (TR_FHAT_INV_MIN_1**2)
!
!       calculate terms associated with cosine and sine coefficients 
!       in rotation increment definition
! 
        COS_THETA = SQRT(P +                                            &
     &         3.0_DBL * P**2 * (1.0_DBL - (P + Q)) / (P + Q)**2 -      &
     &         2.0_DBL * P**3 * (1.0_DBL - (P + Q)) / (P + Q)**3)
        IF (TR_FHAT_INV_MIN_1 .LT. 0.0_DBL) THEN
          COS_THETA = -COS_THETA
        END IF  
!         
        SIN_COEF = 0.5_DBL * SQRT((P * Q * (3.0_DBL - Q) + P**3 + Q**2) &
     &                             / (P + Q)**3)                        &
!      
        IF (ABS (Q) .GT. TOL) THEN   
          COS_COEF = (1.0_DBL - COS_THETA) / (4.0_DBL * Q) 
        ELSE
          COS_COEF = 0.125_DBL +                                        &
     &               Q * (P**2 - 12.0_DBL * (P - 1.0_DBL))              &
     &                 / (32.0_DBL * P**2) +                            &
     &               Q**2 * (P - 2.0_DBL) *                             &
     &                 (P**2 - 10.0_DBL * P + 32.0_DBL) /               &
     &                 (64.0_DBL * P**3) +                              &
     &               Q**3 * (1104.0_DBL - 992.0_DBL * P +               &
     &                 376.0_DBL * P**2 - 72.0_DBL * P**3 +             &
     &                 5.0_DBL * P**4) /                                &
     &                 (512.0_DBL * P**4)
        END IF
!
!       calculate rotation increment
! 
        ROT_INC = DELTA
        FORALL (I = 1 : 3, J = 1 : 3)
        ROT_INC(I,J) = DELTA(I,J) * COS_THETA +                         &
     &                 COS_COEF * ALPHA(I) * ALPHA(J) -                 &
     &                 SIN_COEF * SUM(EPSI(I,J,:) * ALPHA(:))                  
        END FORALL
!
        END ASSOCIATE
!
        RETURN
      END SUBROUTINE STRAIN_INC_LRG_DEF
!
!=======================================================================
!
      END MODULE IP_STRAIN_INC_M
