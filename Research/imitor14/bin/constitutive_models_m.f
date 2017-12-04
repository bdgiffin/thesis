      MODULE CONSTITUTIVE_MODELS_M
!
!     This module contains subroutines that update the material state
!     for all material types supported by the program.  They also
!     return the tangent modulus for the materials.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE MATRIX_M
      IMPLICIT NONE
!
!=======================================================================
!     Module data.  MAT_SZ(2,.) contains the number of properties (1)
!     and the number of state variables (including stress components)
!     (2) for the argument material-type ID.
!=======================================================================
!
      INTEGER :: MAT_SZ(2,2) = RESHAPE( (/ 3, 6,   4, 6 /),             &
     &           SHAPE = (/ 2, 2 /) )
!
!=======================================================================
      CONTAINS
!=======================================================================
!     Isotropic, isothermal linear elasticity constitutive subroutine.
!=======================================================================
!
      SUBROUTINE CM_1 (PROPS, STATE_0, STRN_INC, STATE_1, TAN_MOD)
!
        REAL (DBL), INTENT (IN) :: PROPS(3), STATE_0(6), STRN_INC(6)
        REAL (DBL), INTENT (OUT) :: STATE_1(6) 
        REAL (DBL), INTENT (OUT), OPTIONAL :: TAN_MOD(6,6)
!
!       declare local variables
!
        REAL (DBL) :: BULK, SHEAR, TRM_1, TRM_2, TRM_3
        INTEGER :: I, J
!
!-----------------------------------------------------------------------
!
        BULK = PROPS(2) / (3.0_DBL - 6.0_DBL * PROPS(3))
        SHEAR = PROPS(2) / (2.0_DBL + 2.0_DBL * PROPS(3))
!
        TRM_1 = SHEAR / 3.0_DBL
        TRM_2 = BULK + 4.0_DBL * TRM_1
        TRM_3 = BULK - 2.0_DBL * TRM_1
        TAN_MOD = 0.0_DBL
        FORALL (I = 1 : 3, J = 1 : 3) TAN_MOD(I,J) = TRM_3
        FORALL (I = 1 : 3) TAN_MOD(I,I) = TRM_2
        FORALL (I = 4 : 6) TAN_MOD(I,I) = SHEAR
!
        FORALL (I = 1 : 3) STATE_1(I) = STATE_0(I) +                    &
     &                             SUM(TAN_MOD(I,1:3) * STRN_INC(1:3))
        FORALL (I = 4 : 6) STATE_1(I) = STATE_0(I) +                    &
     &                             2.0_DBL * TAN_MOD(I,I) * STRN_INC(I)
!
        RETURN
      END SUBROUTINE CM_1
!
!=======================================================================
!
      SUBROUTINE CM_2 (PROPS, F0, F1, STRN_INC, STATE_1, TAN_MOD)
!
        REAL (DBL), INTENT (IN) :: PROPS(4), F0(3,3), STRN_INC(6)
        REAL (DBL), INTENT (OUT) :: STATE_1(6), F1(3,3)
        REAL (DBL), INTENT (OUT), OPTIONAL :: TAN_MOD(6,6)
!
!       declare local variables
!
        INTEGER :: I, J, K, L, IJ(3,3)
        REAL (DBL) :: C1, C2, D, I1, I2
        REAL (DBL) :: UHAT(6), DSQ(6), DCUB(6), DETF, B(6), BSQ(6)
        REAL (DBL) :: ZERO, HALF, ONE, TWO, THREE, FIVE, SEVEN
        REAL (DBL) :: THIRD, SIXTH
        REAL (DBL) :: TRM1, TRM2, TRM3, TRM4
        REAL (DBL), ALLOCATABLE :: DU_DD(:,:,:,:), DF_DU(:,:,:,:),      &
     &                             DT_DF(:,:,:,:), DJ_DF(:,:),          &
     &                             DB_DF(:,:,:,:), DT_DJ(:,:),          &
     &                             DT_DB(:,:,:,:), DF_DD(:,:,:,:),      &
     &                             FINV(:,:)
!
!-----------------------------------------------------------------------
! 
        ZERO = 0.0_DBL
        HALF = 0.5_DBL
        ONE = 1.0_DBL
        TWO = 2.0_DBL
        THREE = 3.0_DBL
        FIVE = 5.0_DBL
        SEVEN = 7.0_DBL
        THIRD = ONE / THREE
        SIXTH = ONE / (TWO * THREE)
!         
!       set a map from tensor indices to column-vector indices
!
        IJ(1:3,1) = (/ 1, 6, 5 /)
        IJ(1:3,2) = (/ 6, 2, 4 /)
        IJ(1:3,3) = (/ 5, 4, 3 /)
!
!       material properties
! 
        D = PROPS(2)
        C1 = PROPS(3)
        C2 = PROPS(4)
! 
!       calculate stretch rate quantities
!  
        DSQ = ZERO
        DO I = 1, 3
          DO J = 1, I
            DSQ(IJ(I,J)) = SUM(STRN_INC(IJ(I,:)) * STRN_INC(IJ(:,J)))
          END DO
        END DO
! 
        DCUB = ZERO
        DO I = 1, 3
          DO J = 1, I
            DCUB(IJ(I,J)) = SUM(DSQ(IJ(I,:)) * STRN_INC(IJ(:,J)))
          END DO
        END DO
! 
!       form Uhat, increment without rotation
! 
        UHAT = STRN_INC + HALF * DSQ + SIXTH * DCUB
        DO I = 1, 3
          UHAT(I) = UHAT(I) + ONE
        END DO
!         
!       update deformation gradient without rotation
! 
        F1 = ZERO
        DO I = 1, 3
          DO J = 1, 3
            F1(I,J) = SUM(UHAT(IJ(I,:)) * F0(:,J))
          END DO
        END DO
        DETF = DETM (F1, 3)     
!         
!       compute B = F*F^t
!         
        B = ZERO
        DO I = 1, 3
          DO J = 1, I
            B(IJ(I,J)) = SUM(F1(I,:) * F1(J,:))
          END DO
        END DO
!         
        BSQ = ZERO
        DO I = 1, 3
          DO J = 1, I
            BSQ(IJ(I,J)) = SUM(B(IJ(I,:)) * B(IJ(:,J)))
          END DO
        END DO   
!         
!       compute invariants I1, I2 of B
! 
        I1 = B(1) + B(2) + B(3)
        I2 = HALF * (I1**2 - (BSQ(1) + BSQ(2) + BSQ(3)))
!
!       compute updated unrotated stress        
! 
        TRM1 = TWO * C1 * DETF**(-FIVE * THIRD)
        TRM2 = TWO * C2 * DETF**(-SEVEN * THIRD)
        STATE_1 = ZERO
        DO I = 1, 3
          STATE_1(I) = STATE_1(I) - TRM1 * THIRD * I1 -                 &
     &                            TRM2 * TWO * THIRD * I2 +             &
     &                            TWO * D * (DETF - ONE)
          DO J = 1, I
            STATE_1(IJ(I,J)) = STATE_1(IJ(I,J)) + TRM1 * B(IJ(I,J)) +   &
     &                         TRM2 * (I1 * B(IJ(I,J)) - BSQ(IJ(I,J)))
          END DO
        END DO
! 
!       if tangent modulus is needed, calculate it too
! 
        IF (PRESENT(TAN_MOD)) THEN
          ALLOCATE (DU_DD(3,3,3,3), DF_DU(3,3,3,3), DT_DF(3,3,3,3),     &
     &              DJ_DF(3,3), DB_DF(3,3,3,3), DT_DJ(3,3),             &
     &              DT_DB(3,3,3,3), DF_DD(3,3,3,3), FINV(3,3))
!      
          DU_DD = ZERO
          DO I = 1, 3
            DO J = 1, 3
              DU_DD(I,J,I,J) = DU_DD(I,J,I,J) + HALF
              DU_DD(I,J,J,I) = DU_DD(I,J,J,I) + HALF
              DO K = 1, 3
                DU_DD(I,J,I,K) = DU_DD(I,J,I,K) + HALF *                &
     &                           (HALF * STRN_INC(IJ(K,J)) +            &
     &                           SIXTH * DSQ(IJ(K,J)))
                DU_DD(I,J,K,I) = DU_DD(I,J,K,I) + HALF *                &
     &                           (HALF * STRN_INC(IJ(K,J)) +            &
     &                           SIXTH * DSQ(IJ(K,J)))     
                DU_DD(I,J,K,J) = DU_DD(I,J,K,J) + HALF *                &
     &                           (HALF * STRN_INC(IJ(I,K)) +            &
     &                           SIXTH * DSQ(IJ(I,K)))
                DU_DD(I,J,J,K) = DU_DD(I,J,J,K) + HALF *                &
     &                           (HALF * STRN_INC(IJ(I,K)) +            &
     &                           SIXTH * DSQ(IJ(I,K)))     
                DO L = 1, 3
                  DU_DD(I,J,K,L) = DU_DD(I,J,K,L) + HALF *              &
     &                             (SIXTH * STRN_INC(IJ(I,K)) *         &
     &                             STRN_INC(IJ(L,J)) +                  &
     &                             SIXTH * STRN_INC(IJ(I,L)) *          &
     &                             STRN_INC(IJ(K,J)))
                END DO
              END DO
            END DO
          END DO
!           
          DF_DU = ZERO
          DO I = 1, 3
            DO J = 1, 3
              DO K = 1, 3
                  DF_DU(I,J,I,K) = DF_DU(I,J,I,K) + HALF * F0(K,J)
                  DF_DU(I,J,K,I) = DF_DU(I,J,K,I) + HALF * F0(K,J)                  
              END DO
            END DO
          END DO              
! 
          DJ_DF = ZERO
          CALL INV3 (F1, DETF**(-1), 3, FINV)
          DJ_DF = DETF * TRANSPOSE(FINV)
!           
          DB_DF = ZERO
          DO I = 1, 3
            DO J = 1, 3
              DO K = 1, 3
                DB_DF(I,J,I,K) = DB_DF(I,J,I,K) + F1(J,K)
                DB_DF(I,J,J,K) = DB_DF(I,J,J,K) + F1(I,K)
              END DO
            END DO
          END DO
!           
          TRM3 = -10.0_DBL * THIRD * C1 * DETF**(-8.0_DBL * THIRD)
          TRM4 = -14.0_DBL * THIRD * C2 * DETF**(-10.0_DBL * THIRD)
          DT_DJ = ZERO
          DO I = 1, 3
            DT_DJ(I,I) = DT_DJ(I,I) - TRM3 * THIRD * I1 -               &
     &                   TRM4 * TWO * THIRD * I2 + TWO * D
            DO J = 1, 3
              DT_DJ(I,J) = DT_DJ(I,J) + TRM3 * B(IJ(I,J)) +             &
     &                     TRM4 * (I1 * B(IJ(I,J)) - BSQ(IJ(I,J)))
            END DO
          END DO
!           
          DT_DB = ZERO
          DO I = 1, 3
            DO J = 1, 3
              DT_DB(I,J,I,J) = DT_DB(I,J,I,J) + HALF *                  &
     &                         (TRM1 + TRM2 * I1)
              DT_DB(I,J,J,I) = DT_DB(I,J,J,I) + HALF *                  &
     &                         (TRM1 + TRM2 * I1)     
              DT_DB(I,I,J,J) = DT_DB(I,I,J,J) - TRM1 * THIRD -          &
     &                         TRM2 * TWO * THIRD * I1
              DO K = 1, 3
                DT_DB(I,J,K,K) = DT_DB(I,J,K,K) + TRM2 * B(IJ(I,J))
                DT_DB(I,J,I,K) = DT_DB(I,J,I,K) - HALF *                &
     &                           TRM2 * B(IJ(K,J))
                DT_DB(I,J,K,I) = DT_DB(I,J,K,I) - HALF *                &
     &                           TRM2 * B(IJ(K,J))                
                DT_DB(I,J,K,J) = DT_DB(I,J,K,J) - HALF *                &
     &                           TRM2 * B(IJ(I,K))
                DT_DB(I,J,J,K) = DT_DB(I,J,J,K) - HALF *                &
     &                           TRM2 * B(IJ(I,K))     
                DT_DB(I,I,J,K) = DT_DB(I,I,J,K) +                       &
     &                           TRM2 * TWO * THIRD * B(IJ(K,J))
              END DO
            END DO
          END DO
!           
          DT_DF = ZERO
          DO I = 1, 3
            DO J = 1, 3
              DO K = 1, 3
                DO L = 1, 3
                  DT_DF(I,J,K,L) = DT_DJ(I,J) * DJ_DF(K,L) +            &
     &                             MAT_DOT(DT_DB(I,J,:,:),              &
     &                                     DB_DF(:,:,K,L))
                END DO
              END DO
            END DO
          END DO
! 
          DF_DD = ZERO
          DO I = 1, 3
            DO J = 1, 3
              DO K = 1, 3
                DO L = 1, 3
                  DF_DD(I,J,K,L) = MAT_DOT(DF_DU(I,J,:,:),              &
     &                                     DU_DD(:,:,K,L))
                END DO
              END DO
            END DO
          END DO      
! 
          TAN_MOD = ZERO
          DO I = 1, 3
            DO J = 1, I
              DO K = 1, 3
                DO L = 1, K
                  TAN_MOD(IJ(I,J),IJ(K,L)) = MAT_DOT(DT_DF(I,J,:,:),    &
     &                                               DF_DD(:,:,K,L))
                END DO
              END DO
            END DO
          END DO  
! 
          DEALLOCATE (DU_DD, DF_DU, DT_DF, DJ_DF, DB_DF, DT_DJ, DT_DB,  &
     &                DF_DD, FINV)
!      
        END IF
! 
        RETURN
      END SUBROUTINE CM_2
!
!=======================================================================
!
      END MODULE CONSTITUTIVE_MODELS_M
