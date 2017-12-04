      MODULE SHAPE_FNS_M
!
!     This module provides shape-function and SF-gradient data for
!     each element type.  Note that the SF-gradient values computed
!     herein are gradients WRT parent-element coordinates.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
!
!=======================================================================
      CONTAINS
!=======================================================================
!     Module procedures:  one for each element type.  Each routine
!     is named SF_ELEM_element-type-id.
!=======================================================================
!
      SUBROUTINE SF_ELEM_1 (NIP, NND, SF, SF_GRAD, PARENT_WTS)
!
        INTEGER, INTENT (OUT) :: NIP, NND
        REAL (DBL), ALLOCATABLE, INTENT (OUT) :: SF(:,:),               &
     &                              SF_GRAD(:,:,:), PARENT_WTS(:)
!
!       declare local variables
!
        INTEGER :: J, K
        REAL (DBL) :: XIP(2,4), XND(2,4), SQ
!
!-----------------------------------------------------------------------
!
        ALLOCATE (SF(4,4), SF_GRAD(2,4,4), PARENT_WTS(4))
!
        NIP = 4
        NND = 4
        SQ = 1.0_DBL / SQRT(3.0_DBL)
        XND(1,1:4) = (/ 1.0_DBL, -1.0_DBL, -1.0_DBL, 1.0_DBL /)
        XND(2,1:4) = (/ 1.0_DBL, 1.0_DBL, -1.0_DBL, -1.0_DBL /)
        XIP = SQ * XND
        PARENT_WTS(1:4) = 1.0_DBL
        FORALL (J = 1 : NND, K = 1 : NIP)
          SF(J,K) = 0.25_DBL * (1.0_DBL + XND(1,J) * XIP(1,K)) *        &
     &                         (1.0_DBL + XND(2,J) * XIP(2,K))
          SF_GRAD(1,J,K) = 0.25_DBL * XND(1,J) *                        &
     &                (1.0_DBL + XND(2,J) * XIP(2,K))
          SF_GRAD(2,J,K) = 0.25_DBL * XND(2,J) *                        &
     &                (1.0_DBL + XND(1,J) * XIP(1,K))
        END FORALL
!
        RETURN
      END SUBROUTINE SF_ELEM_1
!
!=======================================================================
!
      SUBROUTINE SF_ELEM_101 (NIP, NND, SF, SF_GRAD, PARENT_WTS)
!
        INTEGER, INTENT (OUT) :: NIP, NND
        REAL (DBL), ALLOCATABLE, INTENT (OUT) :: SF(:,:),               &
     &                              SF_GRAD(:,:,:), PARENT_WTS(:)
!
!       declare local variables
!
        INTEGER :: J, K
        REAL (DBL) :: XIP(1,2), XND(1,2), SQ
!
!-----------------------------------------------------------------------
!
        ALLOCATE (SF(2,2), SF_GRAD(1,2,2), PARENT_WTS(2))
!
        NIP = 2
        NND = 2
        PARENT_WTS(1:2) = 1.0_DBL
        SQ = 1.0_DBL / SQRT(3.0_DBL)
        XND(1,1:2) = (/ -1.0_DBL, 1.0_DBL /)
        XIP = SQ * XND
        FORALL (J = 1 : NND, K = 1 : NIP)
          SF(J,K) = 0.5_DBL * (1.0_DBL + XND(1,J) * XIP(1,K))
          SF_GRAD(1,J,K) = 0.5_DBL * XND(1,J)
        END FORALL
!
        RETURN
      END SUBROUTINE SF_ELEM_101
!
!=======================================================================
!
      SUBROUTINE SF_ELEM_2 (NIP, NND, SF, SF_GRAD, PARENT_WTS)
!
        INTEGER, INTENT (OUT) :: NIP, NND
        REAL (DBL), ALLOCATABLE, INTENT (OUT) :: SF(:,:),               &
     &                              SF_GRAD(:,:,:), PARENT_WTS(:)
!
!       declare local variables
!
        INTEGER :: J, K
        REAL (DBL) :: XIP(3,8), XND(3,8), SQ
!
!-----------------------------------------------------------------------
!
        ALLOCATE (SF(8,8), SF_GRAD(3,8,8), PARENT_WTS(8))
!
        NIP = 8
        NND = 8
        SQ = 1.0_DBL / SQRT(3.0_DBL)
        XND(1,1:8) = (/  1.0_DBL,   1.0_DBL,  -1.0_DBL,  -1.0_DBL,      &
     &                   1.0_DBL,   1.0_DBL,  -1.0_DBL,  -1.0_DBL /)
        XND(2,1:8) = (/  1.0_DBL,  -1.0_DBL,  -1.0_DBL,   1.0_DBL,      &
     &                   1.0_DBL,  -1.0_DBL,  -1.0_DBL,   1.0_DBL /)
        XND(3,1:8) = (/  1.0_DBL,   1.0_DBL,   1.0_DBL,   1.0_DBL,      &
     &                  -1.0_DBL,  -1.0_DBL,  -1.0_DBL,  -1.0_DBL /)        
        XIP = SQ * XND
        PARENT_WTS(1:8) = 1.0_DBL
        FORALL (J = 1 : NND, K = 1 : NIP)
          SF(J,K) = 0.125_DBL * (1.0_DBL + XND(1,J) * XIP(1,K)) *       &
     &                         (1.0_DBL + XND(2,J) * XIP(2,K)) *        &
     &                         (1.0_DBL + XND(3,J) * XIP(3,K))
          SF_GRAD(1,J,K) = 0.125_DBL * XND(1,J) *                       &
     &                (1.0_DBL + XND(2,J) * XIP(2,K)) *                 &
     &                (1.0_DBL + XND(3,J) * XIP(3,K))
          SF_GRAD(2,J,K) = 0.125_DBL * XND(2,J) *                       &
     &                (1.0_DBL + XND(1,J) * XIP(1,K)) *                 &
     &                (1.0_DBL + XND(3,J) * XIP(3,K))
          SF_GRAD(3,J,K) = 0.125_DBL * XND(3,J) *                       &
     &                (1.0_DBL + XND(1,J) * XIP(1,K)) *                 &
     &                (1.0_DBL + XND(2,J) * XIP(2,K))     
        END FORALL
!
        RETURN
      END SUBROUTINE SF_ELEM_2
!
!=======================================================================
!
      SUBROUTINE SF_ELEM_102 (NIP, NND, SF, SF_GRAD, PARENT_WTS)
!
        INTEGER, INTENT (OUT) :: NIP, NND
        REAL (DBL), ALLOCATABLE, INTENT (OUT) :: SF(:,:),               &
     &                              SF_GRAD(:,:,:), PARENT_WTS(:)
!
!       declare local variables
!
        INTEGER :: J, K
        REAL (DBL) :: XIP(2,4), XND(2,4), SQ
!
!-----------------------------------------------------------------------
!
        ALLOCATE (SF(4,4), SF_GRAD(2,4,4), PARENT_WTS(4))
!
        NIP = 4
        NND = 4
        SQ = 1.0_DBL / SQRT(3.0_DBL)
        XND(1,1:4) = (/ 1.0_DBL, -1.0_DBL, -1.0_DBL, 1.0_DBL /)
        XND(2,1:4) = (/ 1.0_DBL, 1.0_DBL, -1.0_DBL, -1.0_DBL /)
        XIP = SQ * XND
        PARENT_WTS(1:4) = 1.0_DBL
        FORALL (J = 1 : NND, K = 1 : NIP)
          SF(J,K) = 0.25_DBL * (1.0_DBL + XND(1,J) * XIP(1,K)) *        &
     &                         (1.0_DBL + XND(2,J) * XIP(2,K))
          SF_GRAD(1,J,K) = 0.25_DBL * XND(1,J) *                        &
     &                (1.0_DBL + XND(2,J) * XIP(2,K))
          SF_GRAD(2,J,K) = 0.25_DBL * XND(2,J) *                        &
     &                (1.0_DBL + XND(1,J) * XIP(1,K))
        END FORALL
!
        RETURN
      END SUBROUTINE SF_ELEM_102
!=======================================================================
!
      SUBROUTINE SF_ELEM_10 (NIP, NND, SF, SF_GRAD, PARENT_WTS)
!
        INTEGER, INTENT (OUT) :: NIP, NND
        REAL (DBL), ALLOCATABLE, INTENT (OUT) :: SF(:,:),               &
     &                              SF_GRAD(:,:,:), PARENT_WTS(:)
!
!       declare local variables
!
        INTEGER :: J, K
        REAL (DBL) :: XIP(2,9), XND(2,9), SQ
!
!-----------------------------------------------------------------------
!
        ALLOCATE (SF(9,9), SF_GRAD(2,9,9), PARENT_WTS(9))
!
        NIP = 9
        NND = 9
        SQ = SQRT(0.6_DBL)
        XND(1,1:9) = (/ 1.0_DBL, -1.0_DBL, -1.0_DBL, 1.0_DBL, 1.0_DBL,  &
     &                  0.0_DBL, -1.0_DBL, 0.0_DBL,  0.0_DBL /)
        XND(2,1:9) = (/ 1.0_DBL, 1.0_DBL, -1.0_DBL, -1.0_DBL, 0.0_DBL,  &
     &                  1.0_DBL, 0.0_DBL, -1.0_DBL,  0.0_DBL /)  
        XIP = SQ * XND     
        PARENT_WTS(1:9) = (/ 25.0_DBL / 81.0_DBL, 25.0_DBL / 81.0_DBL,  &
     &                       25.0_DBL / 81.0_DBL, 25.0_DBL / 81.0_DBL,  &
     &                       40.0_DBL / 81.0_DBL, 40.0_DBL / 81.0_DBL,  &
     &                       40.0_DBL / 81.0_DBL, 40.0_DBL / 81.0_DBL,  &
     &                       64.0_DBL / 81.0_DBL /) 
        FORALL (J = 1 : 4, K = 1 : NIP)
          SF(J,K) = 0.25_DBL * XIP(1,K) * XIP(2,K) *                    &
     &              (XIP(1,K) + XND(1,J)) * (XIP(2,K) + XND(2,J))
          SF_GRAD(1,J,K) = 0.25_DBL * XIP(2,K) * (XIP(2,K) + XND(2,J))  &
     &                     * (2.0_DBL * XIP(1,K) + XND(1,J))
          SF_GRAD(2,J,K) = 0.25_DBL * XIP(1,K) * (XIP(1,K) + XND(1,J))  &
     &                     * (2.0_DBL * XIP(2,K) + XND(2,J))
        END FORALL     
        FORALL (J = 5:7:2, K = 1 : NIP)
          SF(J,K) = -0.5_DBL * XIP(1,K) * (XIP(1,K) + XND(1,J)) *       &
     &              (XIP(2,K)**2 - 1.0_DBL)
          SF_GRAD(1,J,K) = -0.5_DBL * (2.0_DBL * XIP(1,K) + XND(1,J)) * &
     &                     (XIP(2,K)**2 - 1.0_DBL)
          SF_GRAD(2,J,K) = -0.5_DBL * (XIP(1,K)**2 + XIP(1,K) *         &
     &                     XND(1,J)) * (2.0_DBL * XIP(2,K))  
        END FORALL          
        FORALL (J = 6:8:2, K = 1 : NIP)
          SF(J,K) = -0.5_DBL * XIP(2,K) * (XIP(1,K)**2 - 1.0_DBL) *     &
     &              (XIP(2,K) + XND(2,J))
          SF_GRAD(1,J,K) = -0.5_DBL * (XIP(2,K)**2 + XIP(2,K) *         &
     &                     XND(2,J)) * (2.0_DBL * XIP(1,K))
          SF_GRAD(2,J,K) = -0.5_DBL * (2.0_DBL * XIP(2,K) + XND(2,J))   &
     &                     * (XIP(1,K)**2 - 1.0_DBL)  
        END FORALL  
        FORALL (J = 9:9, K = 1 : NIP)
          SF(J,K) = (XIP(1,K)**2 - 1.0_DBL) * (XIP(2,K)**2 - 1.0_DBL)
          SF_GRAD(1,J,K) = 2.0_DBL * XIP(1,K) * (XIP(2,K)**2 - 1.0_DBL)
          SF_GRAD(2,J,K) = 2.0_DBL * XIP(2,K) * (XIP(1,K)**2 - 1.0_DBL)   
        END FORALL
!
        RETURN
      END SUBROUTINE SF_ELEM_10
!
!=======================================================================
!
      SUBROUTINE SF_ELEM_110 (NIP, NND, SF, SF_GRAD, PARENT_WTS)
!
        INTEGER, INTENT (OUT) :: NIP, NND
        REAL (DBL), ALLOCATABLE, INTENT (OUT) :: SF(:,:),               &
     &                              SF_GRAD(:,:,:), PARENT_WTS(:)
!
!       declare local variables
!
        INTEGER :: J, K
        REAL (DBL) :: XIP(1,3), XND(1,3), SQ
!
!-----------------------------------------------------------------------
!
        ALLOCATE (SF(3,3), SF_GRAD(1,3,3), PARENT_WTS(3))
!
        NIP = 3
        NND = 3
        PARENT_WTS(1:3) = (/ 5.0_DBL / 9.0_DBL, 8.0_DBL / 9.0_DBL,      &
     &                       5.0_DBL / 9.0_DBL /) 
        SQ = SQRT(0.6_DBL)
        XND(1,1:3) = (/ -1.0_DBL, 0.0_DBL, 1.0_DBL /)
        XIP = SQ * XND      
        FORALL (J = 1:3:2, K = 1 : NIP)
          SF(J,K) = 0.5_DBL * XIP(1,K) * (XIP(1,K) + XND(1,J))
          SF_GRAD(1,J,K) = 0.5_DBL * (2.0_DBL * XIP(1,K) + XND(1,J))
        END FORALL
        FORALL (J = 2:2, K = 1 : NIP)
          SF(J,K) = 1.0_DBL - XIP(1,K)**2
          SF_GRAD(1,J,K) = -2.0_DBL * XIP(1,K)
        END FORALL        
!
        RETURN
      END SUBROUTINE SF_ELEM_110
!
!=======================================================================
!
      END MODULE SHAPE_FNS_M
