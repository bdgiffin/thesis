       MODULE POST_PROC_M
!
!     This module provides functions for post-processing operations
!     -BDG
!
!=======================================================================
!
      USE KINDS_M
      USE SIM_MESH_M
      USE MISC_TYPES_M
      IMPLICIT NONE
!
!=======================================================================
      CONTAINS    
!=======================================================================
!     visualization subroutines
!=======================================================================
!
      SUBROUTINE OUTPUT_VTK (SM)
!
!       This routine writes requested output to the output .vtk file.
!
        CLASS (SIM_MESH), INTENT (INOUT) :: SM
!
!       declare local variables
!
        INTEGER :: OP_UNIT, I, J, NC, NDOF, NCELLS, CELLSIZE, IJ(8)
        INTEGER, ALLOCATABLE :: NELEMS(:)
        REAL (DBL) :: XYZ(3), U_TOT(20)
        REAL (DBL), ALLOCATABLE :: NODE_VARS(:,:)
        LOGICAL :: FLAG
        CHARACTER (LEN = 50) :: ARG
!
!-----------------------------------------------------------------------
!
!       detect vtk output flag in command line
!
        FLAG = .FALSE.
        DO J = 1, COMMAND_ARGUMENT_COUNT()
          CALL GET_COMMAND_ARGUMENT(J, ARG)
          SELECT CASE (ARG)
          CASE ('-vtk', '-VTK')
            FLAG = .TRUE.
          CASE DEFAULT
          END SELECT
        END DO
!
        ASSOCIATE (STEP_ID => SM%SOLN_STATUS%STEP_NUM)
!
!       if no output was requested, return
!
        IF (SM%STEPS(STEP_ID)%PRINT_CODE .EQ. 0) THEN
          RETURN
        END IF
        IF (FLAG .EQV. .FALSE.) THEN
          RETURN
        END IF
!
!       build the node-elem inverse map if this is the first step
!       (NOTE: Currently, the ELEM_IDS are allocated here, but aren't
!       necessarily deallocated at any point. This could potentially
!       lead to memory leaks. Ideally, this would be fixed by having
!       the nodes themselves explicitly handle the deallocation of
!       the ELEM_IDS)
!
        IF (STEP_ID .EQ. 1) THEN
          ALLOCATE (NELEMS(SIZE(SM%NODES)))
          NELEMS = 0
          DO J = 1, SIZE(SM%ELEMS)
            SELECT TYPE (DAT => SM%ELEMS(J)%LM_DAT)
            TYPE IS (ELEM_DAT_CONTINUUM)
              DO I = 1, SIZE(SM%ELEMS(J)%NODE_IDS(:))
                NELEMS(SM%ELEMS(J)%NODE_IDS(I)) =                       &
     &          NELEMS(SM%ELEMS(J)%NODE_IDS(I)) + 1
              END DO
            END SELECT
          END DO
!
          DO I = 1, SIZE(SM%NODES)
            IF (.NOT. ALLOCATED(SM%NODES(I)%ELEM_IDS)) THEN
              ALLOCATE (SM%NODES(I)%ELEM_IDS(NELEMS(I)))
            END IF
          END DO
!
          NELEMS = 0
          DO J = 1, SIZE(SM%ELEMS)
            SELECT TYPE (DAT => SM%ELEMS(J)%LM_DAT)
            TYPE IS (ELEM_DAT_CONTINUUM)
              DO I = 1, SIZE(SM%ELEMS(J)%NODE_IDS(:))
                NELEMS(SM%ELEMS(J)%NODE_IDS(I)) =                       &
     &          NELEMS(SM%ELEMS(J)%NODE_IDS(I)) + 1
                SM%NODES(SM%ELEMS(J)%NODE_IDS(I))%ELEM_IDS(             &
     &          NELEMS(SM%ELEMS(J)%NODE_IDS(I))) = J
              END DO
            END SELECT
          END DO
          STEP_ID = 0
          DEALLOCATE (NELEMS)
        END IF
!
!       open the output .vtk file for the current step
!
  199   CALL OPEN_VTK_FILE (SM%FILE_OBJ, OP_UNIT, STEP_ID)
!
!       write the basic vtk (unstructured grid) header info
!
        WRITE (OP_UNIT, 200)
  200   FORMAT ('# vtk DataFile Version 3.0')
        WRITE (OP_UNIT, 201)
  201   FORMAT ('vtk output') 
        WRITE (OP_UNIT, 202)
  202   FORMAT ('ASCII') 
        WRITE (OP_UNIT, 203)
  203   FORMAT ('DATASET UNSTRUCTURED_GRID')
!
!       write the nodal coordinate data
!
        WRITE (OP_UNIT, 204) SIZE(SM%NODES)
  204   FORMAT ('POINTS ', I5, ' float')
        NC = SM%SIM_VARS%NCOORD
        DO I = 1, SIZE(SM%NODES)
          NDOF = SM%NODES(I)%GET_NUM_DOF()
          CALL SM%NODES(I)%GET_NODAL_VALS (XYZ = XYZ(1:NC))
          IF (NC .EQ. 2) THEN
            XYZ(3) = 0.0_DBL
          END IF
          WRITE (OP_UNIT, 205) XYZ(1:3)
  205     FORMAT (3(E13.6, 1X))
        END DO
!
!       sum cell data info
!
        NCELLS = 0
        CELLSIZE = 0
        DO I = 1, SIZE(SM%ELEMS)
          SELECT TYPE (DAT => SM%ELEMS(I)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            NCELLS = NCELLS + 1
            SELECT CASE (SM%ELEMS(I)%LM_TYPE)
            CASE (10)
              CELLSIZE = CELLSIZE + SIZE(SM%ELEMS(I)%NODE_IDS(:))            
            CASE DEFAULT
              CELLSIZE = CELLSIZE + 1 + SIZE(SM%ELEMS(I)%NODE_IDS(:))
            END SELECT
          END SELECT
        END DO
!
!       write element connectivity (cell) data
!
        WRITE (OP_UNIT, 206) NCELLS, CELLSIZE
!         
!       set a map from 9-node lagrange nodes to 8-node vtk element nodes
!
        IJ(:) = (/ 1, 2, 3, 4, 6, 7, 8, 5 /)
!         
  206   FORMAT ('CELLS ', I5, 1X, I5)
        DO I = 1, SIZE(SM%ELEMS)
          SELECT TYPE (DAT => SM%ELEMS(I)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            SELECT CASE (SM%ELEMS(I)%LM_TYPE)
            CASE (10)
              WRITE (OP_UNIT, 207) SIZE(SM%ELEMS(I)%NODE_IDS(:)) - 1,   &
     &                             SM%ELEMS(I)%NODE_IDS(IJ(1:8)) - 1              
            CASE DEFAULT
              WRITE (OP_UNIT, 207) SIZE(SM%ELEMS(I)%NODE_IDS(:)),       &
     &                             SM%ELEMS(I)%NODE_IDS(:)-1
  207       FORMAT (I5, 27(I5, 1X))
            END SELECT
          END SELECT
        END DO
!
!       write cell types
!
        WRITE (OP_UNIT, 208) NCELLS
  208   FORMAT ('CELL_TYPES ', I5)
        DO I = 1, SIZE(SM%ELEMS)
          SELECT TYPE (DAT => SM%ELEMS(I)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            SELECT CASE (SM%ELEMS(I)%LM_TYPE)
            CASE (1)
!
!             standard isoparametric 2D 4-node quad element (VTK_QUAD)
!
             WRITE (OP_UNIT, 209) 9
!             
            CASE (2)
!
!             standard isoparametric 3D 8-node hex element 
!             (VTK_HEXAHEDRON)
!
             WRITE (OP_UNIT, 209) 12
!             
            CASE (10)
!
!             standard biquadratic 2D 9-node Lagrange element 
!             (mapped to VTK_QUADRATIC_QUAD)
!
             WRITE (OP_UNIT, 209) 23
!             
            CASE DEFAULT
!
              PRINT *, 'continuum element type code ',                  &
     &                  SM%ELEMS(I)%LM_TYPE,                            &
     &                 ' is currently not supported'
              PRINT *, 'aborting'
              STOP 
!  
            END SELECT
  209        FORMAT (I2)
          END SELECT
        END DO
!
!       map element variables to nodal values
!
        ALLOCATE (NODE_VARS(8,SIZE(SM%NODES)))
        CALL VLMNOD (SM, NODE_VARS)
        IF (STEP_ID .EQ. 0) THEN
          NODE_VARS = 0.0_DBL
        END IF
!
!       write the nodal point data (displacement vectors)
!
        WRITE (OP_UNIT, 210) SIZE(SM%NODES)
  210   FORMAT ('POINT_DATA ', I5)
        WRITE (OP_UNIT, 211)
  211   FORMAT ('VECTORS displacement float')
        DO I = 1, SIZE(SM%NODES)
          NDOF = SM%NODES(I)%GET_NUM_DOF()
          CALL SM%NODES(I)%GET_NODAL_VALS (XYZ = XYZ(1:NC),             &
     &                                     U_TOT = U_TOT(1:NDOF))
          IF (NDOF .EQ. 2) THEN
            U_TOT(3) = 0.0_DBL
          END IF
          IF (STEP_ID .EQ. 0) THEN
            U_TOT = 0.0_DBL
          END IF
          WRITE (OP_UNIT, 212) U_TOT(1:3)
  212     FORMAT (3(E13.6, 1X))
        END DO
!
!       write the element variables (stresses)
!
        DO J = 1, 8
          IF (J .EQ. 1) THEN
            WRITE (OP_UNIT, 214)
          ELSE IF (J .EQ. 2) THEN
            WRITE (OP_UNIT, 215)
          ELSE IF (J .EQ. 3) THEN
            WRITE (OP_UNIT, 216)
          ELSE IF (J .EQ. 4) THEN
            WRITE (OP_UNIT, 217)
          ELSE IF (J .EQ. 5) THEN
            WRITE (OP_UNIT, 218)
          ELSE IF (J .EQ. 6) THEN
            WRITE (OP_UNIT, 219)
          ELSE IF (J .EQ. 7) THEN
            WRITE (OP_UNIT, 220)
          ELSE IF (J .EQ. 8) THEN
            WRITE (OP_UNIT, 221)            
          END IF
          WRITE (OP_UNIT, 222)
          DO I = 1, SIZE(SM%NODES)
            WRITE (OP_UNIT, 213) NODE_VARS(J,I)
          END DO    
        END DO
  213   FORMAT (E13.6)
  214   FORMAT ('SCALARS stress_11 float')
  215   FORMAT ('SCALARS stress_22 float')
  216   FORMAT ('SCALARS stress_33 float')
  217   FORMAT ('SCALARS stress_23 float')
  218   FORMAT ('SCALARS stress_13 float')
  219   FORMAT ('SCALARS stress_12 float')
  220   FORMAT ('SCALARS von_Mises float')
  221   FORMAT ('SCALARS hydr_stress float')  
  222   FORMAT ('LOOKUP_TABLE default')
!
!       deallocate the node variables for the current step
!
        DEALLOCATE (NODE_VARS)
!
!       close the output .vtk file
!
        CLOSE (UNIT = OP_UNIT)
!
!       loop back and output first step if step 0 was just ouput
!
        IF (STEP_ID .EQ. 0) THEN
          STEP_ID = 1
          GO TO 199
        END IF 
!
        END ASSOCIATE
!
        RETURN
      END SUBROUTINE OUTPUT_VTK
!
!=======================================================================
!
      SUBROUTINE VLMNOD (SM,VAREN)
!
!     This routine receives integration-point data on a finite element
!     mesh, and returns the corresponding node-point data.
!     12.18.00
!----------------------------------------------------------------------
!     input:     XY(NC,NND)        nodal coordinates
!                IXH(NLM,NN)       element connectivity
!                VARE(NLM,NEV,NIP) IP data
!
!     output:    VAREN(NEV,NND)    nodal values for element data
!
!     scratch:   NEV               number of variables per IP
!---------------------------------------------------------------------
!
      CLASS (SIM_MESH), INTENT (IN) :: SM
      REAL (DBL), INTENT (INOUT) :: VAREN(:,:)
!
!     initialize local variables
!     
      INTEGER :: ND, KN, NIP, KLM, LM, I, J, K, L, KNP, KK, NLOC, NA
      INTEGER :: IA, IB, IV
      INTEGER, ALLOCATABLE :: LMADJ(:), INP(:), IXHL(:,:), ITEMP(:)
      REAL (DBL) :: VMAX, VMIN
      REAL (DBL), ALLOCATABLE :: AM(:,:), ATA(:,:), B(:), VL(:), ATB(:)
!
!     initialize the output
!
      VAREN = 0.0_DBL
!
!     loop over nodes, and form the nodal value for each field
!
      DO 10 ND = 1, SIZE(SM%NODES)
!
!       Get a list of elements attached to the current node, and
!       a list of nodes associated with the patch of elements
!       connected to the current node.  Consider only elements that
!       are a significant fraction of the total area.
!
        KN = 0
        NIP = 0
        KLM = 0
        DO L = 1, SIZE(SM%NODES(ND)%ELEM_IDS)
          LM = SM%NODES(ND)%ELEM_IDS(L)
          SELECT TYPE (DAT => SM%ELEMS(LM)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            KLM = KLM + 1
            IF (KLM .EQ. 1) THEN
              ALLOCATE (LMADJ(KLM))
            ELSE
              ALLOCATE (ITEMP(SIZE(LMADJ)))
              ITEMP = LMADJ
              DEALLOCATE (LMADJ)
              ALLOCATE (LMADJ(KLM))
              LMADJ(1:(KLM - 1)) = ITEMP
              DEALLOCATE (ITEMP)
            END IF
            LMADJ(KLM) = LM
            NIP = NIP + SIZE(DAT%WTS)
            DO J = 1, SIZE(SM%ELEMS(LM)%NODE_IDS(:))
                KN = KN + 1
                IF (KN .EQ. 1) THEN
                  ALLOCATE (INP(KN))
                ELSE
                  ALLOCATE (ITEMP(SIZE(INP)))
                  ITEMP = INP
                  DEALLOCATE (INP)
                  ALLOCATE (INP(KN))
                  INP(1:(KN - 1)) = ITEMP
                  DEALLOCATE (ITEMP)
                END IF
                INP(KN) = SM%ELEMS(LM)%NODE_IDS(J)
            END DO
          END SELECT
        END DO
!
        CALL ISORT (INP)
        KK = 1
        DO J = 2, KN
          IF (INP(J) .GT. INP(KK)) THEN
            KK = KK + 1
            INP(KK) = INP(J)
          END IF
        END DO
        KNP = KK
        ALLOCATE (ITEMP(SIZE(INP)))
        ITEMP = INP
        DEALLOCATE (INP)
        ALLOCATE (INP(KNP))
        INP = ITEMP(1:KNP)
        DEALLOCATE (ITEMP)
!
!       get the position in the INP list of the current node
!
        DO I = 1, KNP
          IF (INP(I) .EQ. ND) NLOC = I
        END DO
!
!       get a map of position in the INP list for given local element
!       and local node number
!
        ALLOCATE (IXHL(SIZE(SM%ELEMS(LM)%NODE_IDS(:)),KLM))
        DO 16 L = 1, KLM
          LM = LMADJ(L)
          DO 18 J = 1, SIZE(SM%ELEMS(LM)%NODE_IDS(:))
            NA = SM%ELEMS(LM)%NODE_IDS(J)
            DO 20 I = 1, KNP
              IF (INP(I) .EQ. NA) THEN
                IXHL(J,L) = I
                GO TO 18
              END IF
   20       CONTINUE
   18     CONTINUE
   16   CONTINUE
!
!       form the coefficient matrices for the nodal values on the patch
!
        IA = 0
        DO L = 1, KLM
          LM = LMADJ(L)
          SELECT TYPE (DAT => SM%ELEMS(LM)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            DO J = 1, SIZE(SM%ELEMS(LM)%NODE_IDS(:))
              IA = IA + SIZE(DAT%WTS)
            END DO
          END SELECT
        END DO
        ALLOCATE (AM(IA,KNP))
        ALLOCATE (B(IA))
        IA = 0
        AM = 0.0_DBL
        DO L = 1, KLM
          LM = LMADJ(L)
          SELECT TYPE (DAT => SM%ELEMS(LM)%LM_DAT)
          TYPE IS (ELEM_DAT_CONTINUUM)
            DO J = 1, SIZE(SM%ELEMS(LM)%NODE_IDS(:))
              DO K = 1, SIZE(DAT%WTS)
                IA = IA + 1
                IB = IXHL(J, L)
                AM(IA, IB) = DAT%SF_VALS(J, K)
              END DO
            END DO
          END SELECT
        END DO
        ALLOCATE (ATA(KNP,KNP))
        ALLOCATE (ATB(KNP))
        ALLOCATE (VL(KNP))
        FORALL (I = 1 : KNP, J = 1 : KNP)
          ATA(I, J) = SUM(AM(:, I) * AM(:, J))
        END FORALL
!
!       calculate the nodal values for all element fields for the
!       current node
!
        DO 34 IV = 1, 8
!
!         form the B-vector for the current node and field, and also
!         get the max/min IP values on the patch
!
          I = 1
          IA = 0
          B = 0.0_DBL
          DO L = 1, KLM
            LM = LMADJ(L)
            SELECT TYPE (DAT => SM%ELEMS(LM)%LM_DAT)
            TYPE IS (ELEM_DAT_CONTINUUM)
              DO K = 1, SIZE(DAT%WTS)
                IA = IA + 1
                IF (IV .EQ. 7) THEN
!                 compute von Mises stress
                  B(IA) = SQRT(0.5_DBL * (                              &
     &              (DAT%IP_VARS_0(1, K) - DAT%IP_VARS_0(2, K))**2 +    &
     &              (DAT%IP_VARS_0(2, K) - DAT%IP_VARS_0(3, K))**2 +    &
     &              (DAT%IP_VARS_0(3, K) - DAT%IP_VARS_0(1, K))**2 +    &
     &    6.0_DBL * (DAT%IP_VARS_0(4, K)**2 +                           &
     &               DAT%IP_VARS_0(5, K)**2 +                           &
     &               DAT%IP_VARS_0(6, K)**2)))
                ELSE IF (IV .EQ. 8) THEN
                  B(IA) = -1.0_DBL / 3.0_DBL * (DAT%IP_VARS_0(1,K) +    &
     &                    DAT%IP_VARS_0(2,K) + DAT%IP_VARS_0(3,K))
                ELSE
!                 individual Cauchy stress components
                  B(IA) = DAT%IP_VARS_0(IV, K)
                END IF
                IF (I .EQ. 1) THEN
                  I = 0
                  VMAX = B(IA)
                  VMIN = B(IA)
                ELSE
                  VMAX = MAX(VMAX, B(IA))
                  VMIN = MIN(VMIN, B(IA))
                END IF
              END DO
            END SELECT
          END DO
          FORALL (I = 1 : KNP) ATB(I) = SUM(AM(:, I) * B(:))
!
!         Set the nodal value for the current field by least-squares
!         fit of a C_0 field to the IP field on the patch of elements
!         surrounding the node.  In this optimization, the nodal value
!         is constrained to lie within the max/min IP values on the
!         patch.
!
          CALL CONMIN (ATA, ATB, NLOC, VMIN, VMAX, VL)
          VAREN(IV,ND) = VL(NLOC)
   34   CONTINUE
!
        DEALLOCATE(LMADJ, INP, IXHL, VL, ATB, ATA, AM, B)
!
!       close loop over nodes
!
   10 CONTINUE
      RETURN
      END SUBROUTINE VLMNOD
!
!=======================================================================
!
      SUBROUTINE CONMIN (A, B, KC, TA, TB, X)
!
!     This routine returns the vector that minimizes the quadratic
!     function
!                     0.5 x.Ax - b.x
!
!     subject to the constraints  ta < x_k < tb.  Note that the
!     constraints only apply to one entry in the vector x.
!     5-7-02
!----------------------------------------------------------------------
!     input:      A(n,n)
!                 B(n)
!                 KC
!                 TA, TB
!
!     output:     X(n)
!
!     scratch:    AM(n-1,n-1)
!                 BM(n-1)
!                 Y(n-1)
!                 IC(n-1)
!                 N
!----------------------------------------------------------------------
!
      INTEGER, INTENT (IN) :: KC
      REAL (DBL), INTENT (IN) :: A(:,:), B(:), TA, TB
      REAL (DBL), INTENT (INOUT) :: X(:)
!
!     declare local variables
!
      INTEGER :: N, I, J, ICON
      INTEGER, ALLOCATABLE :: IC(:)
      REAL (DBL), ALLOCATABLE :: AM(:,:), BM(:), Y(:)
!
!     determine the size of the system
!
      N = SIZE(B)
      ALLOCATE (AM(N-1,N-1))
      ALLOCATE (BM(N-1))
      ALLOCATE (Y(N-1))
      ALLOCATE (IC(N-1))
!
!     minimize the quadratic without regard for the constraints
!
      CALL GAUSS (A, B, X)
!
!     check for violation of constraints; if found, then modify the
!     system of equations and re-solve
!
      IF (X(KC) .LT. TA) THEN
        ICON = 1
        X(KC) = TA
      ELSE IF (X(KC) .GT. TB) THEN
        ICON = 1
        X(KC) = TB
      ELSE
        ICON = 0
      END IF
!
      IF (ICON .EQ. 1) THEN
        FORALL (I = 1 : (KC - 1)) IC(I) = I
        FORALL (I = KC : (N - 1)) IC(I) = I + 1
        FORALL (I = 1 : (N - 1), J = 1 : (N - 1))
          AM(I,J) = A(IC(I),IC(J))
        END FORALL
        FORALL (I = 1 : (N - 1)) BM(I) = B(IC(I)) - A(IC(I),KC) * X(KC)
        CALL GAUSS (AM, BM, Y)
        FORALL (I = 1 : (N - 1)) X(IC(I)) = Y(I)
      END IF
!
      DEALLOCATE (IC, AM, BM, Y)
!
      RETURN
      END SUBROUTINE CONMIN
!
!=======================================================================
!
      SUBROUTINE GAUSS (A, B, X)
!
!     This routine returns the N x 1 solution vector X to the set of
!     linear equations AX = B.  The solution is obtained using Gaussian
!     elimination followed by back-substitution.
!     Partial pivoting is used prior to each elimination
!     cycle to ensure that the largest element in each column is used
!     to eliminate the nonzero entries below the diagonal in the column.
!-----------------------------------------------------------------------
!     input:    A(N,N)        coefficient matrix 
!               B(N)          right-hand-side vector
!
!     output:   X(N)          solution vector
!
!     scratch:  S(N)          scratch vector used in the solution process
!               ATEMP(N,N)    scratch matrix for temporary storage of 
!                             matrix A
!               BTEMP(N)      scratch vector for temporary storage of 
!                             vector B
!               N             dimension of the equation set
!-----------------------------------------------------------------------
!
      REAL (DBL), INTENT (IN) :: A(:,:), B(:)
      REAL (DBL), INTENT (INOUT) :: X(:)
!     
!     declare local variables
!
      INTEGER :: I, J, K, N, JPIV
      REAL (DBL), ALLOCATABLE :: ATEMP(:,:), BTEMP(:), S(:)
      REAL (DBL) :: PIV, SCALE, SUM
!
!     determine size of the system
!
      N = SIZE(B)
      ALLOCATE (ATEMP(N,N))
      ALLOCATE (BTEMP(N))
      ALLOCATE (S(N))
!
!     copy matrix A to temporary storage space ATEMP, initialize scratch
!     space S(.) and solution vector X(.)
!
      FORALL (I = 1 : N)
        S(I) = 0.0_DBL
        X(I) = 0.0_DBL
        BTEMP(I) = B(I)
      END FORALL
      FORALL (I = 1 : N, J = 1 : N) ATEMP(I,J) = A(I,J)
!
!     loop over columns: eliminate all nonzero entries in each column
!     below the diagonal
!
      DO 10 I = 1, N - 1
!
!       find the pivot (i.e. the biggest entry below the diagonal
!       in the current column)
!
        PIV = 0.0_DBL
        JPIV = 0
        DO 12 J = I, N
          IF (ABS(ATEMP(J,I)) .GT. PIV) THEN
            PIV = ABS(ATEMP(J,I))
            JPIV = J
          END IF
   12   CONTINUE
!
!       swap rows if necessary so that the pivot is on the diagonal
!
        IF (JPIV .NE. I) THEN
          DO 14 J = I, N
   14       S(J) = ATEMP(I,J)
          DO 16 J = I, N
            ATEMP(I,J) = ATEMP(JPIV,J)
   16       ATEMP(JPIV,J) = S(J)
          S(1) = BTEMP(I)
          BTEMP(I) = BTEMP(JPIV)
          BTEMP(JPIV) = S(1)
        END IF
!
!       loop over rows below row I
!
        DO 18 J = I + 1, N
!
!         perform elimination: add a multiple of row I to row J
!
          SCALE = -ATEMP(J,I) / ATEMP(I,I)
          DO 20 K = I + 1, N
   20       ATEMP(J,K) = ATEMP(J,K) + SCALE * ATEMP(I,K)
          BTEMP(J) = BTEMP(J) + SCALE * BTEMP(I)
          ATEMP(J,I) = 0.0_DBL
!
   18   CONTINUE
!
   10 CONTINUE
!
!     The matrix ATEMP is now in upper-diagonal form.  
!     Perform back-substitution.
!
      DO 22 I = N, 1, -1
!
!       form the sum of the coefficients of ATEMP times the
!       already-determined unknowns
!
        SUM = 0.0_DBL
        DO 24 J = I + 1, N
   24     SUM = SUM + ATEMP(I,J) * X(J)
!
!       solve for unknown number I
!
        X(I) = (BTEMP(I) - SUM) / ATEMP(I,I)
!
   22 CONTINUE
!
      DEALLOCATE (ATEMP, BTEMP, S)
!
      RETURN
      END SUBROUTINE GAUSS
!
!=======================================================================
!
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
!     declare dummy variables
!
      INTEGER :: LIST(:)
!
!     declare local variables
!
      INTEGER :: N, I, J, LST
!
!     end of declarations
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
!
!=======================================================================
!
      SUBROUTINE OPEN_VTK_FILE (FN_OBJ, F_UNIT, STEP_ID)
!
        CLASS (FILES), INTENT (IN) :: FN_OBJ
        INTEGER, INTENT (OUT) :: F_UNIT
        INTEGER, INTENT (IN) :: STEP_ID
!
!       declare local variables
!
        INTEGER :: IOS
        CHARACTER (LEN = 5) :: STEPN
!
!-----------------------------------------------------------------------
!
        WRITE (STEPN,500) STEP_ID
  500   FORMAT (I5.5)
        F_UNIT = FN_OBJ%N_UNIT + 1
        OPEN (UNIT = F_UNIT, ACCESS = 'SEQUENTIAL',                     &
     &        FILE = FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//STEPN//'.vtk',      &
     &        FORM = 'FORMATTED', STATUS = 'UNKNOWN', IOSTAT = IOS)
        IF (IOS .NE. 0) THEN
          PRINT *, 'error opening file ',                               &
     &            FN_OBJ%ROOT_NAME(1:FN_OBJ%LN)//'.vtk'
          PRINT *, 'aborting'
          STOP
        END IF
!
        RETURN
      END SUBROUTINE OPEN_VTK_FILE
!
!=======================================================================
!
      END MODULE POST_PROC_M
