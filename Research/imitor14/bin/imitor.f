      PROGRAM IMITOR
!
!     This is the main program for IMITOR, a finite element program
!     for linear and nonlinear mechanics/thermomechanics of
!     deformable continua.  It is intended as a teaching and research
!     tool, and was designed with modification and extension in mind.
!     IMITOR is written entirely in standard-conforming Fortran 2003,
!     using "object-oriented programming" features.  It was written
!     by Prof. Mark Rashid (UC Davis), with contributions by 
!     Mr. Steve Wopschall.  Copyright 2014 by Mark Rashid.
!
!-----------------------------------------------------------------------
!
      USE KINDS_M
      USE SIM_MESH_M
      USE POST_PROC_M
!
!-----------------------------------------------------------------------
!
      TYPE (SIM_MESH) :: SM_OBJ
!
!=======================================================================
!
!     set up the problem (i.e. read input files, fill SM_OBJ components)
!
      CALL SM_OBJ%PROB_SETUP
!
!     loop over time steps
!
      DO I = 1, SIZE(SM_OBJ%STEPS)
!
!       update the solution for the current step
!
        CALL SM_OBJ%STEP_SOLN
!
!       optionally output vtk files for visualization
!       -BDG
!
        CALL OUTPUT_VTK(SM_OBJ)
! 
!       satisfy any output requests for the current step
!
        CALL SM_OBJ%OUTPUT
!
      END DO
!
      STOP
      END PROGRAM IMITOR
