#! /bin/bash
#
#=======================================================================
#
# Fortran Makefile (fake) Project Script 5.24
#
# OPTIONS
#   -s, --srcs
#     Find all source files needed to build the Program Source File.
#     Will generate a file with the same name as the calling Project
#     Script with the extension '.srcs' containing the needed
#     information for all found source files.
#
#     This option is automatically selected whenever the corresponding
#     '.srcs' file cannot be found.
#
#   -v, --verbose
#     Include progress output.
#
#-----------------------------------------------------------------------
#
# Root Directory
#   Location of the root directory of the project.  All source files
#   must be contained in the Root Directory or its subdirectories.
#
  ROOT_DIR="."
#
# Scripts Directory
#   The location of fake.bash, amac.bash, and srcs.bash (absolute or
#   relative).
#
  BASH_DIR="${ROOT_DIR}/fake"
#
# Target Directory
#   Where the Makefile will be generated and executable will be
#   compiled.
#
  MAKE_DIR="."
#
#-----------------------------------------------------------------------
#
# Fortran Compiler
#   Used for compiling Fortran [*.f, *.f90, *.f95, *.f03, *.F, *.F90]
#   source files into object files.
#
  FC="gfortran"
#
# Fortran Flags
#   Compiler flags used when compiling Fortran [*.f, *.f90, *.f95,
#   *.f03, *.F, *.F90] source files.  E.g. --all-warnings, -pedantic, -O3, etc..
#
# "-O3" is supposed equivalent to "-O2 -finline-functions -funswitch-loops -fpredictive-commoning -fgcse-after-reload -ftree-loop-vectorize -fvect-cost-model -ftree-partial-pre -fipa-cp-clone"
# but the former creates executables that crash while the latter does not
#
#  FFLAGS="-D X86 --all-warnings -pedantic -mtune=generic -O2 -finline-functions -funswitch-loops -fpredictive-commoning -fgcse-after-reload -ftree-loop-vectorize -fvect-cost-model -ftree-partial-pre -fipa-cp-clone -funroll-loops -fopenmp" # -fcheck=all -march= -flto
#
  FFLAGS="-Wall"
#
#
# C/C++ Compiler
#   Used for compiling C/C++ [*.c, *.C, *.cxx, *.cpp, *.c++] source
#   files into object files.  If no C/C++ source files are used this may
#   be left blank.
#
  CC=
#
# C/C++ Flags
#   Compiler flags used when compling C/C++ [*.c, *.C, *.cxx, *.cpp,
#   *.c++] source files.  E.g. --all-warnings, -pedantic, -O3, etc..  If no C/C++
#   source files are used this may be left blank/unassigned.
#
  CFLAGS=
#
# Linking Compiler
#   Use the line
#
#     XC=$FC
#
#   in order to set the Fortran Compiler as the Linking Compiler,
#   otherwise set XC equal to your linking compiler command of choice.
#
  XC=$FC
#
# Linking Flags
#   Compiler flags used only when linking (compiling the executable).
#   E.g. -L/library/directory, -llibraryname, etc..
#
#  XFLAGS="-O3 -fopenmp -m32 -Wl,-as-needed -Wl,-non_shared -static-libgcc -static-libstdc++ -lgfortran -lgomp -lquadmath -lfltk -lfltk_gl -lXft -lexpat -lfreetype -lz -lXrender -lGLEW -lGLU -Wl,-call_shared -lfontconfig -lGL"
#
#-----------------------------------------------------------------------
#
# Program Source File
#   The source file containing the PROGRAM block (or 'main' function) of
#   the executable to be built.  This should include the entire path to
#   the source file (absolute or relative).
#
#   All source files will be found by following the USE hierarchy,
#   starting with the Program Source File.  The found source files will
#   be cached in the a file with the same name as the Project Script
#   File and the extension '.srcs'.  To rebuild this cache (whenever new
#   source files are added to the project) call this script with the
#   flag '-s' or '--srcs', or manually edit the '.srcs' file
#   (instructive comments are contained within it).
#
  PSRC="imitor.f"
#
# Executable Name
#   Use the line
#
#     XNAME=`echo "${PSRC}" | awk -F '.' '{ printf("%s", $1); for (i = 2; i < NF; ++i) { printf(".%s", $i); } printf(".x"); }'`
#
#   to set the executable name to be the same name as the Program
#   Source File with the suffix changed to 'x' (e.g. if the Program
#   Source File is named 'myprog.f' then the executable would be named
#   'myprog.x').  Otherwise XNAME should be set to your executable name
#   of choice.
#
  XNAME="imitor"
#
#=======================================================================
#
# STOP! Do NOT Edit Below!
#
#=======================================================================
#
  source ${BASH_DIR}/fake-color.bash
#
#-----------------------------------------------------------------------
#
  if [ ! -e "${PSRC}" ]; then
    if [ "${COLOR}" != "on" ]; then
      echo -n "fake" 1>&2
    else
      echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
    fi
    echo ": File ${PSRC} does not exist." 1>&2
  elif [ ! -r "${PSRC}" ]; then
    if [ "${COLOR}" != "on" ]; then
      echo -n "fake" 1>&2
    else
      echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
    fi
    echo ": File ${PSRC} is not readable." 1>&2
  else
    FND_SRCS="n"
    VER="n"
    for ARG in "$@"; do
      case "$ARG" in
      -s|--srcs)
        FND_SRCS="y"
        ;;
      -v|--verbose)
        VER="y"
        ;;
      -sv|-vs)
        FND_SRCS="y"
        VER="y"
        ;;
      esac
    done
#
    SRCS_FILE=`basename $0 | awk -F '.' '{ printf("%s", $1); for (i = 2; i < NF; ++i) { printf(".%s", $i); } printf(".srcs"); }'`
    if [ ! -e ${SRCS_FILE} ]; then
      if [ ! -w ${MAKE_DIR} ]; then
        if [ "${COLOR}" != "on" ]; then
          echo -n "fake" 1>&2
        else
          echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
        fi
        echo ": Directory ${MAKE_DIR} is not writable." 1>&2
      else
        source "${BASH_DIR}/srcs.bash"
      fi
    elif [ "${FND_SRCS}" == "y" ]; then
      if [ ! -r ${SRCS_FILE} ]; then
        if [ "${COLOR}" != "on" ]; then
          echo -n "fake" 1>&2
        else
          echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
        fi
        echo ": File ${SRCS_FILE} is not readable." 1>&2
      elif [ ! -w ${SRCS_FILE} ]; then
        if [ "${COLOR}" != "on" ]; then
          echo -n "fake" 1>&2
        else
          echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
        fi
        echo ": File ${SRCS_FILE} is not writable." 1>&2
      else
        source "${BASH_DIR}/srcs.bash"
      fi
    fi
#
    source "${SRCS_FILE}"
    source "${BASH_DIR}/fake.bash"
  fi
