#! /bin/bash
#
#=======================================================================
#
# © 2010-2013 Andrew Baldwin
# This file is part of fake [version 5.40].
# fake is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your
# option) any later version.
# fake is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# You should have received a copy of the GNU General Public License
# along with fake.  If not, see <http://www.gnu.org/licenses/>.
#
#=======================================================================
#
# STOP! Do NOT Edit Below!
#
# The file example.bash (or a copy) should edited instead (see fake.txt
# for a detailed explanation).
#
#=======================================================================
#
# Source Files (srcs) Search Script 5.40
#
#-----------------------------------------------------------------------
#
  INC_DIR=`dirname "${BASH_SOURCE}"`
  INC_DIR=`sed -e 's/ /\\\\ /g' <<< "${INC_DIR}"`
#
  . ${INC_DIR}/fake-color.bash
#
#-----------------------------------------------------------------------
#
  NEW_SRCS[${#NEW_SRCS[@]}]="${PSRC}"
  SRCS[${#SRCS[@]}]="${PSRC}"
#
# Create output.
#
  ERROR="n"
  NSRC=0
  NDEP=0
  if [ "${VER}" == "y" ]; then
    echo -n "fake: Found ${NSRC} source files..."
  fi
#
# Keep searching until new new source files are found.
#
  while [ ${#SRCS[@]} -ne $NSRC ]; do
    if [ "${VER}" == "y" ]; then
      DEL=`sed -e 's/./\\\b/g' <<< "${NDEP} source files..."`
      NSRC=${#SRCS[@]}
      (( NDEP = NSRC - 1 ))
      echo -en "${DEL}${NDEP} source files..."
    else
      NSRC=${#SRCS[@]}
    fi
#
#   Find any new modules USEd and headers included by new source files.
#
    unset NEW_MODS
    for SRC in ${NEW_SRCS[@]}; do
      SRC_MODS=`grep -i "^[\/C \t][ \t]*use " ${SRC} | sed -e 's/^C//g' | awk '{ gsub(",","",$2); printf("%s ", $2); }' | sed -e 's/ISO_FORTRAN_ENV //g' | sed -e 's/ISO_C_BINDING //g' | tr -d $'\r'`
      for MOD in ${SRC_MODS[@]}; do
        FND_MOD=`echo "${MODS[@]}" | grep -e "\b${MOD}\b"`
        if [ "${FND_MOD}" == "" ]; then
          NEW_MODS[${#NEW_MODS[@]}]=$MOD
          MODS[${#MODS[@]}]=$MOD
        fi
      done
#
      SRC_INCS=`grep -i "^#include \"" ${SRC} | awk '{ gsub("\"","",$2); printf("%s ", $2); }'`
      for INC in ${SRC_INCS[@]}; do
        FND_INC=`grep -e "\b${INC}\b" <<< "${INCS[@]}"`
        if [ "${FND_INC}" == "" ]; then
          FND_INC=`find ${ROOT_DIR} -iname "${INC}" -not -path "*/\.svn/.*"`
          if [ "${FND_INC}" != "" ]; then
            INCS[${#INCS[@]}]=$FND_INC
          fi
        fi
      done
    done
#
#   Find any new source files required to define new modules.
#
    unset NEW_SRCS
    for MOD in ${NEW_MODS[@]}; do
      SRC=`find ${ROOT_DIR} \( -iname "*.[cf]" -o -iname "*.comp" -o -iname "*.frag" -o -iname "*.geom" -o -iname "*.shdr"  -o -iname "*.tesc"  -o -iname "*.tese"  -o -iname "*.vert" \) -not -path "*/\.svn/.*" | xargs grep -l "^[ \t]*MODULE[ \t]*${MOD}\b"`
#
      NFND_SRC=`wc -w <<< ${SRC}`
      if [ "${NFND_SRC}" != "1" ]; then
        ERROR="y"
#
        if [ "${VER}" == "y" ]; then
          echo ""
        fi
#
        if [ "${NFND_SRC}" == "0" ]; then
          if [ "${COLOR}" != "on" ]; then
            echo -n "fake" 1>&2
          else
            echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
          fi
          echo ": No file with a MODULE ${MOD} declaration was found." 1>&2
        else
          ERROR_SRC=`awk '{ if (NF > 2) { for (i = 1; i < NF; ++i) { printf("%s, ", $i); } } else { printf("%s ", $1); } printf("and %s", $NF); }' <<< ${SRC}`
#
          if [ "${COLOR}" != "on" ]; then
            echo -n "fake" 1>&2
          else
            echo -n -e "${ERROR_COLOR}fake${ECHO_DEFAULT_COLOR}" 1>&2
          fi
          echo ": MODULE ${MOD} multiply declared in the files ${ERROR_SRC}." 1>&2
        fi
#
        if [ "${VER}" == "y" ]; then
          echo -n "fake: Found ${NSRC} source files..."
        fi
      fi
#
      FND_SRC=`grep -e "\b${SRC}\b" <<< "${SRCS[@]}"`
      if [ "${FND_SRC}" == "" ]; then
        NEW_SRCS[${#NEW_SRCS[@]}]=$SRC
        SRCS[${#SRCS[@]}]=$SRC
      fi
    done
  done
#
  if [ "${VER}" == "y" ]; then
    echo -e "\b\b  "
  fi
#
  IFS_SWP=$IFS
  IFS=':'
  TMP_DIRS=`awk '{ for (i = 1; i < NF; ++i) { printf("%s\n", $i); } print $NF; }' <<< "${SRCS[@]} ${INCS[@]}" | awk -F '/' '{ for (i = 1; i < NF; ++i) { printf("%s/", $i); } printf("\n"); }' | sort | uniq | awk '{ printf("%s:", $1); }'`
  WRK_DIRS="."
  for DIR in $TMP_DIRS; do
    if [ "${DIR}" != "" ]; then
      DIR_DIFF=`diff . ${DIR}`
      if [ "${DIR_DIFF}" != "" ]; then
        WRK_DIRS="${WRK_DIRS}:${DIR}"
      fi
    fi
  done
  IFS=$IFS_SWP
#
  echo "#! /bin/bash
#
# This file contains a list of all Source Files (and the Work Directores
# that they reside in) for the executable ${XNAME}.
#
# Generated by fake [version 5.40]
#
#=======================================================================
#
# WAIT! This file can be generated automatically calling the associated
# Fortran Makefile (fake) Project Script with the flag '-s' or '--srcs'.
#
#=======================================================================
#
# Work Directories
#   Directories that contain all of the source files (e.g. .f, .f90, .C,
#   .H, etc. files).  Seperate directories inside the variable with ':'s
#   (e.g. '.:./firstdir:./secondir:./firstdir/withsubdir').
#
  WRK_DIRS=\"${WRK_DIRS}\"
#
# Source Files
#   The variable SRCS[\${#SRCS[@]}] can always be used to set the name of
#   a new source file.  So, to add a new source file, simply add the
#   line
#
#     SRCS[\${#SRCS[@]}]=\"sourcefile.f\"
#
#   with your source file's name instead of 'sourcefile.f'. List only
#   the names of the source files (e.g. mysource.f), while specifying
#   locations using Work Directories (WRK_DIRS).
#" > ${SRCS_FILE}
  awk '{ for (i = 1; i < NF; ++i) { printf("%s\n", $i); } print $NF; }' <<< "${SRCS[@]}" | awk -F '/' '{ printf("  SRCS[${#SRCS[@]}]=\""); printf("%s", $NF); printf("\"\n"); }' | sort >> ${SRCS_FILE}
#
  unset MODS
  unset SRCS
#
  if [ "${ERROR}" == "y" ]; then
    false
  else
    true
  fi

