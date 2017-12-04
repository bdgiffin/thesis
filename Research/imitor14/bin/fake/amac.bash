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
# Augmented MODULE-Aware Compiling (amac) Script 5.40
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
# Compilation Arguments
#
  FC="${1}"
  FLAGS="${2}"
  SRC="${3}"
  MODS="${4}"
#
# Strip all directory information off of the source file name.
#
  SRC_DIR=`awk -F '/' '{ if (NF > 1) { printf("%s", $1); for (i = 2; i < NF; ++i) { printf("/%s", $i); } } else { print "."; } }' <<< "${SRC}"`
  SRC=`awk -F '/' '{ print $NF; }' <<< "${SRC}"`
  OBJ=`awk -F '.' '{ printf("%s", $1); for (i = 2; i < NF; ++i) { printf(".%s", $i) }; printf(".o"); }' <<< "${SRC}"`
#
  if [ "${COLOR}" != "on" ]; then
    echo "${FC} ${FLAGS} -J${SRC_DIR} -c ${SRC_DIR}/${SRC} -o ${SRC_DIR}/${OBJ}"
  else
    echo -e "${FC_COLOR}${FC} ${FLAGS} -J${SRC_DIR} -c ${SRC_DIR}/${SRC} -o ${SRC_DIR}/${OBJ}${ECHO_DEFAULT_COLOR}"
  fi
#
  if [ "${FC}" == "gfortran" -o "${FC}" == "gcc" -o "${FC}" == "g++" ]; then
    FC_OUT=`${FC} ${FLAGS} -J${SRC_DIR} -c ${SRC_DIR}/${SRC} -o ${SRC_DIR}/${OBJ} 2>&1`
#
    EXIT_STATUS=$?
#
    if [ "${FC_OUT}" != "" ]; then
      if [ "${COLOR}" != "on" ]; then
        echo "${FC_OUT}"
      else
        echo -e "${DEFAULT_FC_OUT_COLOR}${FC_OUT}${ECHO_DEFAULT_COLOR}" | sed -e "s/\(warning\):/${WARNING_COLOR}\1${DEFAULT_FC_OUT_COLOR}:/gI" | sed -e "s/\(fatal error\):/${ERROR_COLOR}\1${DEFAULT_FC_OUT_COLOR}:/gI" | sed -e "s/\(error\):/${ERROR_COLOR}\1${DEFAULT_FC_OUT_COLOR}:/gI" | sed -e "s/\([0-9]\+\)\([\.:]\)\([0-9]\+\)/${LINE_NUM_COLOR}\1${DEFAULT_FC_OUT_COLOR}\2${COLUMN_NUM_COLOR}\3${DEFAULT_FC_OUT_COLOR}/g" | sed -e "s/^\(\s*\)\^\(\s*\)${END_OF_LINE}/\1${COLUMN_NUM_COLOR}^${DEFAULT_FC_OUT_COLOR}\2/g" >&2
      fi
    fi
  elif [ "${FC}" == "ifort" ]; then
    ${FC} ${FLAGS} -module ${SRC_DIR} -c ${SRC_DIR}/${SRC} -o ${SRC_DIR}/${OBJ}
#
    EXIT_STATUS=$?
  else
    ${FC} ${FLAGS} -c ${SRC_DIR}/${SRC} -o ${SRC_DIR}/${OBJ}
#
    EXIT_STATUS=$?
  fi
#
  if [ $EXIT_STATUS != 0 ]; then
    if [ "${FAKE_NOTIFY}" == "on" ]; then
      type notify-send &>/dev/null && notify-send "fake" "failed to compile ${SRC}"
#
      type osascript &>/dev/null && osascript -e "display notification \"failed to compile ${SRC}\" with title \"fake\""
    fi
  else
    if [ "${FAKE_WATSON}" == "on" ]; then
      type watson &>/dev/null && (cd "${INC_DIR}"; watson -n -f ${OLDPWD}/${SRC_DIR}/${SRC})
    fi
  fi
#
  exit $EXIT_STATUS
