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
# fake-color Script 5.40
#
#-----------------------------------------------------------------------
#
# An 'Escaped' Dollar Sign for Use with sed
#
  SED_END_OF_LINE='$'
#
# Color Escape Codes
#
  ECHO_DEFAULT_COLOR="\033[0m"
#
  ECHO_BLACK="\033[0;30m"
  ECHO_BLUE="\033[0;34m"
  ECHO_BROWN="\033[0;33m"
  ECHO_CYAN="\033[0;36m"
  ECHO_DARK_GRAY="\033[1;30m"
  ECHO_GREEN="\033[0;32m"
  ECHO_LIGHT_BLUE="\033[1;34m"
  ECHO_LIGHT_CYAN="\033[1;36m"
  ECHO_LIGHT_GRAY="\033[0;37m"
  ECHO_LIGHT_GREEN="\033[1;32m"
  ECHO_LIGHT_PURPLE="\033[1;35m"
  ECHO_LIGHT_RED="\033[1;31m"
  ECHO_PURPLE="\033[0;35m"
  ECHO_RED="\033[0;31m"
  ECHO_YELLOW="\033[1;33m"
  ECHO_WHITE="\033[1;37m"
#
  ECHO_COLORS=( $ECHO_DEFAULT_COLOR $ECHO_BLACK $ECHO_BLUE $ECHO_BROWN $ECHO_CYAN $ECHO_DARK_GRAY $ECHO_GREEN $ECHO_LIGHT_BLUE $ECHO_LIGHT_CYAN $ECHO_LIGHT_GRAY $ECHO_LIGHT_GREEN $ECHO_LIGHT_PURPLE $ECHO_LIGHT_RED $ECHO_PURPLE $ECHO_RED $ECHO_YELLOW $ECHO_WHITE )
#
  SED_DEFAULT_COLOR="\x1b[0m"
#
  SED_BLACK="\x1b[0;30m"
  SED_BLUE="\x1b[0;34m"
  SED_BROWN="\x1b[0;33m"
  SED_CYAN="\x1b[0;36m"
  SED_DARK_GRAY="\x1b[1;30m"
  SED_GREEN="\x1b[0;32m"
  SED_LIGHT_BLUE="\x1b[1;34m"
  SED_LIGHT_CYAN="\x1b[1;36m"
  SED_LIGHT_GRAY="\x1b[0;37m"
  SED_LIGHT_GREEN="\x1b[1;32m"
  SED_LIGHT_PURPLE="\x1b[1;35m"
  SED_LIGHT_RED="\x1b[1;31m"
  SED_PURPLE="\x1b[0;35m"
  SED_RED="\x1b[0;31m"
  SED_YELLOW="\x1b[1;33m"
  SED_WHITE="\x1b[1;37m"
#
  SED_COLORS=( $SED_DEFAULT_COLOR $SED_BLACK $SED_BLUE $SED_BROWN $SED_CYAN $SED_DARK_GRAY $SED_GREEN $SED_LIGHT_BLUE $SED_LIGHT_CYAN $SED_LIGHT_GRAY $SED_LIGHT_GREEN $SED_LIGHT_PURPLE $SED_LIGHT_RED $SED_PURPLE $SED_RED $SED_YELLOW $SED_WHITE )
#
  function color_index {
    case $1 in
    black)
      return 1
      ;;
    blue)
      return 2
      ;;
    brown)
      return 3
      ;;
    cyan)
      return 4
      ;;
    "dark gray")
      return 5
      ;;
    green)
      return 6
      ;;
    "light blue")
      return 7
      ;;
    "light cyan")
      return 8
      ;;
    "light gray")
      return 9
      ;;
    "light green")
      return 10
      ;;
    "light purple")
      return 11
      ;;
    "light red")
      return 12
      ;;
    none)
      return 0
      ;;
    purple)
      return 13
      ;;
    red)
      return 14
      ;;
    yellow)
      return 15
      ;;
    white)
      return 16
      ;;
    *)
      return 255
      ;;
    esac
  }
#
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# Color Settings with Environment Variables (Where Available)
#
  COLOR="${FAKE_COLOR}"
#
  color_index "${FAKE_ERROR_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    ERROR_COLOR="${ECHO_COLORS[$COLOR_INDEX]}"
  else
    ERROR_COLOR="${ECHO_RED}"
  fi
#
  color_index "${FAKE_FC_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    FC_COLOR="${ECHO_COLORS[$COLOR_INDEX]}"
  else
    FC_COLOR="${ECHO_DEFAULT_COLOR}"
  fi
#
  color_index "${FAKE_DEFAULT_FC_OUT_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    DEFAULT_FC_OUT_COLOR="${SED_COLORS[$COLOR_INDEX]}"
  else
    DEFAULT_FC_OUT_COLOR="${SED_BLACK}"
  fi
#
  color_index "${FAKE_COLUMN_NUM_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    COLUMN_NUM_COLOR="${SED_COLORS[$COLOR_INDEX]}"
  else
    COLUMN_NUM_COLOR="${SED_CYAN}"
  fi
#
  color_index "${FAKE_ERROR_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    ERROR_COLOR="${SED_COLORS[$COLOR_INDEX]}"
  else
    ERROR_COLOR="${SED_RED}"
  fi
#
  color_index "${FAKE_LINE_NUM_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    LINE_NUM_COLOR="${SED_COLORS[$COLOR_INDEX]}"
  else
    LINE_NUM_COLOR="${SED_BLUE}"
  fi
#
  color_index "${FAKE_WARNING_COLOR}"
  COLOR_INDEX=$?
  if [ $COLOR_INDEX != 255 ]; then
    WARNING_COLOR="${SED_COLORS[$COLOR_INDEX]}"
  else
    WARNING_COLOR="${SED_YELLOW}"
  fi
