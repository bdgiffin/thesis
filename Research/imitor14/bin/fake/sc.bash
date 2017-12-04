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
# Shader Conversion (sc) Script 5.40
#
#-----------------------------------------------------------------------
#
# Shader Conversion Arguments
#
  SHD=${1}
#
# Strip all directory information off of the source file name.
#
  SHD_DIR=`awk -F '/' '{ if (NF > 1) { printf("%s", $1); for (i = 2; i < NF; ++i) { printf("/%s", $i); } } else { print "."; } }' <<< "${SHD}"`
  SHD=`awk -F '/' '{ print $NF; }' <<< "${SHD}"`
#
# Output Header File Parameters
#
  HDR="${SHD}.H"
  HDR_DEF=`awk -F '.' '{ printf("%s", toupper($1)); for (i = 2; i <= NF; ++i) { printf("_%s", toupper($i)); }; }' <<< "${HDR}"`
#
# Create Variable Name
#
  CHR=`awk -F '[._]' '{ printf("%s", tolower($1)); for (i = 2; i < NF; ++i) { printf("%s%s", toupper(substr($i,1,1)), tolower(substr($i,2))); }; }' <<< "${SHD}"`
#
  EXT=`awk -F '.' '{ printf("%s%s", toupper(substr($NF,1,1)), tolower(substr($NF,2))) }' <<< "${SHD}"`
  if [ "${EXT}" == "Comp" ]; then
    CHR="${CHR}ComputeShaderSource"
  elif [ "${EXT}" == "Frag" ]; then
    CHR="${CHR}FragmentShaderSource"
  elif [ "${EXT}" == "Geom" ]; then
    CHR="${CHR}GeometryShaderSource"
  elif [ "${EXT}" == "Shdr" ]; then
    CHR="${CHR}ShaderSource"
  elif [ "${EXT}" == "Tesc" ]; then
    CHR="${CHR}TessellationControlShaderSource"
  elif [ "${EXT}" == "Tese" ]; then
    CHR="${CHR}TessellationEvaluationShaderSource"
  elif [ "${EXT}" == "Vert" ]; then
    CHR="${CHR}VertexShaderSource"
  else
    CHR="${CHR}${EXT}"
  fi
#
# Header File Generation
#
  echo "#ifndef ${HDR_DEF}"                                                       >  "${SHD_DIR}/${HDR}"
  echo "#define ${HDR_DEF}"                                                       >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "//======================================================================" >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "//  This header file module provides accress to the shader with source"   >> "${SHD_DIR}/${HDR}"
  echo "//  file ${SHD}."                                                         >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "//  In order to modify the shader the source file should be edited."      >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "//----------------------------------------------------------------------" >> "${SHD_DIR}/${HDR}"
  echo "//  #include wherever shader access is desired."                          >> "${SHD_DIR}/${HDR}"
  echo "//----------------------------------------------------------------------" >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "#include <FL/gl.h> // FLTK (OpenGL)"                                      >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "//======================================================================" >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "  const GLchar* ${CHR} = \"\\"                                            >> "${SHD_DIR}/${HDR}"
  sed -e 's/$/\\n\\/g' ${SHD_DIR}/${SHD} | sed -e 's/"/\\"/g'                     >> "${SHD_DIR}/${HDR}"
  echo "\\0\";"                                                                   >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "//======================================================================" >> "${SHD_DIR}/${HDR}"
  echo "//"                                                                       >> "${SHD_DIR}/${HDR}"
  echo "#endif"                                                                   >> "${SHD_DIR}/${HDR}"
#
  exit 0
