#! /bin/sh
#
# DOS-linux: Removes DOS '\r' records and converts case to lower.
# Example: DOS-linux *.NET *.net
#
if [ $# -eq 1 ]; then
  source=${1%% *}            # first part of parameter (before the space)
  target=${1##* }            # second part of paramter (after the space)
  source1=${source%\**}      # first part of source pattern (preceding *)
  source2=${source#*\*}      # second part of source pattern (following *)
  target1=${target%\**}      # first part of target pattern (preceindg *)
  target2=${target#*\*}      # second part of target pattern (following *)
#
# check whether source or target string is empty
#
  if [ -z "$source" ]; then
    echo "DOS-linux: Source pattern is missing"
    exit
  fi
  if [ "$source" = "$target" ]; then
    echo "DOS-linux: Target pattern is missing or same as source pattern"
    exit
  fi
#
# check whether source and target contain exactly one * each
#
  if [ $[ ${#source1} + ${#source2} +1 ] -ne ${#source} ]; then
    echo "DOS-linux: Source pattern must contain exactly one *"
    exit
  fi
  if [ $[ ${#target1} + ${#target2} +1 ] -ne ${#target} ]; then
    echo "DOS-linux: Target pattern must contain exactly one *"
    exit
  fi
#
# check whether any source files are found at all
#
  files=$(echo $source)      # source files to be processed
  if [ "$files" = "$source" ]; then
    echo "DOS-linux: Cannot find any source files"
    exit
  fi
#
# perform file rename option
#
  for i in $files; do
    tmp=${i#$source1}        # eliminate character preceding *
    tmp=${tmp%$source2}      # eliminate character following *
    tmp=$target1$tmp$target2 # assemble target file
    new=$(echo $tmp | tr A-Z a-z)
    echo "Processing $i > $new"
    tr -d '\r' < $i > $new
  done
else
  echo "DOS-linux: Converts files using patterns"
  echo "           The entire parameter string must be enclosed in '"
  echo "           Source and target patter must contain exactly one *"
  echo 
  echo "           Example: DOS-linux '*.NET' *.net'"
fi
