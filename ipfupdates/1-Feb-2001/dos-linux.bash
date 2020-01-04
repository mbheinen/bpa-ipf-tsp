#! /bin/sh
#
# DOS-linux: Removes DOS '\r' records and converts case to lower.
# Example: dos-linux *.NET
#
#
for file in $*; do
  status="$(file $file | grep ASCII)"
#  echo "status= $status"
  if [ "$status" = "" ]; then
    status="$(file $file | grep English)"
#  echo "status= $status"
    if [ "$status" = "" ]; then
      status="$(file $file | grep troff)"
#  echo "status= $status"
    fi
  fi
  if [ "$status" != "" ]; then
    tmp=/tmp/$file
    cp $file $tmp
    new=$(echo $file | tr A-Z a-z)
    echo "Processing $file > $new"
    tr -d '\r' < $tmp > $new
    rm -f $tmp
  else
    echo "Skipping non-ASCII text file $file"
  fi
done
