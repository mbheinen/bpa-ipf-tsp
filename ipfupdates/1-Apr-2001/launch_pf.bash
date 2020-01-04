#!/bin/bash
#
# Script to launch batch jobs
#
# Usage: ./launch_pf.bash <pfc_filen> <user_master_file> <cor_file>
#

pfc_file=$1
master_analysis_file=$2
cor_file=$3

if [ ! -e $pfc_file ]; then
  echo "File $pfc_file does not exist."
  exit 1
fi
if [ "$user_master_file" = "" ]; then
  echo
elif ! [ -e $master_analysis_file ]; then
  echo "New User Master File $master_analysis_file created."
  echo "" > $master_analysis_file
fi
if [ "$cor_file" = "" ]; then
  echo
elif [ ! -e $cor_file ]; then
  echo "File $cor_file does not exist."
  cor_file=""
fi

new_base_file=""
output_analysis_file=""

# Read pfc file to obtain new_base file and user_analysis_output file

{ while read line; do
    case $line in

      /*NEW_BASE*|/*new_base* )
        echo "PF command [$line]"
        IFS=" ,="
        let count=0
        let state=0
        for token in $line; do
#         echo " state [$state] token[$count] = [$token]"
          let count=count+1
          case $state in
            0 )
              if [ "$token" = "FILE" ] || [ "$token" = "file" ]; then
                let state=1
              fi ;;

            1 )
              if [ "$token" != "" ]; then
                new_base_file=$token
                echo "New base file = [$new_base_file]"
                break
              fi ;;
          esac
        done ;;

      /*USER_ANALYSIS*|/*user_analysis* )
        echo "PF command [$line]"
        IFS=" ,="
        let count=0
        let state=0
        for token in $line; do
#         echo " state [$state] token[$count] = [$token]"
          let count=count+1
          case $state in
            0 )
              if [ "$token" = "OUTPUT" ] || [ "$token" = "output" ]; then
                let state=1
              fi ;;

            1 )
              if [ "$token" != "" ]; then
                output_analysis_file=$token
                echo "output_analysis file = [$output_analysis_file]"
                break
              fi ;;
          esac
        done ;;

     * )
#       echo "PF record  [$line]" ;;
    esac
  done
} < $pfc_file

# launch bpf job

echo "**************************************************************"
echo "Launching bpf $pfc_file"
echo "**************************************************************"
bpf $pfc_file

if [ "$output_analysis_file" != "" ] && [ "$master_analysis_file" != "" ]; then
  echo "**************************************************************"
  echo "Appending $(wc $output_analysis_file) records to Master file $master_analysis_file"
  echo "**************************************************************"
  cat $output_analysis_file >> $master_analysis_file
else
  echo "Error in appending User Analysis Output file $output_analysis_file to Master file $master_analysis_file"
fi

# conditionally launch plotting job

if ! [ -e $IPFDIRS/pfmaster.post ]; then
  echo "Missing vital pfmaster.post file in directory IPFDIRS"
elif [ "$cor_file" != "" ] && [ -e $new_base_file ]; then
  echo "**************************************************************"
  echo "Launching ipfplot $cor_file $new_base_file"
  echo "**************************************************************"
  ipfplot $cor_file $new_base_file
fi

echo "Completed!"
