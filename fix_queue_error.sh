if [[ `qstat | grep -o Eqw |wc -l` != 0 ]]; then qmod -cj `qstat | grep Eqw | cut -c-7`;  fi
