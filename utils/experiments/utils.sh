#!/usr/bin/env bash

UTILS_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
USER_HOME_DIR="$(cd ~ && pwd)"

export MALLOC_ARENA_MAX=1 # Iceberg's requirement

export _JAVA_OPTIONS="-Xmx4096M -XX:MaxHeapSize=1024M"
export MAVEN_OPTS="-Xmx1024M"
export ANT_OPTS="-Xmx2048M -XX:MaxHeapSize=1024M"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

#
# Print error message to the stdout and exit.
#
die() {
  echo "$@" >&2
  exit 1
}

#
# Check whether the machine has the software that allows one to run jobs
# in parallel.  Return true ('0') if yes, otherwise it returns false ('1').
#
_can_I_run_jobs_simultaneously() {
  if module > /dev/null 2>&1; then
    return 0 # true
  fi
  if man qsub > /dev/null 2>&1; then
    return 0 # true
  fi
  if sbatch --version > /dev/null 2>&1; then
    return 0 # true
  fi
  if man parallel > /dev/null 2>&1; then
    return 0 # true
  fi
  return 1 # false
}

#
# Return an abbreviation of the host name.
#
_get_host_name() {
  local host_name=$(hostname)

  if [[ $host_name == "iceberg-"* ]]; then
    # iceberg-login1
    echo "iceberg"
    return 0
  elif [[ $host_name == "sharc-"* ]]; then
    # sharc-login2.shef.ac.uk
    echo "sharc"
    return 0
  elif [[ $host_name == *".polaris.leeds.ac.uk" ]]; then
    # login1.polaris.leeds.ac.uk
    echo "n8"
    return 0
  elif [[ $host_name == *".bob.macc.fct.pt" ]]; then
    # c805-001.bob.macc.fct.pt
    echo "macc"
    return 0
  elif [ "$host_name" == "slurmsub.grid.fe.up.pt" ]; then
    # slurmsub.grid.fe.up.pt
    echo "feup-grid"
    return 0
  fi

  echo "localhost"
  return 0
}

#
# Submit a job script using cluster's infrastructure.
#
_submit_job_script() {
  local USAGE="Usage: ${FUNCNAME[0]} <pid>"
  if [ "$#" != 1 ]; then
    echo "$USAGE" >&2
    return 1
  fi

  local job_script="$1"
  [ -s "$job_script" ] || die "[ERROR] $job_script does not exist or it is empty!"

  cluster=$(_get_host_name)
  if [ "$cluster" == "iceberg" ] || [ "$cluster" == "sharc" ] || [ "$cluster" == "n8" ]; then
    qsub "$job_script"
  elif [ "$cluster" == "macc" ] || [ "$cluster" == "feup-grid" ]; then
    sbatch "$job_script"
  else
    return 1
  fi

  return 0
}

# EOF
