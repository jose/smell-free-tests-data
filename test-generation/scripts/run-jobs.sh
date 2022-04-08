#!/usr/bin/env bash
#
# ------------------------------------------------------------------------------
# This script runs the many jobs in the provided directory either using
# [GNU Parallel](https://www.gnu.org/software/parallel) or the cluster's API,
# if any.
#
# Usage:
# run-jobs.sh
#   --jobs_dir_path <full path>
#   [--seconds_per_job <time in seconds allowed to run each job, e.g., 360>]
#   [--max_number_batches <maximum number of batches (where one batch is composed by many jobs), e.g., 32>]
#   [--max_number_cores <maxinum number of cores per CPU, e.g., 16>]
#   [help]
# ------------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"

#
# Print error message to the stdout and exit.
#
die() {
  echo "$@" >&2
  exit 1
}

# ----------------------------------------------------------------- Requirements

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
_can_I_run_jobs_simultaneously || die "[ERROR] Scripts are optimized to run on clusters with a SGE system or a machine with [GNU Parallel](https://www.gnu.org/software/parallel). Please make sure it is the case."

#
# Generate a header for a batch based on the host's name.
#
_generate_batch_header() {
  local USAGE="Usage: ${FUNCNAME[0]} <batch script file patch> <batch timeout in seconds> <max number of cores>"
  if [ "$#" != "3" ]; then
    echo "$USAGE" >&2
    return 1
  fi

  # Args
  local batch_script_file_path="$1"
  local batch_timeout_in_seconds="$2"
  local max_number_cores="$3"

  # Local vars
  local host_name=$(hostname)
  local timeout=$(printf '%02d:%02d:%02d\n' $((batch_timeout_in_seconds/3600)) $((batch_timeout_in_seconds%3600/60)) $((batch_timeout_in_seconds%60)))

  # hash-bang
  echo "#!/usr/bin/env bash"

  if [[ $host_name == "iceberg-"* ]] || [[ $host_name == "sharc-"* ]]; then # e.g., iceberg-login1 or harc-login2.shef.ac.uk
    #
    # https://www.sheffield.ac.uk/wrgrid/iceberg or https://www.sheffield.ac.uk/cics/research/hpc/sharc
    #
    echo "#$ -l h_rt=$timeout"
    echo "#$ -l rmem=4G"
    echo "#$ -e $batch_script_file_path.err"
    echo "#$ -o $batch_script_file_path.out"
  elif [[ $host_name == *".polaris.leeds.ac.uk" ]]; then # e.g., login1.polaris.leeds.ac.uk
    #
    # https://n8hpc.org.uk
    #
    echo "#$ -l h_rt=$timeout"
    echo "#$ -l h_vmem=4G"
    echo "#$ -pe smp 2"
    echo "#$ -e $batch_script_file_path.err"
    echo "#$ -o $batch_script_file_path.out"
  elif [[ $host_name == *".bob.macc.fct.pt" ]] || [ "$host_name" == "slurmsub.grid.fe.up.pt" ]; then # e.g., c805-001.bob.macc.fct.pt or slurmsub.grid.fe.up.pt
    echo "#SBATCH --job-name=$(basename "$batch_script_file_path")"
    echo "#SBATCH --output=$batch_script_file_path.out"
    echo "#SBATCH --error=$batch_script_file_path.err"
    echo "#SBATCH --nodes=1 # allocation of 1 Node"
    echo "#SBATCH --ntasks=$max_number_cores # allocation of $max_number_cores CPUs"
    echo "#SBATCH --mem-per-cpu=4096 # allocation of 4GB per CPU"
    echo "#SBATCH --time=$timeout # allocation for X hours (hour:minute:second)"
    echo "module purge # unload all loaded modules"
  fi

  echo ""
  return 0
}

#
# Run a batch script using host's infrastructure.
#
_run_batch_script() {
  local USAGE="Usage: ${FUNCNAME[0]} <batch script file patch>"
  if [ "$#" != "1" ]; then
    echo "$USAGE" >&2
    return 1
  fi

  local batch_script_file_path="$1"
  [ -s "$batch_script_file_path" ] || die "[ERROR] $batch_script_file_path does not exist or it is empty!"

  local host_name=$(hostname)
  if [[ $host_name == "iceberg-"* ]] || [[ $host_name == "sharc-"* ]] || [[ $host_name == *".polaris.leeds.ac.uk" ]]; then
    qsub "$batch_script_file_path"
  elif [[ $host_name == *".bob.macc.fct.pt" ]] || [ "$host_name" == "slurmsub.grid.fe.up.pt" ]; then
    sbatch "$batch_script_file_path"
  else
    bash "$batch_script_file_path"
  fi

  return 0
}

# ------------------------------------------------------------------------- Args

USAGE="Usage: ${BASH_SOURCE[0]} \
  --jobs_dir_path <full path> \
  [--seconds_per_job <time in seconds allowed to run each job, e.g., 360>] \
  [--max_number_batches <maximum number of batches (where one batch is composed by many jobs), e.g., 32>] \
  [--max_number_cores <maxinum number of cores per CPU, e.g., 16>] \
  [help]"
if [ "$#" -ne "1" ] && [ "$#" -ne "2" ] && [ "$#" -ne "4" ] && [ "$#" -ne "6" ] && [ "$#" -ne "8" ]; then
  die "$USAGE"
fi

jobs_dir_path=""
seconds_per_job="360" # Assuming an EvoSuite call runs for 180 seconds
max_number_batches="32" # A batch is composed by one or more jobs
max_number_cores="16" # Each job has access to 1 node (i.e., CPU) with 16 cores

while [[ "$1" = --* ]]; do
  OPTION=$1; shift
  case $OPTION in
    (--jobs_dir_path)
      jobs_dir_path=$1;
      shift;;
    (--seconds_per_job)
      seconds_per_job=$1;
      shift;;
    (--max_number_batches)
      max_number_batches=$1;
      shift;;
    (--max_number_cores)
      max_number_cores=$1;
      shift;;
    (--help)
      echo "$USAGE"
      exit 0
    (*)
      die "$USAGE";;
  esac
done

# Check whether all arguments have been initialized
[ "$jobs_dir_path" != "" ]      || die "[ERROR] Missing --jobs_dir_path argument!"
[ "$seconds_per_job" != "" ]    || die "[ERROR] Missing --seconds_per_job argument!"
[ "$max_number_batches" != "" ] || die "[ERROR] Missing --max_number_batches argument!"
[ "$max_number_cores" != "" ]   || die "[ERROR] Missing --max_number_cores argument!"

# Check whether required directories/files do exist
[ -s "$jobs_dir_path" ]    || die "[ERROR] $jobs_dir_path does not exist!"

# ------------------------------------------------------------------------- Main

# Number of jobs (i.e., EvoSuite call per Java class and configuration)
number_of_jobs_to_run=$(find "$jobs_dir_path" -type f -name "job.sh" | wc -l)

# Number of batches that could be executed in parallel, given machine's limits
number_of_jobs_per_batch=$(echo "$number_of_jobs_to_run / $max_number_batches" | bc -l | python -c "import math; print int(math.ceil(float(raw_input())))")

# How much time would a batch require to run all jobs
batch_timeout_in_seconds=$(echo "$number_of_jobs_per_batch / $max_number_cores * $seconds_per_job" | bc -l | python -c "import math; print int(math.ceil(float(raw_input())))")
if [ "$batch_timeout_in_seconds" -lt "$seconds_per_job" ]; then
  batch_timeout_in_seconds="$seconds_per_job"
fi

# Create batches
batch_id=0
count_number_jobs_in_batch=0

for script in $(find "$jobs_dir_path" -type f -name "job.sh" | shuf); do
  if [ "$count_number_jobs_per_batch" -eq "0" ]; then
    # New batch
                  batch_id=$((batch_id+1))
    batch_script_file_path="$jobs_dir_path/batch-$batch_id.sh"
      batch_jobs_file_path="$jobs_dir_path/batch-$batch_id.txt"

    # Init batch file
    _generate_batch_header \
      "$batch_script_file_path" \
      "$batch_timeout_in_seconds" \
      "$max_number_cores"                                                     > "$batch_script_file_path" || die "[ERROR] Failed to init batch file $batch_script_file_path!"
    # Call [GNU Parallel](https://www.gnu.org/software/parallel)
    echo ""                                                                  >> "$batch_script_file_path"
    echo "parallel --progress -j $max_number_cores -a $batch_jobs_file_path" >> "$batch_script_file_path"
    echo "echo \"DONE!\""                                                    >> "$batch_script_file_path"
    echo "exit 0"                                                            >> "$batch_script_file_path"
    echo ""                                                                  >> "$batch_script_file_path"
  fi

  echo "timeout --signal=SIGTERM ${seconds_per_job}s bash $script" >> "$batch_jobs_file_path"
  count_number_jobs_in_batch=$((count_number_jobs_in_batch+1))

  if [ "$count_number_jobs_in_batch" -ge "$number_of_jobs_per_batch" ]; then
    count_number_jobs_in_batch=0 # Reset counter
  fi
done

# Run batches
for batch_script_file_path in $(find "$jobs_dir_path" -mindepth 1 -maxdepth 1 -type f -name "batch-*.sh"); do
  _run_batch_script "$batch_script_file_path" || die "[ERROR] Failed to run $batch_script_file_path!"
done

echo "DONE!"
exit 0

# EOF
