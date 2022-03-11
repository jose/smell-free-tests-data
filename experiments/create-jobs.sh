#!/usr/bin/env bash
#
# ------------------------------------------------------------------------------
# This script creates and submits as many jobs as the number of classes * number
# of different configurations to automatically generate test cases.
#
# Usage:
# create-jobs.sh
#   --experiments_data_dir <path>
#   --max_num_jobs <int>
#   --min_seed <int, e.g., 0>
#   --max_seed <int, e.g., 30>
#   --projects_dir <path, e.g., ../tools/dynamosa-study-classes>
#   --classes_file <path, e.g., ../tools/dynamosa-study-classes/data/classes.csv>
#   [help]
#
# Requirements:
#   Execution of tools/get_tools.sh script.
# ------------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source "$SCRIPT_DIR/../utils/experiments/utils.sh" || exit 1

export SCRIPT_DIR="$SCRIPT_DIR"

# ------------------------------------------------------------------------- Envs

# Check whether JAVA_HOME exists
JAVA_HOME="$SCRIPT_DIR/../tools/jdk-8"
if [ ! -d "$JAVA_HOME" ]; then
  die "Could not find java 'jdk-8' directory! Did you run 'tools/get_tools.sh'?"
else
  export JAVA_HOME="$JAVA_HOME"
fi

# Check whether EVOSUITE_JAR exists
EVOSUITE_JAR="$SCRIPT_DIR/../tools/evosuite.jar"
if [ ! -s "$EVOSUITE_JAR" ]; then
  die "Could not find 'evosuite.jar' file! Did you run 'tools/get_tools.sh'?"
else
  export EVOSUITE_JAR="$EVOSUITE_JAR"
fi

# Check whether script has been executed in the right environment
_can_I_run_jobs_simultaneously || die "Scripts are optimized to run on clusters with a SGE system or a machine with gnu-parallel. Please make sure it is the case."

HOSTNAME=$(_get_host_name)
if [ "$?" -ne "0" ]; then
  die "Host '$HOSTNAME' not supported"
fi
export HOSTNAME="$HOSTNAME"

# ------------------------------------------------------------------------- Args

USAGE="Usage: ${BASH_SOURCE[0]} --experiments_data_dir <path> --max_num_jobs <int> --min_seed <int, e.g., 0> --max_seed <int, e.g., 30> --classes_file <path, e.g., ../dynamosa-study-classes-support/dynamosa-selection-training.txt or ../dynamosa-study-classes-support/dynamosa-selection-test.txt> [help]"
if [ "$#" -ne "1" ] && [ "$#" -ne "12" ]; then
  die "$USAGE"
fi

EXPERIMENTS_DATA_DIR=""
MAX_NUM_JOBS=""
MIN_SEED=""
MAX_SEED=""
PROJECTS_DIR=""
CLASSES_FILE=""

while [[ "$1" = --* ]]; do
  OPTION=$1; shift
  case $OPTION in
    (--experiments_data_dir)
      EXPERIMENTS_DATA_DIR=$1;
      shift;;
    (--max_num_jobs)
      MAX_NUM_JOBS=$1;
      shift;;
    (--min_seed)
      MIN_SEED=$1;
      shift;;
    (--max_seed)
      MAX_SEED=$1;
      shift;;
    (--projects_dir)
      PROJECTS_DIR=$1;
      shift;;
    (--classes_file)
      CLASSES_FILE=$1;
      shift;;
    (--help)
      echo "$USAGE"
      exit 0
    (*)
      die "$USAGE";;
  esac
done

[ "$EXPERIMENTS_DATA_DIR" != "" ] || die "[ERROR] Missing --experiments_data_dir argument!"
[ "$MAX_NUM_JOBS" != "" ]         || die "[ERROR] Missing --max_num_jobs argument!"
[ "$MIN_SEED" != "" ]             || die "[ERROR] Missing --min_seed argument!"
[ "$MAX_SEED" != "" ]             || die "[ERROR] Missing --max_seed argument!"
[ "$PROJECTS_DIR" != "" ]         || die "[ERROR] Missing --projects_dir argument!"
[ -d "$PROJECTS_DIR" ]            || die "[ERROR] '$PROJECTS_DIR' does not exist!"
[ "$CLASSES_FILE" != "" ]         || die "[ERROR] Missing --classes_file argument!"
[ -s "$CLASSES_FILE" ]            || die "[ERROR] '$CLASSES_FILE' does not exist or it is empty!"

rm -rf "$EXPERIMENTS_DATA_DIR"
EXPERIMENTS_SCRIPTS_DIR="$EXPERIMENTS_DATA_DIR/scripts"

# ------------------------------------------------------------------------- Main

python "test-generation.py" \
  "$EXPERIMENTS_DATA_DIR" \
  "$MAX_NUM_JOBS" \
  "$MIN_SEED" \
  "$MAX_SEED" \
  "$PROJECTS_DIR" \
  "$CLASSES_FILE" || die "[ERROR] test_generation.py returned an error!"

unset SCRIPT_DIR # remove from env

pushd . > /dev/null 2>&1
cd "$EXPERIMENTS_SCRIPTS_DIR"
  if [ "$HOSTNAME" == "macc" ] || [ "$HOSTNAME" == "feup-grid" ]; then
    num_gen_job_scripts=$(find . -mindepth 1 -maxdepth 1 -type f -name "*.sh" | wc -l)
    max_num_bashs_per_job_script=$(echo "$num_gen_job_scripts / 32" | bc -l | python -c "import math; print int(math.ceil(float(raw_input())))")

    seconds=$(printf '%d\n' $((max_num_bashs_per_job_script/16*3*60)))
    timeout=$(printf '%02d:%02d:%02d\n' $((seconds/3600)) $((seconds%3600/60)) $((seconds%60)))

    job_id=0
    count_num_bash_per_job_script=0
    for script in $(find . -mindepth 1 -maxdepth 1 -type f -name "*.sh" | shuf); do
      if [ "$count_num_bash_per_job_script" -eq "0" ]; then
        # new job
        job_id=$((job_id+1))
        job_script_file="$(pwd)/gnu-parallel-jobs-$job_id.sh"
         job_calls_file="$(pwd)/gnu-parallel-jobs-$job_id.txt"

        echo "#!/usr/bin/env bash"                                                    > "$job_script_file"
        echo "#SBATCH --job-name=j-$job_id"                                          >> "$job_script_file"
        echo "#SBATCH --output=$job_script_file.out"                                 >> "$job_script_file"
        echo "#SBATCH --error=$job_script_file.err"                                  >> "$job_script_file"
        echo "#SBATCH --nodes=1 # allocation of 1 Node"                              >> "$job_script_file"
        echo "#SBATCH --ntasks=16 # allocation of 16 CPUs"                           >> "$job_script_file"
        echo "#SBATCH --time=$timeout # allocation for X hours (hour:minute:second)" >> "$job_script_file"
        echo "#SBATCH --account=cpca_a0_7402_2020 # CPCA/A0/7402/2020"               >> "$job_script_file"
        echo "module purge # unload all modules"                                     >> "$job_script_file"
        echo "module load gcc/7.4.0 parallel/20190222 # load GNU Parallel"           >> "$job_script_file"
        echo ""                                                                      >> "$job_script_file"
        echo "parallel --progress -j 16 -a $job_calls_file"                          >> "$job_script_file"
        echo "echo \"DONE!\""                                                        >> "$job_script_file"
        echo "exit 0"                                                                >> "$job_script_file"
      fi

      echo "bash $(pwd)/$script" >> "$job_calls_file"
      count_num_bash_per_job_script=$((count_num_bash_per_job_script+1))

      if [ "$count_num_bash_per_job_script" -ge "$max_num_bashs_per_job_script" ]; then
        count_num_bash_per_job_script=0
      fi
    done

    for script in $(find . -mindepth 1 -maxdepth 1 -type f -name "gnu-parallel-jobs-*.sh"); do
      _submit_job_script "$script" || die "[ERROR] Failed to submit '$script'!"
    done

  else
    for script in $(find . -mindepth 1 -maxdepth 1 -type f -name "*.sh"); do
      if [ "$HOSTNAME" == "localhost" ]; then
        jobs_file="$(pwd)/gnu-parallel-jobs.txt"
        echo "bash $(pwd)/$script" >> "$jobs_file"
      else
        _submit_job_script "$script" || die "[ERROR] Failed to submit '$script'!"
      fi
    done
  fi
popd > /dev/null 2>&1

if [ "$HOSTNAME" == "localhost" ]; then
  echo "All jobs have been created! To now run jobs in parallel execute the following command:"
  echo "nohup parallel --progress -j \$(cat /proc/cpuinfo | grep 'cpu cores' | sort -u | cut -f2 -d':' | cut -f2 -d' ') -a $EXPERIMENTS_SCRIPTS_DIR/gnu-parallel-jobs.txt >> $EXPERIMENTS_SCRIPTS_DIR/gnu-parallel-jobs.txt.log 2>&1 &"
else
  echo "All jobs have been created and submitted!"
fi

# EOF
