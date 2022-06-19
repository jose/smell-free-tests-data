#!/usr/bin/env bash
#
# ------------------------------------------------------------------------------
# This script creates as many jobs (where each job is the execution of
# EvoSuite on a specific Java project and configuration) as the number of
# projects x number of seeds.
#
# Usage:
# create-jobs.sh
#   [--min_seed <int, e.g., 0>]
#   --max_seed <int, e.g., 30>
#   [--classes_file_path <path, e.g., $SCRIPT_DIR/../../tools/dynamosa-study-projects/data/classes.csv>]
#   --components <path to all components that compose a single job, use ':' to define more than one component, e.g., $SCRIPT_DIR/components/header/hash-bang.txt:$SCRIPT_DIR/components/header/env.txt:$SCRIPT_DIR/components/header/args.txt:$SCRIPT_DIR/components/body/pre-generation.txt:$SCRIPT_DIR/components/body/java-call.txt:$SCRIPT_DIR/components/body/output-variables.txt:$SCRIPT_DIR/components/body/vanilla.txt:$SCRIPT_DIR/components/footer/post-generation.txt:$SCRIPT_DIR/components/footer/check-csv.txt:$SCRIPT_DIR/components/footer/the-end.txt>
#   --output_dir_path <full path, e.g., $SCRIPT_DIR/../data/generated/experiment-1>
#   [help]
#
# Requirements:
#   Execution of ../../tools/get-tools.sh script.
# ------------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"

#
# Print error message to the stdout and exit.
#
die() {
  echo "$@" >&2
  exit 1
}

# ------------------------------------------------------------------------- Envs

# Check whether Java 8 has been installed and it is available
JAVA_HOME="$SCRIPT_DIR/../../tools/jdk-8"
[ -d "$JAVA_HOME" ] || die "[ERROR] $JAVA_HOME does not exist! Did you run $SCRIPT_DIR/../../tools/get-tools.sh?"

# Check whether the benchmark is available
BENCHMARK_DIR_PATH="$SCRIPT_DIR/../../tools/dynamosa-study-projects/subjects"
[ -d "$BENCHMARK_DIR_PATH" ] || die "[ERROR] $BENCHMARK_DIR_PATH does not exist! Did you run $SCRIPT_DIR/../../tools/get-tools.sh?"

# Check whether the EvoSuite's jar file is available
EVOSUITE_JAR_FILE_PATH="$SCRIPT_DIR/../../tools/evosuite.jar"
[ -s "$EVOSUITE_JAR_FILE_PATH" ] || die "[ERROR] $EVOSUITE_JAR_FILE_PATH does not exist or it is an empty file! Did you run $SCRIPT_DIR/../../tools/get-tools.sh?"

# ------------------------------------------------------------------------- Args

USAGE="Usage: ${BASH_SOURCE[0]} \
  [--min_seed <int, e.g., 0>] \
  --max_seed <int, e.g., 30> \
  [--classes_file_path <path, e.g., $SCRIPT_DIR/../../tools/dynamosa-study-projects/data/classes.csv>] \
  --components <path to all components that compose a single job, use ':' to define more than one component, e.g., $SCRIPT_DIR/components/header/hash-bang.txt:$SCRIPT_DIR/components/header/env.txt:$SCRIPT_DIR/components/header/args.txt:$SCRIPT_DIR/components/body/pre-generation.txt:$SCRIPT_DIR/components/body/java-call.txt:$SCRIPT_DIR/components/body/output-variables.txt:$SCRIPT_DIR/components/body/vanilla.txt:$SCRIPT_DIR/components/footer/post-generation.txt:$SCRIPT_DIR/components/footer/check-csv.txt:$SCRIPT_DIR/components/footer/the-end.txt> \
  --output_dir_path <full path, e.g., $SCRIPT_DIR/../data/generated/experiment-1> \
  [help]"
if [ "$#" -ne "1" ] && [ "$#" -ne "6" ] && [ "$#" -ne "8" ] && [ "$#" -ne "10" ]; then
  die "$USAGE"
fi

min_seed="0"
max_seed=""
classes_file_path="$SCRIPT_DIR/../../tools/dynamosa-study-projects/data/classes.csv"
components=""
output_dir_path=""

while [[ "$1" = --* ]]; do
  OPTION=$1; shift
  case $OPTION in
    (--min_seed)
      min_seed=$1;
      shift;;
    (--max_seed)
      max_seed=$1;
      shift;;
    (--classes_file_path)
      classes_file_path=$1;
      shift;;
    (--components)
      components=$1;
      shift;;
    (--output_dir_path)
      output_dir_path=$1;
      shift;;
    (--help)
      echo "$USAGE"
      exit 0
    (*)
      die "$USAGE";;
  esac
done

# Check whether all arguments have been initialized
[ "$min_seed" != "" ]          || die "[ERROR] Missing --min_seed argument!"
[ "$max_seed" != "" ]          || die "[ERROR] Missing --max_seed argument!"
[ "$classes_file_path" != "" ] || die "[ERROR] Missing --classes_file_path argument!"
[ "$components" != "" ]        || die "[ERROR] Missing --components argument!"
[ "$output_dir_path" != "" ]   || die "[ERROR] Missing --output_dir_path argument!"

# Check whether required directories/files do exist
[ -s "$classes_file_path" ]    || die "[ERROR] $classes_file_path does not exist or it is empty!"
for component in $(echo "$components" | tr ':' ' '); do
  [ -s "$component" ] || die "[ERROR] $component does not exist or it is empty!"
done

# ------------------------------------------------------------------------- Main

rm -rf "$output_dir_path"
mkdir -p "$output_dir_path" || die "[ERROR] Failed to create $output_dir_path!"

# Create experiment's directories
           reports_dir_path="$output_dir_path/reports"
             tests_dir_path="$output_dir_path/tests"
              logs_dir_path="$output_dir_path/logs"
              jobs_dir_path="$output_dir_path/jobs"
master_job_script_file_path="$jobs_dir_path/master-job.sh"
mkdir -p "$reports_dir_path" "$tests_dir_path" "$logs_dir_path" "$jobs_dir_path"
touch "$master_job_script_file_path"

# Create master job script based on the defined components
for component in $(echo "$components" | tr ':' ' '); do
  cat "$component" >> "$master_job_script_file_path"
done

# Create jobs
while read -r row; do
  project=$(echo "$row" | cut -f1 -d',')
    class=$(echo "$row" | cut -f2 -d',')

  for seed in $(seq "$min_seed" "$max_seed"); do
     job_report_dir_path="$reports_dir_path/$project/$class/$seed"
       job_test_dir_path="$tests_dir_path/$project/$class/$seed"
        job_log_dir_path="$logs_dir_path/$project/$class/$seed"
       job_log_file_path="$job_log_dir_path/job.log"
     job_script_dir_path="$jobs_dir_path/$project/$class/$seed"
    job_script_file_path="$job_script_dir_path/job.sh"

    mkdir -p "$job_report_dir_path" "$job_test_dir_path" "$job_log_dir_path" "$job_script_dir_path"
    touch "$job_log_file_path" "$job_script_file_path"

    echo "#!/usr/bin/env bash"                                                    > "$job_script_file_path"
    echo "#"                                                                     >> "$job_script_file_path"
    echo "export JAVA_HOME=\"$JAVA_HOME\""                                       >> "$job_script_file_path"
    echo "export MALLOC_ARENA_MAX=1 # Iceberg's requirement"                     >> "$job_script_file_path"
    echo "export _JAVA_OPTIONS=\"-Xmx4096M -XX:MaxHeapSize=1024M\""              >> "$job_script_file_path"
    echo "export LC_ALL=en_US.UTF-8"                                             >> "$job_script_file_path"
    echo "export LANG=en_US.UTF-8"                                               >> "$job_script_file_path"
    echo "export LANGUAGE=en_US.UTF-8"                                           >> "$job_script_file_path"
    echo "bash $master_job_script_file_path \
  --project_dir_path \"$BENCHMARK_DIR_PATH/$project\" \
  --project \"$project\" \
  --class \"$class\" \
  --seed \"$seed\" \
  --report_dir_path \"$job_report_dir_path\" \
  --test_dir_path \"$job_test_dir_path\" \
  --evosuite_jar_path \"$EVOSUITE_JAR_FILE_PATH\" > \"$job_log_file_path\" 2>&1" >> "$job_script_file_path"

  done
done < <(tail -n +2 "$classes_file_path")

echo "DONE!"
exit 0

# EOF
