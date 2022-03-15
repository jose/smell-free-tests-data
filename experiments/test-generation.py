#!/usr/bin/env python
#
# ------------------------------------------------------------------------------
# This script creates as many .sh files as the number of classes * number of
# different configurations to automatically generate test cases.  Each .sh file
# contains the necessary code to call the generation tool.
#
# Usage:
# test-generation.py <exps dir> <max num jobs> <min seed> <max seed> <seconds per call> <projects dir> <classes file>
#
# Parameters:
#   <EXPS DIR>      Directory to which test cases will be generated
#   <MAX NUM JOBS>  Maximum number of jobs allowed by the host
#   <MIN SEED>      Min seed
#   <MAX SEED>      Max seed
#   <SEARCH BUDGET> Amount of seconds EvoSuite's search is allowed to run
#   <PROJECTS DIR>  Directory with projects under test
#   <CLASSES FILE>  File with all classes under test
#
# Environment variables:
# - JAVA_HOME     Needs to be set and must point to the Java-8 installation.
# - EVOSUITE_JAR  Path to EvoSuite.jar file.
# - HOSTNAME      Host name: sharc, iceberg, n8, macc, feup-grid, or localhost.
# ------------------------------------------------------------------------------

from utils import *

SRC=os.path.dirname(sys.argv[0])
USERNAME=getpass.getuser()

SCRIPT_DIR=os.environ['SCRIPT_DIR']
JAVA_HOME=os.environ['JAVA_HOME']
EVOSUITE_JAR=os.environ['EVOSUITE_JAR']
HOSTNAME=os.environ['HOSTNAME']

if len(sys.argv) != 8:
  print("Usage:\ntest-generation.py <exps dir> <max num jobs> <min seed> <max seed> <search budget> <projects dir> <classes file>")
  exit(1)

#
# Exps dir
#

BASEDIR = os.path.abspath(sys.argv[1])
if not os.path.isdir(BASEDIR):
  os.makedirs(BASEDIR)
else:
  print("[ERROR] Target folder already exists!")
  exit(1)

#
# Max JOBS
#

MAX_JOBS = int(sys.argv[2])

#
# Seeds
#

MINSEED = int(sys.argv[3])
MAXSEED = int(sys.argv[4])

#
# Time
#

SEARCH_BUDGET = int(sys.argv[5])

#
# Projects
#

PROJECTS_DIR=sys.argv[6]
if not os.path.isdir(PROJECTS_DIR):
  print('[ERROR] Could not find projects directory ' + PROJECTS_DIR)
  exit(1)

#
# Classes
#

CLASSES_FILE=sys.argv[7]
if not os.path.isfile(CLASSES_FILE):
  print('[ERROR] Could not find classes file ' + CLASSES_FILE)
  exit(1)

CLASSES = []
with open(CLASSES_FILE) as f:
  next(f) # to skip header
  for line in f:
    entry = line.rstrip().split('\t')
    CLASSES.append(entry)
  f.close()
NUM_CLASSES=len(CLASSES)

#
# Where to put stuff (default in subdirs of BASEDIR)
#

REPORTS_DIR="%s/reports" % BASEDIR
SCRIPTS_DIR="%s/scripts" % BASEDIR
LOGS_DIR="%s/logs" % BASEDIR
TESTS_DIR="%s/tests" % BASEDIR
os.makedirs(REPORTS_DIR)
os.makedirs(SCRIPTS_DIR)
os.makedirs(LOGS_DIR)
os.makedirs(TESTS_DIR)

#
# Global counter of jobs created
#

JOB_ID=1
CONFIG_ID=0
ENTRIES_PER_JOB=0

#
# Creates the scripts for a given config and seed range
#
def createJobs(minSeed, maxSeed, configId, extra_parameters):
  global USERNAME
  global SCRIPT_DIR
  global SCRIPTS_DIR
  global JAVA_HOME
  global PROJECTS_DIR
  global JOB_ID
  global CLASSES
  global ENTRIES_PER_JOB
  global MAX_ENTRIES_PER_JOB
  global LOGS_DIR
  global REPORTS_DIR
  global TESTS_DIR
  global EVOSUITE_JAR
  global CONFIG_ID
  global HOSTNAME

  script_path = "%s/%s_%d.sh" %(SCRIPTS_DIR, USERNAME, JOB_ID)
  if os.path.isfile(script_path):
    script=open(script_path, "a")
  else:
    script=open(script_path, "w")
    script.write(jobHeader(script_path, SCRIPT_DIR, JAVA_HOME, PROJECTS_DIR, HOSTNAME))

  for seed in range(minSeed, maxSeed):
    # important if cluster gives issues
    random.shuffle(CLASSES)

    # entry : project_name,class_name
    for entry in CLASSES:
      if ENTRIES_PER_JOB >= MAX_ENTRIES_PER_JOB:
        script.write("\n\n")
        script.close()

        JOB_ID +=1
        ENTRIES_PER_JOB = 1

        script_path = "%s/%s_%d.sh" %(SCRIPTS_DIR, USERNAME, JOB_ID)
        script=open(script_path, "w")
        script.write(jobHeader(script_path, SCRIPT_DIR, JAVA_HOME, PROJECTS_DIR, HOSTNAME))
      else:
        ENTRIES_PER_JOB += 1

      script.write(createEvoSuiteCall(LOGS_DIR, REPORTS_DIR, TESTS_DIR, PROJECTS_DIR, EVOSUITE_JAR, seed, configId, extra_parameters, entry[0], entry[1]))
  script.close()

  CONFIG_ID += 1

  return

# ------------------------------------------------------- Create the actual jobs

# How many calls to EvoSuite should go in one script
N_CONF = 1 # FIXME change it, if we have more/less configurations
MAX_ENTRIES_PER_JOB = math.ceil( (N_CONF * (NUM_CLASSES * (MAXSEED - MINSEED)) / float(MAX_JOBS) ) )

# ----- Configurations

createJobs(MINSEED, MAXSEED, "vanilla", " -Dsearch_budget=%d \
    -Dsecondary_objectives=\"TOTAL_LENGTH\" \
    -Dinline=true \
    -Dminimize=true \
    -Dassertions=true \
    -Dtest_smell_optimization=false " %(SEARCH_BUDGET))

# ----- Stats

print("Seeds: %d, classes: %d, configs: %d" % ((MAXSEED - MINSEED), NUM_CLASSES, CONFIG_ID))
print("Total number of jobs created: %d" % JOB_ID)
print("Calls per job: %d" % MAX_ENTRIES_PER_JOB)

# EOF
