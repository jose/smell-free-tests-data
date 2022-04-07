#!/usr/bin/env python
#
# ------------------------------------------------------------------------------
# Util functions for test-generation experiments.
# ------------------------------------------------------------------------------

import math
import sys
import shutil
import re
import random
import getpass
import os

#
# EvoSuite parameters
#
def getEvoSuiteParameters(measure_smell_timelines=False):
  execution_variables="configuration_id,group_id,Random_Seed,TARGET_CLASS,Size,Result_Size,Length,Result_Length,search_budget,Total_Time,criterion,Statements_Executed,Tests_Executed"
  ga_variables="algorithm,Fitness_Evaluations,Generations,population,mutation_rate,crossover_function,crossover_rate,selection_function,rank_bias,tournament_size,elite"
  goals_variables="Total_Goals,Coverage,CoverageBitString,Lines,Covered_Lines,LineCoverage,LineCoverageBitString,Total_Branches,Covered_Branches,BranchCoverage,BranchCoverageBitString,ExceptionCoverage,ExceptionCoverageBitString,Mutants,WeakMutationScore,WeakMutationCoverageBitString,OutputCoverage,OutputCoverageBitString,Total_Methods,MethodCoverage,MethodCoverageBitString,MethodNoExceptionCoverage,MethodNoExceptionCoverageBitString,CBranchCoverage,CBranchCoverageBitString,MutationScore,MutationCoverageBitString"
  smeel_variables="TestSmellAssertionRoulette,TestSmellBrittleAssertion,TestSmellDuplicateAssert,TestSmellEagerTest,TestSmellEmptyTest,TestSmellIndirectTesting,TestSmellLackOfCohesionOfMethods,TestSmellLazyTest,TestSmellLikelyIneffectiveObjectComparison,TestSmellMysteryGuest,TestSmellObscureInlineSetup,TestSmellOverreferencing,TestSmellRedundantAssertion,TestSmellResourceOptimism,TestSmellRottenGreenTests,TestSmellSensitiveEquality,TestSmellSlowTests,TestSmellTestCodeDuplication,TestSmellUnknownTest,TestSmellUnusedInputs,TestSmellVerboseTest"
  line_variables="LineFitnessTimeline,LineCoverageTimeline"
  branch_variables="BranchCoverageTimeline"
  exception_variables="ExceptionFitnessTimeline,ExceptionCoverageTimeline,TotalExceptionsTimeline"
  weakmutation_variables="WeakMutationCoverageTimeline"
  output_variables="OutputFitnessTimeline,OutputCoverageTimeline"
  method_variables="MethodFitnessTimeline,MethodCoverageTimeline"
  methodnoexception_variables="MethodNoExceptionFitnessTimeline,MethodNoExceptionCoverageTimeline"
  cbranch_variables="CBranchFitnessTimeline,CBranchCoverageTimeline"
  # Extra ones
  extra_timeline_variables="CoverageTimeline,FitnessTimeline,SizeTimeline,LengthTimeline"
  # Smelly ones
  smell_timeline_variables="TestSmellAssertionRouletteTimeline,TestSmellBrittleAssertionTimeline,TestSmellDuplicateAssertTimeline,TestSmellEagerTestTimeline,TestSmellEmptyTestTimeline,TestSmellIndirectTestingTimeline,TestSmellLackOfCohesionOfMethodsTimeline,TestSmellLazyTestTimeline,TestSmellLikelyIneffectiveObjectComparisonTimeline,TestSmellMysteryGuestTimeline,TestSmellObscureInlineSetupTimeline,TestSmellOverreferencingTimeline,TestSmellRedundantAssertionTimeline,TestSmellResourceOptimismTimeline,TestSmellRottenGreenTestsTimeline,TestSmellSensitiveEqualityTimeline,TestSmellSlowTestsTimeline,TestSmellTestCodeDuplicationTimeline,TestSmellUnknownTestTimeline,TestSmellUnusedInputsTimeline,TestSmellVerboseTestTimeline"

  # By default, all but smell timeline variables are considered
  output_variables="%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s" %(execution_variables, ga_variables, goals_variables, smeel_variables, line_variables, branch_variables, exception_variables, weakmutation_variables, output_variables, method_variables, methodnoexception_variables, cbranch_variables, extra_timeline_variables)
  if measure_smell_timelines: # If it is to measure smell timeline variables
    output_variables += ",%s" %(smell_timeline_variables)

  parameters = " -mem 4096 \
    -Dshow_progress=false \
    -Duse_deprecated=true \
    -Dp_functional_mocking=0.8 \
    -Dp_reflection_on_private=0.5 \
    -Dcriterion=\"LINE:BRANCH:EXCEPTION:WEAKMUTATION:OUTPUT:METHOD:METHODNOEXCEPTION:CBRANCH\" \
    -Danalysis_criteria=\"LINE,BRANCH,EXCEPTION,WEAKMUTATION,OUTPUT,METHOD,METHODNOEXCEPTION,CBRANCH,STRONGMUTATION\" \
    -Dtimeline_interval=1000 \
    -Doutput_variables=\"%s\" \
    -Dminimization_timeout=600 \
    -Dassertion_timeout=600 \
    -Djunit_tests=true \
    -Djunit_check=false \
    -Dsave_all_data=false \
    -Dglobal_timeout=600 \
    -Dextra_timeout=600" %(output_variables)

  return parameters

#
# https://www.sheffield.ac.uk/wrgrid/iceberg or https://www.sheffield.ac.uk/cics/research/hpc/sharc
#
def icebergSharcHeader(script_path):
  header=""
  header+="#$ -l h_rt=08:00:00\n" # 168 ?
  header+="#$ -l rmem=4G\n"
  header+="#$ -e %s\n" %(script_path + ".err")
  header+="#$ -o %s\n" %(script_path + ".out")
  return header

#
# https://n8hpc.org.uk
#
def n8Header(script_path):
  header=""
  header+="#$ -l h_rt=8:00:00\n" # 168 ?
  header+="#$ -l h_vmem=4G\n"
  header+="#$ -pe smp 2\n"
  header+="#$ -e %s\n" %(script_path + ".err")
  header+="#$ -o %s\n" %(script_path + ".out")
  return header

#
# https://macc.fccn.pt
# https://docs.macc.fccn.pt
# https://grid.fe.up.pt/dokuwiki/doku.php?id=documentation
#
def maccHeader(script_path):
  # header=""
  # header+="#SBATCH --job-name=%s                 #\n" %(script_path[script_path.rfind("/")+1:])
  # header+="#SBATCH --output=%s                   #\n" %(script_path + ".out")
  # header+="#SBATCH --error=%s                    #\n" %(script_path + ".err")
  # header+="#SBATCH --nodes=1                     # allocation of 1 Node\n"
  # header+="#SBATCH --ntasks=16                   # allocation of 16 CPUs\n"
  # header+="#SBATCH --time=08:00:00               # allocation for 8 hours (hour:minute:second)\n"
  # header+="#SBATCH --mem-per-cpu=4096            # allocation of 4GB per CPU\n"
  # header+="module purge                          # unload all loaded modules\n"
  # return header
  return localhost(script_path)

#
# "Normal" machine
#
def localhost(script_path):
  header=""
  header+="\n"
  return header

#
#
#
def hostHeader(HOSTNAME, script_path):
  if HOSTNAME == "iceberg" or HOSTNAME == "sharc":
    return icebergSharcHeader(script_path)
  elif HOSTNAME == "n8":
    return n8Header(script_path)
  elif HOSTNAME == "macc" or HOSTNAME == "feup-grid":
    return maccHeader(script_path)
  elif HOSTNAME == "localhost":
    return localhost(script_path)
  else:
    raise Exception("Host not supported!")

#
#
#
def jobHeader(script_path, SCRIPT_DIR, JAVA_HOME, PROJECTS_DIR, HOSTNAME):
  header="#!/usr/bin/env bash\n"
  header+=hostHeader(HOSTNAME, script_path)
  header+="source \"%s/utils.sh\" || exit 1\n" % (SCRIPT_DIR)
  header+="export JAVA_HOME=\"%s\"\n" % (JAVA_HOME)
  header+="export PROJECTS_DIR=\"%s\"\n" %(PROJECTS_DIR)
  header+="export PATH=\"$JAVA_HOME/bin:$PATH\"\n"
  header+="\n"

  header += "__check_csv_file() {\n"
  header += "  local csv_file=$1\n"
  header += "  echo \"[INFO] Double-checking the .csv file created by EvoSuite\"\n"
  # does EvoSuite's csv file exit?
  header += "  if [ ! -f \"$csv_file\" ]; then\n"
  header += "    echo \"[ERROR] '$csv_file' file not found!\"\n"
  header += "    return 1;\n"
  header += "  fi\n"
  # is it empty?
  header += "  if [ ! -s \"$csv_file\" ]; then\n"
  header += "    echo \"[ERROR] '$csv_file' file is empty!\"\n"
  header += "    return 1;\n"
  header += "  fi\n"
  # does it have 2 rows?
  header += "  num_rows_evosuite_stats_file=$(cat \"$csv_file\" | wc -l)\n"
  header += "  if [ \"$num_rows_evosuite_stats_file\" -lt \"2\" ]; then\n"
  header += "    echo \"[ERROR] '$csv_file' does not have at least 2 rows!\"\n"
  header += "    return 1;\n"
  header += "  fi\n"
  header += "  return 0;\n"
  header += "}\n"

  header += "\n\n"
  return header

#
#
#
def createEvoSuiteCall(LOGS_DIR, REPORTS_DIR, TESTS_DIR, PROJECTS_DIR, EVOSUITE_JAR,
                        seed, configId, extra_parameters, project_name, class_name,
                        measure_smell_timelines):
  log_dir="%s/%s/%s/%s/%d" % (LOGS_DIR, configId, project_name, class_name, seed)
  os.makedirs(log_dir)
  log_file="%s/log.txt" % (log_dir)
  open(log_file, 'w') # placeholder log file

  report_dir="%s/%s/%s/%s/%d" % (REPORTS_DIR, configId, project_name, class_name, seed)
  os.makedirs(report_dir)

  test_dir="%s/%s/%s/%s/%d" % (TESTS_DIR, configId, project_name, class_name, seed)
  os.makedirs(test_dir)

  evosuite_parameters = getEvoSuiteParameters(measure_smell_timelines)

  result = "echo \"PID: $$\" > %s 2>&1\n" %(log_file)
  result += "hostname >> %s 2>&1\n" %(log_file)
  result += "echo \"[INFO] Generating test cases for %s::%s\" >> %s 2>&1\n\n" %(project_name, class_name, log_file)

  # To make sure no re-execution will populate the 'statistics.csv' with more than
  # one data row, clean up the 'statistics.csv' file (if any) before attempting to
  # run EvoSuite and generate tests
  result += "rm -f %s/statistics.csv >> %s 2>&1\n" %(report_dir, log_file)

  result += "pushd . > /dev/null 2>&1\n"
  result += "cd %s/%s\n" %(PROJECTS_DIR, project_name)

  result += "java -Djava.io.tmpdir=%s -Xmx512M -jar %s" %(log_dir, EVOSUITE_JAR)
  result += " -seed %d" %(seed)
  result += " -Dconfiguration_id=%s" %(configId)
  result += " -Dgroup_id=%s" %(project_name)
  result += " -Dreport_dir=%s" %(report_dir)
  result += " -Dtest_dir=%s" %(test_dir)
  result += " -class %s" %(class_name)
  result += " "+evosuite_parameters
  result += " "+extra_parameters
  if project_name == "110_firebird":
    result += " -libraryPath=%s/%s/native" %(PROJECTS_DIR, project_name)
  elif project_name == "27_gangup":
    result += " -libraryPath=%s/%s/native/linux-amd64" %(PROJECTS_DIR, project_name)
  result += " >> %s 2>&1\n" %(log_file)

  result += "popd > /dev/null 2>&1\n\n"

  result += "__check_csv_file %s/statistics.csv >> %s 2>&1;\n" %(report_dir, log_file)
  result += "if [ $? -eq 0 ]; then\n"
  result += "  echo \"\" >> %s 2>&1\n" %(log_file)
  result += "  echo \"DONE!\" >> %s 2>&1\n" %(log_file)
  result += "fi\n"

  result += "\n"
  return result

# EOF
