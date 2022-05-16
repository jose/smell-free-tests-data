# ------------------------------------------------------------------------------
# This script reports the mean values of some columns.
#
# Usage:
#   Rscript mean-values.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
# TODO ?

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop('USAGE: Rscript mean-values.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz>')
}

# Args
INPUT_FILE <- args[1]

# ------------------------------------------------------------------------- Main

coverage_columns <- c(
  'LineCoverage',
  'BranchCoverage',
  'ExceptionCoverage',
  'WeakMutationScore',
  'OutputCoverage',
  'MethodCoverage',
  'MethodNoExceptionCoverage',
  'CBranchCoverage',
  'MutationScore',
  'OverallCoverage'
)

# Load data
df                     <- load_TABLE(INPUT_FILE)
# Compute overall coverage
df$'OverallCoverage'   <- (df$'LineCoverage' +
                           df$'BranchCoverage' +
                           df$'ExceptionCoverage' +
                           df$'WeakMutationScore' +
                           df$'OutputCoverage' +
                           df$'MethodCoverage' +
                           df$'MethodNoExceptionCoverage' +
                           df$'CBranchCoverage') / 8.0
# Collect smelly columns
all_smell_columns      <- grep(pattern='^TestSmell', x=colnames(df), value=TRUE)
timeline_smell_columns <- grep(pattern='^TestSmell.*Timeline_T', x=colnames(df), value=TRUE)
smell_columns          <- setdiff(all_smell_columns, timeline_smell_columns)
# Select relevant columns for this script
df                     <- df[ , which(colnames(df) %in% c('configuration_id', 'group_id', 'TARGET_CLASS', smell_columns, coverage_columns)) ]
# Aggregate at class under test level
df                     <- aggregate(x=as.formula(paste('cbind(', paste(c(smell_columns, coverage_columns), collapse=','), ') ~ configuration_id + group_id + TARGET_CLASS', sep=' ')), data=df, FUN=mean)

# Summary per configuration
for (configuration_id in unique(df$'configuration_id')) {
  cat('\n\n--- ', pretty_configuration_id(configuration_id), ' ---\n', sep='')
  print(summary(df[df$'configuration_id' == configuration_id, ]))
}

# EOF
