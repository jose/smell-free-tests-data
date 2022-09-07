# ------------------------------------------------------------------------------
# This script reports (as a tex table) mean values per configuration.
#
# Usage:
#   Rscript tuning-data.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
#     <output tex file, e.g., tuning-data.tex>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript tuning-data.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output tex file, e.g., tuning-data.tex>')
}

# Args
INPUT_FILE      <- args[1]
OUTPUT_TEX_FILE <- args[2]

# ------------------------------------------------------------------------- Main

# Smells to be considered/analyzed, i.e., the ones that could be optimized as a
# secondary criteria
smells <- c(
  'TestSmellEagerTest',
  'TestSmellIndirectTesting',
  'TestSmellObscureInlineSetup',
  'TestSmellOverreferencing',
  'TestSmellRottenGreenTests',
  'TestSmellVerboseTest'
)
relative_smells <- paste0('Relative', smells) # Prefix smell metrics names

# TODO Note, the following code only considers after post-processing smells, one
# might want to consider pre-processing smells

# Load and pre-process tuning data
df <- load_data(INPUT_FILE, smells)
# Remove EvoSuite's default
df <- df[df$'configuration_id' %!in% c('verbose-test'), ]
# Compute smelliness of each test
df <- compute_smelliness(df, smells)
print(head(df)) # debug
print(summary(df)) # debug

# Aggregate `df` so that we have coverage, mutation score, and smelliness values per configuration, target class, and random seed
# Note that at this point `df` is at test case level
df <- aggregate(as.formula(paste0('cbind(Size, OverallCoverage, MutationScore, Smelliness, ', paste0(smells, collapse=','), ') ~ configuration_id + TARGET_CLASS + Random_Seed')), data=df, FUN=mean)
# Compute relative OverallCoverage, MutationScore, Smelliness
df <- compute_relativeness(df, c('OverallCoverage'))
df <- compute_relativeness(df, c('MutationScore'))
df <- compute_relativeness(df, c(smells, 'Smelliness'))
df <- aggregate(as.formula(paste0('cbind(Size, RelativeOverallCoverage, RelativeMutationScore, RelativeSmelliness, ', paste0(relative_smells, collapse=','), ') ~ configuration_id + TARGET_CLASS')), data=df, FUN=mean)

# Set and init tex file
unlink(OUTPUT_TEX_FILE)
sink(OUTPUT_TEX_FILE, append=FALSE, split=TRUE)
# Header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} l r rrr rrr ', paste0(replicate(length(smells), 'r'), collapse=''), ' rrr } \\toprule\n', sep='')
cat('              &           & \\multicolumn{3}{c}{Coverage}                                                              & \\multicolumn{3}{c}{Mutation}                                                              & \\multicolumn{', length(smells), '}{c}{} & \\multicolumn{3}{c}{Smelliness} \\\\\n', sep='')
cat('Configuration & \\# Tests & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI}', sep='')
for (smell in relative_smells) {
  cat(' & \\multicolumn{1}{c}{', gsub("^RelativeTestSmell", '', smell), '}', sep='')
}
cat(' & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} \\\\\n', sep='')
cat('\\midrule\n', sep='')

# Body
for (configuration_id in unique(df$'configuration_id')) {
  mask <- df$'configuration_id' == configuration_id
  cat(pretty_configuration_id_as_abbreviation(configuration_id), sep='')

  # Number of tests
  cat(' & ', sprintf('%.0f', round(mean(df$'Size'[mask]), 2)), sep='')

  # Coverage
  ci <- get_ci(df$'RelativeOverallCoverage'[mask])
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeOverallCoverage'[mask]), 2)),
      ' & ', sprintf('%.2f', round(sd(df$'RelativeOverallCoverage'[mask]), 2)),
      ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
      sep='')

  # Mutation
  ci <- get_ci(df$'RelativeMutationScore'[mask])
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeMutationScore'[mask]), 2)),
      ' & ', sprintf('%.2f', round(sd(df$'RelativeMutationScore'[mask]), 2)),
      ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
      sep='')

  # Individual smells
  for (smell in relative_smells) {
    cat(' & ', sprintf('%.2f', round(mean(df[[smell]][mask]), 2)), sep='')
  }

  # Smelliness
  ci <- get_ci(df$'RelativeSmelliness'[mask])
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeSmelliness'[mask]), 2)),
      ' & ', sprintf('%.2f', round(sd(df$'RelativeSmelliness'[mask]), 2)),
      ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
      sep='')

  cat(' \\\\\n', sep='')
}

# Table's footer
cat('\\bottomrule', '\n', sep='')
cat('\\end{tabular}', '\n', sep='')
sink()

# EOF
