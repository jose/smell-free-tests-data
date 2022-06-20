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
  stop('USAGE: Rscript tuning-friedman-test-tournament.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output tex file, e.g., tuning-data.tex>')
}

# Args
INPUT_FILE      <- args[1]
OUTPUT_TEX_FILE <- args[2]

# ------------------------------------------------------------------------- Main

# Load and pre-process tuning data
df <- load_tuning_data(INPUT_FILE)
print(head(df)) # debug
print(summary(df)) # debug

# Aggregate `df` so that we have coverage, mutation score, and smelliness values per configuration and target class
df <- aggregate(cbind(Size, OverallCoverage, MutationScore, RelativeTestSmellEagerTest, RelativeTestSmellIndirectTesting, RelativeTestSmellObscureInlineSetup, RelativeTestSmellOverreferencing, RelativeTestSmellRottenGreenTests, RelativeTestSmellVerboseTest, RelativeSmelliness) ~ configuration_id + TARGET_CLASS, data=df, FUN=mean, na.rm=TRUE, na.action=NULL)
print(head(df)) # debug

# Set and init tex file
unlink(OUTPUT_TEX_FILE)
sink(OUTPUT_TEX_FILE, append=FALSE, split=TRUE)
# Header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} l r rrr rrr rrrrrr rrr} \\toprule\n', sep='')
cat('              &           & \\multicolumn{3}{c}{Coverage}                                                              & \\multicolumn{3}{c}{Mutation}                                                              & \\multicolumn{6}{c}{} & \\multicolumn{3}{c}{Smelliness} \\\\\n', sep='')
cat('Configuration & \\# Tests & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{Eager Test} & \\multicolumn{1}{c}{Indirect Testing} & \\multicolumn{1}{c}{Obscure Inline Setup} & \\multicolumn{1}{c}{Overreferencing} & \\multicolumn{1}{c}{Rotten Green Tests} & \\multicolumn{1}{c}{Verbose Test} & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} \\\\\n', sep='')
cat('\\midrule\n', sep='')

# Body
for (configuration_id in unique(df$'configuration_id')) {
  mask <- df$'configuration_id' == configuration_id
  cat(pretty_configuration_id(configuration_id), sep='')

  # Number of tests
  cat(' & ', sprintf('%.0f', round(mean(df$'Size'[mask]), 2), sep=''))

  # Coverage
  ci <- get_ci(df$'OverallCoverage'[mask])
  cat(' & ', sprintf('%.2f', round(mean(df$'OverallCoverage'[mask]), 2)),
      ' & ', sprintf('%.2f', round(sd(df$'OverallCoverage'[mask]), 2)),
      ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
      sep='')

  # Mutation
  ci <- get_ci(df$'MutationScore'[mask])
  cat(' & ', sprintf('%.2f', round(mean(df$'MutationScore'[mask]), 2)),
      ' & ', sprintf('%.2f', round(sd(df$'MutationScore'[mask]), 2)),
      ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
      sep='')

  # Individual smells
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeTestSmellEagerTest'[mask]), 2)), sep='')
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeTestSmellIndirectTesting'[mask]), 2)), sep='')
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeTestSmellObscureInlineSetup'[mask]), 2)), sep='')
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeTestSmellOverreferencing'[mask]), 2)), sep='')
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeTestSmellRottenGreenTests'[mask]), 2)), sep='')
  cat(' & ', sprintf('%.2f', round(mean(df$'RelativeTestSmellVerboseTest'[mask]), 2)), sep='')

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
