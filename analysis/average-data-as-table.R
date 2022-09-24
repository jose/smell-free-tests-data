# ------------------------------------------------------------------------------
# This script reports (as a tex table) mean values of number of tests, number of
# statements in each test, coverage, mutation, optimized smells, and smelliness
# per configuration presented in the given data file.
#
# Usage:
#   Rscript average-data-as-table.R
#     <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz>
#     <output tex file, e.g., experiments-data.tex>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript average-data-as-table.R <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz> <output tex file, e.g., experiments-data.tex>')
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
df <- df[df$'configuration_id' %!in% c('verbose-test'), ] # FIXME should not even be in the CSV file

# Compute smelliness of each test case
df <- compute_smelliness(df, smells, column_name='Smelliness')

# Aggregate `df` so that we have average coverage, mutation score, and smell values per configuration, target class, and random seed
# Note that after the following line, `df` is at test suite level
df <- aggregate(as.formula(paste0('cbind(Size, Length, OverallCoverage, MutationScore, Smelliness, ', paste0(smells, collapse=','), ') ~ configuration_id + group_id + TARGET_CLASS + Random_Seed')), data=df, FUN=mean)

# Compute relative value of each smell
df <- compute_relativeness(df, smells)
# Compute relative smelliness of each test suite
df <- compute_relativeness(df, c('Smelliness'))
# Compute relative OverallCoverage and MutationScore of each test suite
df <- compute_relativeness(df, c('OverallCoverage'))
df <- compute_relativeness(df, c('MutationScore'))

# Aggregate `df` so that we have average coverage, mutation score, smelliness, and smell values per configuration and target class
# Note: basically average at target class level
df <- aggregate(as.formula(paste0('cbind(Size, Length, OverallCoverage, RelativeOverallCoverage, MutationScore, RelativeMutationScore, Smelliness, RelativeSmelliness, ', paste0(smells, collapse=','), ', ', paste0(relative_smells, collapse=','), ') ~ configuration_id + group_id + TARGET_CLASS')), data=df, FUN=mean)

# Set and init tex file
unlink(OUTPUT_TEX_FILE)
sink(OUTPUT_TEX_FILE, append=FALSE, split=TRUE)
# Header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} l rr rrr rrr ', paste0(replicate(length(smells), 'r'), collapse=''), ' rrr } \\toprule\n', sep='')
cat('              &           &            & \\multicolumn{3}{c}{Coverage}                                                              & \\multicolumn{3}{c}{Mutation}                                                              & \\multicolumn{', length(smells), '}{c}{} & \\multicolumn{3}{c}{Smelliness} \\\\\n', sep='')
cat('Configuration & \\# Tests & \\# Length & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI}', sep='')
for (smell in smells) {
  cat(' & \\multicolumn{1}{c}{', gsub("^TestSmell", '', smell), '}', sep='')
}
cat(' & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} \\\\\n', sep='')

# Body
print_body <- function(df, cov, mut, smells, smellines, label) {
  cat('\\midrule\n', sep='')
  cat('\\rowcolor{gray!25}\n', sep='')
  cat('\\multicolumn{18}{c}{\\textbf{\\textit{', label, '}}} \\\\\n', sep='')

  for (configuration_id in unique(df$'configuration_id')) {
    mask <- df$'configuration_id' == configuration_id
    cat(pretty_configuration_id_as_abbreviation(configuration_id), sep='')

    # Number of tests
    cat(' & ', sprintf('%.0f', round(mean(df$'Size'[mask]), 0)), sep='')

    # Number of statements tests
    cat(' & ', sprintf('%.0f', round(mean(df$'Length'[mask]), 0)), sep='')

    # Coverage
    ci <- get_ci(df[[cov]][mask])
    cat(' & ', sprintf('%.2f', round(mean(df[[cov]][mask]), 2)),
        ' & ', sprintf('%.2f', round(sd(df[[cov]][mask]), 2)),
        ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
        sep='')

    # Mutation
    ci <- get_ci(df[[mut]][mask])
    cat(' & ', sprintf('%.2f', round(mean(df[[mut]][mask]), 2)),
        ' & ', sprintf('%.2f', round(sd(df[[mut]][mask]), 2)),
        ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
        sep='')

    # Individual smells
    for (smell in smells) {
      cat(' & ', sprintf('%.2f', round(mean(df[[smell]][mask]), 2)), sep='')
    }

    # Smelliness
    ci <- get_ci(df[[smellines]][mask])
    cat(' & ', sprintf('%.2f', round(mean(df[[smellines]][mask]), 2)),
        ' & ', sprintf('%.2f', round(sd(df[[smellines]][mask]), 2)),
        ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
        sep='')

    cat(' \\\\\n', sep='')
  }
}

print_body(df, 'OverallCoverage', 'MutationScore', smells, 'Smelliness', 'Raw values')
print_body(df, 'RelativeOverallCoverage', 'RelativeMutationScore', relative_smells, 'RelativeSmelliness', 'Relative values')

# Table's footer
cat('\\bottomrule', '\n', sep='')
cat('\\end{tabular}', '\n', sep='')
sink()

# EOF
