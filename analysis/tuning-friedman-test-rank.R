# ------------------------------------------------------------------------------
# This script performs friedman rank test of all configurations in a given data
# file.
#
# Usage:
#   Rscript tuning-friedman-test-rank.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
#     <output pdf file, e.g., tuning-friedman-test-tournament.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
library('PMCMRplus') # install.packages('PMCMRplus')
library('ggplot2') # install.packages('ggplot2')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript tuning-friedman-test-tournament.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output pdf file, e.g., tuning-friedman-test-tournament.pdf>')
}

# Args
INPUT_FILE      <- args[1]
OUTPUT_PDF_FILE <- args[2]

# ------------------------------------------------------------------------- Main

# Load data
df                     <- load_TABLE(INPUT_FILE)
# Select relevant columns to compute some values
df                     <- df[ , which(colnames(df) %in% c(
  'configuration_id',
  'TARGET_CLASS',
  'LineCoverage',
  'BranchCoverage',
  'ExceptionCoverage',
  'WeakMutationScore',
  'OutputCoverage',
  'MethodCoverage',
  'MethodNoExceptionCoverage',
  'CBranchCoverage',
  'MutationScore',
  'TestSmellEagerTest',
  'TestSmellIndirectTesting',
  'TestSmellObscureInlineSetup',
  'TestSmellOverreferencing',
  'TestSmellRottenGreenTests',
  'TestSmellVerboseTest')
) ]
# Compute overall coverage
df$'OverallCoverage' <- (df$'LineCoverage' +
                         df$'BranchCoverage' +
                         df$'ExceptionCoverage' +
                         df$'WeakMutationScore' +
                         df$'OutputCoverage' +
                         df$'MethodCoverage' +
                         df$'MethodNoExceptionCoverage' +
                         df$'CBranchCoverage') / 8.0
# Compute overall smelliness
df$'Smelliness'      <- (df$'TestSmellEagerTest' +
                         df$'TestSmellIndirectTesting' +
                         df$'TestSmellObscureInlineSetup' +
                         df$'TestSmellOverreferencing' +
                         df$'TestSmellRottenGreenTests' +
                         df$'TestSmellVerboseTest') / 6.0
# Select relevant columns to perform the analysis
df                     <- df[ , which(colnames(df) %in% c(
  'configuration_id',
  'TARGET_CLASS',
  'MutationScore',
  'OverallCoverage',
  'Smelliness')
)]
print(head(df)) # debug
print(summary(df)) # debug

# Compute pseudo-overall metric
df$'OverallMetric' <- df$'OverallCoverage' + df$'MutationScore' + df$'Smelliness'
# Aggregate `df` so that we have coverage, mutation score, and smelliness values per configuration and target class
df <- aggregate(cbind(OverallCoverage, Smelliness, MutationScore, OverallMetric) ~ configuration_id + TARGET_CLASS, data=df, FUN=mean, na.rm=TRUE, na.action=NULL)
# Rank each configuration on each target class
df$'rank_cov'     <- with(df, ave(OverallCoverage, TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))
df$'rank_mut'     <- with(df, ave(MutationScore,   TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))
df$'rank_smell'   <- with(df, ave(Smelliness,      TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))
df$'rank_overall' <- with(df, ave(OverallMetric,   TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))

#
# TODO add documentation
#
perform_friedman_test <- function(df, rank_column) {
  # Perform friedman test on the computed rankings
  friedman_test <- friedman.test(get(rank_column) ~ configuration_id | TARGET_CLASS, data=df)
  chi_squared   <- as.numeric(friedman_test$'statistic')
  p_value       <- as.numeric(friedman_test$'p.value')
  # Perform posthoc tests
  nemenyi_posthoc_test          <- frdAllPairsNemenyiTest(get(rank_column) ~ configuration_id | TARGET_CLASS, data=df)
  nemenyi_posthoc_test_p_values <- as.matrix(nemenyi_posthoc_test$'p.value')
  conover_posthoc_test          <- frdAllPairsConoverTest(y=df[[rank_column]], groups=df$'configuration_id', blocks=df$'TARGET_CLASS')
  conover_posthoc_test_p_values <- as.matrix(conover_posthoc_test$'p.value')

  #
  # Nemenyi posthoc test's p values
  #
  # Label
  plot_label("Nemenyi posthoc test's p-values")
  # Pretty print configuration's names
  rownames(nemenyi_posthoc_test_p_values) <- sapply(rownames(nemenyi_posthoc_test_p_values), pretty_configuration_id)
  colnames(nemenyi_posthoc_test_p_values) <- sapply(colnames(nemenyi_posthoc_test_p_values), pretty_configuration_id)
  # Melt data to ease plot
  melted_data <- reshape2::melt(nemenyi_posthoc_test_p_values, na.rm=TRUE)
  # Round values
  # melted_data$'value' <- sapply(melted_data$'value', function(x) round(x, 2))
  print(head(melted_data))
  # Plot it
  p <- ggplot(melted_data, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color='white')
  p <- p + scale_fill_gradient2(low='#E46726', high='#6D9EC1', mid='white', midpoint=0.50, limit=c(0,1), name='p-value')
  # p <- p + geom_text(aes(Var1, Var2, label=value), color='black', size=5)
  p <- p + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle=90, vjust=0.3, hjust=1, size=8),
    axis.text.y = element_text(size=8))
  p <- p + coord_fixed()
  print(p)

  #
  # Conover posthoc test's p values
  #
  # Label
  plot_label("Conover posthoc test's p-values")
  # Pretty print configuration's names
  rownames(conover_posthoc_test_p_values) <- sapply(rownames(conover_posthoc_test_p_values), pretty_configuration_id)
  colnames(conover_posthoc_test_p_values) <- sapply(colnames(conover_posthoc_test_p_values), pretty_configuration_id)
  # Melt data to ease plot
  melted_data <- reshape2::melt(conover_posthoc_test_p_values, na.rm=TRUE)
  # Round values
  # melted_data$'value' <- sapply(melted_data$'value', function(x) round(x, 2))
  # Plot it
  p <- ggplot(melted_data, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color='white')
  p <- p + scale_fill_gradient2(low='#E46726', high='#6D9EC1', mid='white', midpoint=0.50, limit=c(0,1), name='p-value')
  # p <- p + geom_text(aes(Var1, Var2, label=value), color='black', size=5)
  p <- p + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle=90, vjust=0.3, hjust=1, size=8),
    axis.text.y = element_text(size=8))
  p <- p + coord_fixed()
  print(p)

  # TODO return values? print values to a table?
}

# Aggregate `df` so that we have each average rank position per configuration
x <- aggregate(cbind(rank_cov, rank_mut, rank_smell, rank_overall) ~ configuration_id, data=df, FUN=mean, na.rm=TRUE, na.action=NULL)

# Set and remove any existing pdf file
unlink(OUTPUT_PDF_FILE)
pdf(file=OUTPUT_PDF_FILE, family='Helvetica', width=13, height=13)
# Add a cover page to the pdf
plot_label('Tuning analysis\nFriedman test')

  plot_label('Coverage-based friedman test')
  cat('\n\n--- Coverage-based friedman test ---', sep='')
  print(head(x[order(x$'rank_cov'), ], n=5))
  perform_friedman_test(df, 'rank_cov')

  plot_label('Mutation-based friedman test')
  cat('\n\n--- Mutation-based friedman test ---', sep='')
  print(head(x[order(x$'rank_mut'), ], n=5))
  perform_friedman_test(df, 'rank_mut')

  plot_label('Smelliness-based friedman test')
  cat('\n\n--- Smelliness-based friedman test ---', sep='')
  print(head(x[order(x$'rank_smell'), ], n=5))
  perform_friedman_test(df, 'rank_smell')

  plot_label('Overall-based friedman test')
  cat('\n\n--- Overall friedman test ---', sep='')
  print(head(x[order(x$'rank_overall'), ], n=5))
  perform_friedman_test(df, 'rank_overall')

# "pdf's footer"
dev.off()
embed_fonts_in_a_pdf(OUTPUT_PDF_FILE)

# EOF
