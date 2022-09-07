# ------------------------------------------------------------------------------
# This script performs friedman rank test of all configurations in a given data
# file.
#
# Usage:
#   Rscript tuning-friedman-test-rank.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
#     <output tex file, e.g., tuning-friedman-test-tournament.tex>
#     <output pdf file, e.g., tuning-friedman-test-tournament.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
library('PMCMRplus') # install.packages('PMCMRplus')
library('ggplot2') # install.packages('ggplot2')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
  stop('USAGE: Rscript tuning-friedman-test-tournament.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output tex file, e.g., tuning-friedman-test-tournament.tex> <output pdf file, e.g., tuning-friedman-test-tournament.pdf>')
}

# Args
INPUT_FILE      <- args[1]
OUTPUT_TEX_FILE <- args[2]
OUTPUT_PDF_FILE <- args[3]

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
# df <- df[df$'configuration_id' %!in% c('verbose-test'), ]
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
# Compute pseudo-overall metric
# df$'RelativeOverallMetric' <- df$'RelativeOverallCoverage' + df$'RelativeMutationScore' - df$'RelativeSmelliness'
# df <- aggregate(as.formula(paste0('cbind(RelativeOverallCoverage, RelativeMutationScore, RelativeSmelliness, RelativeOverallMetric, ', paste0(relative_smells, collapse=','), ') ~ configuration_id + TARGET_CLASS')), data=df, FUN=mean)
df <- aggregate(as.formula(paste0('cbind(RelativeOverallCoverage, RelativeMutationScore, RelativeSmelliness, ', paste0(relative_smells, collapse=','), ') ~ configuration_id + TARGET_CLASS')), data=df, FUN=mean)
# Rank each configuration on each target class
df$'rank_cov'     <- with(df, ave(RelativeOverallCoverage,    TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))
df$'rank_mut'     <- with(df, ave(RelativeMutationScore,      TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))
df$'rank_smell'   <- with(df, ave(RelativeSmelliness,         TARGET_CLASS, FUN=function(x) rank( x, ties.method='average')))
# df$'rank_overall' <- with(df, ave(RelativeOverallMetric,      TARGET_CLASS, FUN=function(x) rank(-x, ties.method='average')))
df$'rank_overall' <- (df$'rank_cov' + df$'rank_mut' + df$'rank_smell') / 3.0

#
# TODO add documentation
#
perform_friedman_test <- function(df, label, rank_column) {
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
  # Table
  #

  # Aggregate `df` so that we have each average values per configuration
  x      <- aggregate(cbind(rank_cov, rank_mut, rank_smell, rank_overall) ~ configuration_id, data=df, FUN=mean, na.rm=TRUE, na.action=NULL)
  top    <- head(x[order(x[[rank_column]]), ], n=5)
  bottom <- tail(x[order(x[[rank_column]]), ], n=5)

  sink(OUTPUT_TEX_FILE, append=TRUE, split=TRUE)
  cat('\\midrule\n\\rowcolor{gray!25}\n', sep='')
  cat('\\multicolumn{7}{c}{\\textbf{\\textit{ranked by ', label, '}}',
        ' ($\\chi^2=', sprintf('%.2f', round(chi_squared, 2)), '$',
        ', \\textit{p}-value ', pretty_print_p_value(p_value), ')', '} \\\\\n', sep='')

  print_row <- function(configuration_id) {
    mask <- df$'configuration_id' == configuration_id
    cat(pretty_configuration_id_as_abbreviation(configuration_id), sep='')

    ci <- get_ci(df[[rank_column]][mask])
    cat(' & ', sprintf('%.2f', round(mean(df[[rank_column]][mask]), 2)),
        ' & ', sprintf('%.2f', round(sd(df[[rank_column]][mask]), 2)),
        ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
        sep='')
    cat(' & ', sprintf('%.2f', round(mean(df$'RelativeOverallCoverage'[mask]), 2)), sep='')
    cat(' & ', sprintf('%.2f', round(mean(df$'RelativeMutationScore'[mask]), 2)), sep='')
    cat(' & ', sprintf('%.2f', round(mean(df$'RelativeSmelliness'[mask]), 2)), sep='')

    cat(' \\\\\n', sep='')
  }

  for (i in 1:nrow(top)) {
    print_row(top$'configuration_id'[i])
  }
  for (i in 1:nrow(bottom)) {
    print_row(bottom$'configuration_id'[i])
  }

  sink()

  #
  # Plots
  #

  #
  # Ranks as boxplot
  #
  # Pretty print configuration_id's names
  df$'configuration_id' <- sapply(df$'configuration_id', pretty_configuration_id_as_abbreviation)
  # Label
  plot_label('Ranks as boxplot')
  # Basic box plot with colors by groups
  p <- ggplot(df, aes(x=configuration_id, y=get(rank_column), fill=configuration_id)) + geom_violin() + geom_boxplot(width=0.25)
  # Change x axis label and control the order configuration_id's names appear in the boxplot
  p <- p + scale_x_discrete(name='')
  # Change y axis label and set its range
  p <- p + scale_y_continuous(name='Rank', limits=c(1, max(df[[rank_column]])), breaks=seq(1,max(df[[rank_column]]), by=10))
  # # Use grey scale color palette
  # p <- p + scale_fill_grey()
  # Remove legend's title and increase size of [x-y]axis labels
  p <- p + theme(legend.position='none',
    axis.text.x=element_text(size=13,  hjust=0.5, vjust=0.5),
    axis.text.y=element_text(size=13,  hjust=1.0, vjust=0.0),
    axis.title.x=element_text(size=15, hjust=0.5, vjust=0.0),
    axis.title.y=element_text(size=15, hjust=0.5, vjust=0.5)
  )
  # Make it horizontal
  p <- p + coord_flip()
  # Add mean points
  p <- p + stat_summary(fun=mean, geom='point', shape=8, size=2, fill='black', color='black')
  print(p)

  #
  # Nemenyi posthoc test's p values
  #
  # Label
  plot_label("Nemenyi posthoc test's p-values")
  # Pretty print configuration's names
  rownames(nemenyi_posthoc_test_p_values) <- sapply(rownames(nemenyi_posthoc_test_p_values), pretty_configuration_id_as_abbreviation)
  colnames(nemenyi_posthoc_test_p_values) <- sapply(colnames(nemenyi_posthoc_test_p_values), pretty_configuration_id_as_abbreviation)
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
  rownames(conover_posthoc_test_p_values) <- sapply(rownames(conover_posthoc_test_p_values), pretty_configuration_id_as_abbreviation)
  colnames(conover_posthoc_test_p_values) <- sapply(colnames(conover_posthoc_test_p_values), pretty_configuration_id_as_abbreviation)
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
}

# Set and init tex file
unlink(OUTPUT_TEX_FILE)
sink(OUTPUT_TEX_FILE, append=FALSE, split=TRUE)
# Header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} l rrr rrr} \\toprule', '\n', sep='')
cat('Configuration & \\multicolumn{1}{c}{Rank} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{Coverage} & \\multicolumn{1}{c}{Mutation} & \\multicolumn{1}{c}{Smelliness} \\\\\n', sep='')
sink()

# Set and remove any existing pdf file
unlink(OUTPUT_PDF_FILE)
pdf(file=OUTPUT_PDF_FILE, family='Helvetica', width=13, height=13)
# Add a cover page to the pdf
plot_label('Tuning analysis\nFriedman test')

  plot_label('Coverage-based friedman test')
  cat('\n\n--- Coverage-based friedman test ---', sep='')
  perform_friedman_test(df, 'Coverage', 'rank_cov')

  plot_label('Mutation-based friedman test')
  cat('\n\n--- Mutation-based friedman test ---', sep='')
  perform_friedman_test(df, 'Mutation', 'rank_mut')

  plot_label('Smelliness-based friedman test')
  cat('\n\n--- Smelliness-based friedman test ---', sep='')
  perform_friedman_test(df, 'Smelliness', 'rank_smell')

  plot_label('Overall-based friedman test')
  cat('\n\n--- Overall friedman test ---', sep='')
  perform_friedman_test(df, 'Coverage, Mutation, and Smelliness', 'rank_overall')

# Table's footer
sink(OUTPUT_TEX_FILE, append=TRUE, split=TRUE)
cat('\\bottomrule', '\n', sep='')
cat('\\end{tabular}', '\n', sep='')
sink()

# "pdf's footer"
dev.off()
embed_fonts_in_a_pdf(OUTPUT_PDF_FILE)

# EOF
