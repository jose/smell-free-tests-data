# ------------------------------------------------------------------------------
# This script performs statistical tests on whether configurations A (vanilla)
# performs any better or worse than any other optimized configurations.
#
# Usage:
#   Rscript configuration-a-vs-any.R
#     <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz>
#     <output pdf file, e.g., configuration-a-vs-any.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
library('data.table') # install.packages('data.table')
library('ggplot2') # install.packages('ggplot2')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript configuration-a-vs-any.R <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz> <output pdf file, e.g., configuration-a-vs-any.pdf>')
}

# Args
INPUT_FILE  <- args[1]
OUTPUT_FILE <- args[2]

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

# Load and pre-process tuning data
df <- load_data(INPUT_FILE, smells)

# Revert a normalized value to its non-normalized value
df <- compute_non_normalized_values(df, smells)

# Aggregate `df` so that we have average coverage, mutation score, and smell values per configuration, target class, and random seed
# Note that after the following line, `df` is at test suite level
df <- aggregate(as.formula(paste0('cbind(Size, Length, OverallCoverage, MutationScore, ', paste0(smells, collapse=','), ') ~ configuration_id + group_id + TARGET_CLASS + Random_Seed')), data=df, FUN=mean)

# Compute relative OverallCoverage, MutationScore, and all smells
df <- compute_relativeness(df, c('OverallCoverage'))
df <- compute_relativeness(df, c('MutationScore'))
df <- compute_relativeness(df, c(smells))

# Compute smelliness of each test suite
df <- compute_smelliness(df, relative_smells, column_name='RelativeSmelliness')

# Pretty configurations' names
df$'configuration_id' <- sapply(df$'configuration_id', pretty_configuration_id_as_abbreviation)

CONF_A     <- 'Vanilla'
OTHER_CONF <- setdiff(unique(df$'configuration_id'), CONF_A)

#
# TODO add doc
#
run_stats <- function(df, column) {
  comparisons <- list(); i <- 1
  for (clazz in unique(df$'TARGET_CLASS')) {
    clazz_mask <- df$'TARGET_CLASS' == clazz

    for (conf_b in OTHER_CONF) {
      A <- df[[column]][clazz_mask & df$'configuration_id' == CONF_A]
      B <- df[[column]][clazz_mask & df$'configuration_id' == conf_b]
      stopifnot(!is.null(A) && length(A) > 0)
      stopifnot(!is.null(B) && length(B) > 0)

      a12     <- A12(A, B)
      w       <- wilcox.test(A, B, exact=FALSE, paired=FALSE)
      p.value <- ifelse(is.nan(w$'p.value'), 0.0, w$'p.value')

      # When the A12 value is == 0.5, then the two configurations achieve equal performance.
      # When the A12 value is < than 0.5, the first configuration is worse.
      # When the A12 value is > than 0.5, the second configuration is worse (i.e., first configuration is better).

      comparisons[[i]] <- data.frame(conf_a=CONF_A, conf_b=conf_b, c=clazz, val_a=mean(A), val_b=mean(B), a12=a12, p=p.value)
      i <- i + 1
    }
  }

  comparisons_as_data_frame <- rbindlist(comparisons, use.names=TRUE, fill=TRUE)
  # print(head(comparisons_as_data_frame)) # debug

  return(comparisons_as_data_frame)
}

# Remove any existing output file and create a new one
unlink(OUTPUT_FILE)
pdf(file=OUTPUT_FILE, family='Helvetica', width=7, height=4)
# Add a cover page to the output file
plot_label(paste0(CONF_A, ' vs.'))

#
# Create boxplot
#
plot_it <- function(df, y_var) {
  # Basic boxplot with colors by groups
  p <- ggplot(df, aes(x=conf_b, y=get(y_var), fill=conf_b)) + geom_violin() + geom_boxplot(width=0.25)
  # Change x axis label
  p <- p + scale_x_discrete(name='')
  # Change y axis label
  p <- p + scale_y_continuous(name='')
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
  # Print it
  print(p)
}

for (column in c('Size', 'Length', 'RelativeOverallCoverage', 'RelativeMutationScore', 'RelativeSmelliness')) {
  dx <- run_stats(df, column)
  plot_label(paste0(column, '\n A12')); plot_it(dx, 'a12')
  plot_label(paste0(column, '\n p-value')); plot_it(dx, 'p')
}

# Close output file
dev.off()
# Embed fonts
embed_fonts_in_a_pdf(OUTPUT_FILE)

#
# Overall table
#

# TODO print table to .tex file

# Header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} lrrrrrr} \\toprule\n', sep='')
cat(' & & & & & & \\multicolumn{1}{c}{Relative} \\\\\n', sep='')
cat('Configuration & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{$\\hat{A}_{12}$} & \\multicolumn{1}{c}{p-value} & \\multicolumn{1}{c}{improvement} \\\\\n', sep='')

body_it <- function(df, label='Coverage') {
  cat('\\midrule\n', sep='')
  cat('\\rowcolor{gray!25}\n', sep='')
  cat('\\multicolumn{7}{c}{\\textbf{\\textit{', label, '}}} \\\\\n', sep='')

  #
  # Baseline
  mask <- df$'conf_a' == CONF_A & df$'conf_b' == head(unique(df$'conf_b'), n=1)
  # Name
  cat(CONF_A, sep='')
  # Data
  ci <- get_ci(df$'val_a'[mask])
  cat(' & ', sprintf('%.2f', round(mean(df$'val_a'[mask]), 2)),
      ' & ', sprintf('%.2f', round(sd(df$'val_a'[mask]), 2)),
      ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
      sep='')
  cat(' & ', '---', sep='')
  cat(' & ', '---', sep='')
  cat(' & ', '---', sep='')
  cat(' \\\\\n', sep='')

  # Other
  for (conf_b in unique(df$'conf_b')) {
    mask <- df$'conf_b' == conf_b

    # Name
    cat(conf_b, sep='')

    # Data
    ci <- get_ci(df$'val_b'[mask])
    cat(' & ', sprintf('%.2f', round(mean(df$'val_b'[mask]), 2)),
        ' & ', sprintf('%.2f', round(sd(df$'val_b'[mask]), 2)),
        ' & [', sprintf('%.2f', round(ci[1], 2)), ', ', sprintf('%.2f', round(ci[2], 2)), ']',
        sep='')
    cat(' & ', sprintf('%.2f', round(mean(df$'a12'[mask]), 2)), sep='')
    cat(' & ', sprintf('%.2f', round(mean(df$'p'[mask]), 2)), sep='')
    cat(' & ', sprintf('%.2f', round(relative_improvement_value(df$'val_a'[mask], df$'val_b'[mask])*100.0, 2)), '\\%', sep='')

    cat(' \\\\\n', sep='')
  }
}

body_it(run_stats(df, 'Size'), 'Size')
body_it(run_stats(df, 'Length'), 'Length')
body_it(run_stats(df, 'RelativeOverallCoverage'), 'Coverage')
body_it(run_stats(df, 'RelativeMutationScore'), 'Mutation score')
body_it(run_stats(df, 'RelativeSmelliness'), 'Smelliness')

# Table's footer
cat('\\bottomrule', '\n', sep='')
cat('\\end{tabular}', '\n', sep='')
# sink()

#
# Per project
#

# TODO print table to .tex file

# Header
# cat('\\begin{tabular}{@{\\extracolsep{\\fill}} lr rr rrr} \\toprule\n', sep='')
cat('\\toprule\n', sep='')
cat('Project & \\# Classes & Vanilla & \\textit{other} & \\# Better & \\# Worse & \\# No diff. \\\\\n', sep='')
cat('\\midrule\n', sep='')
cat('\\endhead % all the lines above this will be repeated on every page\n', sep='')

# Body
avg_num_better  <- 0
avg_num_worse   <- 0
avg_num_no_diff <- 0
for (project in sort(unique(df$'group_id'))) {
  project_mask <- df$'group_id' == project
  cat(gsub('_', '-', project), sep='')

  num_classes <- length(unique(df$'TARGET_CLASS'[project_mask]))
  cat(' & ', num_classes, sep='')

  A <- mean(df$'RelativeSmelliness'[project_mask & df$'configuration_id' == CONF_A])
  B <- mean(df$'RelativeSmelliness'[project_mask & df$'configuration_id' == OTHER_CONF])
  cat(' & ', ifelse(A < B, paste('\\textbf{', sprintf('%.2f', round(A, 2)), '}', sep=''), sprintf('%.2f', round(A, 2))), sep='')
  cat(' & ', ifelse(B < A, paste('\\textbf{', sprintf('%.2f', round(B, 2)), '}', sep=''), sprintf('%.2f', round(B, 2))), sep='')

  num_better  <- 0
  num_worse   <- 0
  num_no_diff <- 0
  for (clazz in unique(df$'TARGET_CLASS'[project_mask])) {
    clazz_mask <- df$'TARGET_CLASS' == clazz

    A <- df$'RelativeSmelliness'[project_mask & clazz_mask & df$'configuration_id' == CONF_A]
    B <- df$'RelativeSmelliness'[project_mask & clazz_mask & df$'configuration_id' == OTHER_CONF]
    stopifnot(!is.null(A) && length(A) > 0)
    stopifnot(!is.null(B) && length(B) > 0)

    a12     <- A12(A, B)
    w       <- wilcox.test(A, B, exact=FALSE, paired=FALSE)
    p.value <- ifelse(is.nan(w$'p.value'), 0.0, w$'p.value')

    # higher is better
    # When the A12 value is == 0.5, then the two configurations achieve equal performance.
    # When the A12 value is < than 0.5, the first configuration is worse.
    # When the A12 value is > than 0.5, the second configuration is worse (i.e., first configuration is better).

    # lower is better
    # When the A12 value is == 0.5, then the two configurations achieve equal performance.
    # When the A12 value is > than 0.5, the first configuration is worse (i.e., second configuration is better).
    # When the A12 value is < than 0.5, the second configuration is worse.

    if (mean(A) > mean(B)) { # B is better
      num_better <- num_better + 1
    } else if (mean(A) < mean(B)) { # B is worse
      num_worse <- num_worse + 1
    } else if (mean(A) == mean(B)) { # equal
      num_no_diff <- num_no_diff + 1
    }
  }

  cat(' & ', num_better, ' & ', num_worse, ' & ', num_no_diff, sep='')
  cat(' \\\\\n', sep='')

  avg_num_better  <- avg_num_better + num_better
  avg_num_worse   <- avg_num_worse + num_worse
  avg_num_no_diff <- avg_num_no_diff + num_no_diff
}

cat('\\midrule\n', sep='')
cat('Average & ', sep='')
A <- mean(df$'RelativeSmelliness'[df$'configuration_id' == CONF_A])
B <- mean(df$'RelativeSmelliness'[df$'configuration_id' == OTHER_CONF])
cat(' & ', ifelse(A < B, paste('\\textbf{', sprintf('%.2f', round(A, 2)), '}', sep=''), sprintf('%.2f', round(A, 2))), sep='')
cat(' & ', ifelse(B < A, paste('\\textbf{', sprintf('%.2f', round(B, 2)), '}', sep=''), sprintf('%.2f', round(B, 2))), sep='')

cat(' & ', avg_num_better,  ' (', sprintf('%.2f', round(avg_num_better  / length(unique(df$'TARGET_CLASS')) * 100.0, 2)), '\\%)', sep='')
cat(' & ', avg_num_worse,   ' (', sprintf('%.2f', round(avg_num_worse   / length(unique(df$'TARGET_CLASS')) * 100.0, 2)), '\\%)', sep='')
cat(' & ', avg_num_no_diff, ' (', sprintf('%.2f', round(avg_num_no_diff / length(unique(df$'TARGET_CLASS')) * 100.0, 2)), '\\%)', sep='')
cat(' \\\\\n', sep='')

# Table's footer
cat('\\bottomrule', '\n', sep='')
# cat('\\end{tabular}', '\n', sep='')
# sink()

# EOF
