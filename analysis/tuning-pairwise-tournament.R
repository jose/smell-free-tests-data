# ------------------------------------------------------------------------------
# This script performs pairwise tournaments across all configurations in a given
# data file.
#
# Usage:
#   Rscript tuning-pairwise-tournament.R
#     <input data file, e.g., ../test-generation/data/generated/tuning-data.csv.gz>
#     <output tex file, e.g., tuning-pairwise-tournament.tex>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript tuning-pairwise-tournament.R <input data file, e.g., ../test-generation/data/generated/tuning-data.csv.gz> <output tex file, e.g., tuning-pairwise-tournament.tex>')
}

# Args
INPUT_FILE  <- args[1]
OUTPUT_FILE <- args[2]

# ------------------------------------------------------------------------- Util

#
# Perform statistical pairwise tournaments of xs vs ys using the Vargha-Delaney
# A measure and the Wilcoxon–Mann–Whitney test.
#
perform_pairwise_tournaments <- function(df, column, xs, ys, higher_is_better=TRUE) {
  library('foreach') # install.packages('foreach')
  library('doParallel') # install.packages('doParallel')
  library('data.table') # install.packages('data.table')
  registerDoParallel(2)

  tournaments <- foreach(configuration=xs) %dopar% {
    configuration_mask      <- df$'configuration_id' == configuration
    df_configuration_masked <- df[configuration_mask, ]
    stopifnot(nrow(df_configuration_masked) > 0)

    conf_tournaments <- list(); i <- 1

    for (other_configuration in ys) {
      if (other_configuration == configuration) {
        next
      }
      other_configuration_mask      <- df$'configuration_id' == other_configuration
      df_other_configuration_masked <- df[other_configuration_mask, ]
      stopifnot(nrow(df_other_configuration_masked) > 0)

      for (clazz in unique(df$'TARGET_CLASS')) {
        df_clazz_masked       <- df_configuration_masked[df_configuration_masked$'TARGET_CLASS' == clazz, ]
        df_other_clazz_masked <- df_other_configuration_masked[df_other_configuration_masked$'TARGET_CLASS' == clazz, ]
        stopifnot(nrow(df_clazz_masked) > 0)
        stopifnot(nrow(df_other_clazz_masked) > 0)

        A <- df_clazz_masked[[column]]
        B <- df_other_clazz_masked[[column]]
        stopifnot(!is.null(A) && length(A) > 0)
        stopifnot(!is.null(B) && length(B) > 0)

        a12     <- A12(A, B)
        w       <- wilcox.test(A, B, exact=FALSE, paired=FALSE)
        p.value <- ifelse(is.nan(w$'p.value'), 0.0, w$'p.value')

        if (higher_is_better) {
          # When the A12 value is == 0.5, then the two configurations achieve equal performance.
          # When the A12 value is < than 0.5, the first configuration is worse.
          # When the A12 value is > than 0.5, the second configuration is worse (i.e., first configuration is better).

          if (p.value < 0.05 && a12 > 0.50) {
            # is 'configuration' significantly better than 'other_configuration'?
            conf_tournaments[[i]] <- data.frame(
              x=configuration, y=other_configuration, c=clazz,
              a12=a12, p=p.value,
              a12_better_than=a12, p_better_than=p.value,
              a12_worse_than=NA, p_worse_than=NA,
              better=1, worse=0
            )
          } else if (p.value < 0.05 && a12 < 0.50) {
            # is 'configuration' significantly worse than 'other_configuration'?
            conf_tournaments[[i]] <- data.frame(
              x=configuration, y=other_configuration, c=clazz,
              a12=a12, p=p.value,
              a12_better_than=NA, p_better_than=NA,
              a12_worse_than=a12, p_worse_than=p.value,
              better=0, worse=1
            )
          } else {
            conf_tournaments[[i]] <- data.frame(
              x=configuration, y=other_configuration, c=clazz,
              a12=a12, p=p.value,
              a12_better_than=NA, p_better_than=NA,
              a12_worse_than=NA, p_worse_than=NA,
              better=0, worse=0
            )
          }
        } else {
          # When the A12 value is == 0.5, then the two configurations achieve equal performance.
          # When the A12 value is > than 0.5, the first configuration is worse (i.e., second configuration is better).
          # When the A12 value is < than 0.5, the second configuration is worse.

          if (p.value < 0.05 && a12 > 0.50) {
            # is 'configuration' significantly worse than 'other_configuration'?
            conf_tournaments[[i]] <- data.frame(
              x=configuration, y=other_configuration, c=clazz,
              a12=a12, p=p.value,
              a12_better_than=NA, p_better_than=NA,
              a12_worse_than=a12, p_worse_than=p.value,
              better=0, worse=1
            )
          } else if (p.value < 0.05 && a12 < 0.50) {
            # is 'configuration' significantly better than 'other_configuration'?
            conf_tournaments[[i]] <- data.frame(
              x=configuration, y=other_configuration, c=clazz,
              a12=a12, p=p.value,
              a12_better_than=a12, p_better_than=p.value,
              a12_worse_than=NA, p_worse_than=NA,
              better=1, worse=0
            )
          } else {
            conf_tournaments[[i]] <- data.frame(
              x=configuration, y=other_configuration, c=clazz,
              a12=a12, p=p.value,
              a12_better_than=NA, p_better_than=NA,
              a12_worse_than=NA, p_worse_than=NA,
              better=0, worse=0
            )
          }
        }

        i <- i + 1
      }
    }

    stopifnot(i >= 2)
    conf_tournaments_as_data_frame <- rbindlist(conf_tournaments, use.names=TRUE, fill=TRUE)
    stopifnot(is.na(conf_tournaments_as_data_frame$'y') == FALSE)
    return(conf_tournaments_as_data_frame)
  }

  stopImplicitCluster()

  tournaments_as_data_frame <- as.data.frame(rbindlist(tournaments, use.names=TRUE, fill=TRUE))
  stopifnot(is.na(tournaments_as_data_frame$'y') == FALSE)
  return(tournaments_as_data_frame)
}

#
# Append a given prefix to some columnames.
#
append_prefix <- function(df, prefix) {
  names(df)[names(df) == 'a12']             <- paste(prefix, 'a12', sep='_')
  names(df)[names(df) == 'p']               <- paste(prefix, 'p', sep='_')
  names(df)[names(df) == 'a12_better_than'] <- paste(prefix, 'a12_better_than', sep='_')
  names(df)[names(df) == 'p_better_than']   <- paste(prefix, 'p_better_than', sep='_')
  names(df)[names(df) == 'a12_worse_than']  <- paste(prefix, 'a12_worse_than', sep='_')
  names(df)[names(df) == 'p_worse_than']    <- paste(prefix, 'p_worse_than', sep='_')
  names(df)[names(df) == 'better']          <- paste(prefix, 'better', sep='_')
  names(df)[names(df) == 'worse']           <- paste(prefix, 'worse', sep='_')
  return(df)
}

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

# Compute pairwise tournament over all configurations for all classes
xs <- unique(df$'configuration_id')
ys <- xs
cat('Performing coverage-based pairwise tournaments\n')
coverge_pairwise_tournaments  <- perform_pairwise_tournaments(df, 'RelativeOverallCoverage', xs, ys)
print(head(coverge_pairwise_tournaments)) # debug

cat('Performing mutation-based pairwise tournaments\n')
mutation_pairwise_tournaments <- perform_pairwise_tournaments(df, 'RelativeMutationScore', xs, ys)
print(head(mutation_pairwise_tournaments)) # debug

cat('Performing smelliness-based pairwise tournaments\n')
smell_pairwise_tournaments    <- perform_pairwise_tournaments(df, 'RelativeSmelliness', xs, ys, higher_is_better=FALSE)
print(head(smell_pairwise_tournaments)) # debug

stopifnot(nrow(coverge_pairwise_tournaments) == nrow(smell_pairwise_tournaments) &&
          nrow(coverge_pairwise_tournaments) == nrow(mutation_pairwise_tournaments))

# Rename some columns to ease merge
coverge_pairwise_tournaments  <- append_prefix(coverge_pairwise_tournaments,  'cov')
mutation_pairwise_tournaments <- append_prefix(mutation_pairwise_tournaments, 'mut')
smell_pairwise_tournaments    <- append_prefix(smell_pairwise_tournaments,    'smell')

# Merge all tournaments' data
pairwise_tournaments <- merge(smell_pairwise_tournaments, mutation_pairwise_tournaments, by=c('x', 'y', 'c'))
pairwise_tournaments <- merge(coverge_pairwise_tournaments, pairwise_tournaments, by=c('x', 'y', 'c'))
num_tournaments      <- nrow(pairwise_tournaments)
stopifnot(nrow(pairwise_tournaments) == nrow(coverge_pairwise_tournaments) &&
          nrow(pairwise_tournaments) == nrow(mutation_pairwise_tournaments) &&
          nrow(pairwise_tournaments) == nrow(smell_pairwise_tournaments))
cat('Performed ', num_tournaments, ' tournaments\n', sep='')

# Drop column y (aka other configuration)
pairwise_tournaments <- pairwise_tournaments[ , which(colnames(pairwise_tournaments) %!in% c('y')) ]

# Aggregate (sum) data per configuration
agg_sum <- aggregate(cbind(cov_better, cov_worse, mut_better, mut_worse, smell_better, smell_worse) ~ x, data=pairwise_tournaments, FUN=sum, na.rm=TRUE, na.action=NULL)
# Points per metric
agg_sum$'cov_diff'   <- agg_sum$'cov_better'   - agg_sum$'cov_worse'
agg_sum$'mut_diff'   <- agg_sum$'mut_better'   - agg_sum$'mut_worse'
agg_sum$'smell_diff' <- agg_sum$'smell_better' - agg_sum$'smell_worse'
# Points overall
agg_sum$'better'     <- agg_sum$'cov_better' + agg_sum$'mut_better' + agg_sum$'smell_better'
agg_sum$'worse'      <- agg_sum$'cov_worse'  + agg_sum$'mut_worse'  + agg_sum$'smell_worse'
agg_sum$'diff'       <- agg_sum$'cov_diff' + agg_sum$'mut_diff' + (5 * agg_sum$'smell_diff')
# Aggregate (mean) data per configuration
agg_mean <- aggregate(cbind(
  cov_a12,   cov_p,   cov_a12_better_than,   cov_p_better_than,   cov_a12_worse_than,   cov_p_worse_than,
  mut_a12,   mut_p,   mut_a12_better_than,   mut_p_better_than,   mut_a12_worse_than,   mut_p_worse_than,
  smell_a12, smell_p, smell_a12_better_than, smell_p_better_than, smell_a12_worse_than, smell_p_worse_than) ~ x, data=pairwise_tournaments, FUN=mean, na.rm=TRUE, na.action=NULL)
# Merge
agg <- merge(agg_sum, agg_mean, by=c('x'), all=TRUE)
stopifnot(nrow(agg) == nrow(agg_sum) &&
          nrow(agg) == nrow(agg_mean))
print(head(agg)) # debug
print(summary(agg)) # debug

# Remove the output file if any
unlink(OUTPUT_FILE)
sink(OUTPUT_FILE, append=FALSE, split=TRUE)

# Header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} l rrrr rrrr rrrr} \\toprule\n', sep='')
cat('',
    # Coverage
    ' & ', 'Coverage', ' &  & ', 'Coverage', ' & ',
    # Mutation
    ' & ', 'Mutation', ' &  & ', 'Mutation', ' & ',
    # Smelliness
    ' & ', 'Smelliness', ' &  & ', 'Smelliness', ' & ',
    ' \\\\\n', sep='')
cat('Configuration',
    # Coverage
    ' & ', 'better on', ' & ', '\\multicolumn{1}{c}{$\\hat{A}_{12}$}', ' & ', 'worse on', ' & ', '\\multicolumn{1}{c}{$\\hat{A}_{12}$}',
    # Mutation
    ' & ', 'better on', ' & ', '\\multicolumn{1}{c}{$\\hat{A}_{12}$}', ' & ', 'worse on', ' & ', '\\multicolumn{1}{c}{$\\hat{A}_{12}$}',
    # Smelliness
    ' & ', 'better on', ' & ', '\\multicolumn{1}{c}{$\\hat{A}_{12}$}', ' & ', 'worse on', ' & ', '\\multicolumn{1}{c}{$\\hat{A}_{12}$}',
    ' \\\\\n', sep='')

# Body
print_top <- function(label, top) {
  cat('\\midrule\n', sep='')
  cat('\\rowcolor{gray!25}\n', sep='')
  cat('\\multicolumn{13}{c}{\\textbf{\\textit{', label, '}}}', ' \\\\\n', sep='')
  for (i in 1:nrow(top)) {
    row <- top[i, ]
    # Configuration
    cat(pretty_configuration_id_as_abbreviation(row$'x'), sep='')
    # Tournaments' coverage data
    cat(' & ', row$'cov_better', ' & ', sprintf('%.2f', round(row$'cov_a12_better_than', 2)), sep='')
    cat(' & ', row$'cov_worse', ' & ', sprintf('%.2f', round(row$'cov_a12_worse_than', 2)), sep='')
    # Tournaments' mutation data
    cat(' & ', row$'mut_better', ' & ', sprintf('%.2f', round(row$'mut_a12_better_than', 2)), sep='')
    cat(' & ', row$'mut_worse', ' & ', sprintf('%.2f', round(row$'mut_a12_worse_than', 2)), sep='')
    # Tournaments' smelliness data
    cat(' & ', row$'smell_better', ' & ', sprintf('%.2f', round(row$'smell_a12_better_than', 2)), sep='')
    cat(' & ', row$'smell_worse', ' & ', sprintf('%.2f', round(row$'smell_a12_worse_than', 2)), sep='')
    # new line
    cat(' \\\\\n', sep='')
  }
}

print_top('ranked by Coverage',                           head(agg[order(-agg$'cov_diff',   -agg$'cov_better',   agg$'cov_worse',   -agg$'cov_a12_better_than',   agg$'cov_a12_worse_than',    -agg$'cov_a12'), ],   n=10))
print_top('ranked by Mutation',                           head(agg[order(-agg$'mut_diff',   -agg$'mut_better',   agg$'mut_worse',   -agg$'mut_a12_better_than',   agg$'mut_a12_worse_than',    -agg$'mut_a12'), ],   n=10))
print_top('ranked by Smelliness',                         head(agg[order(-agg$'smell_diff', -agg$'smell_better', agg$'smell_worse',  agg$'smell_a12_better_than', -agg$'smell_a12_worse_than',  agg$'smell_a12'), ], n=10))
print_top('ranked by Coverage, Mutation, and Smelliness', head(agg[order(-agg$'diff',       -agg$'better',       agg$'worse'), ],                                                                                    n=10))

# Footer
cat('\\bottomrule\n', sep='')
cat('\\end{tabular}\n', sep='')

# Flush data
sink()

# EOF
