# ------------------------------------------------------------------------------
# This script performs pairwise tournaments across all configurations in a given
# data file.
#
# Usage:
#   Rscript tuning-pairwise-tournament.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
#     <output tex file, e.g., tuning-pairwise-tournament.tex>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript tuning-pairwise-tournament.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output tex file, e.g., tuning-pairwise-tournament.tex>')
}

# Args
INPUT_FILE  <- args[1]
OUTPUT_FILE <- args[2]

# ------------------------------------------------------------------------- Util

#
# Perform statistical pairwise tournaments of xs vs ys using the Vargha-Delaney
# A measure and the Wilcoxon–Mann–Whitney test.
#
perform_pairwise_tournaments <- function(df, column, xs, ys) {
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

# Load data
df                     <- load_TABLE(INPUT_FILE)
# Select relevant columns
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
print(head(df)) # debug
print(summary(df)) # debug

# Compute pairwise tournament over all configurations for all 
xs <- unique(df$'configuration_id')
ys <- xs
cat('Performing coverge-based pairwise tournaments\n')
coverge_pairwise_tournaments  <- perform_pairwise_tournaments(df, 'OverallCoverage', xs, ys)
print(head(coverge_pairwise_tournaments)) # debug

cat('Performing smelliness-based pairwise tournaments\n')
smell_pairwise_tournaments    <- perform_pairwise_tournaments(df, 'Smelliness',      xs, ys)
print(head(smell_pairwise_tournaments)) # debug

cat('Performing mutation-based pairwise tournaments\n')
mutation_pairwise_tournaments <- perform_pairwise_tournaments(df, 'MutationScore',   xs, ys)
print(head(mutation_pairwise_tournaments)) # debug

stopifnot(nrow(coverge_pairwise_tournaments) == nrow(smell_pairwise_tournaments) &&
          nrow(coverge_pairwise_tournaments) == nrow(mutation_pairwise_tournaments))

# Rename some columns to ease merge
coverge_pairwise_tournaments  <- append_prefix(coverge_pairwise_tournaments,  'cov')
smell_pairwise_tournaments    <- append_prefix(smell_pairwise_tournaments,    'smell')
mutation_pairwise_tournaments <- append_prefix(mutation_pairwise_tournaments, 'mut')

# Merge all tournaments' data
pairwise_tournaments <- merge(smell_pairwise_tournaments, mutation_pairwise_tournaments, by=c('x', 'y', 'c'))
pairwise_tournaments <- merge(coverge_pairwise_tournaments, pairwise_tournaments, by=c('x', 'y', 'c'))
num_tournaments      <- nrow(pairwise_tournaments)
stopifnot(nrow(pairwise_tournaments) == nrow(coverge_pairwise_tournaments) &&
          nrow(pairwise_tournaments) == nrow(smell_pairwise_tournaments) &&
          nrow(pairwise_tournaments) == nrow(mutation_pairwise_tournaments))
cat('Performed ', num_tournaments, ' tournaments\n', sep='')

# Drop column y (aka other configuration)
pairwise_tournaments <- pairwise_tournaments[ , which(colnames(pairwise_tournaments) %!in% c('y')) ]

# Aggregate data per configuration
agg <- aggregate(cbind(cov_better, cov_worse, smell_better, smell_worse, mut_better, mut_worse) ~ x, data=pairwise_tournaments, FUN=sum, na.rm=TRUE, na.action=NULL)
# Points per metric
agg$'cov_diff'   <- agg$'cov_better'   - agg$'cov_worse'
agg$'smell_diff' <- agg$'smell_better' - agg$'smell_worse'
agg$'mut_diff'   <- agg$'mut_better'   - agg$'mut_worse'
# Points overall
agg$'better'     <- agg$'cov_better' + agg$'smell_better' + agg$'mut_better'
agg$'worse'      <- agg$'cov_worse'  + agg$'smell_worse'  + agg$'mut_worse'
agg$'diff'       <- agg$'better' - agg$'worse'

# TODO uncomment and verify it
# Aggregate `df` so that we have coverage, mutation score, and smelliness values per configuration
# df  <- aggregate(cbind(OverallCoverage, Smelliness, MutationScore) ~ configuration_id + TARGET_CLASS, data=df, FUN=mean, na.rm=TRUE, na.action=NULL)
# df  <- aggregate(cbind(OverallCoverage, Smelliness, MutationScore) ~ configuration_id, data=df, FUN=mean, na.rm=TRUE, na.action=NULL)
# # Merge it with `agg` by (x <==> configuration_id) so that we could see in the following experiments the overall coverage, mutation score, and smelliness value
# agg <- merge(agg, df, by.x=c('x'), by.y=c('configuration_id'), all=TRUE)

# Remove the output file if any
unlink(OUTPUT_FILE)
sink(OUTPUT_FILE, append=FALSE, split=TRUE)

# Write down the table header
cat('\\begin{tabular}{@{\\extracolsep{\\fill}} lrrr} \\toprule\n', sep='')
cat('Configuration', ' & ', '\\multicolumn{1}{c}{Coverage}', ' & ', '\\multicolumn{1}{c}{Mutation}', ' & ', '\\multicolumn{1}{c}{Smelliness}', ' \\\\\n', sep='')

# Body
print_top <- function(label, top) {
  cat('\\midrule\n', sep='')
  cat('\\rowcolor{gray!25}\n', sep='')
  cat('\\multicolumn{4}{c}{\\textbf{\\textit{', label, '}}}', ' \\\\\n', sep='')
  for(i in 1:nrow(top)) {
    row <- top[i, ]
    cat(pretty_configuration_id(row$'x'),
        ' & ', row$'cov_better',   ' / ', row$'cov_worse',
        ' & ', row$'mut_better',   ' / ', row$'mut_worse',
        ' & ', row$'smell_better', ' / ', row$'smell_worse', ' \\\\\n', sep='')
    # TODO would we like/want to report average coverage, mutation score, and smelliness
    # TODO would we like/want to report average A12 and p-values
  }
}

print_top('Coverage > Mutation > Smelliness',
  head(agg[order(-agg$'cov_diff',   -agg$'mut_diff',   -agg$'smell_diff', -agg$'cov_better',   -agg$'mut_better',   -agg$'smell_better', agg$'cov_worse',   agg$'mut_worse',   agg$'smell_worse'), ], n=5))
print_top('Coverage > Smelliness > Mutation',
  head(agg[order(-agg$'cov_diff',   -agg$'smell_diff', -agg$'mut_diff',   -agg$'cov_better',   -agg$'smell_better', -agg$'mut_better',   agg$'cov_worse',   agg$'smell_worse', agg$'mut_worse'), ],   n=5))
print_top('Mutation > Coverage > Smelliness',
  head(agg[order(-agg$'mut_diff',   -agg$'cov_diff',   -agg$'smell_diff', -agg$'mut_better',   -agg$'cov_better',   -agg$'smell_better', agg$'mut_worse',   agg$'cov_worse',   agg$'smell_worse'), ], n=5))
print_top('Mutation > Smelliness > Coverage',
  head(agg[order(-agg$'mut_diff',   -agg$'smell_diff', -agg$'cov_diff',   -agg$'mut_better',   -agg$'smell_better', -agg$'cov_better',   agg$'mut_worse',   agg$'smell_worse', agg$'cov_worse'), ],   n=5))
print_top('Smelliness > Coverage > Mutation',
  head(agg[order(-agg$'smell_diff', -agg$'cov_diff',   -agg$'mut_diff',   -agg$'smell_better', -agg$'cov_better',   -agg$'mut_better',   agg$'smell_worse', agg$'cov_worse',   agg$'mut_worse'), ],   n=5))
print_top('Smelliness > Mutation > Coverage',
  head(agg[order(-agg$'smell_diff', -agg$'mut_diff',   -agg$'cov_diff',   -agg$'smell_better', -agg$'mut_better',   -agg$'cov_better',   agg$'smell_worse', agg$'mut_worse',   agg$'cov_worse'), ],   n=5))
print_top('Coverage + Mutation + Smelliness',
  head(agg[order(-agg$'diff', -agg$'better', agg$'worse'), ], n=5))

# Footer
cat('\\bottomrule\n', sep='')
cat('\\end{tabular}\n', sep='')

# Flush data
sink()

# EOF
