# ------------------------------------------------------------------------------
# This script plots data of a single configuration.
#
# Usage:
#   Rscript standalone-configuration.R
#     <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz>
#     <configuration id, e.g., vanilla-measure-smells-timelines or eager-test-and-indirect-testing-and-obscure-inline-setup-and-verbose-test>
#     <output tex dir, e.g., paper/tables>
#     <output pdf dir, e.g., paper/figures>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

library('corrplot') # install.packages('corrplot') # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library('PerformanceAnalytics') # install.packages('PerformanceAnalytics')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop('USAGE: Rscript standalone-configuration.R <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz> <configuration id, e.g., vanilla-measure-smells-timelines or eager-test-and-indirect-testing-and-obscure-inline-setup-and-verbose-test> <output tex dir, e.g., paper/tables> <output pdf dir, e.g., paper/figures>')
}

# Args, input
INPUT_FILE       <- args[1]
CONFIGURATION_ID <- args[2]
# Output
OUTPUT_TEX_DIR   <- args[3]
OUTPUT_PDF_DIR   <- args[4]

# -------------------------------------------------------- Load and prepare data

# BeforePostProcess smells to be considered/analyzed
smells_before_post_process <- c(
  'TestSmellAssertionRouletteBeforePostProcess',
  'TestSmellDuplicateAssertBeforePostProcess',
  'TestSmellEagerTestBeforePostProcess',
  'TestSmellIndirectTestingBeforePostProcess',
  'TestSmellLackOfCohesionOfMethodsBeforePostProcess',
  'TestSmellLazyTestBeforePostProcess',
  'TestSmellLikelyIneffectiveObjectComparisonBeforePostProcess',
  'TestSmellObscureInlineSetupBeforePostProcess',
  'TestSmellOverreferencingBeforePostProcess',
  'TestSmellRedundantAssertionBeforePostProcess',
  'TestSmellRottenGreenTestsBeforePostProcess',
  'TestSmellTestRedundancyBeforePostProcess',
  'TestSmellUnknownTestBeforePostProcess',
  'TestSmellUnrelatedAssertionsBeforePostProcess',
  'TestSmellUnusedInputsBeforePostProcess',
  'TestSmellVerboseTestBeforePostProcess'
)

# AfterPostProcess smells to be considered/analyzed
smells_after_post_process <- c(
  'TestSmellAssertionRoulette',
  'TestSmellDuplicateAssert',
  'TestSmellEagerTest',
  'TestSmellIndirectTesting',
  'TestSmellLackOfCohesionOfMethods',
  'TestSmellLazyTest',
  'TestSmellLikelyIneffectiveObjectComparison',
  'TestSmellObscureInlineSetup',
  'TestSmellOverreferencing',
  'TestSmellRedundantAssertion',
  'TestSmellRottenGreenTests',
  'TestSmellTestRedundancy',
  'TestSmellUnknownTest',
  'TestSmellUnrelatedAssertions',
  'TestSmellUnusedInputs',
  'TestSmellVerboseTest'
)

stopifnot(length(smells_before_post_process) == length(smells_after_post_process))
smells <- smells_after_post_process

# Load data
df_before_post_process <- load_data(INPUT_FILE, smells_before_post_process)
df_after_post_process  <- load_data(INPUT_FILE, smells_after_post_process)
# Rename 'before' smells to ease concatenation of both data.frames
names(df_before_post_process) <- sapply(names(df_before_post_process), function(x) gsub("BeforePostProcess$", '', x))

# Set whether a smell has measure before or after EvoSuite's post-processing step
df_before_post_process$'smell_at' <- 'Before post-process'
df_after_post_process$'smell_at'  <- 'After post-process'

# Append/Concatena pre and post-process data
df <- rbind(df_before_post_process, df_after_post_process)
stopifnot(nrow(df) == nrow(df_before_post_process) + nrow(df_after_post_process))

# Elect a configuration to analyze
df <- df[df$'configuration_id' == CONFIGURATION_ID, ]

# Set order of factors
df$'smell_at' <- factor(df$'smell_at', levels=c('Before post-process', 'After post-process'))

# Revert a normalized value to its non-normalized value
df <- compute_non_normalized_values(df, smells)
# Collect raw-smells-columns created by the `compute_non_normalized_values` function
raw_smells <- grep(pattern='^RawTestSmell', x=colnames(df), value=TRUE)

# Apply thresholds
df <- apply_thresholds(df)
# Collect smelly-columns created by the `apply_thresholds` function
smelly <- grep(pattern='^SmellyTestSmell', x=colnames(df), value=TRUE)

# Aggregate data at seed level
agg_at_seed <- aggregate(x=. ~ smell_at + configuration_id + group_id + TARGET_CLASS + Random_Seed, data=df, FUN=mean)
print(head(agg_at_seed)) # debug
print(summary(agg_at_seed)) # debug

# Compute smelliness of each test suite
agg_at_seed <- compute_smelliness(agg_at_seed, smells, column_name='Smelliness (all smells)')
agg_at_seed <- compute_smelliness(agg_at_seed, c(
  'TestSmellEagerTest',
  'TestSmellIndirectTesting',
  'TestSmellObscureInlineSetup',
  'TestSmellOverreferencing',
  'TestSmellRottenGreenTests',
  'TestSmellVerboseTest'
), column_name='Smelliness (optimized smells)')

# Aggregate data at classel level, using different functions
agg_mean   <- aggregate(x=. ~ smell_at + configuration_id + group_id + TARGET_CLASS, data=agg_at_seed, FUN=mean)
agg_median <- aggregate(x=. ~ smell_at + configuration_id + group_id + TARGET_CLASS, data=agg_at_seed, FUN=median)
agg_min    <- aggregate(x=. ~ smell_at + configuration_id + group_id + TARGET_CLASS, data=agg_at_seed, FUN=min)
agg_max    <- aggregate(x=. ~ smell_at + configuration_id + group_id + TARGET_CLASS, data=agg_at_seed, FUN=max)

# ------------------------------------------------------------------------- Main

boxplot_it <- function(df, xname='', yname='', normalized=TRUE, split=FALSE) {
  num_smells_at <- length(unique(df$'smell_at'))
  # Basic boxplot
  if (num_smells_at == 1) {
    p <- ggplot(df, aes(x=variable, y=value)) + geom_boxplot(width=0.50)
  } else {
    p <- ggplot(df, aes(x=variable, y=value, fill=smell_at)) + geom_boxplot(width=0.50)
  }
  # Change x axis label
  p <- p + scale_x_discrete(name=xname)
  # Change y axis label
  p <- p + scale_y_continuous(name=yname) #, trans='log10')
  if (normalized) {
    # Remove legend's title and increase size of [x-y]axis labels
    p <- p + theme(legend.position='top', # ifelse(split, 'none', 'top'),
      legend.title=element_blank(),
      axis.text.x=element_text(size=13,  hjust=0.5, vjust=0.5),
      axis.text.y=element_text(size=13,  hjust=1.0, vjust=0.0),
      axis.title.x=element_text(size=15, hjust=0.5, vjust=0.0),
      axis.title.y=element_text(size=15, hjust=0.5, vjust=0.5)
    )
    # Make it horizontal
    p <- p + coord_flip()
    if (split) { # Create facets
      p <- p + facet_wrap(~ smell_at)
    }
    # Add mean lines for each color/fill
    zzz <- aggregate(value ~ smell_at, data=df, FUN=mean)
    zzz$'value' <- round(zzz$'value', 2)
    p <- p + geom_hline(data=zzz, aes(yintercept=value, color=smell_at), size=1, linetype='longdash')
    p <- p + geom_text(data=zzz, aes(x=0.5, y=value, label=value, color=smell_at), vjust=-0.10, hjust=-0.25)
  } else {
    # Remove legend's title and x-ticks, and increase size of y-axis labels
    p <- p + theme(legend.position='top', # ifelse(split, 'none', 'top'),
      legend.title=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_text(size=11,  hjust=1.0, vjust=0.0),
      axis.title.y=element_text(size=15, hjust=0.5, vjust=0.5)
    )
    # Create facets
    if (split) {
      p <- p + facet_grid(smell_at ~ variable, scale='free')
    } else {
      p <- p + facet_wrap(~ variable, ncol=4, scale='free')
    }
  }
  # Add mean points
  if (num_smells_at == 1) {
    p <- p + stat_summary(fun=mean, geom='point', shape=8, size=2, fill='black', color='black')
  }

  # Print it
  print(p)
}

# Remove any existing output file and create a new one
OUTPUT_PDF_FILE <- paste0(OUTPUT_PDF_DIR, '/', CONFIGURATION_ID, '-data.pdf')
unlink(OUTPUT_PDF_FILE)
pdf(file=OUTPUT_PDF_FILE, family='Helvetica', width=12, height=7)
plot_label(paste0('Standalone analysis of\n', CONFIGURATION_ID))

#
# Plot normalized metric values
#

plot_label('Normalized metric values')
x            <- reshape2::melt(agg_mean, id.vars=c('smell_at', 'configuration_id', 'TARGET_CLASS'), measured.vars=smells)
x            <- x[x$'variable' %in% smells, ]
x$'variable' <- sapply(x$'variable', function(x) gsub("^TestSmell", '', x))
x$'value'    <- as.numeric(x$'value')
boxplot_it(x, xname='Metric', yname='Normalized metric value', split=FALSE)
boxplot_it(x, xname='Metric', yname='Normalized metric value', split=TRUE)

#
# Plot raw metric values
#

plot_label('Non-Normalized metric values')
x            <- reshape2::melt(agg_mean, id.vars=c('smell_at', 'configuration_id', 'TARGET_CLASS'), measured.vars=raw_smells)
x            <- x[x$'variable' %in% raw_smells, ]
x$'variable' <- sapply(x$'variable', function(x) gsub("^Raw", '', x))
x$'variable' <- sapply(x$'variable', function(x) gsub("^TestSmell", '', x))
x$'value'    <- as.numeric(x$'value')
boxplot_it(x, yname='Raw metric value', normalized=FALSE, split=FALSE)
boxplot_it(x, yname='Raw metric value', normalized=FALSE, split=TRUE)

# % of test cases with smell X

# mean
plot_label('% of test cases with smell X\nmean')
x            <- reshape2::melt(agg_mean, id.vars=c('smell_at', 'configuration_id', 'TARGET_CLASS'), measured.vars=smelly)
x            <- x[x$'variable' %in% smelly, ]
x$'variable' <- sapply(x$'variable', function(x) gsub("^SmellyTestSmell", '', x))
x$'value'    <- as.numeric(x$'value')
x$'value'    <- x$'value' * 100.0
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=FALSE)
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=TRUE)

# median
plot_label('% of test cases with smell X\nmedian')
x            <- reshape2::melt(agg_median, id.vars=c('smell_at', 'configuration_id', 'TARGET_CLASS'), measured.vars=smelly)
x            <- x[x$'variable' %in% smelly, ]
x$'variable' <- sapply(x$'variable', function(x) gsub("^SmellyTestSmell", '', x))
x$'value'    <- as.numeric(x$'value')
x$'value'    <- x$'value' * 100.0
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=FALSE)
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=TRUE)
# min
plot_label('% of test cases with smell X\nmin -- best')
x            <- reshape2::melt(agg_min, id.vars=c('smell_at', 'configuration_id', 'TARGET_CLASS'), measured.vars=smelly)
x            <- x[x$'variable' %in% smelly, ]
x$'variable' <- sapply(x$'variable', function(x) gsub("^SmellyTestSmell", '', x))
x$'value'    <- as.numeric(x$'value')
x$'value'    <- x$'value' * 100.0
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=FALSE)
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=TRUE)
# max
plot_label('% of test cases with smell X\nmax -- worst')
x            <- reshape2::melt(agg_max, id.vars=c('smell_at', 'configuration_id', 'TARGET_CLASS'), measured.vars=smelly)
x            <- x[x$'variable' %in% smelly, ]
x$'variable' <- sapply(x$'variable', function(x) gsub("^SmellyTestSmell", '', x))
x$'value'    <- as.numeric(x$'value')
x$'value'    <- x$'value' * 100.0
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=FALSE)
boxplot_it(x, xname='Smell', yname='% of test cases with smell X', split=TRUE)

#
# Plot metrics and smells as a kernel density plot
#

kernel_plot_it <- function(df, smell_column, split=FALSE) {
  # Basic density plot
  p <- ggplot(df, aes(x=get(smell_column), fill=smell_at, colour=smell_at)) + geom_density(alpha=0.25, trim=TRUE)
  # Change x axis label
  p <- p + scale_x_continuous(name='Value')
  # Change y axis label
  p <- p + scale_y_continuous(name='Density')
  # Remove legend's title and increase size of [x-y]axis labels
  p <- p + theme(legend.position='top',
    legend.title=element_blank(),
    axis.text.x=element_text(size=13,  hjust=0.5, vjust=0.5),
    axis.text.y=element_text(size=13,  hjust=1.0, vjust=0.0),
    axis.title.x=element_text(size=15, hjust=0.5, vjust=0.0),
    axis.title.y=element_text(size=15, hjust=0.5, vjust=0.5)
  )
  if (split) {
    p <- p + facet_wrap(~ smell_at)
  }
  # Print it
  print(p)
}

for (smell_column in c(smells, raw_smells, smelly)) {
  plot_label(paste('Kernel density plot of ', smell_column, sep=''))
  # one smell_at per page
  for (smell_at in unique(agg_mean$'smell_at')) {
    kernel_plot_it(agg_mean[agg_mean$'smell_at' == smell_at, ], smell_column)
  }
  # all smell_at in one page
  kernel_plot_it(agg_mean, smell_column, split=FALSE)
  kernel_plot_it(agg_mean, smell_column, split=TRUE)
}

#
# Plot correlations
#

plot_corr <- function(df) {
  corr_matrix      <- cor(df, method=c('spearman'))
  corr_test_matrix <- cor.mtest(df, conf.level=0.95)

  # Plot all correlations
  plot_label('All correlations')
  corrplot(corr_matrix, method='circle', type='lower', diag=FALSE, cl.ratio=0.2, tl.srt=45, tl.col='grey25')

  # Plot only significant correlations
  plot_label('Significant correlations')
  corrplot(corr_matrix, method='circle', type='lower', diag=FALSE, cl.ratio=0.2, tl.srt=45, tl.col='grey25',
    p.mat=corr_test_matrix$'p', sig.level=0.05, insig='blank', addCoef.col='black', number.cex=0.4)

  plot_label('As a correlation matrix chart')
  chart.Correlation(df, histogram=FALSE, method=c('spearman'))
}

for (smell_at in unique(agg_mean$'smell_at')) {
  plot_label(paste0('Normalized values\n', smell_at))
  x        <- agg_mean[agg_mean$'smell_at' == smell_at, which(colnames(agg_mean) %in% smells)]
  names(x) <- sapply(names(x), function(z) gsub("^TestSmell", '', z))
  plot_corr(x)
}

for (smell_at in unique(agg_mean$'smell_at')) {
  plot_label(paste0('Raw values\n', smell_at))
  x        <- agg_mean[agg_mean$'smell_at' == smell_at, which(colnames(agg_mean) %in% raw_smells)]
  names(x) <- sapply(names(x), function(z) gsub("^RawTestSmell", '', z))
  plot_corr(x)
}

#
# Plot number of classes with X% of test with smells
#

bar_plot_it <- function(df) {
  df <- df[df$'value' > 0, ]
  df$'interval' <- NA
  df$'interval'[                   df$'value' <= 1]   <-  '1'
  df$'interval'[df$'value' >  1  & df$'value' <= 10]  <- '10'
  df$'interval'[df$'value' > 10  & df$'value' <= 20]  <- '20'
  df$'interval'[df$'value' > 20  & df$'value' <= 30]  <- '30'
  df$'interval'[df$'value' > 30  & df$'value' <= 40]  <- '40'
  df$'interval'[df$'value' > 40  & df$'value' <= 50]  <- '50'
  df$'interval'[df$'value' > 50  & df$'value' <= 60]  <- '60'
  df$'interval'[df$'value' > 60  & df$'value' <= 70]  <- '70'
  df$'interval'[df$'value' > 70  & df$'value' <= 80]  <- '80'
  df$'interval'[df$'value' > 80  & df$'value' <= 90]  <- '90'
  df$'interval'[df$'value' > 90                    ]  <- '100'
  stopifnot(NA %!in% unique(df$'interval'))
  df$'interval' <- factor(df$'interval', levels=c('1', '10', '20', '30', '40', '50', '60', '70', '80', '90', '100'))

  # Basic histogram
  p <- ggplot(df, aes(x=interval, y=..count..)) + geom_bar()
  # Change x axis label
  p <- p + scale_x_discrete(name='% of test cases with smell X')
  # Change y axis label
  # p <- p + scale_y_continuous(name='# Occurrences', expand=expansion(add=50))
  p <- p + scale_y_continuous(name='# Occurrences', limits=c(0,length(unique(df$'TARGET_CLASS'))+50))
  # Remove legend's title and increase size of [x-y]axis labels
  p <- p + theme(legend.position='none',
    axis.text.x=element_text(size=12,  hjust=0.5, vjust=0.5),
    axis.text.y=element_text(size=12,  hjust=1.0, vjust=0.0),
    axis.title.x=element_text(size=15, hjust=0.5, vjust=0.0),
    axis.title.y=element_text(size=15, hjust=0.5, vjust=0.5)
  )
  p <- p + facet_wrap(~ variable, ncol=4, drop=FALSE) #, scale='free_y')
  # Add labels over bars
  p <- p + stat_count(geom='text', colour='black', size=4, aes(label=..count..), position=position_identity(), vjust=-0.5)
  # Print it
  print(p)
  print(layer_data(p)) # Debug me
}

for (smell_at in unique(agg_mean$'smell_at')) {
  plot_label(paste0('Number of classes with X% of test with smells\n', smell_at))
  x            <- agg_mean[agg_mean$'smell_at' == smell_at, ]
  x            <- reshape2::melt(x, id.vars=c('configuration_id', 'TARGET_CLASS'), measured.vars=smelly)
  x            <- x[x$'variable' %in% smelly, ]
  x$'variable' <- sapply(x$'variable', function(x) gsub("^SmellyTestSmell", '', x))
  x$'variable' <- factor(x$'variable', levels=sapply(smelly, function(x) gsub("^SmellyTestSmell", '', x))) # Set all levels
  x$'value'    <- as.numeric(x$'value')
  x$'value'    <- x$'value' * 100.0
  bar_plot_it(x)
}

# Close output file
dev.off()
# Embed fonts
embed_fonts_in_a_pdf(OUTPUT_PDF_FILE)

# Tex tables

table_it <- function(tex_file, df, columns, average=TRUE, smelliness=FALSE) {
  # Set and init tex file
  unlink(tex_file)
  sink(tex_file, append=FALSE, split=TRUE)
  # Header
  cat('\\begin{tabular}{@{\\extracolsep{\\fill}} l rrr rrr r} \\toprule\n', sep='')
  cat('       & \\multicolumn{3}{c}{Before post-process}                                                   & \\multicolumn{3}{c}{After post-process}                                                    & \\multicolumn{1}{c}{Relative} \\\\\n', sep='')
  cat('Metric & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{$\\bar{x}$} & \\multicolumn{1}{c}{$\\sigma$} & \\multicolumn{1}{c}{CI} & \\multicolumn{1}{c}{improvement} \\\\\n', sep='')
  cat('\\midrule\n', sep='')

  table_data <- matrix(, nrow=length(columns), ncol=9); i <- 1
  for (column in columns) {
    cat(gsub("^TestSmell", '', gsub("^Raw", '', gsub("^Smelly", '', column))), sep='')

    j <- 0
    for (smell_at in c('Before post-process', 'After post-process')) {
      values <- df[[column]][df$'smell_at' == smell_at]
      ci     <- get_ci(values)

      # mean
      j <- j + 1; table_data[i, j] <- ifelse(grepl("^Smelly", column), round(mean(values)*100.0, 2), round(mean(values), 2))
      cat(' & ', sprintf('%.2f', table_data[i, j]), ifelse(grepl("^Smelly", column), '\\%', ''), sep='')

      # sd
      j <- j + 1; table_data[i, j] <- round(sd(values), 2)
      cat(' & ', sprintf('%.2f', table_data[i, j]), sep='')

      # ci
      j <- j + 1; table_data[i, j] <- round(ci[1], 2)
      cat(' & [', sprintf('%.2f', table_data[i, j]), sep='')
      j <- j + 1; table_data[i, j] <- round(ci[2], 2)
      cat(', ', sprintf('%.2f', table_data[i, j]), ']', sep='')
    }

    values_before <- df[[column]][df$'smell_at' == 'Before post-process']
    values_after  <- df[[column]][df$'smell_at' == 'After post-process']
    j <- j + 1; table_data[i, j] <- round(relative_improvement_value(values_before, values_after)*100.0, 2)
    cat(' & ', sprintf('%.2f', table_data[i, j]), '\\%', sep='')

    cat(' \\\\\n', sep='')
    i <- i + 1
  }

  if (average == TRUE) {
    cat('\\midrule\n', sep='')
    cat('Average', sep='')
    for (j in 1:ncol(table_data)) {
      cat(ifelse(j==4||j==8, ', ', ' & '), ifelse(j==3||j==7, '[', ''),
          sprintf('%.2f', round(mean(table_data[, j]), 2)),
          ifelse(j==4||j==8, ']', ifelse(((j==1||j==5) && grepl("^Smelly", columns[1])) || j==ncol(table_data), '\\%', '')), sep='')
    }
    cat(' \\\\\n', sep='')
  }

  if (smelliness == TRUE) {
    cat('\\midrule\n', sep='')
    for (smelliness_column in c('Smelliness (all smells)', 'Smelliness (optimized smells)')) {
      cat(smelliness_column, sep='')

      for (smell_at in c('Before post-process', 'After post-process')) {
        values <- df[[smelliness_column]][df$'smell_at' == smell_at]
        ci     <- get_ci(values)

        # mean
        cat(' & ', sprintf('%.2f', round(mean(values), 2)), sep='')

        # sd
        cat(' & ', sprintf('%.2f', round(sd(values), 2)), sep='')

        # ci
        cat(' & [', sprintf('%.2f', round(ci[1], 2)), sep='')
        cat(', ', sprintf('%.2f', round(ci[2], 2)), ']', sep='')
      }

      values_before <- df[[smelliness_column]][df$'smell_at' == 'Before post-process']
      values_after  <- df[[smelliness_column]][df$'smell_at' == 'After post-process']
      cat(' & ', sprintf('%.2f', round(relative_improvement_value(values_before, values_after)*100.0, 2)), '\\%', sep='')

      cat(' \\\\\n', sep='')
    }
  }

  # Table's footer
  cat('\\bottomrule', '\n', sep='')
  cat('\\end{tabular}', '\n', sep='')
  sink()
}

table_it(paste0(OUTPUT_TEX_DIR, '/', CONFIGURATION_ID, '-data-normalized-values.tex'), agg_mean, smells, average=FALSE, smelliness=TRUE)
table_it(paste0(OUTPUT_TEX_DIR, '/', CONFIGURATION_ID, '-data-raw-values.tex'), agg_mean, raw_smells, average=FALSE, smelliness=TRUE)
table_it(paste0(OUTPUT_TEX_DIR, '/', CONFIGURATION_ID, '-data-percentage-tests-with-smell-X.tex'), agg_mean, smelly, average=TRUE, smelliness=FALSE)

# EOF
