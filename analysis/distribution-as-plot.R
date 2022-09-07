# ------------------------------------------------------------------------------
# This script plots the distribution of several columns (e.g., coverage, mutation,
# size, smelliness) as boxplot.
#
# Usage:
#   Rscript distribution-as-plot.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
#     <output pdf file, e.g., distribution-as-plot.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
library('ggplot2') # install.packages('ggplot2')

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript distribution-as-plot.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output pdf file, e.g., distribution-as-plot.pdf>')
}

# Args
INPUT_FILE  <- args[1]
OUTPUT_FILE <- args[2]

# ------------------------------------------------------------------------- Main

COLUMNS_TO_ANALYZE <- c('Size', 'OverallCoverage', 'MutationScore', 'Smelliness') # TODO anything else
RELATIVE_COLUMNS_TO_ANALYZE <- c('Size', 'RelativeOverallCoverage', 'RelativeMutationScore', 'RelativeSmelliness') # TODO anything else

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

# Load data
df <- load_data(INPUT_FILE, smells)
# Compute smelliness of each test
df <- compute_smelliness(df, smells)
# Aggregate `df` so that we have coverage, mutation score, and smelliness values per configuration, target class, and random seed
# Note that at this point `df` is at test case level
df <- aggregate(as.formula(paste0('cbind(', paste0(c(COLUMNS_TO_ANALYZE, smells), collapse=','), ') ~ configuration_id + TARGET_CLASS + Random_Seed')), data=df, FUN=mean)
# Compute relative OverallCoverage, MutationScore, and Smelliness
df <- compute_relativeness(df, c('OverallCoverage'))
df <- compute_relativeness(df, c('MutationScore'))
df <- compute_relativeness(df, c(smells, 'Smelliness'))
# Aggregate `df` so that we have coverage, mutation score, and smelliness values per configuration, target class
df <- aggregate(as.formula(paste0('cbind(', paste0(RELATIVE_COLUMNS_TO_ANALYZE, collapse=','), ') ~ configuration_id + TARGET_CLASS')), data=df, FUN=mean)
# Pretty configurations' names
df$'configuration_id' <- sapply(df$'configuration_id', pretty_configuration_id_as_abbreviation)
print(head(df)) # debug
print(summary(df)) # debug

# Remove any existing output file and create a new one
unlink(OUTPUT_FILE)
pdf(file=OUTPUT_FILE, family='Helvetica', width=7, height=4)
# Add a cover page to the output file
plot_label('Distributions as boxplot')

#
# Create boxplot
#
plot_it <- function(df, y_var) {
  # Basic boxplot with colors by groups
  p <- ggplot(df, aes(x=configuration_id, y=get(y_var), fill=configuration_id)) + geom_violin() + geom_boxplot(width=0.25)
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

for (column in RELATIVE_COLUMNS_TO_ANALYZE) {
  plot_label(column)
  plot_it(df, column)
}

# Close output file
dev.off()
# Embed fonts
embed_fonts_in_a_pdf(OUTPUT_FILE)

# EOF
