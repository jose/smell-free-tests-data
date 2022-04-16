# ------------------------------------------------------------------------------
# This script plots the distribution of smells as boxplot.
#
# Usage:
#   Rscript distribution-as-plot.R
#     <input data file, e.g., ../test-generation/data/generated/data.csv.gz>
#     <output pdf file, e.g., smell-distribution-as-plot.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
library('ggplot2', lib.loc='../tools/R/x86_64-pc-linux-gnu-library/3.6.3') # FIXME path
library('reshape2', lib.loc='../tools/R/x86_64-pc-linux-gnu-library/3.6.3') # FIXME path

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript distribution-as-plot.R <input data file, e.g., ../test-generation/data/generated/data.csv.gz> <output pdf file, e.g., smell-distribution-as-plot.pdf>')
}

# Args
INPUT_FILE  <- args[1]
OUTPUT_FILE <- args[2]

# ------------------------------------------------------------------------- Main

# Load data
df                     <- load_TABLE(INPUT_FILE)
# Collect smelly columns
all_smell_columns      <- grep(pattern='^TestSmell', x=colnames(df), value=TRUE)
timeline_smell_columns <- grep(pattern='^TestSmell.*Timeline_T', x=colnames(df), value=TRUE)
smell_columns          <- setdiff(all_smell_columns, timeline_smell_columns)
# Select relevant columns for this script
df                     <- df[ , which(colnames(df) %in% c(smell_columns)) ] # FIXME we might need to select configuration, target_class, and/or algorithm once we have data for all experiments
# Convert dataframe from column-based to row-based to ease plotting
df                     <- reshape2::melt(df)

# Remove any existing output file and create a new one
unlink(OUTPUT_FILE)
pdf(file=OUTPUT_FILE, family='Helvetica', width=12, height=7)
# Add a cover page to the output file
plot_label('Distributions as boxplot')

#
# Create boxplot
#

# Basic boxplot with colors by groups
p <- ggplot(df, aes(x=variable, y=value), fill=variable) + geom_violin() + geom_boxplot(width=0.25)
# Change x axis label
p <- p + scale_x_discrete(name='')
# Change y axis label and set its scale
p <- p + scale_y_continuous(name='', trans='sqrt')
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

# Close output file
dev.off()
# Embed fonts
embed_fonts_in_a_pdf(OUTPUT_FILE)

# EOF
