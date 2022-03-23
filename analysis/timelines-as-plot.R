# ------------------------------------------------------------------------------
# This script plots all TestSmell*Timeline over time.
#
# Usage:
#   Rscript timelines-as-plot.R
#     <input data file, e.g., ../data/data.csv.gz>
#     <output pdf file, e.g., timelines-as-plot.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

# Load external packages
library('ggplot2', lib.loc='../tools/R/x86_64-pc-linux-gnu-library/3.6.3') # FIXME path
library('reshape2', lib.loc='../tools/R/x86_64-pc-linux-gnu-library/3.6.3') # FIXME path

# --------------------------------------------------------------- Util functions

get_timeline_timestamp <- function(timeline) {
  return(as.numeric(gsub(".*Timeline_T([0-9]+)$", "\\1", timeline)))
}

get_variable_name <- function(timeline) {
  return(gsub("(.*)Timeline_T[0-9]+$", "\\1", timeline))
}

# ------------------------------------------------------------------------- Args

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop('USAGE: Rscript timelines-as-plot.R <input data file, e.g., ../data/data.csv.gz> <output pdf file, e.g., timelines-as-plot.pdf>')
}

# Args
INPUT_FILE  <- args[1]
OUTPUT_FILE <- args[2]

# ------------------------------------------------------------------------- Main

# Load data
df        <- load_TABLE(INPUT_FILE)
# Collect timeline smelly columns
timelines <- grep(pattern='^TestSmell.*Timeline_T', x=colnames(df), value=TRUE)
# Select relevant columns for this script
df        <- df[ , which(colnames(df) %in% c(timelines)) ] # FIXME we might need to select configuration, target_class, and/or algorithm once we have data for all experiments

#
# Pre-process data
#

# Create formula to agg
form <- as.formula(paste('cbind(', paste(timelines, collapse=','), ') ~ .', sep=' '))
# Compute mean of columns
agg_mean <- aggregate(formula=form, data=df, FUN=mean)
agg_mean <- melt(agg_mean, id.vars=c(), measure.vars=timelines, value.name='mean')
# Compute sd of columns
agg_sd   <- aggregate(formula=form, data=df, FUN=sd)
agg_sd   <- melt(agg_sd, id.vars=c(), measure.vars=timelines, value.name='sd')
# Compute max of columns
agg_max  <- aggregate(formula=form, data=df, FUN=max)
agg_max  <- melt(agg_max, id.vars=c(), measure.vars=timelines, value.name='max')
# Compute min of columns
agg_min  <- aggregate(formula=form, data=df, FUN=min)
agg_min  <- melt(agg_min, id.vars=c(), measure.vars=timelines, value.name='min')
# Melt the data frame into a more suitable structure to plot
by_c <- c('variable')
df   <- merge(agg_min, merge(agg_max, merge(agg_mean, agg_sd, by=by_c), by=by_c), by=by_c)
# Rename column's name that contains all timestamps in the format, e.g., TestSmellAssertionRouletteTimeline_T1
names(df)[names(df) == 'variable'] <- 'timestamp'
# Create a new column for the smell name based on the timeline name
df$'timeline'  <- lapply(df$'timestamp', get_variable_name)
df$'timeline'  <- as.factor(unlist(df$'timeline'))
# Rename timelines' timestamps to ease the creation of the x-axis
df$'timestamp' <- as.numeric(lapply(df$'timestamp', get_timeline_timestamp))

#
# Plot data
#

# Remove any existing output file and create a new one
unlink(OUTPUT_FILE)
pdf(file=OUTPUT_FILE, family='Helvetica', width=13, height=10)
# Add a cover page to the output file
plot_label('Timelines as plot')

plot_it <- function(df, facets=FALSE) {
  # Create ggplot instance
  gg <- ggplot(df, aes(x=timestamp, y=mean, ymin=min, ymax=max)) + geom_pointrange(size=0.20, shape=16, color='black')
  # Change x-axis label
  gg <- gg + xlab('Seconds')
  # Set x-axis range
  gg <- gg + scale_x_continuous(limits=c(0, max(df$'timestamp')), breaks=seq(0, max(df$'timestamp'), by=max(df$'timestamp') / (max(df$'timestamp') / 10)))
  # Change y-axis label
  gg <- gg + ylab('')
  # Set y-axis range
  if (max(df$'max') > 1.0) {
    gg <- gg + scale_y_continuous(limits=c(0.0, max(df$'max')))
  } else {
    gg <- gg + scale_y_continuous(limits=c(0.0, 1.0), breaks=seq(0.0, 1.0, by=0.1))
  }
  # Create facets, one per timeline variable
  if (facets) {
    gg <- gg + facet_wrap(~ timeline, ncol=3)
  }
  # Print it
  print(gg)
}

# Overall
plot_label('Overall')
plot_it(df, facets=TRUE)

# Per timeline
for (timeline in unique(df$'timeline')) {
  plot_label(timeline)
  plot_it(df[df$'timeline' == timeline, ], facets=FALSE)
}

# Per class under test
# TODO

# Close output file
dev.off()
# Embed fonts
embed_fonts_in_a_pdf(OUTPUT_FILE)

# EOF