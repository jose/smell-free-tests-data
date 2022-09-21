# ------------------------------------------------------------------------------
# This script plots, per configuration, all TestSmell*Timeline over time.
#
# Usage:
#   Rscript timelines-as-plot.R
#     <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz>
#     <output pdf file, e.g., experiments-data-timelines-as-plot.pdf>
# ------------------------------------------------------------------------------

source('../utils/analysis/utils.R')

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
  stop('USAGE: Rscript timelines-as-plot.R <input data file, e.g., ../test-generation/data/generated/experiments-data.csv.gz> <output pdf file, e.g., experiments-data-timelines-as-plot.pdf>')
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
df        <- df[ , which(colnames(df) %in% c('configuration_id', 'group_id', 'TARGET_CLASS', timelines)) ]
# Aggregate data
df        <- aggregate(x=as.formula(paste('cbind(', paste(timelines, collapse=','), ') ~ configuration_id + group_id + TARGET_CLASS', sep=' ')), data=df, FUN=mean)
# Pretty configurations' names
df$'configuration_id' <- sapply(df$'configuration_id', pretty_configuration_id)

#
# Pre-process data
#

# Create formula to agg
form <- as.formula(paste('cbind(', paste(timelines, collapse=','), ') ~ configuration_id', sep=' '))
# Compute mean of columns
agg_mean <- aggregate(x=form, data=df, FUN=mean)
agg_mean <- melt(agg_mean, id.vars=c('configuration_id'), measure.vars=timelines, value.name='mean')
# Compute sd of columns
agg_sd   <- aggregate(x=form, data=df, FUN=sd)
agg_sd   <- melt(agg_sd, id.vars=c('configuration_id'), measure.vars=timelines, value.name='sd')
# Compute max of columns
agg_max  <- aggregate(x=form, data=df, FUN=max)
agg_max  <- melt(agg_max, id.vars=c('configuration_id'), measure.vars=timelines, value.name='max')
# Compute min of columns
agg_min  <- aggregate(x=form, data=df, FUN=min)
agg_min  <- melt(agg_min, id.vars=c('configuration_id'), measure.vars=timelines, value.name='min')
# Melt the data frame into a more suitable structure to plot
by_c <- c('configuration_id', 'variable')
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
pdf(file=OUTPUT_FILE, family='Helvetica', width=11, height=11)
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

# One configuration per page, all timelines in a single page
for (configuration_id in unique(df$'configuration_id')) {
  plot_label(paste('Optimized\n', configuration_id, sep=''))
  plot_it(df[df$'configuration_id' == configuration_id, ], facets=TRUE)
}

# Per class under test
# TODO

# Close output file
dev.off()
# Embed fonts
embed_fonts_in_a_pdf(OUTPUT_FILE)

# EOF
