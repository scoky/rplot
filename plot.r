#!/usr/bin/env Rscript
require(argparse)

parser <- ArgumentParser(description='Generate a plot')
parser$add_argument('--sources', nargs='*', help='source x and y point files')
parser$add_argument('--sourcelabels', nargs='*', help='labels for the sources')
parser$add_argument('--xlabel', default='x', help='label for the x-axis')
parser$add_argument('--ylabel', default='y', help='label for the y-axis')
parser$add_argument('--xscale', default='linear', choices=c('log', 'linear', 'asinh'), help='scaling for the x-axis')
parser$add_argument('--yscale', default='linear', choices=c('log', 'linear', 'asinh'), help='scaling for the y-axis')
parser$add_argument('--xrange', nargs='*', help='range for the x-axis')
parser$add_argument('--yrange', nargs='*', help='range for the y-axis')
parser$add_argument('--xtype', default='none', help='type of the x data')
parser$add_argument('--ytype', default='none', help='type of the y data')
parser$add_argument('--xbreaks', nargs='*', help='tick points on the x-axis')
parser$add_argument('--ybreaks', nargs='*', help='tick points on the y-axis')
parser$add_argument('--xlabels', nargs='*', help='tick labels on the x-axis')
parser$add_argument('--ylabels', nargs='*', help='tick labels on the y-axis')
parser$add_argument('--fontsize', default='19', help='size of text')
parser$add_argument('--geom', nargs='*', default=c('step'), help='type of the y data')
parser$add_argument('--linetype', nargs='*', help='linetypes of data')
parser$add_argument('--colour', nargs='*', help='colour of data')
parser$add_argument('--shape', nargs='*', help='shape of data')
parser$add_argument('--fill', nargs='*', help='fill colour of data')
parser$add_argument('--alpha', nargs='*', default=c(1), help='transparency of data')
parser$add_argument('--size', nargs='*', default=c(1), help='size of data')
parser$add_argument('--canvas', nargs='*', default=c(7,5.75), help='size of the canvas')
parser$add_argument('--filename', default='Rplot.pdf', help='output filename')
parser$add_argument('--legend', default='true', choices=c('true', 'false'), help='show or hide legend')
parser$add_argument('--legendColumns', default='5', help='number of columns in the legend')
parser$add_argument('--keySize', default='2', help='Size of the key in the legend')
parser$add_argument('--flip', default='false', choices=c('true', 'false'), help='flip the x and y axis')
args <- parser$parse_args()
args$legendColumns = as.numeric(args$legendColumns)

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

# Collect sources
i <- 1
ns <- c()
xs <- c()
ys <- c()
y2s <- c()
srclabels <- c()
while (i <= length(args$sources)) {
    label <- paste('source', i)
    if (i <= length(args$sourcelabels)) {
        label = args$sourcelabels[i]
    }
    if (args$geom[min(i, length(args$geom))] == 'ribbon') {
        data <- read.table(args$sources[i], col.names = c('x', 'y', 'y2'))
        y2s <- c(y2s, data$y2)
    } else {
        data <- read.table(args$sources[i], col.names = c('x', 'y'))
        y2s <- c(y2s, rep(0, length(data$x)))
    }
    ns <- c(ns, rep(label, length(data$x)))
    xs <- c(xs, data$x)
    ys <- c(ys, data$y)
    srclabels <- c(srclabels, label)
    data <- NULL
    i <- i + 1
}
dt <- data.frame(factor(ns, levels=srclabels, labels=srclabels), xs, ys, y2s)
names(dt) <- c('f', 'x', 'y', 'y2')
ns <- NULL
xs <- NULL
ys <- NULL
y2s <- NULL
#srclabels <- match(srclabels, levels(dt$f))

require(ggplot2)
require(scales)

# Data Types
if (args$xtype == 'date') {
  dt$x <- as.POSIXct(dt$x, origin='1970-01-01')
}
if (args$ytype == 'date') {
  dt$y <- as.POSIXct(dt$y, origin='1970-01-01')
}

# Collect defaults
if (is.null(args$xrange)) {
  if (args$xscale == 'log') {
    args$xrange <- c(max(min(dt$x),1), max(dt$x))
  } else {
    args$xrange <- c(min(dt$x), max(dt$x))
  }
} else {
  args$xrange <- as.numeric(args$xrange)
}
if (is.null(args$yrange)) {
  if (args$yscale == 'log') {
    args$yrange <- c(max(min(dt$y),1), max(dt$y))
  } else {
    args$yrange <- c(min(dt$y), max(dt$y))
  }
} else {
  args$yrange <- as.numeric(args$yrange)
}


# Create plot
pl <- ggplot(dt, aes(x = x, y = y, group = f, colour = f, linetype = f, shape = f, fill = f))
if (is.null(args$fontsize)) {
    pl <- pl + theme_bw()
} else {
    pl <- pl + theme_bw(base_size = as.numeric(args$fontsize))
}

# How to draw
genGeom <- function(geom_name, dt, sze, aph) {
    if (geom_name == 'point') {
        geom_point(data = dt, alpha = aph, size = sze)
    } else if (geom_name == 'bar') {
        geom_bar(data = dt, stat = 'identity', alpha = aph)
    } else if (geom_name == 'ribbon') {
        geom_ribbon(data = dt, aes(ymin = y, ymax = y2), alpha = aph)
    } else if (geom_name == 'boxplot') {
        geom_boxplot(data = dt, aes(factor(x), y)) #, outlier.shape = NA)
    } else {
        get(paste('geom_', geom_name, sep=''))(data = dt, alpha = aph, size = sze)
    }
}

if (length(args$geom) == 1 && length(args$size) == 1 && length(args$alpha) == 1) {
    sze <- as.numeric(args$size[1])
    aph <- as.numeric(args$alpha[1])
    for (g in strsplit(args$geom[1], "\\+")[[1]]) {
        pl <- pl + genGeom(g, dt, sze, aph)
    }
} else {
    i <- 1
    while (i <= length(levels(dt$f))) {
        lev <- levels(dt$f)[i]
        gom <- args$geom[min(i, length(args$geom))]
        sze <- as.numeric(args$size[min(i, length(args$size))])
        aph <- as.numeric(args$alpha[min(i, length(args$alpha))])
        for (g in strsplit(gom, "\\+")[[1]]) {
            pl <- pl + genGeom(g, dt[dt$f == lev, ], sze, aph)
        }
        i <- i + 1
    }
}

# Scaling The Axis
if (is.null(args$ybreaks)) {
    args$ybreaks <- waiver()
} else {
    args$ybreaks <- as.numeric(args$ybreaks)
}
if (is.null(args$ylabels)) {
    args$ylabels <- comma
}
if (is.null(args$xbreaks)) {
    args$xbreaks <- waiver()
} else {
    args$xbreaks <- as.numeric(args$xbreaks)
}
if (is.null(args$xlabels)) {
    args$xlabels <- comma
}
if (args$yscale == 'log') {
  pl <- pl + scale_y_log10(breaks=args$ybreaks, labels=args$ylabels)
  args$ylabel <- paste(args$ylabel, '(log scale)')
} else if (args$yscale == 'asinh') {
  pl <- pl + scale_y_continuous(trans='asinh', breaks=args$ybreaks, labels=args$ylabels)
  args$ylabel <- paste(args$ylabel, '(asinh scale)')
} else {
  pl <- pl + scale_y_continuous(breaks=args$ybreaks, labels=args$ylabels)
}
if (args$xscale == 'log') {
  pl <- pl + scale_x_log10(breaks=args$xbreaks, labels=args$xlabels)
  args$xlabel <- paste(args$xlabel, '(log scale)')
} else if (args$xscale == 'asinh') {
  pl <- pl + scale_x_continuous(trans='asinh', breaks=args$xbreaks, labels=args$xlabels)
  args$xlabel <- paste(args$xlabel, '(asinh scale)')
} else {
  if (length(args$geom) == 1 && args$geom == 'boxplot') {
    pl <- pl + scale_x_discrete(breaks=args$xbreaks, labels=args$xlabels)
  } else {
    pl <- pl + scale_x_continuous(breaks=args$xbreaks, labels=args$xlabels)
  }
}

pl <- pl + ylab(args$ylabel) + xlab(args$xlabel) + coord_cartesian(xlim=args$xrange,ylim=args$yrange)
if (args$legend == 'true') {
  g <- guide_legend(ncol = args$legendColumns, keywidth = as.numeric(args$keySize))
  pl <- pl + theme(legend.position="top") + guides(colour = g, linetype = g, shape = g, fill = g) #+ guides(col = guide_legend(ncol = 4))
} else {
  pl <- pl + theme(legend.position="none")
}

if (is.null(args$linetype)) {
  pl <- pl + scale_linetype_discrete(name=element_blank(), breaks=srclabels)
} else {
  if (length(args$linetype) == 1) {
    args$linetype <- rep(args$linetype[1], length(levels(dt$f)))
  }
  pl <- pl + scale_linetype_manual(name=element_blank(), breaks=srclabels, values=as.numeric(args$linetype[order(srclabels)]))
}
if (is.null(args$shape)) {
  pl <- pl + scale_shape_discrete(name=element_blank(), breaks=srclabels)
} else {
  if (length(args$shape) == 1) {
    args$shape <- rep(args$shape[1], length(levels(dt$f)))
  }
  pl <- pl + scale_shape_manual(name=element_blank(), breaks=srclabels, values=as.numeric(args$shape[order(srclabels)]))
}
if (is.null(args$colour)) {
  pl <- pl + scale_colour_discrete(name=element_blank(), breaks=srclabels)
} else {
  if (length(args$colour) == 1) {
    args$colour <- rep(args$colour[1], length(levels(dt$f)))
  }
  pl <- pl + scale_colour_manual(name=element_blank(), breaks=srclabels, values=args$colour[order(srclabels)])
}
if (is.null(args$fill)) {
  pl <- pl + scale_fill_discrete(name=element_blank(), breaks=srclabels)
} else {
  if (length(args$fill) == 1) {
    args$fill <- rep(args$fill[1], length(levels(dt$f)))
  }
  pl <- pl + scale_fill_manual(name=element_blank(), breaks=srclabels, values=args$fill[order(srclabels)])
}

# Output plot
ggsave(args$filename, pl, width=as.numeric(args$canvas[1]), height=as.numeric(args$canvas[2]))
