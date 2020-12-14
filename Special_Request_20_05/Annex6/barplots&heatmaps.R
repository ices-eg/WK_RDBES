# ============================================
# Barplots and heatmaps in annex 6 of ICES RDBES special request
# ============================================

		library(data.table)
		library(xlsx)
		library(mapplots)
		library(reshape2)
		
		source("funs/func_barplot_var_by_one_var.r")
		source("funs/func_heatmap_ices_rect_one_var.r")

	# =======================	
	# Inputs 
	# =======================
			
		# read RDBES data
		rm(list=ls())
		colour_table<-read.table("aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")

		# note: in this example tables are read from separate file; and alternative using RDBES extract format can be devised
			load("FO_df.Rdata")
			load("FT_df.Rdata")
			load("CLrdbes.Rdata")
			load("CErdbes.Rdata")

		# preparation of RDBES data
			# generic prep: creates a variable with landings in tonnes 
			CLrdbes$CLofficialWeight1000<-CLrdbes$CLofficialWeight/1000
			# example specific prep: subsets a species, gear, and area
			CLrdbes<-CLrdbes[CLrdbes$CLspeciesCode==126436 & CLrdbes$CLarea=="27.3.a.21" & substring(CLrdbes$CLmetier6,1,7) %in% c("OTB_MCD","OTT_MCD","OTB_DEF","OTT_DEF","OTT_CRU","OTT_CRU"),]
			CErdbes<-CErdbes[CErdbes$CEarea=="27.3.a.21" & substring(CErdbes$CEmetier6,1,7) %in% c("OTB_MCD","OTT_MCD","OTB_DEF","OTT_DEF","OTT_CRU","OTT_CRU"),]

		## read shapefiles	[required for map only]
			library(shapefiles)
			map_fao_areas<-list("shp" = read.shp("shapefiles/RCG_BA/RCG_BA_FAOareas.shp"), "shx" = read.shx("shapefiles/RCG_BA/RCG_BA_FAOareas.shx"), "dbf"= read.dbf("shapefiles/RCG_BA/RCG_BA_FAOareas.dbf"))
			map_ices_rect<-list("shp" = read.shp("shapefiles/RCG_BA/RCG_BA_ICESrect.shp"), "shx" = read.shx("shapefiles/RCG_BA/RCG_BA_ICESrect.shx"), "dbf"= read.dbf("shapefiles/RCG_BA/RCG_BA_ICESrect.dbf"))
			data_dir<-"shapefiles\\GSHHG\\gshhg-shp-2.3.7\\GSHHS_shp\\l"
			map_coast<-list("shp" = read.shp(paste(data_dir,"GSHHS_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"GSHHS_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"GSHHS_l_L1.dbf", sep="/")))
			data_dir<-"shapefiles\\GSHHG\\gshhg-shp-2.3.7\\WDBII_shp\\l"
			map_borders<-list("shp" = read.shp(paste(data_dir,"WDBII_border_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"WDBII_border_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"WDBII_border_l_L1.dbf", sep="/")))
			map_list<-list(map_ices_rect = map_ices_rect, map_fao_areas = map_fao_areas, map_ices_rect = map_ices_rect, map_coast = map_coast, map_borders = map_borders)


	# =================
	# Barplots
	# =================

	# select departure or arrival
		targetVar <- "FTdepartureDate"

	# select date type
		timeAgg <- "quarter"
		timeAgg <- "month"

	# add quarter or month
		if(timeAgg == "quarter") FT_df$targetVarTimeAgg<-factor(as.integer(gsub("Q","",quarters(as.Date(as.character(data.frame(FT_df)[[targetVar]])), abbreviate=T))), levels=c(1:4))
		if(timeAgg == "month") FT_df$targetVarTimeAgg<-factor(as.integer(format(as.Date(as.character(data.frame(FT_df)[[targetVar]])), format="%m" )), levels=c(1:12))

	# some calculations
		if(any(FT_df$FTstratification=="Y")) stop ("code for stratified designs not yet developed")
		nAggSampledExclNonResponses<-melt(table(FT_df$targetVarTimeAgg[FT_df$FTsampled=="Y"]))
		nAggSampledInclNonResponses<-melt(table(FT_df$targetVarTimeAgg))

		# ==========
		# monthly barplot
		# ==========
	
		if(timeAgg == "month"){
	
			# general barplot settings (taken from graphics::barplot.default)
			space=0.2
			width=rep_len(1, 12)
			w.r <- cumsum(space + width)
			delta <- width/2   
			w.m <- w.r - delta
			w.l <- w.m - delta
			xlimite=c(min(w.l), max(w.r))
			windows (15,15); par(xaxs="r",yaxs="i")

			a<-barplot_var_by_one_var (x = as.data.frame(CLrdbes),  Var = "CLofficialWeight1000", var1 = paste("CL", timeAgg, sep=""), tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = NULL, sorted = FALSE, graph_par=list(oma = c(1,1,1.5,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1.2, col=NA), grouped = FALSE, title_root="")
			par(new=TRUE, yaxs="i")
			plot(nAggSampledInclNonResponses$value~a$plot.coords, type="n", col="black", pch=19, axes=F, xlim=xlimite, ylim=c(0,max(nAggSampledInclNonResponses$value)*1.2),xlab="", ylab="")
			points(x = a$plot.coords, y = nAggSampledInclNonResponses$value, type="o", col="red", pch=19)
			par(new=TRUE, yaxs="i")
			plot(nAggSampledExclNonResponses$value~a$plot.coords, type="n", col="black", pch=19, axes=F, xlim=xlimite, ylim=c(0,max(nAggSampledInclNonResponses$value)*1.2),xlab="", ylab="")
			points(x = a$plot.coords, y = nAggSampledExclNonResponses$value, type="o", col="black", pch=19)
			axis(4,cex=1.2); mtext("Number of trips",4, line=2.5, cex=1.2)
			mtext("Month",1,line=3, cex=1.2)
			legend(x=0.3, y=max(nAggSampledInclNonResponses$value)*1.15, legend=c("target trips including non-response","trips effectively sampled"), lty=1, pch=19, col=c("red","black"), cex=1.2)
			box()
			}
			
		if(timeAgg == "quarter"){			
			
			# general barplot settings (taken from graphics::barplot.default)
			space=0.2
			width=rep_len(1, 4)
			w.r <- cumsum(space + width)
			delta <- width/2   
			w.m <- w.r - delta
			w.l <- w.m - delta
			xlimite=c(min(w.l), max(w.r))
			par(xaxs="r",yaxs="i")			
			
			a<-barplot_var_by_one_var (x = as.data.frame(CLrdbes),  Var = "CLofficialWeight1000", var1 = paste("CL", timeAgg, sep=""), tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = NULL, sorted = FALSE, graph_par=list(oma = c(1,1,1.5,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1, col=NA), grouped = FALSE, title_root="")
			par(new=TRUE, yaxs="i")
			plot(nAggSampledInclNonResponses$value~a$plot.coords, type="n", col="black", pch=19, axes=F, xlim=xlimite, ylim=c(0,max(nAggSampledInclNonResponses$value)*1.2),xlab="", ylab="")
			points(x = a$plot.coords, y = nAggSampledInclNonResponses$value, type="o", col="red", pch=19)
			par(new=TRUE, yaxs="i")
			plot(nAggSampledExclNonResponses$value~a$plot.coords, type="n", col="black", pch=19, axes=F, xlim=xlimite, ylim=c(0,max(nAggSampledInclNonResponses$value)*1.2),xlab="", ylab="")
			points(x = a$plot.coords, y = nAggSampledExclNonResponses$value, type="o", col="black", pch=19)
			axis(4); mtext("Number of trips",4, line=2.5, cex=0.8)
			mtext("Quarter",1,line=3, cex=0.8)
			legend(x=0.3, y=max(nAggSampledInclNonResponses$value)*1.15, legend=c("target trips including non-response","trips effectively sampled"), lty=1, pch=19, col=c("red","black"))
			box()
			}			
			}
			
	# =================
	# HeatMaps
	# =================

	standard_breaks <- c(0,quantile(tapply(CLrdbes$CLofficialWeight, CLrdbes$CLstatisticalRectangle, sum), probs=seq(0,1, by=0.10), na.rm=TRUE))

	windows(15,20)
	par(mfrow=c(2,1))

	heatmap_ices_rect_one_var(x = as.data.frame(CLrdbes), Var = "CLofficialWeight1000", var1 = "CLstatisticalRectangle", F=sum, map_title = "Fleet landings vs Hauls sampled", legend_title = "Tons", nbreaks = 6, xlim=c(10.5,14.5), ylim=c(55.25,59.5), legend_amp=1, map_list = map_list)
	points(FO_df$FOstartLon,FO_df$FOstartLat, col="black", pch=19, cex=0.7)
	aux<-reshape2::melt(table(ices.rect(data.frame(FO_df)$FOstatRect)))
	aux<-aux[aux$value!=0,]
	text(aux$lon, aux$lat, labels=aux$value, cex=0.9)

	heatmap_ices_rect_one_var(x = as.data.frame(CErdbes), Var = "CEscientificDaysAtSea", var1 = "CEstatisticalRectangle", F=sum, map_title = "Fleet days-at-sea vs hauls sampled", legend_title = "Days-at-sea", nbreaks = 6, xlim=c(10.5,14.5), ylim=c(55.25,59.5), legend_amp=1, map_list = map_list)
	points(FO_df$FOstartLon,FO_df$FOstartLat, col="black", pch=19, cex=0.7)
	aux<-reshape2::melt(table(ices.rect(data.frame(FO_df)$FOstatRect)))
	aux<-aux[aux$value!=0,]
	text(aux$lon, aux$lat, labels=aux$value, cex=0.9)















windows(15,7)
par(mfrow=c(1,2))

a<-barplot_var_by_one_var (x = as.data.frame(CLrdbes),  Var = "CLofficialWeight1000", var1 = paste("CL", timeAgg, sep=""), tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = NULL, sorted = FALSE, graph_par=list(oma = c(1,1,1.5,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1, col=NA), grouped = FALSE, title_root="")
par(new=TRUE)
plot(nAggSampled$value~a$plot.coords, type="n", col="black", pch=19, axes=F, xlim=c(0, nrow(aux)+1), ylim=c(0,max(aux$value)),xlab="", ylab="")
points(a$plot.coords, nAggSampled$value, type="o", col="black", pch=19)
axis(4); mtext("Number of samples",4, line=2.5, cex=0.8)
mtext("Quarter",1,line=3, cex=0.8)
box()

source("funs/func_barplot_var_by_one_var.r")

a<-barplot_var_by_one_var (x = as.data.frame(CErdbes),  Var = "CEscientificDaysAtSea", var1 = "CEquarter", tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = NULL, sorted = FALSE, graph_par=list(oma = c(1,1.5,1,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1, col=NA), grouped = TRUE, title_root="")
aux<-reshape2::melt(table()))

par(new=TRUE, xaxs="i")
plot(aux$value~a$plot.coords, type="n", col="black", pch=19, axes=F, xlim=c(0, nrow(aux)+1), ylim=c(0,max(aux$value)),xlab="", ylab="")
points(a$plot.coords, aux$value, type="o", col="black", pch=19)
axis(4); mtext("Number of samples",4, line=2.5, cex=0.8)
mtext("Quarter",1,line=3, cex=0.8)
box()

# month
	space=0.2
	width=rep_len(1, 12)
    w.r <- cumsum(space + width)
     delta <- width/2   
	w.m <- w.r - delta
    w.l <- w.m - delta
xlimite=c(min(w.l), max(w.r))
par(xaxs="r",yaxs="i")
a<-barplot_var_by_one_var (x = as.data.frame(CLrdbes),  Var = "CLofficialWeight1000", var1 = "CLmonth", tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = NULL, sorted = FALSE, graph_par=list(oma = c(1,1,1,1), mai = c(1,1,1,.5), ylab_line = 4, cex.x = 1, col=NA), grouped = FALSE, title_root="")
aux_samp<-reshape2::melt(table(factor(as.integer(format(as.Date(as.character(data.frame(FO_df[FO_df$FOsamp=="Y",])$FOstartDate)), format="%m" )), levels=c(1:12))))



aux_tot<-reshape2::melt(table(factor(as.integer(format(as.Date(as.character(data.frame(unique(FO_df[c("FTid","FOstartDate"],)$FOstartDate)), format="%m" )), levels=c(1:12))))





m=matrix(c(1:12,1:12,13:24),ncol=3)
par(new=FALSE, xaxs="r")
a<-barplot(m[,1])
par(new=TRUE, xaxs="i")
#xlimite=c(0,13.9)
#plot(m[,3]~m[,1], type="n", xlim=xlimite, col="black", pch=19, axes=F, xlab="", ylab="")
plot(m[,3]~m[,1], type="n", col="black", pch=19, axes=F, xlab="", ylab="")
axis(4); mtext("Number of samples",4, line=2.5, cex=0.8)
points(x = m[,1], y = m[,3], type="o", col="black", pch=19)


table(FO_df$FOmetier6)


## example
## read data
	# testData<-read.table("map_fish_data.txt", sep=" ", dec=".", header=TRUE)
## read shapefiles	
	# library(shapefiles)
	# map_fao_areas<-list("shp" = read.shp("ExampleData/shapefiles/RCG_BA/RCG_BA_FAOareas.shp"), "shx" = read.shx("ExampleData/shapefiles/RCG_BA/RCG_BA_FAOareas.shx"), "dbf"= read.dbf("ExampleData/shapefiles/RCG_BA/RCG_BA_FAOareas.dbf"))
	# map_ices_rect<-list("shp" = read.shp("ExampleData/shapefiles/RCG_BA/RCG_BA_ICESrect.shp"), "shx" = read.shx("ExampleData/shapefiles/RCG_BA/RCG_BA_ICESrect.shx"), "dbf"= read.dbf("ExampleData/shapefiles/RCG_BA/RCG_BA_ICESrect.dbf"))
	# data_dir<-"ExampleData/shapefiles\\GSHHG\\gshhg-shp-2.3.7\\GSHHS_shp\\l"
	# map_coast<-list("shp" = read.shp(paste(data_dir,"GSHHS_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"GSHHS_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"GSHHS_l_L1.dbf", sep="/")))
	# data_dir<-"ExampleData/shapefiles\\GSHHG\\gshhg-shp-2.3.7\\WDBII_shp\\l"
	# map_borders<-list("shp" = read.shp(paste(data_dir,"WDBII_border_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"WDBII_border_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"WDBII_border_l_L1.dbf", sep="/")))
	# map_list<-list(map_ices_rect = map_ices_rect, map_fao_areas = map_fao_areas, map_ices_rect = map_ices_rect, map_coast = map_coast, map_borders = map_borders)
## map data [internal breaks]
	# main_title<-"some fish data"
	# heatmap_ices_rect_one_var(x = testData, Var = "kg", var1 = "EURUTA", F=sum, map_title = main_title, legend_title = "kg", nbreaks = 6, xlim=c(10.5,25.5), ylim=c(52.25,65.5), legend_amp=1, map_list = map_list)
## map data [external breaks]
	# main_title<-"some fish data"
	# standard_breaks <- c(0,quantile(tapply(testData$kg, testData$EURUTA, sum), probs=seq(0,1, by=0.10), na.rm=TRUE))
	# par(mfrow=c(1,2)) 
	# heatmap_ices_rect_one_var(x = testData, Var = "kg", var1 = "EURUTA", F=sum, map_title = main_title, legend_title = "kg", standard_breaks = standard_breaks, nbreaks = 6, xlim=c(10.5,25.5), ylim=c(52.25,65.5), legend_amp=1, map_list = map_list)
	# heatmap_ices_rect_one_var(x = testData[1:2000,], Var = "kg", var1 = "EURUTA", F=sum, map_title = main_title, legend_title = "kg", standard_breaks = standard_breaks, nbreaks = 6, xlim=c(10.5,25.5), ylim=c(52.25,65.5), legend_amp=1, map_list = map_list)


function (height, width = 1, space = NULL, names.arg = NULL, 
    legend.text = NULL, beside = FALSE, horiz = FALSE, density = NULL, 
    angle = 45, col = NULL, border = par("fg"), main = NULL, 
    sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
    xpd = TRUE, log = "", axes = TRUE, axisnames = TRUE, 
    cex.axis = par("cex.axis"), cex.names = par("cex.axis"), 
    inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0, add = FALSE, 
    ann = !add && par("ann"), args.legend = NULL, ...) 
{
    if (!missing(inside)) 
        .NotYetUsed("inside", error = FALSE)
    if (is.null(space)) 
        space <- if (is.matrix(height) && beside) 
            c(0, 1)
        else 0.2
    space <- space * mean(width)
    if (plot && axisnames && is.null(names.arg)) 
        names.arg <- if (is.matrix(height)) 
            colnames(height)
        else names(height)
    if (is.vector(height) || (is.array(height) && (length(dim(height)) == 
        1))) {
        height <- cbind(height)
        beside <- TRUE
        if (is.null(col)) 
            col <- "grey"
    }
    else if (is.matrix(height)) {
        if (is.null(col)) 
            col <- gray.colors(nrow(height))
    }
    else stop("'height' must be a vector or a matrix")
    if (is.logical(legend.text)) 
        legend.text <- if (legend.text && is.matrix(height)) 
            rownames(height)
    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
        logx <- length(grep("x", log)) > 0L
        logy <- length(grep("y", log)) > 0L
    }
    if ((logx || logy) && !is.null(density)) 
        stop("Cannot use shading lines in bars when log scale is used")
    NR <- nrow(height)
    NC <- ncol(height)
    if (beside) {
        if (length(space) == 2) 
            space <- rep.int(c(space[2L], rep.int(space[1L], 
                NR - 1)), NC)
        width <- rep_len(width, NR)
    }
    else {
        width <- rep_len(width, NC)
    }
    offset <- rep_len(as.vector(offset), length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    log.dat <- (logx && horiz) || (logy && !horiz)
    if (log.dat) {
        if (min(height + offset, na.rm = TRUE) <= 0) 
            stop("log scale error: at least one 'height + offset' value <= 0")
        if (logx && !is.null(xlim) && min(xlim) <= 0) 
            stop("log scale error: 'xlim' <= 0")
        if (logy && !is.null(ylim) && min(ylim) <= 0) 
            stop("log scale error: 'ylim' <= 0")
        rectbase <- if (logy && !horiz && !is.null(ylim)) 
            ylim[1L]
        else if (logx && horiz && !is.null(xlim)) 
            xlim[1L]
        else 0.9 * min(height, na.rm = TRUE)
    }
    else rectbase <- 0
    if (!beside) 
        height <- rbind(rectbase, apply(height, 2L, cumsum))
    rAdj <- offset + (if (log.dat) 
        0.9 * height
    else -0.01 * height)
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
        if (is.null(xlim)) 
            xlim <- range(rAdj, height + offset, na.rm = TRUE)
        if (is.null(ylim)) 
            ylim <- c(min(w.l), max(w.r))
    }
    else {
        if (is.null(xlim)) 
            xlim <- c(min(w.l), max(w.r))
        if (is.null(ylim)) 
            ylim <- range(rAdj, height + offset, na.rm = TRUE)
    }
    if (beside) 
        w.m <- matrix(w.m, ncol = NC)
    if (plot) {
        dev.hold()
        opar <- if (horiz) 
            par(xaxs = "i", xpd = xpd)
        else par(yaxs = "i", xpd = xpd)
        on.exit({
            dev.flush()
            par(opar)
        })
        if (!add) {
            plot.new()
            plot.window(xlim, ylim, log = log, ...)
        }
        xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, 
            ...) {
            if (horizontal) 
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
        }
        if (beside) 
            xyrect(rectbase + offset, w.l, c(height) + offset, 
                w.r, horizontal = horiz, angle = angle, density = density, 
                col = col, border = border)
        else {
            for (i in 1L:NC) {
                xyrect(height[1L:NR, i] + offset[i], w.l[i], 
                  height[-1, i] + offset[i], w.r[i], horizontal = horiz, 
                  angle = angle, density = density, col = col, 
                  border = border)
            }
        }
        if (axisnames && !is.null(names.arg)) {
            at.l <- if (length(names.arg) != length(w.m)) {
                if (length(names.arg) == NC) 
                  colMeans(w.m)
                else stop("incorrect number of names")
            }
            else w.m
            axis(if (horiz) 
                2
            else 1, at = at.l, labels = names.arg, lty = axis.lty, 
                cex.axis = cex.names, ...)
        }
        if (!is.null(legend.text)) {
            legend.col <- rep_len(col, length(legend.text))
            if ((horiz & beside) || (!horiz & !beside)) {
                legend.text <- rev(legend.text)
                legend.col <- rev(legend.col)
                density <- rev(density)
                angle <- rev(angle)
            }
            xy <- par("usr")
            if (is.null(args.legend)) {
                legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1), 
                  legend = legend.text, angle = angle, density = density, 
                  fill = legend.col, xjust = 1, yjust = 1)
            }
            else {
                args.legend1 <- list(x = xy[2L] - xinch(0.1), 
                  y = xy[4L] - yinch(0.1), legend = legend.text, 
                  angle = angle, density = density, fill = legend.col, 
                  xjust = 1, yjust = 1)
                args.legend1[names(args.legend)] <- args.legend
                do.call("legend", args.legend1)
            }
        }
        if (ann) 
            title(main = main, sub = sub, xlab = xlab, ylab = ylab, 
                ...)
        if (axes) 
            axis(if (horiz) 
                1
            else 2, cex.axis = cex.axis, ...)
        invisible(w.m)
    }
    else w.m
}
