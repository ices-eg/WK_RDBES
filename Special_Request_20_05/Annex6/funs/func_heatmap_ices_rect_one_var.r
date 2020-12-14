heatmap_ices_rect_one_var <- function(x, Var, var1, F, map_title, legend_title, legend_amp, standard_breaks=NULL, nbreaks, quantile1=0.975, xlim = c(-42,+5), ylim = c(32.75,63.25), legend_Var_scale=1, map_list = "", suffix ="", tidy=FALSE){	
	# Nuno Prista, SLU, Sweden
	# Developed @ RCM NS&EA 2017-18, RCG subgroup work 2019
	
	# draws grid map with heat-mapped ices rectangles
	# x is a data.frame where Var is integer/numeric and var1 are ices rectangles). 
	# F is the function that is applied to on each ices rectangle (e.g., sum, mean)
	# xlim and ylim help define map range
	# legend_title, legend_Var_scale and suffix are for legend only. Note: use legend.amp = 100 if Var data is percentage

	#2017/09/11-15: first version develop during RCG NSEA 2017
	#2018/09/03-10: improvement during RCG NSEA 2018
	#2019/04/02-06: significant upgrade of RCG NSEA 2018 function:
					# bugs corrected
					# improved display of ices rectangles (shapefile avoids display outside target area)
					# inclusion of percent coverages and map subtitle
					# added argument "quantile1"
					# added argument "map_list"
					# improved annotation
	# 2020-06-08: included legend_grid2 function
	# 2020-06-08: added argument standard_breaks [when specified, allows input of externqal grid, see example]
	# 2020-06-08: added example standard_breaks 
	# 2020-06-09: added argument tidy (cleans subtitle)
	
	
	#browser()
	legend_grid2 <- function (x, y = NULL, breaks, col, digits = 2, suffix = "", 
    type = 1, pch = 15, pt.cex = 2.5, bg = "lightblue", ...) 
{
	
	# Original code: Hans Gerritsen @ mapplots:legend.grid
	# Nuno Prista: modification of original code to allow specification of pt.bg when pch %in% c(21:25)

    ncol <- length(breaks) - 1
    if (missing(col)) 
        col <- colorRampPalette(c("lightyellow", "yellow", "orange", 
            "red", "brown4"))(ncol)
    tempfun <- function(x) format(x, digits = digits)
    min <- sapply(breaks[(ncol):1], tempfun)
    mid <- sapply((breaks[(ncol):1] + breaks[(ncol + 1):2])/2, 
        tempfun)
    max <- sapply(breaks[(ncol + 1):2], tempfun)
    if (type == 1) 
        legend <- paste(mid, suffix, sep = "")
    if (type == 2) 
        legend <- paste(min, " - ", max, suffix, sep = "")
    if(pch %in% 1:15)
		{legend(x, y, legend = legend, col = col[ncol:1], pch = pch, 
			pt.cex = pt.cex, bg = bg, ...)
		} else {
			legend(x, y, legend = legend, pt.bg = col[ncol:1], col=1, pch = pch, 
				pt.cex = pt.cex, bg = bg, ...)
				}	
}
	
	
	require(mapplots)

	#browser()
	percent_Var <- round(sum(!is.na(x[,Var]))/dim(x)[1]*100,2)
	percent_var1 <- round(sum(!is.na(x[,var1]) & !x[,var1]=="99u9")/dim(x)[1]*100,2)
	
	x<-x[!is.na(x[,var1]) & !x[,var1]=="99u9",]
	ls1<-split(x, factor(x[,var1]))
	ls2<-lapply(ls1, function(x){data.frame(StatisticalRectangle = x[,var1][1], value = F(x[[Var]], na.rm=T))})
	df1<-do.call("rbind", ls2)
	df1<-data.frame(df1, ices.rect(as.character(df1$StatisticalRectangle)), row.names=NULL)
	
	Var_in_grid <- sum(df1$value[df1$StatisticalRectangle %in% map_ices_rect$dbf$dbf$ICESNAME])/sum(df1$value)
	percent_Var_in_grid <- round(sum(df1$value[df1$StatisticalRectangle %in% map_ices_rect$dbf$dbf$ICESNAME])/sum(df1$value)*100,2)
	percent_Var_outside_grid <- 100-percent_Var_in_grid
	
	# restrict values to grid
	df1<-droplevels(df1[df1$StatisticalRectangle %in% as.character(map_ices_rect$dbf$dbf$ICESNAME),])
	
	# determines grid and breaks (if standard_breaks==NULL) 
	byy = 0.5; byx = 1;
	grd<-make.grid(x = df1$lon, y = df1$lat, z = df1$value, byx = byx, byy = byy, xlim = xlim, ylim = ylim)
	if(is.null(standard_breaks)){breaks <- breaks.grid(grd, quantile1, ncol=nbreaks, zero=TRUE)} else {breaks <- standard_breaks}
	print(quantile(grd, probs=c(0,0.01, 0.025, 0.975, 0.99, 1), na.rm=TRUE))
	print(breaks)
	
	# plots maps
	basemap(xlim=xlim, ylim=ylim, main = "", bg="white", xaxs="i", yaxs="i")
	draw.grid(grd,breaks)
	if("map_ices_rect" %in% names(map_list)) draw.shape(map_ices_rect, border="gray", col="transparent")
	if("map_fao_areas" %in% names(map_list)) draw.shape(map_fao_areas, border="black", col="transparent")
	if("map_coast" %in% names(map_list)) draw.shape(map_coast, col="beige")
	if("map_borders" %in% names(map_list)) draw.shape(map_borders, type="l", col="black")	
	# adds titles
	title(map_title, line=2)
	if(!tidy) title(main=paste("with main_var: ", percent_Var,"%; with rect: ",percent_var1,"%; total main_var_in_grid: ", percent_Var_in_grid, sep=""), cex.main=0.8, line = 1)
	# adds legend
	legend_grid2(x="topleft", breaks=breaks*legend_Var_scale, pch= 22, pt.cex=1.7, cex=0.7, type=2, inset=0.02, title=legend_title, bg="white", suffix = suffix)
	

	}
	
## example
## read data
	# testData<-read.table("ExampleData/map_fish_data.txt", sep=" ", dec=".", header=TRUE)
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

	