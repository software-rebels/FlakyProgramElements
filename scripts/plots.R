library("beanplot")
library("ggplot2")
library("treemapify")
library("plyr")

####### Treemap for Argument A1
generate_dispersion_plot <- function(project) {

	module_dispersion_csv <- sprintf("./db/%s/module_dispersion.csv", project)
	module_dispersion_data <- read.csv(module_dispersion_csv,header=TRUE)
	module_dispersion_plot <-  sprintf("./figures/%s_a1.pdf",project)
	pdf(module_dispersion_plot)
		plot_md <- ggplot(module_dispersion_data, aes(area = total, fill=fCover, label=file, subgroup=module)) + 
	geom_treemap() + 
	geom_treemap_subgroup_border(colour = "black",size = 2) + 
	geom_treemap_subgroup_text(place= "topleft", reflow = T, color = "blue", fontface="bold", min.size = 0,size = 16) +
	geom_treemap_text(color="black", place= "bottomright" , reflow = T,size =12) +  
	theme(legend.position = "bottom",legend.title=element_text(size=20),legend.text=element_text(size=10)) + 
	labs(fill="Number of flakily covered statements") + 
	scale_fill_gradient(low="white",high="red")

	print(plot_md)
	dev.off()
}

####### Plot for Argument B.1
generate_stmt_experience_plots <- function(project,add_label=FALSE) {
	statements_csv <- sprintf("./db/%s/statements.csv", project) 
	data <- read.csv(statements_csv,header=TRUE)
	flaky_stmts <- subset(data, coverage == "F")
	nflaky_stmts <- subset(data, coverage == "NF")	
	plot <- sprintf("./figures/%s_b1.pdf",project)
	pdf(plot)
	par(oma = c(1, 1, 1, 1))
	beanplot(
		nflaky_stmts$experience+1,
		flaky_stmts$experience+1,
		xaxt="n",
		side="both",
		log="y",
		col=list('gray',c('black','white')),
		what=c(0,1,1,0),beanlines = "median",
		cex.axis = 1.6
	)
	title(ylab="Author Experience",cex.lab = 1.6)
	if(add_label) {
	par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
	legend("bottom",fill=c('gray','black'), legend= c('Robustly Covered statements ', 'Flakily covered statements'), xpd = TRUE, bty = "n", cex = 1.6)
	}
	dev.off()
}

####### Plot for Argument B.2
generate_test_experience_plots <- function(project,add_label=FALSE) {
	statements_csv <- sprintf("./db/%s/test_exp.csv",project) 
	data <- read.csv(statements_csv,header=TRUE)
	flaky_tests <- subset(data, is_flaky == "1")
	nflaky_tests <- subset(data, is_flaky == "0")
	plot_1 <- sprintf("./figures/%s_b2.pdf",project)
	pdf(plot_1)
	beanplot(
		nflaky_tests$experience+1,
		flaky_tests$experience+1,
		xaxt="n",
		side="both",
		log="y",
		col=list('gray',c('black','white')),
		what=c(0,1,1,0),beanlines = "median",
		cex.axis = 1.6
	)
	title(ylab="Author Experience",cex.lab = 1.6)
	if(add_label) {
	par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
	legend("bottom",fill=c('gray','black'), legend= c('Robust tests', 'Flaky tests'), xpd = TRUE, bty = "n", cex = 1.6)
	}

	dev.off()
}
####### Plot for Argument C.1
generate_age_plots <- function(project,add_label=FALSE) {
	statements_csv <- sprintf("./db/%s/statements.csv", project) 
	data <- read.csv(statements_csv,header=TRUE)
	flaky_stmts <- subset(data, coverage == "F")
	nflaky_stmts <- subset(data, coverage == "NF")	
	plot <- sprintf("./figures/%s_c1.pdf",project)
	pdf(plot)
	#make margins wider
	par(oma = c(1, 1, 1, 1))
	beanplot(
		nflaky_stmts$age+1,
		flaky_stmts$age+1,
		xaxt="n",
		side="both",
		log="y",
		col=list('gray',c('black','white')),
		what=c(0,1,1,0),beanlines = "median",
		cex.axis = 1.6
	)
	title(ylab="Statement Age",cex.lab = 1.6)
	
	#overlay another empty plot that contains only the legends
		if(add_label) {
	par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
	legend("bottom",fill=c('gray','black'), legend= c('Robustly Covered statements ', 'Flakily covered statements'), xpd = TRUE, bty = "n", cex = 1.6)
	}
	dev.off()
}

####### Plot for Argument C.2
generate_churn_plots <- function(project,add_label=FALSE) {
	statements_csv <- sprintf("./db/%s/statements.csv", project) 
	data <- read.csv(statements_csv,header=TRUE)
	flaky_stmts <- subset(data, coverage == "F")
	nflaky_stmts <- subset(data, coverage == "NF")	
	plot <- sprintf("./figures/%s_c2.pdf",project)
	pdf(plot)
	par(oma = c(1, 1, 1, 1))
	beanplot(
		nflaky_stmts$churn+1,
		flaky_stmts$churn+1,
		xaxt="n",
		side="both",
		log="y",
		col=list('gray',c('black','white')),
		what=c(0,1,1,0),beanlines = "median",
		cex.axis = 1.6
	)
	title(ylab="Statement Churn",cex.lab = 1.6)
	if(add_label) {
	par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
	legend("bottom",fill=c('gray','black'), legend= c('Robustly Covered statements ', 'Flakily covered statements'), xpd = TRUE, bty = "n", cex = 1.6)
	}
	dev.off()
}

