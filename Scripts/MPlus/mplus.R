# R scripts for extracting and plotting data stored in Mplus graphic
# information in GH5 files.  Uses the rhdf5 package for loading the
# the GH5 file.
#
# Version history:
# 2013-09-13 File Version 3 for Mplus Version 7.3
# 2014-04-30 Fix for sample and estimated means.
# 2014-10-07 Fix IRT ICC and IIC functions, turning properties into integers
# 2014-10-08 Add functions for Discrete survival curves
# 2014-11-20 Fix estimated probabilities function, turning categories into integers.
# 2014-11-21 Add legend to plot of estimated probabilities.
#
#
# Written by: Thuy Nguyen
#             Muthen & Muthen
#
# Reference:
#
# Bernd Fischer and Gregoire Pau (). rhdf5: HDF5 interface to R. R
# package version 2.4.0.
#

if (require(rhdf5,quietly=TRUE)) {
	print("Loaded rhdf5 package")
} else {
	print("trying to install rhdf5 from bioconductor.org")
	source("http://bioconductor.org/biocLite.R")
	biocLite("rhdf5")
	if (require(rhdf5)) {
		print("Loaded missing rhdf5 package ")
	} else {
		stop("could not install rhdf5")
	}
}

##########################################################################
#
# mplus.view.plots - loads the file and lists all available plots
#
# arguments:
#    file - the quoted name of an existing GH5 file
#
# eg. mplus.view.plots('ex.gh5')
#
mplus.view.plots <- function(file) {
	mplus.load(file)
}


##########################################################################
#
# mplus.load - loads the file and lists all available plots
#
# arguments:
#    file - the quoted name of an existing GH5 file
#
# eg. mplus.load('ex.gh5')
#
mplus.load <- function(file) {

	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		cat(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	cat(c("\nPlot functions:\n"))

	if ("individual_data" %in% names(gh5)) {
		if (exists("mplus.plot.histogram",mode="function")) {
			cat(c(" - mplus.plot.histogram('"),file,"',variable,bins)\n",sep="")
		}
		if (exists("mplus.plot.scatterplot",mode="function")) {
			cat(c(" - mplus.plot.scatterplot('"),file,"',xvar,yvar)\n",sep="")
		}
	}

	if ("process_data" %in% names(gh5) && "means_and_variances_data" %in% names(gh5)) {
		np <- length(attr(gh5$process_data,"names"))
		for (i in c(1:np)) {
			cstr <- paste(c("process"), as.character(i), sep="")
			proc <- gh5$process_data[[cstr]]

			# Replace the line below with series of low-level function calls
			cstr2 <- paste(c("process_data"),"/",cstr,"", sep="")
			prop <- mplus.get.group.attribute(file, cstr2, 'properties')

			values <- attr(gh5$means_and_variances_data,"names")

			if (prop[1] == 1) {
				sm_ind <- pmatch("y_observed_means",values,nomatch=0)
				if (sm_ind > 0 && exists("mplus.plot.sample_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.sample_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("y_estimated_means",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				if (sm_ind>0 && em_ind>0 && exists("mplus.plot.sample_and_estimated_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.sample_and_estimated_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("y_estimated_modes",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_modes",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_modes('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("y_estimated_medians",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_medians",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_medians('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}
			} else if (prop[1] == 2) {
				em_ind <- pmatch("e_estimated_means",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("e_estimated_modes",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_modes",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_modes('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("e_estimated_medians",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_medians",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_medians('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}
			} else if (prop[1] == 3) {
				em_ind <- pmatch("observed_probs",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.sample_proportions",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.sample_proportions('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("estimated_probs",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.plot.estimated_probabilities",mode="function")) {
					cstr2 <- paste(c(" - mplus.plot.estimated_probabilities('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
					cat(cstr2)
				}
			} else {
				cstr2 <- paste(c("'"),cstr,"' has unknown series type.\n")
				cat(cstr2)
			}
		}
	}

	if ("loop_data" %in% names(gh5)) {
		if (exists("mplus.list.loop.labels",mode="function")) {
			cat(c(" - mplus.list.loop.labels('"),file,"')\n",sep="")
		}
		if (exists("mplus.plot.loop",mode="function")) {
			cat(c(" - mplus.plot.loop('"),file,"')\n",sep="")
		}
	}

	if ("irt_data" %in% names(gh5)) {
		if (exists("mplus.list.irt.variables",mode="function")) {
			cat(c(" - mplus.list.irt.variables('"),file,"')\n",sep="")
		}
		if (exists("mplus.list.irt.xvariables",mode="function")) {
			cat(c(" - mplus.list.irt.xvariables('"),file,"')\n",sep="")
		}
		if (exists("mplus.plot.irt.icc",mode="function")) {
			cat(c(" - mplus.plot.irt.icc('"),file,"',group,xvar,uvar,cat,cat2,covariates,xrange,xstep,lloc)\n",sep="")
		}
		if (exists("mplus.plot.irt.iic",mode="function")) {
			cat(c(" - mplus.plot.irt.iic('"),file,"',group,xvar,uvar,covariates,xrange,xstep,lloc)\n",sep="")
		}
		if (exists("mplus.plot.irt.tic",mode="function")) {
			cat(c(" - mplus.plot.irt.tic('"),file,"',group,xvar,uvar,covariates,xrange,xstep)\n",sep="")
		}
	}

	if ("survival_data" %in% names(gh5)) {
		if (exists("mplus.plot.survival.kaplanmeier",mode="function")) {
			cat(c(" - mplus.plot.survival.kaplanmeier('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.survival.baseline",mode="function")) {
			cat(c(" - mplus.plot.survival.baseline('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.survival.basehazard",mode="function")) {
			cat(c(" - mplus.plot.survival.basehazard('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.survival.sample.logcumulative",mode="function")) {
			cat(c(" - mplus.plot.survival.sample.logcumulative('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.survival.estimated.logcumulative",mode="function")) {
			cat(c(" - mplus.plot.survival.estimated.logcumulative('"),file,"',survar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.survival.kaplanmeier.vs.baseline",mode="function")) {
			cat(c(" - mplus.plot.survival.kaplanmeier.vs.baseline('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.survival.sample.vs.estimated.logcumulative",mode="function")) {
			cat(c(" - mplus.plot.survival.sample.vs.estimated.logcumulative('"),file,"',survvar,classnum)\n",sep="")
		}
	}

	if ("discrete_survival_data" %in% names(gh5)) {
		if (exists("mplus.plot.discrete.survival.kaplanmeier",mode="function")) {
			cat(c(" - mplus.plot.discrete.survival.kaplanmeier('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.discrete.survival.baseline",mode="function")) {
			cat(c(" - mplus.plot.discrete.survival.baseline('"),file,"',survvar,classnum)\n",sep="")
		}
		if (exists("mplus.plot.discrete.survival.kaplanmeier.vs.baseline",mode="function")) {
			cat(c(" - mplus.plot.discrete.survival.kaplanmeier.vs.baseline('"),file,"',survvar,classnum)\n",sep="")
		}
	}

	if ("bayesian_data" %in% names(gh5)) {
		if ("parameters_autocorr" %in% names(gh5$bayesian_data)) {
			if ("parameters" %in% names(gh5$bayesian_data$parameters_autocorr)) {
				if (exists("mplus.list.bayesian.parameters",mode="function")) {
					cat(c(" - mplus.list.bayesian.parameters('"),file,"',parameter)\n",sep="")
				}
				if (exists("mplus.plot.bayesian.traceplot",mode="function")) {
					cat(c(" - mplus.plot.bayesian.traceplot('"),file,"',parameter)\n",sep="")
				}
				if (exists("mplus.plot.bayesian.distribution",mode="function")) {
					cat(c(" - mplus.plot.bayesian.distribution('"),file,"',parameter,bins)\n",sep="")
				}
			}
			if ("priors" %in% names(gh5$bayesian_data$parameters_autocorr)) {
				if (exists("mplus.plot.bayesian.prior.distribution",mode="function")) {
					cat(c(" - mplus.plot.bayesian.prior.distribution('"),file,"',parameter,bins)\n",sep="")
				}
			}
			if ("autocorrelation" %in% names(gh5$bayesian_data$parameters_autocorr)) {
				if (exists("mplus.plot.bayesian.autocorrelation",mode="function")) {
					cat(c(" - mplus.plot.bayesian.autocorrelation('"),file,"',parameter,chain)\n",sep="")
				}
			}
		}
		if ("predictive" %in% names(gh5$bayesian_data)) {
			if (exists("mplus.list.bayesian.predictive.labels",mode="function")) {
				cat(c(" - mplus.list.bayesian.predictive.labels('"),file,"')\n",sep="")
			}
			if ("observed" %in% names(gh5$bayesian_data$predictive) && "replicated" %in% names(gh5$bayesian_data$predictive)) {
				if (exists("mplus.plot.bayesian.predictive.scatterplot",mode="function")) {
					cat(c(" - mplus.plot.bayesian.predictive.scatterplot('"),file,"',plabel)\n",sep="")
				}
				if (exists("mplus.plot.bayesian.predictive.distribution",mode="function")) {
					cat(c(" - mplus.plot.bayesian.predictive.distribution('"),file,"',plabel,bins)\n",sep="")
				}
			}
		}
		if ("plausible" %in% names(gh5$bayesian_data)) {
			if (exists("mplus.list.bayesian.plausible.labels",mode="function")) {
				cat(c(" - mplus.list.bayesian.plausible.labels('"),file,"')\n",sep="")
			}
			if (exists("mplus.plot.bayesian.plausible.distribution",mode="function")) {
				cat(c(" - mplus.plot.bayesian.plausible.distribution('"),file,"',plauslabel,obs,bins)\n",sep="")
			}
		}
	}

	cat(c("\nPlot data extraction functions:\n"))

	if ("individual_data" %in% names(gh5)) {
		if (exists("mplus.list.variables",mode="function")) {
			cat(c(" - mplus.list.variables('"),file,"')\n",sep="")
		}
		if (exists("mplus.get.data",mode="function")) {
			cat(c(" - mplus.get.data('"),file,"',variable)\n",sep="")
		}
	}

	if ("process_data" %in% names(gh5)) {
		if (exists("mplus.list.processes",mode="function")) {
			cat(c(" - mplus.list.processes('"),file,"')\n",sep="")
		}
	}

	if ("loop_data" %in% names(gh5)) {
		if (exists("mplus.get.loop.estimates",mode="function")) {
			cat(c(" - mplus.get.loop.estimates('"),file,"',looplabel)\n",sep="")
		}
		if (exists("mplus.get.loop.lowerci",mode="function")) {
			cat(c(" - mplus.get.loop.lowerci('"),file,"',looplabel)\n",sep="")
		}
		if (exists("mplus.get.loop.upperci",mode="function")) {
			cat(c(" - mplus.get.loop.upperci('"),file,"',looplabel)\n",sep="")
		}
		if (exists("mplus.get.loop.xvalues",mode="function")) {
			cat(c(" - mplus.get.loop.xvalues('"),file,"')\n",sep="")
		}
	}

	if ("irt_data" %in% names(gh5)) {
		if (exists("mplus.compute.irt.icc",mode="function")) {
			cat(c(" - mplus.compute.irt.icc('"),file,"',group,xvar,uvar,cat,xvector,covariates)\n",sep="")
		}
		if (exists("mplus.compute.irt.iic",mode="function")) {
			cat(c(" - mplus.compute.irt.iic('"),file,"',group,xvar,uvar,xvector,covariates)\n",sep="")
		}
	}

	if ("process_data" %in% names(gh5) && "means_and_variances_data" %in% names(gh5)) {
		np <- length(attr(gh5$process_data,"names"))
		for (i in c(1:np)) {
			cstr <- paste(c("process"), as.character(i), sep="")
			proc <- gh5$process_data[[cstr]]

			# Replace the line below with series of low-level function calls
			cstr2 <- paste(c("process_data"),"/",cstr,"", sep="")
			prop <- mplus.get.group.attribute(file, cstr2, 'properties')

			values <- attr(gh5$means_and_variances_data,"names")

			if (prop[1] == 1) {
				sm_ind <- pmatch("y_observed_means",values,nomatch=0)
				if (sm_ind > 0 && exists("mplus.get.sample_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.sample_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("y_estimated_means",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("y_estimated_modes",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_modes",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_modes('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("y_estimated_medians",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_medians",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_medians('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}
			} else if (prop[1] == 2) {
				em_ind <- pmatch("e_estimated_means",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_means",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_means('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("e_estimated_modes",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_modes",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_modes('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("e_estimated_medians",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_medians",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_medians('"),file,"','",cstr,"')\n",sep="")
					cat(cstr2)
				}
			} else if (prop[1] == 3) {
				em_ind <- pmatch("observed_probs",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.sample_proportions",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.sample_proportions('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
					cat(cstr2)
				}

				em_ind <- pmatch("estimated_probs",values,nomatch=0)
				if (em_ind > 0 && exists("mplus.get.estimated_probabilities",mode="function")) {
					cstr2 <- paste(c(" - mplus.get.estimated_probabilities('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
					cat(cstr2)
				}
			} else {
				cstr2 <- paste(c("'"),cstr,"' has unknown series type.\n")
				cat(cstr2)
			}
		}
	}

	if ("survival_data" %in% names(gh5)) {
		if (exists("mplus.list.survival.variables",mode="function")) {
			cat(c(" - mplus.list.survival.variables('"),file,"')\n",sep="")
		}
		if (exists("mplus.get.survival.kaplanmeier.values",mode="function")) {
			cat(c(" - mplus.get.survival.kaplanmeier.values('"),file,"',survvar,classnum,time)\n",sep="")
		}
		if (exists("mplus.compute.survival.sample.logcumulative.values",mode="function")) {
			cat(c(" - mplus.compute.survival.sample.logcumulative.values('"),file,"',survvar,classnum,time)\n",sep="")
		}
		if (exists("mplus.get.survival.baseline.values",mode="function")) {
			cat(c(" - mplus.get.survival.baseline.values('"),file,"',survvar,survvar2,clasnum,time)\n",sep="")
		}
		if (exists("mplus.compute.survival.estimated.logcumulative.values",mode="function")) {
			cat(c(" - mplus.compute.survival.estimated.logcumulative.values('"),file,"',survvar,classnum,time)\n",sep="")
		}
		if (exists("mplus.get.survival.basehazard.values",mode="function")) {
			cat(c(" - mplus.get.survival.basehazard.values('"),file,"',file,survvar,classnum,time)\n",sep="")
		}
	}

	if ("discrete_survival_data" %in% names(gh5)) {
		if (exists("mplus.list.discrete.survival.variables",mode="function")) {
			cat(c(" - mplus.list.discrete.survival.variables('"),file,"')\n",sep="")
		}
		if (exists("mplus.get.discrete.survival.kaplanmeier.values",mode="function")) {
			cat(c(" - mplus.get.discrete.survival.kaplanmeier.values('"),file,"',survvar,classnum,time)\n",sep="")
		}
		if (exists("mplus.get.discrete.survival.baseline.values",mode="function")) {
			cat(c(" - mplus.get.discrete.survival.baseline.values('"),file,"',survvar,survvar2,clasnum,time)\n",sep="")
		}
	}

	if ("bayesian_data" %in% names(gh5)) {
		if ("parameters_autocorr" %in% names(gh5$bayesian_data)) {
			if ("parameters" %in% names(gh5$bayesian_data$parameters_autocorr)) {
				if (exists("mplus.get.bayesian.parameter.data",mode="function")) {
					cat(c(" - mplus.get.bayesian.parameter.data('"),file,"',parameter,chain)\n",sep="")
				}
			}
			if ("priors" %in% names(gh5$bayesian_data$parameters_autocorr)) {
				if (exists("mplus.get.bayesian.prior.parameter.data",mode="function")) {
					cat(c(" - mplus.get.bayesian.prior.parameter.data('"),file,"',parameter)\n",sep="")
				}
			}
			if ("autocorrelation" %in% names(gh5$bayesian_data$parameters_autocorr)) {
				if (exists("mplus.get.bayesian.autocorrelation",mode="function")) {
					cat(c(" - mplus.get.bayesian.autocorrelation('"),file,"',parameter,chain)\n",sep="")
				}
			}
		}
		if ("predictive" %in% names(gh5$bayesian_data)) {
			if ("observed" %in% names(gh5$bayesian_data$predictive)) {
				if (exists("mplus.get.bayesian.predictive.observed",mode="function")) {
					cat(c(" - mplus.get.bayesian.predictive.observed('"),file,"',plabel)\n",sep="")
				}
			}
			if ("replicated" %in% names(gh5$bayesian_data$predictive)) {
				if (exists("mplus.get.bayesian.predictive.replicated",mode="function")) {
					cat(c(" - mplus.get.bayesian.predictive.replicated('"),file,"',plabel)\n",sep="")
				}
			}
			if ("pvalues" %in% names(gh5$bayesian_data$predictive)) {
				if (exists("mplus.get.bayesian.predictive.lowerci",mode="function")) {
					cat(c(" - mplus.get.bayesian.predictive.lowerci('"),file,"',plabel)\n",sep="")
				}
				if (exists("mplus.get.bayesian.predictive.upperci",mode="function")) {
					cat(c(" - mplus.get.bayesian.predictive.upperci('"),file,"',plabel)\n",sep="")
				}
				if (exists("mplus.get.bayesian.predictive.pvalue",mode="function")) {
					cat(c(" - mplus.get.bayesian.predictive.pvalue('"),file,"',plabel)\n",sep="")
				}
				if (exists("mplus.get.bayesian.predictive.pvalue_type",mode="function")) {
					cat(c(" - mplus.get.bayesian.predictive.pvalue('"),file,"',plabel)\n",sep="")
				}
			}
		}
		if ("plausible" %in% names(gh5$bayesian_data)) {
			if (exists("mplus.get.bayesian.plausible.data",mode="function")) {
				cat(c(" - mplus.get.bayesian.plausible.data('"),file,"',plauslabel,obs)\n",sep="")
			}
		}
	}

	invisible(file)
}


##########################################################################
#
# mplus.clear - clears all mplus-related data from a previous mplus_load
#
# arguments: none
#
# eg. mplus.clear()
#
#mplus.clear <- function() {
#	cat(c("\nRemoved the following:\n"))
#
#	if (exists("matrix_data",)) {
#	    rm(matrix_data, inherits=TRUE)
#		cat(c(" - matrix_data\n"))
#	}
#	if (exists("process_data",)) {
#	    rm(process_data, inherits=TRUE)
#		cat(c(" - process_data\n"))
#	}
#	if (exists("class_data")) {
#	    rm(class_data, inherits=TRUE)
#		cat(c(" - class_data\n"))
#	}
#	if (exists("categorical_data")) {
#	    rm(categorical_data, inherits=TRUE)
#		cat(c(" - categorical_data\n"))
#	}
#	if (exists("individual_data")) {
#	    rm(individual_data, inherits=TRUE)
#		cat(c(" - individual_data\n"))
#	}
#	if (exists("means_and_variances_data")) {
#	    rm(means_and_variances_data, inherits=TRUE)
#		cat(c(" - means_and_variances_data\n"))
#	}
#}


##########################################################################
#
# mplus.list.processes - list all available processes
#
# arguments:
#    file - the quoted name of an existing GH5 file
#
# eg. mplus.list.processes('ex8.1.gh5')
#
mplus.list.processes <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	if (!("process_data" %in% names(gh5))) {
		stop("mplus.list.proceses requires series information.\n\nUse the SERIES option in Mplus to specify series information.\n")
	}

	cat(c("\nList of process names to use in the following functions:\n"))
	cat(c(" - mplus.plot.sample_means\n"))	
	cat(c(" - mplus.plot.estimated_means\n"))	
	cat(c(" - mplus.plot.sample_and_estimated_means\n"))	
	cat(c(" - mplus.plot.sample_proportions\n"))	
	cat(c(" - mplus.plot.estimated_probabilities\n"))	

	cat(c(" - mplus.get.sample_means\n"))	
	cat(c(" - mplus.get.estimated_means\n"))	
	cat(c(" - mplus.get.sample_proportions\n"))	
	cat(c(" - mplus.get.estimated_probabilities\n"))	

	cat(c("\nProcesses:\n"))

	allpnames <- attr(gh5$process_data,"names")
	allpnames
}


##########################################################################
#
# mplus.plot.estimated_means - plot estimated means for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#
# eg. mplus.plot.estimated_means('ex8.1.gh5','process1')
#
mplus.plot.estimated_means <-function(file,procstr='process1',ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated means.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file, cstr2, 'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1 || series_type == 2) ) {
		cstr <- paste("- process does not have estimated means:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the estimated means
	dims <- attr(proc$time_scores,"dim")
	yy <- mplus.get.estimated_means(file,procstr)

	# plot the means
	cstr <- paste("Estimated means for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n')
	symb <- array(c(21,22,23,24,25),c(dims[1]))
	colors <- rainbow(dims[2])
	for (i in c(1:dims[2])) {
		lines(xx[,i],yy[,i],type=ptype,pch=symb[i],col=colors[i])
	}

	ldesc <- array(0,c(dims[2]))
	lty <- array(0,c(dims[2]))
	lwd <- array(0,c(dims[2]))
	lcol <- array(0,c(dims[2]))
	for (i in c(1:dims[2])) {
		ldesc[i] <- sprintf("Class %d", i)
		lty[i] = 1
		lwd[i] = 2.5
		lcol[i] <- colors[i]
	}
	legend('bottomright',col=lcol,pch=symb,ldesc,lty=lty,lwd=lwd)
}


##########################################################################
#
# mplus.plot.estimated_modes - plot estimated modes for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#
# eg. mplus.plot.estimated_modes('ex8.1.gh5','process1')
#
mplus.plot.estimated_modes <-function(file,procstr='process1',ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated modes.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file, cstr2, 'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1 || series_type == 2) ) {
		cstr <- paste("- process does not have estimated modes:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the estimated means
	dims <- attr(proc$time_scores,"dim")
	yy <- mplus.get.estimated_modes(file,procstr)

	# plot the means
	cstr <- paste("Estimated modes for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n')
	symb <- array(c(21,22,23,24,25),c(dims[1]))
	colors <- rainbow(dims[2])
	for (i in c(1:dims[2])) {
		lines(xx[,i],yy[,i],type=ptype,pch=symb[i],col=colors[i])
	}

	ldesc <- array(0,c(dims[2]))
	lty <- array(0,c(dims[2]))
	lwd <- array(0,c(dims[2]))
	lcol <- array(0,c(dims[2]))
	for (i in c(1:dims[2])) {
		ldesc[i] <- sprintf("Class %d", i)
		lty[i] = 1
		lwd[i] = 2.5
		lcol[i] <- colors[i]
	}
	legend('bottomright',col=lcol,pch=symb,ldesc,lty=lty,lwd=lwd)
}


##########################################################################
#
# mplus.plot.estimated_medians - plot estimated medians for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#
# eg. mplus.plot.estimated_medians('ex8.1.gh5','process1')
#
mplus.plot.estimated_medians <-function(file,procstr='process1',ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated medians.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file, cstr2, 'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1 || series_type == 2) ) {
		cstr <- paste("- process does not have estimated medians:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the estimated means
	dims <- attr(proc$time_scores,"dim")
	yy <- mplus.get.estimated_medians(file,procstr)

	# plot the means
	cstr <- paste("Estimated medians for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n')
	symb <- array(c(21,22,23,24,25),c(dims[1]))
	colors <- rainbow(dims[2])
	for (i in c(1:dims[2])) {
		lines(xx[,i],yy[,i],type=ptype,pch=symb[i],col=colors[i])
	}

	ldesc <- array(0,c(dims[2]))
	lty <- array(0,c(dims[2]))
	lwd <- array(0,c(dims[2]))
	lcol <- array(0,c(dims[2]))
	for (i in c(1:dims[2])) {
		ldesc[i] <- sprintf("Class %d", i)
		lty[i] = 1
		lwd[i] = 2.5
		lcol[i] <- colors[i]
	}
	legend('bottomright',col=lcol,pch=symb,ldesc,lty=lty,lwd=lwd)
}


##########################################################################
#
# mplus.plot.sample_means - plot sample means for the quoted process
#
# arguments:
#	procstr - the quoted name of a series
#
# eg. mplus.plot.sample_means('ex6.1.gh5','process1')
#
mplus.plot.sample_means <-function(file,procstr='process1',ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith sample means.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file, cstr2, 'properties')

	series_type <- prop[1]

	if ( ! (series_type == 1) ) {
		cstr <- paste("- process does not have sample means:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the estimated means
	dims <- attr(proc$time_scores,"dim")
	yy <- mplus.get.sample_means(file,procstr)

	# plot the means
	cstr <- paste("Sample means for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n')
	symb <- array(c(21,22,23,24,25),c(dims[1]))
	colors <- rainbow(dims[2])
	for (i in c(1:dims[2])) {
		lines(xx[,i],yy[,i],type=ptype,pch=symb[i],col=colors[i])
	}

	ldesc <- array(0,c(dims[2]))
	lty <- array(0,c(dims[2]))
	lwd <- array(0,c(dims[2]))
	lcol <- array(0,c(dims[2]))
	for (i in c(1:dims[2])) {
		ldesc[i] <- sprintf("Class %d", i)
		lty[i] = 1
		lwd[i] = 2.5
		lcol[i] <- colors[i]
	}
	legend('bottomright',col=lcol,pch=symb,ldesc,lty=lty,lwd=lwd)
}


##########################################################################
#
# mplus.plot.sample_and_estimated_means - plot sample and estimated means for the
# quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#
# eg. mplus.plot.sample_and_estimated_means('process1')
#
mplus.plot.sample_and_estimated_means <-function(file,procstr='process1',ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith sample and estimated means.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1) ) {
		cstr <- paste("- process does not have sample means:",procstr,"\n\n")
		stop(cstr)
	}

	# get the dimensions of the time_scores array and create an array with twice the size of the
	# first dimension
	dims <- attr(proc$time_scores,"dim")
	xx <- array(0, c(dims[1],2*dims[2]))
	yy <- array(0, c(dims[1],2*dims[2]))

	samp <- mplus.get.sample_means(file,procstr)
	emean <- mplus.get.estimated_means(file,procstr)

	for (i in c(1:dims[1])) {
		for (j in c(1:dims[2])) {
			# set the time scores and pick up sample means
			xx[i,2*j-1] <- proc$time_scores[i,j]
			yy[i,2*j-1] <- samp[i,j]

			# set the time scores and pick up estimated means
			xx[i,2*j] <- proc$time_scores[i,j]
			yy[i,2*j] <- emean[i,j]
		}
	}

	# plot the means
	cstr <- paste("Sample and estimated means for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n')
	symb <- array(c(21,22,23,24,25),c(dims[2]))
	colors <- rainbow(dims[2])
	for (i in c(1:dims[2])) {
		lines(xx[,2*i-1],yy[,2*i-1],type=ptype,pch=symb[i],col=colors[i])
		lines(xx[,2*i],yy[,2*i],type=ptype,pch=symb[i],col=colors[i])
	}

	ldesc <- array(0,c(2*dims[2]))
	lty <- array(0,c(2*dims[2]))
	lwd <- array(0,c(2*dims[2]))
	lcol <- array(0,c(2*dims[2]))
	lsymb <- array(0,c(2*dims[2]))
	for (i in c(1:dims[2])) {
		ldesc[2*i-1] <- sprintf("Sample means, Class %d", i)
		lty[2*i-1] = 1
		lwd[2*i-1] = 2.5
		lsymb[2*i-1] <- symb[i]

		lcol[2*i] <- colors[i]
		ldesc[2*i] <- sprintf("Estimated means, Class %d", i)
		lty[2*i] = 1
		lwd[2*i] = 2.5
		lcol[2*i] <- colors[i]
		lsymb[2*i] <- symb[i]
	}
	legend('bottomright',col=lcol,pch=lsymb,ldesc,lty=lty,lwd=lwd)
}


##########################################################################
#
# mplus.plot.estimated_probabilities - plot estimated probabilities for the
# quoted process, summing up probabilities of the first to the last category
# chosen
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#	cat1 - the first category to include
#	cat2 - the last category to include
#
# eg. mplus.plot.estimated_probabilities('ex8.4.gh5','process1',1,1)
#
mplus.plot.estimated_probabilities <- function(file,procstr='process1',cat1=1,cat2=1,ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated probabilities.\n")
	}

	# if cat2 is missing and cat1 is given, then we should assign cat2 to cat1.
	if (missing(cat2)) {
		if (!(missing(cat1))) {
			cat2 <- cat1
		}
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	# Replace the line below with series of low-level function calls
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( !(series_type == 3) ) {
		cstr <- paste("- process does not have estimated probabilities:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the estimated probabilities
	dims <- attr(proc$time_scores,"dim")
	yy <- mplus.get.estimated_probabilities(file,procstr,cat1,cat2)

	# plot the probabilities
	cstr <- paste("Estimated probabilities for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n',ylim=c(0:1))
	symb <- array(c(21,22,23,24,25),c(dims[1]))
	colors <- rainbow(dims[2])
	for (i in c(1:dims[2])) {
		lines(xx[,i],yy[,i],type=ptype,pch=symb[i],col=colors[i])
	}

	ldesc <- array(0,c(dims[2]))
	lty <- array(0,c(dims[2]))
	lwd <- array(0,c(dims[2]))
	lcol <- array(0,c(dims[2]))
	for (i in c(1:dims[2])) {
		ldesc[i] <- sprintf("Class %d", i)
		lty[i] = 1
		lwd[i] = 2.5
		lcol[i] <- colors[i]
	}
	legend('bottomright',col=lcol,pch=symb,ldesc,lty=lty,lwd=lwd)
}


##########################################################################
#
# mplus.plot.sample_proportions - plot sample proportions for the
# quoted process, summing up proportions of the first to the last category
# chosen
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#	cat1 - the first category to include
#	cat2 - the last category to include
#
# eg. mplus.plot.sample_proportions('ex8.4.gh5','process1',1,1)
#
mplus.plot.sample_proportions <-function(file,procstr='process1',cat1=1,cat2=1,ptype='o') {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith sample proportions.\n")
	}

	# if cat2 is missing and cat1 is given, then we should assign cat2 to cat1.
	if (missing(cat2)) {
		if (!(missing(cat1))) {
			cat2 <- cat1
		}
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		cat(cstr)
		return(invisible(cstr))
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( !(series_type == 3) ) {
		cstr <- paste("- process does not have sample proportions:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the sample proportions
	dims <- attr(proc$time_scores,"dim")
	# dims[1] is the number of time points, dims[2] is the number of classes
	yy <- mplus.get.sample_proportions(file,procstr,cat1,cat2)

	# plot the proportions
	cstr <- paste("Sample proportions for",procstr)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n',ylim=c(0:1))
	symb <- array(c(21,22,23,24,25),c(dims[1]))
	for (k in c(1:dims[2])) {
		lines(xx[,k],yy[,k],type=ptype,pch=symb[k])
	}
}


##########################################################################
#
# mplus.get.estimated_means - plot estimated means for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file, required
#	procstr - the quoted name of a series, not required.  Defaults to 'process1' (the first process)
#	classidx - the class index, not required - 0 for all classes.  Default to 0.
#
# eg. mplus.get.estimated_means('ex8.1.gh5','process1',3)
#
mplus.get.estimated_means <-function(file,procstr='process1',classidx=0) {
	if (missing(file)) {
		stop(" - name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated means.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	# Replace the line below with series of low-level function calls
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1 || series_type == 2) ) {
		cstr <- paste("- process does not have estimated means:",procstr,"\n\n")
		stop(cstr)
	}

	# set up the array for the estimated means
	dims <- attr(proc$time_scores,"dim")
	# if all classes, dimension it by number of classes.  Otherwise, just dimension by 1.
	if (classidx == 0) {
		yy <- array(0, c(dims[1],dims[2]))
	} else {
		# check that the classidx is within range.
		if (classidx < 0 || classidx > dims[2]) {
			cstr <- paste("- classidx is out of range, 1 to ",dims[2],": ",classidx,"\n\n")
			stop(cstr)
		}
		yy <- array(0, c(dims[1],1))
	}

	# get the indices of variables in the series
	var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

	if (series_type == 1) {
		mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/y_estimated_means','variables')
	} else {
		mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/e_estimated_means','variables')
	}
	var_indices <- pmatch(var_names, mean_vars, nomatch=0)

	# type 1 is estimated means for observed variables
	if (series_type == 1) {
		if (classidx == 0) {
			for (i in c(1:dims[2])) {
				for (j in c(1:dims[1])) {
					yy[j,i] <- gh5$means_and_variances_data$y_estimated_means$values[var_indices[j],i]
				}
			}
		} else {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$y_estimated_means$values[var_indices[j],classidx]
			}
		}
	}

	# type 2 is estimated means for latent variables
	if (series_type == 2) {
		if (classidx == 0) {
			for (i in c(1:dims[2])) {
				for (j in c(1:dims[1])) {
					yy[j,i] <- gh5$means_and_variances_data$e_estimated_means$values[var_indices[j],i]
				}
			}
		} else {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$e_estimated_means$values[var_indices[j],classidx]
			}
		}
	}

	# return the means
	return(yy)
}


##########################################################################
#
# mplus.get.sample_means - return sample means for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file, required
#	procstr - the quoted name of a series, not required.  Defaults to 'process1' (the first process)
#	classidx - the class index, not required - 0 for all classes.  Default to 0.
#
# eg. mplus.get.sample_means('ex8.1.gh5','process1',3)
#
mplus.get.sample_means <- function(file,procstr='process1',classidx=0) {
	if (missing(file)) {
		stop("- the name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information.\n\nUse the SERIES option in Mplus to specify series information for processes\nwith sample means.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	# Replace the line below with series of low-level function calls
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]

	if ( ! (series_type == 1) ) {
		cstr <- paste("- process does not have sample means:",procstr,"\n\n")
		stop(cstr)
	}

	# get the time scores
	xx <- proc$time_scores

	# set up the array for the estimated means
	dims <- attr(proc$time_scores,"dim")
	# if all classes, dimension it by number of classes.  Otherwise, just dimension by 1.
	if (classidx == 0) {
		yy <- array(0, c(dims[1],dims[2]))
	} else {
		# check that the classidx is within range.
		if (classidx < 0 || classidx > dims[2]) {
			cstr <- paste("- classidx is out of range, 1 to ",dims[2],": ",classidx,"\n\n")
			stop(cstr)
		}
		yy <- array(0, c(dims[1],1))
	}

	# get the indices of variables in the series
	var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

	mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/y_observed_means','variables')
	var_indices <- pmatch(var_names, mean_vars, nomatch=0)

	# only type 1 has sample means
	if (classidx == 0) {
		for (i in c(1:dims[2])) {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$y_observed_means$values[var_indices[j],i]
			}
		}
	} else {
		for (j in c(1:dims[1])) {
			yy[j,i] <- gh5$means_and_variances_data$y_observed_means$values[var_indices[j],classidx]
		}
	}

	# return the means
	return(yy)
}


##########################################################################
#
# mplus.get.estimated_modes - plot estimated modes for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file, required
#	procstr - the quoted name of a series, not required.  Defaults to 'process1' (the first process)
#	classidx - the class index, not required - 0 for all classes.  Default to 0.
#
# eg. mplus.get.estimated_modes('ex8.1.gh5','process1',3)
#
mplus.get.estimated_modes <-function(file,procstr='process1',classidx=0) {
	if (missing(file)) {
		stop(" - name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated modes.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	# Replace the line below with series of low-level function calls
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1 || series_type == 2) ) {
		cstr <- paste("- process does not have estimated modes:",procstr,"\n\n")
		stop(cstr)
	}

	# set up the array for the estimated modes
	dims <- attr(proc$time_scores,"dim")
	# if all classes, dimension it by number of classes.  Otherwise, just dimension by 1.
	if (classidx == 0) {
		yy <- array(0, c(dims[1],dims[2]))
	} else {
		# check that the classidx is within range.
		if (classidx < 0 || classidx > dims[2]) {
			cstr <- paste("- classidx is out of range, 1 to ",dims[2],": ",classidx,"\n\n")
			stop(cstr)
		}
		yy <- array(0, c(dims[1],1))
	}

	# get the indices of variables in the series
	var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

	if (series_type == 1) {
		mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/y_estimated_modes','variables')
	} else {
		mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/e_estimated_modes','variables')
	}
	var_indices <- pmatch(var_names, mean_vars, nomatch=0)

	# type 1 is estimated means for observed variables
	if (series_type == 1) {
		if (classidx == 0) {
			for (i in c(1:dims[2])) {
				for (j in c(1:dims[1])) {
					yy[j,i] <- gh5$means_and_variances_data$y_estimated_modes$values[var_indices[j],i]
				}
			}
		} else {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$y_estimated_modes$values[var_indices[j],classidx]
			}
		}
	}

	# type 2 is estimated means for latent variables
	if (series_type == 2) {
		if (classidx == 0) {
			for (i in c(1:dims[2])) {
				for (j in c(1:dims[1])) {
					yy[j,i] <- gh5$means_and_variances_data$e_estimated_modes$values[var_indices[j],i]
				}
			}
		} else {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$e_estimated_modes$values[var_indices[j],classidx]
			}
		}
	}

	# return the modes
	return(yy)
}



##########################################################################
#
# mplus.get.estimated_medians - plot estimated medians for the quoted process
#
# arguments:
#	file - the quoted name of an existing GH5 file, required
#	procstr - the quoted name of a series, not required.  Defaults to 'process1' (the first process)
#	classidx - the class index, not required - 0 for all classes.  Default to 0.
#
# eg. mplus.get.estimated_medians('ex8.1.gh5','process1',3)
#
mplus.get.estimated_medians <-function(file,procstr='process1',classidx=0) {
	if (missing(file)) {
		stop(" - name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated medians.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	# Replace the line below with series of low-level function calls
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( ! (series_type == 1 || series_type == 2) ) {
		cstr <- paste("- process does not have estimated medians:",procstr,"\n\n")
		stop(cstr)
	}

	# set up the array for the estimated medians
	dims <- attr(proc$time_scores,"dim")
	# if all classes, dimension it by number of classes.  Otherwise, just dimension by 1.
	if (classidx == 0) {
		yy <- array(0, c(dims[1],dims[2]))
	} else {
		# check that the classidx is within range.
		if (classidx < 0 || classidx > dims[2]) {
			cstr <- paste("- classidx is out of range, 1 to ",dims[2],": ",classidx,"\n\n")
			stop(cstr)
		}
		yy <- array(0, c(dims[1],1))
	}

	# get the indices of variables in the series
	var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

	if (series_type == 1) {
		mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/y_estimated_medians','variables')
	} else {
		mean_vars <- mplus.get.group.attribute(file,'means_and_variances_data/e_estimated_medians','variables')
	}
	var_indices <- pmatch(var_names, mean_vars, nomatch=0)

	# type 1 is estimated means for observed variables
	if (series_type == 1) {
		if (classidx == 0) {
			for (i in c(1:dims[2])) {
				for (j in c(1:dims[1])) {
					yy[j,i] <- gh5$means_and_variances_data$y_estimated_medians$values[var_indices[j],i]
				}
			}
		} else {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$y_estimated_medians$values[var_indices[j],classidx]
			}
		}
	}

	# type 2 is estimated means for latent variables
	if (series_type == 2) {
		if (classidx == 0) {
			for (i in c(1:dims[2])) {
				for (j in c(1:dims[1])) {
					yy[j,i] <- gh5$means_and_variances_data$e_estimated_medians$values[var_indices[j],i]
				}
			}
		} else {
			for (j in c(1:dims[1])) {
				yy[j,i] <- gh5$means_and_variances_data$e_estimated_medians$values[var_indices[j],classidx]
			}
		}
	}

	# return the modes
	return(yy)
}



##########################################################################
#
# mplus.get.time_scores - return time scores for the quoted process
#
# arguments:
#	procstr - the quoted name of a series
#
# eg. mplus.get.time_scores('ex6.1.gh5', 'process1')
#
mplus.get.time_scores <- function(file,procstr='process1') {
	if (missing(file)) {
		stop("- the name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information.\n\nUse the SERIES option in Mplus to specify series information for processes\nwith sample means.\n")
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	# Replace the line below with series of low-level function calls
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]

	# get the time scores
	xx <- proc$time_scores
	return(xx)
}



##########################################################################
#
# mplus.get.estimated_probabilities - return estimated probabilities for the
# quoted process, summing up probabilities of the first to the last category
# chosen
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#	cat1 - the first category to include
#	cat2 - the last category to include
#
# eg. mplus.get.estimated_probabilities('ex8.4.gh5','process1',1,1)
#
mplus.get.estimated_probabilities <- function(file,procstr='process1',cat1=1,cat2=1) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith estimated probabilities.\n")
	}

	# if cat2 is missing and cat1 is given, then we should assign cat2 to cat1.
	if (missing(cat2)) {
		if (!(missing(cat1))) {
			cat2 <- cat1
		}
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( !(series_type == 3) ) {
		cstr <- paste("- process does not have estimated probabilities:",procstr,"\n\n")
		stop(cstr)
	}

	# set up the array for the estimated probabilities
	dims <- attr(proc$time_scores,"dim")
	yy <- array(0, c(dims[1],dims[2]))

	# get indices and names of the variables in the series
	var_indices <- mplus.get.group.attribute(file,cstr2,'var_indices')
	var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

	# get categorical data information then look up the variables in the process
	# in categorical_data so we can get the number of categories for each variable in the process
	# this would be achieved by categories[cat_indices[i]] for variable i in the process
	categories <- mplus.get.group.attribute(file,'categorical_data','categories')

	catvars <- mplus.get.group.attribute(file,'categorical_data','var_names')
	cat_indices <- pmatch(var_names, catvars, nomatch=0)

	# get the probabilities
	for (i in c(1:dims[1])) {
		for (j in c(1:dims[2])) {
			start_index <- sum(as.integer(categories[1:cat_indices[i]-1]))

			startk <- cat1 + start_index
			endk <- cat2 + start_index
			yy[i,j] <- sum(gh5$means_and_variances_data$estimated_probs$values[startk:endk,j])
		}
	}

	# return the probabilities
	return(yy);
}




##########################################################################
#
# mplus.get.sample_proportions - return sample proportions for the
# quoted process, summing up proportions of the first to the last category
# chosen
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	procstr - the quoted name of a series
#	cat1 - the first category to include
#	cat2 - the last category to include
#
# eg. mplus.get.sample_proportions('ex8.4.gh5','process1',1,1)
#
mplus.get.sample_proportions <-function(file,procstr='process1',cat1=1,cat2=1) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	# check that the series exists
	if (!("process_data" %in% names(gh5))) {
		stop("- requires series information\n\nUse the SERIES option in Mplus to specify series information for processes\nwith sample proportions.\n")
	}

	if (missing(procstr)) {
		stop("- requires the name of a series\n\nUse mplus.list.processes() to get the list of series processes.")
	}

	# if cat2 is missing and cat1 is given, then we should assign cat2 to cat1.
	if (missing(cat2)) {
		if (!(missing(cat1))) {
			cat2 <- cat1
		}
	}

	allpnames <- attr(gh5$process_data,"names")
	pind <- pmatch(procstr, allpnames, nomatch=0)
	if (pind == 0) {
		cstr <- paste("- process does not exist:",procstr,"\n\n")
		stop(cstr)
	}

	# get the process
	proc <- gh5$process_data[[procstr]]

	# get the series type in properties
	cstr2 <- paste(c("process_data"),"/",procstr,"", sep="")
	prop <- mplus.get.group.attribute(file,cstr2,'properties')

	series_type <- prop[1]
	if ( ! (series_type == 3) ) {
		cstr <- paste("- process does not have sample proportions:",procstr,"\n\n")
		stop(cstr)
	}

	# set up the array for the sample proportions
	dims <- attr(proc$time_scores,"dim")
	# dims[1] is the number of time points, dims[2] is the number of classes
	yy <- array(0, c(dims[1],dims[2]))

	# get indices and names of the variables in the series
	var_indices <- mplus.get.group.attribute(file,cstr2,'var_indices')
	var_names <- mplus.get.group.attribute(file,cstr2,'var_names')

	# get categorical data information then look up the variables in the process
	# in categorical_data so we can get the number of categories for each variable in the process
	# this would be achieved by categories[cat_indices[i]] for variable i in the process
	categories <- mplus.get.group.attribute(file,'categorical_data','categories')

	catvars <- mplus.get.group.attribute(file,'categorical_data','var_names')
	cat_indices <- pmatch(var_names, catvars, nomatch=0)

	# get the proportions
	for (i in c(1:dims[1])) {
		for (j in c(1:dims[2])) {
			start_index <- sum(categories[1:cat_indices[i]-1])

			startk <- cat1 + start_index
			endk <- cat2 + start_index
			yy[i,j] <- sum(gh5$means_and_variances_data$observed_probs$values[startk:endk,j])
		}
	}

	# return the proportions
	return(yy)
}



##########################################################################
#
# mplus.list.variables - list the variables in individual data
#
# arguments: none
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.variables('ex8.1.gh5')
#
mplus.list.variables <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if individual data exists
	if ( !("individual_data" %in% names(gh5)) ) {
		stop("mplus.list.variables requires individual data.\n\nUse TYPE=PLOT1 or TYPE=PLOT3 setting in Mplus to store individual data.")
	}

	cat(c("\nList of variable names to use in the following functions:\n"))
	cat(c(" - mplus.plot.histogram\n"))
	cat(c(" - mplus.plot.scatterplot\n"))
	cat(c(" - mplus.get.data\n"))

	cat(c("\nVariables:\n"))

	# get the variable names from individual_data and lookup the indices
	var_names <- mplus.get.group.attribute(file, 'individual_data', 'var_names')
	var_names <- gsub("(^\\s+|\\s+$)", "", var_names, perl=TRUE)
	var_names
}


##########################################################################
#
# mplus.get.data - return the individual data for the quoted variable
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	v - name of variable to plot
#
# eg. mplus.get.data('ex8.1.gh5','y1')
#
mplus.get.data <- function(file,v) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	# check that the series exists
	gh5 <- h5dump(file, load=TRUE)

	if (!("individual_data" %in% names(gh5))) {
		stop("mplus.get.data requires individual data.\n\nUse TYPE=PLOT1 or TYPE=PLOT3 setting in Mplus to store individual data.")
	}

	if (missing(v)) {
		stop("mplus.get.data requires the name of a variable.\n\nUse mplus.list.variables() to get the list of variable names.")
	}

	# variables are stored in uppercase
	var <- toupper(v)

	# get the variable names from individual_data and lookup the indices
	var_names <- mplus.get.group.attribute(file, 'individual_data', 'var_names')

	index <- pmatch(var, var_names, nomatch=0)

	if (index == 0) {
		cstr <- paste(c("Unknown variable:"),var,"\n")
		stop(cstr)
	}

	# get the data for the variable
	xx <- gh5$individual_data$raw_data[index,]
	xx
}


##########################################################################
#
# mplus.plot.scatterplot - plot the scatterplot for the 2 quoted variables
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	xv - name of variable on the x-axis
#	yv - name of variable on the y-axis
#
# eg. mplus.plot.scatterplot('ex8.1.gh5','y1','y2')
#
mplus.plot.scatterplot <- function(file, xv, yv) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if individual data exists
	if ( !("individual_data" %in% names(gh5)) ) {
		stop("mplus.plot.scatterplot requires individual data.\n\nUse TYPE=PLOT1 or TYPE=PLOT3 setting in Mplus to store individual data\nfor scatterplots.")
	}

	if (missing(xv) || missing(yv)) {
		stop("mplus.plot.scatterplot requires the names of two variables.\n\nUse mplus.list.variables() to get the list of variable names.")
	}

	# variables are stored in uppercase
	xvar <- toupper(xv)
	yvar <- toupper(yv)

	# get the variable names from individual_data and lookup the indices
	var_names <- mplus.get.group.attribute(file, 'individual_data', 'var_names')

	xindex <- pmatch(xvar, var_names, nomatch=0)
	yindex <- pmatch(yvar, var_names, nomatch=0)

	if (xindex == 0) {
		cstr <- paste(c("Unknown x-variable:"),xvar,"\n")
		stop(cstr)
	}
	if (yindex == 0) {
		cstr <- paste(c("Unknown y-variable:"),yvar,"\n")
		stop(cstr)
	}

	# get the data for the 2 variables
	xx <- mplus.get.data(file,xvar)
	yy <- mplus.get.data(file,yvar)

	plot(xx,yy,xlab=xvar,ylab=yvar)
}


##########################################################################
#
# mplus.plot.histogram - plot the histogram for the quoted variable, using the
# specified number of bins (the default is 10 bins)
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	v - name of variable to plot
#	bins - the number of bins to use
#
# eg. mplus.plot.histogram('y1',5)
#
mplus.plot.histogram <- function(file,v,bins=10) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if individual data exists
	if ( !("individual_data" %in% names(gh5)) ) {
		stop("mplus.plot.histogram requires individual data.\n\nUse TYPE=PLOT1 or TYPE=PLOT3 setting in Mplus to store individual data\nfor histograms.")
	}

	if (missing(v)) {
		stop("mplus.plot.histogram requires the name of a variable.\n\nUse mplus.list.variables() to get the list of variable names.")
	}

	# the number of bins should be greater than 0
	if (bins <= 0) {
		stop("The number of bins should be greater than 0.")
	}

	# variables are stored in uppercase
	var <- toupper(v)

	# get the variable names from individual_data and lookup the indices
	var_names <- mplus.get.group.attribute(file, 'individual_data', 'var_names')

	index <- pmatch(var, var_names, nomatch=0)

	if (index == 0) {
		cstr <- paste(c("Unknown variable:"),var,"\n")
		stop(cstr)
	}

	xx <- mplus.get.data(file,v)
	cstr <- paste(c("Histogram of"),var)
	hist(xx,breaks=seq(min(xx),max(xx),length=bins+1),col="red",main=cstr,xlab=var,right=TRUE)
}


######################################################################################################
# Functions for BAYESIAN plots
######################################################################################################


#=========================================================================
#
# mplus.list.bayesian.parameters - list the parameters in bayesian data
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.bayesian.parameters('ex8.1.gh5')
#
mplus.list.bayesian.parameters <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("mplus.list.bayesian.parameters requires bayesian data.\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	cat(c("\nList of parameters to use in the following functions:\n"))
	cat(c(" - mplus.plot.bayesian.trace_plots\n"))

	cat(c("\nParameters:\n"))

	# get the parameter statements from bayesian_data and lookup the indices
	statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	nplaus <- length(statements)
	for (i in c(1:nplaus)) {
		cstr <- sprintf("[%d] %s", i, statements[i])
		cat(cstr,sep="\n")
	}
	invisible(statements)
}

#=========================================================================
#
# mplus.get.bayesian.parameter.data - get the bayesian data for the given parameter/chain
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   paramstr - the quoted name of a parameter or the parameter index
#	chainnum - the chain number
#
# eg. mplus.get.bayesian.parameter.data('ex8.1.gh5','parameter 1',1)
#
mplus.get.bayesian.parameter.data <- function(file,paramstr,chainnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data.\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (is.character(paramstr)) {
		statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
		statements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("Unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		# first dimension is the number of parameters
		# second dimension is the number of iterations
		# third dimension is the number of chains
		dims <- attr(gh5$bayesian_data$parameters_autocorr$parameters,"dim")

		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	xx <- gh5$bayesian_data$parameters_autocorr$parameters[paramidx,,chainnum]
	xx
}


#=========================================================================
#
# mplus.get.bayesian.prior.parameter.data - get the prior data for the given parameter
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	paramstr - the quoted parameter label or the parameter index
#
# eg. mplus.get.bayesian.prior.parameter.data('ex8.1.gh5',1)
#
mplus.get.bayesian.prior.parameter.data <- function(file,paramstr) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data.\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	# first dimension is the number of parameters
	# second dimension is the number of priors
	dims <- attr(gh5$bayesian_data$parameters_autocorr$priors,"dim")

	if (is.character(paramstr)) {
		statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
		statements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		# first dimension is the number of parameters
		# second dimension is the number of priors
		dims <- attr(gh5$bayesian_data$parameters_autocorr$priors,"dim")

		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	xx <- gh5$bayesian_data$parameters_autocorr$priors[,paramidx]
	xx
}


#=========================================================================
#
# mplus.get.bayesian.autocorrelation - get the autocorrelation data for the given parameter
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   paramidx - the quoted parameter label
#   chainnum - the chain number
#
# eg. mplus.get.bayesian.autocorrelation('ex8.1.gh5','parameter 1',1)
#
mplus.get.bayesian.autocorrelation <- function(file,paramstr,chainnum=1) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data.\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	# first dimension is the number of autocorrelation
	# second dimension is the number of parameters
	# third dimension is the number of chains
	dims <- attr(gh5$bayesian_data$parameters_autocorr$autocorrelation,"dim")
	if (is.character(paramstr)) {
		statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
		statements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("Unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	if (chainnum < 1 && chainnum > dims[3]) {
		cstr <- paste("- invalid chain number: ", chainnum,"\n\nThe chain number must be between 1 and ", dims[3], ".")
		stop(cstr)
	}

	xx <- gh5$bayesian_data$parameters_autocorr$autocorrelation[,paramidx,chainnum]
	xx
}

#=========================================================================
#
# mplus.plot.bayesian.traceplot - list the parameters in bayesian data
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   paramstr - the quoted name of a parameter
#
# eg. mplus.plot.bayesian.traceplot('ex8.1.gh5','parameter 1')
#
mplus.plot.bayesian.traceplot <- function(file,paramstr) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	# get the dimensions of parameters array
	# first dimension is the number of parameters
	# second dimension is the number of iterations
	# third dimension is the number of chains
	dims <- attr(gh5$bayesian_data$parameters_autocorr$parameters,"dim")

	statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')

	if (is.character(paramstr)) {
		lcstatements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, lcstatements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}
	label <- statements[paramidx]
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	xx <- array(0, c(dims[2],dims[3]))
	yy <- array(0, c(dims[2],dims[3]))

	for (i in c(1:dims[3])) {
		yy[,i] <- mplus.get.bayesian.parameter.data(file, paramidx, i)
	}
	for (i in c(1:dims[2])) {
		xx[i,] <- i
	}

	colors <- rainbow(dims[3])


	ndist <- mplus.get.dataset.attribute(file, 'bayesian_data/parameters_autocorr/parameters', 'ndistribution')

	# plot the traceplot
	cstr <- paste("Trace plot of:",label)
	plot(xx,yy,xlab="",ylab="",main=cstr,type='n')
	for (i in c(1:dims[3])) {
		lines(xx[,i],yy[,i],col=colors[i])
	}
	abline(v=ndist,untf=FALSE,col='red')
}


#=========================================================================
#
# mplus.plot.bayesian.distribution - plot the histogram for the parameter, using the
# specified number of bins (the default is 100 bins)
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	paramstr - the quoted name of the parameter
#	bins - the number of bins to use
#
# eg. mplus.plot.bayesian.distribution('bayes.gh5','parameter 1',50)
#
mplus.plot.bayesian.distribution <- function(file,paramstr,bins=100) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(paramstr)) {
		stop("- requires the parameter label or index.\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	# the number of bins should be greater than 0
	if (bins <= 0) {
		stop("The number of bins should be greater than 0.")
	}

	# get the dimensions of parameters array
	# first dimension is the number of parameters
	# second dimension is the number of iterations
	# third dimension is the number of chains
	dims <- attr(gh5$bayesian_data$parameters_autocorr$parameters,"dim")

	statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	if (is.character(paramstr)) {
		lcstatements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, lcstatements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("Parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}
	label <- statements[paramidx]

	ndist <- mplus.get.dataset.attribute(file, 'bayesian_data/parameters_autocorr/parameters', 'ndistribution')

	xxc <- array(0, c(dims[2],dims[3]))
	xx <- array(0, c((dims[2]-ndist)*dims[3]))
	for (i in c(1:dims[3])) {
		xxc[,i] <- mplus.get.bayesian.parameter.data(file, paramidx, i)
	}
	start <- 0
	#print(dims)
	for (i in c(1:dims[3])) {
		for (j in c((ndist+1):dims[2])) {
			start <- start + 1
			#cstr <- paste(start, j, i)
			#print(cstr)
			#print(xxc[j,i])
			xx[start] <- xxc[j,i]
		}
	}

	cstr <- paste(c("Distribution of:"),label)
	h <- hist(xx,breaks=seq(min(xx),max(xx),length=bins+1),col="red",main=cstr,xlab='Estimate',ylab='Count')

	xxmode <- h$mids[h$counts == max(h$counts)]
	xxmean <- mean(xx)
	xxsd <- sd(xx)
	xxmedian <- median(xx)

	left <- quantile(xx, 0.025)
	right <- quantile(xx, 0.975)
	
	abline(v=xxmode,untf=FALSE,col='green')
	abline(v=xxmean,untf=FALSE,col='brown')
	abline(v=xxmedian,untf=FALSE,col='purple')
	abline(v=left,untf=FALSE,col='blue')
	abline(v=right,untf=FALSE,col='blue')

	modestr <- sprintf("Mode = %0.5f", xxmode)
	meanstr <- sprintf("Mean = %0.5f, Std Dev = %0.5f", xxmean, xxsd)
	medianstr <- sprintf("Median = %0.5f", xxmedian)
	lowci <- sprintf("95%% Lower CI = %0.5f", left)
	uppci <- sprintf("95%% Upper CI = %0.5f", right)
	ldesc <- c(meanstr, medianstr, modestr, lowci, uppci)

	lcol <- c('brown','purple','green','blue','blue')
	legend("topright",ldesc,col=lcol,lty=c(1,1,1,1,1),lwd=c(2.5,2.5,2.5,2.5,2.5))

	#invisible(xx)
}



#=========================================================================
#
# mplus.plot.bayesian.prior.distribution - plot the histogram for the parameter, using the
# specified number of bins (the default is 100 bins)
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	paramstr - the quoted name of the parameter
#	bins - the number of bins to use
#
# eg. mplus.plot.bayesian.prior.distribution('bayes.gh5','parameter 1',50)
#
mplus.plot.bayesian.prior.distribution <- function(file,paramstr,bins=100) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(paramstr)) {
		stop("- requires the parameter index\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	# the number of bins should be greater than 0
	if (bins <= 0) {
		stop("- the number of bins should be greater than 0")
	}

	statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	# get the dimensions of parameters array
	# first dimension is the number of parameters
	# second dimension is the number of priors
	dims <- attr(gh5$bayesian_data$parameters_autocorr$priors,"dim")

	if (is.character(paramstr)) {
		lcstatements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, lcstatements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}
	plabel <- statements[paramidx]

	xx <- mplus.get.bayesian.prior.parameter.data(file, paramidx)

	if (min(xx) == 999 && max(xx) == 999) {
		stop("- prior distributions for this parameter cannot be displayed because the prior is improper")
	} else if (min(xx) == 998 && max(xx) == 998) {
		stop("- prior distributions for this parameter are not available")
	}

	cstr <- paste(c("Prior distribution of:"),plabel)
	h <- hist(xx,breaks=seq(min(xx),max(xx),length=bins+1),col="red",main=cstr,xlab='Estimate',ylab='Count')

	xxmode <- h$mids[h$counts == max(h$counts)]
	xxmean <- mean(xx)
	xxsd <- sd(xx)
	xxmedian <- median(xx)

	left <- quantile(xx, 0.025)
	right <- quantile(xx, 0.975)
	
	abline(v=xxmode,untf=FALSE,col='green')
	abline(v=xxmean,untf=FALSE,col='brown')
	abline(v=xxmedian,untf=FALSE,col='purple')
	abline(v=left,untf=FALSE,col='blue')
	abline(v=right,untf=FALSE,col='blue')

	modestr <- sprintf("Mode = %0.5f", xxmode)
	meanstr <- sprintf("Mean = %0.5f, Std Dev = %0.5f", xxmean, xxsd)
	medianstr <- sprintf("Median = %0.5f", xxmedian)
	lowci <- sprintf("95%% Lower CI = %0.5f", left)
	uppci <- sprintf("95%% Upper CI = %0.5f", right)
	ldesc <- c(meanstr, medianstr, modestr, lowci, uppci)

	lcol <- c('brown','purple','green','blue','blue')
	legend("topright",ldesc,col=lcol,lty=c(1,1,1,1,1),lwd=c(2.5,2.5,2.5,2.5,2.5))

	invisible(xx)
}



#=========================================================================
#
# mplus.plot.bayesian.autocorrelation - plot the autocorrelation histogram for the parameter
#	for the given chain
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	paramstr - the quoted name of the parameter
#	chainnum - the chain number
#
# eg. mplus.plot.bayesian.autocorrelation('bayes.gh5','parameter 1',1)
#
mplus.plot.bayesian.autocorrelation <- function(file,paramstr,chainnum=1) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian dat.\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(paramstr)) {
		stop("- requires the parameter label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	statements <- mplus.get.group.dataset(file, 'bayesian_data/parameters_autocorr', 'statements')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	# get the dimensions of parameters array
	# first dimension is the number of autocorrelations
	# second dimension is the number of parameters
	# third dimension is the number of chains
	dims <- attr(gh5$bayesian_data$parameters_autocorr$autocorrelation,"dim")

	if (is.character(paramstr)) {
		lcstatements <- tolower(statements)
		paramstr <- tolower(paramstr)
		paramidx <- pmatch(paramstr, lcstatements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown parameter:"),paramstr,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- paramstr
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- parameter index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}
	plabel <- statements[paramidx]

	if (chainnum < 1 && chainnum > dims[3]) {
		cstr <- paste("- invalid chain number: ", chainnum,"\n\nThe chain number must be between 1 and ", dims[3], ".")
		stop(cstr)
	}

	yy <- mplus.get.bayesian.autocorrelation(file,paramidx,chainnum)
	xx <- as.character(1:dims[1])

	cstr <- paste(c("Autocorrelation (chain "),format(chainnum),c("): "),plabel)
	barplot(yy,ylim=c(-1,1),names.arg=xx,col='red',main=cstr)

	invisible(xx)
}


#=========================================================================
#
# mplus.list.bayesian.predictive.labels - list the parameters in bayesian data
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.bayesian.predictive.labels('ex8.1.gh5')
#
mplus.list.bayesian.predictive.labels <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	cat(c("\nList of parameters to use in the following functions:\n"))
	cat(c(" - mplus.plot.bayesian.predictive.scatterplots\n"))
	cat(c(" - mplus.plot.bayesian.predictive.distribution\n"))

	cat(c("\nPredictive labels:\n"))

	# get the parameter statements from bayesian_data and lookup the indices
	statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)
	statements
}


#=========================================================================
#
# mplus.get.bayesian.predictive.observed - get the predictive observed data
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the quoted name of the parameter
#
# eg. mplus.get.bayesian.predictive.observed('bayes.gh5','parameter 1')
#
mplus.get.bayesian.predictive.observed <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	if (is.character(plabel)) {
		statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
		statements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of parameters array
		# first dimension is the number of ???
		# second dimension is the number of predictive labels
		dims <- attr(gh5$bayesian_data$predictive$observed,"dim")

		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[2]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	xx <- gh5$bayesian_data$predictive$observed[,paramidx]
	xx
}



#=========================================================================
#
# mplus.get.bayesian.predictive.replicated - get the predictive replicated data
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the quoted name of the parameter
#
# eg. mplus.get.bayesian.predictive.replicated('bayes.gh5','parameter 1')
#
mplus.get.bayesian.predictive.replicated <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	if (is.character(plabel)) {
		statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
		statements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of parameters array
		# first dimension is the number of ???
		# second dimension is the number of predictive labels
		dims <- attr(gh5$bayesian_data$predictive$replicated,"dim")

		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[2]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	xx <- gh5$bayesian_data$predictive$replicated[,paramidx]
	xx
}


#=========================================================================
#
# mplus.get.bayesian.predictive.lowerci - get the predictive lower CI
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the quoted name of the parameter
#
# eg. mplus.get.bayesian.predictive.lowerci('bayes.gh5','parameter 1')
#
mplus.get.bayesian.predictive.lowerci <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	if (is.character(plabel)) {
		statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
		statements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of parameters array
		# first dimension is the number of pvalues
		# second dimension is the number of predictive labels
		dims <- attr(gh5$bayesian_data$predictive$pvalues,"dim")

		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[2]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	gh5$bayesian_data$predictive$pvalues[1,paramidx]
}



#=========================================================================
#
# mplus.get.bayesian.predictive.upperci - get the predictive upper CI
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the quoted name of the parameter
#
# eg. mplus.get.bayesian.predictive.upperci('bayes.gh5','parameter 1')
#
mplus.get.bayesian.predictive.upperci <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	if (is.character(plabel)) {
		statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
		statements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of parameters array
		# first dimension is the number of pvalues
		# second dimension is the number of predictive labels
		dims <- attr(gh5$bayesian_data$predictive$pvalues,"dim")

		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[2]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	gh5$bayesian_data$predictive$pvalues[2,paramidx]
}




#=========================================================================
#
# mplus.get.bayesian.predictive.pvalue - get the predictive pvalue
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the quoted name of the parameter
#
# eg. mplus.get.bayesian.predictive.pvalue('bayes.gh5','parameter 1')
#
mplus.get.bayesian.predictive.pvalue <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	if (is.character(plabel)) {
		statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
		statements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of parameters array
		# first dimension is the number of pvalues
		# second dimension is the number of predictive labels
		dims <- attr(gh5$bayesian_data$predictive$pvalues,"dim")

		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[2]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	gh5$bayesian_data$predictive$pvalues[3,paramidx]
}




#=========================================================================
#
# mplus.get.bayesian.predictive.pvalue_type - get the predictive pvalue type
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the quoted name of the parameter
#
# eg. mplus.get.bayesian.predictive.pvalue_type('bayes.gh5','parameter 1')
#
mplus.get.bayesian.predictive.pvalue_type <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	ptypes <- mplus.get.group.attribute(file,'/bayesian_data/predictive','types')

	if (is.character(plabel)) {
		statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
		statements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, statements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of parameters array
		# first dimension is the number of pvalues
		dims <- attr(ptypes,"dim")

		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	ptypes[paramidx]
}




#=========================================================================
#
# mplus.plot.bayesian.predictive.scatterplot - plot the predictive checking scatterplot
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the predictive label
#
# eg. mplus.plot.bayesian.predictive.scatterplot('bayes.gh5','label 1')
#
mplus.plot.bayesian.predictive.scatterplot <- function(file,plabel) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the predictive label\n\nUse mplus.list.bayesian.parameters() to get the list of parameters.")
	}

	statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	dims <- attr(statements,"dim")

	if (is.character(plabel)) {
		lcstatements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, lcstatements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	rep <- mplus.get.bayesian.predictive.replicated(file,paramidx)
	obs <- mplus.get.bayesian.predictive.observed(file,paramidx)

	omin <- min(obs)
	omax <- max(obs)
	
	rmin <- min(rep)
	rmax <- max(rep)
	
	if (omin < rmin) {
		rmin <- omin
	}
	if (omax > rmax) {
		rmax <- omax
	}
	
	plot(obs,rep,xlab='Observed',ylab='Replicated',xlim=c(rmin,rmax),ylim=c(rmin,rmax))
#	print(rmin)
#	print(rmax)
	xx=c(rmin,rmax)
	yy=c(rmin,rmax)
	lines(xx,yy,col='green')
	#text(50,50,"test")

	lowci <- mplus.get.bayesian.predictive.lowerci(file,paramidx)
	uppci <- mplus.get.bayesian.predictive.upperci(file,paramidx)
	pval <- mplus.get.bayesian.predictive.pvalue(file,paramidx)
	ptype <- mplus.get.bayesian.predictive.pvalue_type(file,paramidx)

	if (ptype == -1) {
		text2 <- "(Proportion of Points in the Lower Right Half)";
	}
	else if (ptype == 1) {
		text2 <- "(Proportion of Points in the Upper Left Half)";
	} else {
		text2 <- "(Smallest Proportion of Points in the Upper versus Lower Halves)";
	}

	#ldesc <- sprintf("95%% Confidence Interval for the Difference\n%0.3f     %0.3f\nPosterior Predictive P-Value %0.3f\n%s",
	#		lowci, uppci, pval, text2)

	#mtext(ldesc, side=3)

	line1 <- sprintf("95%% Confidence Interval for the Difference")
	line2 <- sprintf("            %0.3f     %0.3f                ", lowci, uppci)
	line3 <- sprintf("")
	line4 <- sprintf("   Posterior Predictive P-Value %0.3f      ", pval)
	line5 <- sprintf("")
	line6 <- text2

	ldesc <- c(line1,line2,line3,line4,line5,line6)
	legend('topleft',ldesc,xjust=1)

	title(statements[paramidx])
}




#=========================================================================
#
# mplus.plot.bayesian.predictive.distribution - plot the predictive checking distribution
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plabel - the predictive label
#	bins - the number of bins, default is 10
#
# eg. mplus.plot.bayesian.predictive.distribution('bayes.gh5','label 1')
#
mplus.plot.bayesian.predictive.distribution <- function(file,plabel,bins=100) {
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data\n\nUse TYPE=PLOT2 setting in Mplus with a Bayesian analysis.")
	}

	if (missing(plabel)) {
		stop("- requires the index of the predictive label\n\nUse mplus.list.bayesian.predictive.labels() to get the list of parameters.")
	}

	statements <- mplus.get.group.attribute(file, 'bayesian_data/predictive', 'labels')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	dims <- attr(statements,"dim")

	if (is.character(plabel)) {
		lcstatements <- tolower(statements)
		plabel <- tolower(plabel)
		paramidx <- pmatch(plabel, lcstatements, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown predictive label:"),plabel,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- plabel
		if (paramidx < 1 || paramidx > dims[1]) {
			cstr <- paste("- predictive label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.parameters to see the list of parameters.\n")
			stop(cstr)
		}
	}

	rep <- mplus.get.bayesian.predictive.replicated(file,paramidx)
	obs <- mplus.get.bayesian.predictive.observed(file,paramidx)

	omin <- min(obs)
	omax <- max(obs)
	
	rmin <- min(rep)
	rmax <- max(rep)
	
	if (omin < rmin) {
		rmin <- omin
	}
	if (omax > rmax) {
		rmax <- omax
	}

	npred <- length(rep)
	vals <- array(c(npred))
	for (i in c(1:npred)) {
		vals[i] <- obs[i] - rep[i]
	}
	hist(vals,breaks=seq(min(vals),max(vals),length=bins+1),col="red",main=statements[paramidx],xlab='Observed - Replicated',ylab='Count')

	xxmedian <- median(vals)
	abline(v=xxmedian,untf=FALSE,col='purple')

#	print(rmin)
#	print(rmax)
	xx=c(rmin,rmax)
	yy=c(rmin,rmax)
	lines(xx,yy,col='green')
	#text(50,50,"test")

	lowci <- mplus.get.bayesian.predictive.lowerci(file,paramidx)
	uppci <- mplus.get.bayesian.predictive.upperci(file,paramidx)
	pval <- mplus.get.bayesian.predictive.pvalue(file,paramidx)

	#ldesc <- sprintf("95%% Confidence Interval for the Difference\n%0.3f     %0.3f\nPosterior Predictive P-Value %0.3f\n%s",
	#		lowci, uppci, pval, text2)

	#mtext(ldesc, side=3)

	line1 <- sprintf("95%% Confidence Interval for the Difference")
	line2 <- sprintf("            %0.3f     %0.3f                ", lowci, uppci)
	line3 <- sprintf("")
	line4 <- sprintf("   Posterior Predictive P-Value %0.3f      ", pval)

	ldesc <- c(line1,line2,line3,line4)
	legend('topleft',ldesc,xjust=1)
}



#=========================================================================
#
# mplus.list.bayesian.plausible.labels - list the plausible labels in bayesian data
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.bayesian.plausible.labels('ex8.1.gh5')
#
mplus.list.bayesian.plausible.labels <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("mplus.list.bayesian.plausible.labels requires bayesian data and factor scores.\n\nUse TYPE=PLOT3 and the FACTORS option in Mplus with a Bayesian analysis.")
	}

	# check if plausible exists
	if ( !("plausible" %in% names(gh5$bayesian_data)) ) {
		stop("mplus.list.bayesian.plausible.labels requires bayesian data factor scores.\n\nUse TYPE=PLOT3 and the FACTORS option in Mplus with a Bayesian analysis.")
	}

	cat(c("\nList of labels to use in the following functions:\n"))
	cat(c(" - mplus.plot.bayesian.plausible.distribution\n"))

	cat(c("\nPlausible labels:\n"))

	# get the parameter statements from bayesian_data and lookup the indices
	statements <- mplus.get.group.attribute(file, 'bayesian_data/plausible', 'plauslabels')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	nplaus <- length(statements)
	for (i in c(1:nplaus)) {
		cstr <- sprintf("[%d] %s", i, statements[i])
		cat(cstr,sep="\n")
	}
#	cat(statements,sep="\n")
	invisible(statements)
}



#=========================================================================
#
# mplus.get.bayesian.plausible.data - get plausible data for the given plausible label
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	plauslabel - the plausible label or the index of the plausible label
#	obs - the observation index or 0 for overall
#
# eg. mplus.get.bayesian.plausible.data('ex8.1.gh5',1,obs)
#
mplus.get.bayesian.plausible.data <- function(file,plauslabel,obs=0) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data and factor scores\n\nUse TYPE=PLOT3 and the FACTORS option in Mplus with a Bayesian analysis.")
	}

	# check if plausible exists
	if ( !("plausible" %in% names(gh5$bayesian_data)) ) {
		stop("- requires bayesian data and factor scores\n\nUse TYPE=PLOT3 and the FACTORS option in Mplus with a Bayesian analysis.")
	}

	if (missing(plauslabel)) {
		stop("- requires the plausible label\n\nUse mplus.list.bayesian.plausible.labels() to get the list of plausible labels.")
	}

	if (is.character(plauslabel)) {
		labels <- mplus.get.group.attribute(file,'bayesian_data/plausible','plauslabels')
		labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)
		labels <- tolower(labels)
		plauslabel <- tolower(plauslabel)
		paramidx <- pmatch(plauslabel, labels, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown plausible label:"),plauslabel,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of plausible array
		# first dimension is the number of observations
		# second dimension is the number of imputations
		# third dimension is the number of labels
		dims <- attr(gh5$bayesian_data$plausible$plausible,"dim")

		paramidx <- plauslabel
		if (paramidx < 1 || paramidx > dims[3]) {
			cstr <- paste("- plausible label index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.plausible.labels to see the list of plausible labels.\n")
			stop(cstr)
		}
	}

	if (obs == 0) {
		xx <- array(0, c(dims[1]*dims[2]))
		start <- 0
		for (i in c(1:dims[1])) {
			for (j in c(1:dims[2])) {
				start <- start + 1
				xx[start] <- gh5$bayesian_data$plausible$plausible[i,j,paramidx]
			}
		}
	} else {
		xx <- gh5$bayesian_data$plausible$plausible[obs,,paramidx]
	}
	xx
}



#=========================================================================
#
# mplus.plot.bayesian.plausible.distribution - plot the histogram for the plausible label, using the
# specified number of bins (the default is 100 bins for overall and 10 for a specific observation)
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	paramstr - name or index of variable to plot
#	obs - the observation number or 0
#	bins - the number of bins to use
#
# eg. mplus.plot.bayesian.plausible.distribution('bayes.gh5',1,0)
#
mplus.plot.bayesian.plausible.distribution <- function(file,plauslabel,obs=0,bins=100) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if bayesian data exists
	if ( !("bayesian_data" %in% names(gh5)) ) {
		stop("- requires bayesian data and factor scores\n\nUse TYPE=PLOT3 and the FACTORS option in Mplus with a Bayesian analysis.")
	}

	# check if plausible exists
	if ( !("plausible" %in% names(gh5$bayesian_data)) ) {
		stop("- requires bayesian data and factor scores\n\nUse TYPE=PLOT3 and the FACTORS option in Mplus with a Bayesian analysis.")
	}

	if (missing(plauslabel)) {
		stop("- requires the index of the plausible label\n\nUse mplus.list.bayesian.plausible.labels() to get the list of plausible labels.")
	}

	if (missing(bins)) {
		if (obs == 0) {
			bins = 100
		} else {
			bins = 10
		}
	}

	# the number of bins should be greater than 0
	if (bins <= 0) {
		stop("- the number of bins should be greater than 0")
	}

	labels <- mplus.get.group.attribute(file,'bayesian_data/plausible','plauslabels')
	labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)
	adim <- attr(labels,'dim')

	if (is.character(plauslabel)) {
		lclabels <- tolower(labels)
		plauslabel <- tolower(plauslabel)
		paramidx <- pmatch(plauslabel, lclabels, nomatch=0)

		if (paramidx == 0) {
			cstr <- paste(c("- unknown plausible label:"),plauslabel,"\n")
			stop(cstr)
		}
	} else {
		paramidx <- plauslabel
		if (paramidx < 1 || paramidx > adim[1]) {
			cstr <- paste("- plausible index is out of range: ",paramidx,"\n\nUse mplus.list.bayesian.plausible.labels to see the list of plausible labels.\n")
			stop(cstr)
		}
	}

	xx <- mplus.get.bayesian.plausible.data(file,paramidx,obs)

	xxmax <- max(xx)
	xxmin <- min(xx)
#	print(xxmax)
#	print(xxmin)

	if (obs == 0) {
		cstr <- paste(c("Overall distribution of"),labels[paramidx])
	} else {
		cstr <- sprintf("Distribution of %s for Individual %d", labels[paramidx], obs)
	}
	h <- hist(xx,breaks=seq(min(xx),max(xx),length=bins+1),col="red",main=cstr,xlab='Estimate',ylab='Count')

	xxmode <- h$mids[h$counts == max(h$counts)]
	xxmean <- mean(xx)
	xxsd <- sd(xx)
	xxmedian <- median(xx)

	left <- quantile(xx, 0.025,type=3)
	right <- quantile(xx, 0.975,type=3)
	
	abline(v=xxmode,untf=FALSE,col='green')
	abline(v=xxmean,untf=FALSE,col='brown')
	abline(v=xxmedian,untf=FALSE,col='purple')
	abline(v=left,untf=FALSE,col='blue')
	abline(v=right,untf=FALSE,col='blue')

	modestr <- sprintf("Mode = %0.5f", xxmode)
	meanstr <- sprintf("Mean = %0.5f, Std Dev = %0.5f", xxmean, xxsd)
	medianstr <- sprintf("Median = %0.5f", xxmedian)
	lowci <- sprintf("95%% Lower CI = %0.5f", left)
	uppci <- sprintf("95%% Upper CI = %0.5f", right)
	ldesc <- c(meanstr, medianstr, modestr, lowci, uppci)

	lcol <- c('brown','purple','green','blue','blue')
	legend("topleft",ldesc,col=lcol,lty=c(1,1,1,1,1),lwd=c(2.5,2.5,2.5,2.5,2.5))

	invisible(xx)
}

######################################################################################################
# Functions for LOOP PLOT
######################################################################################################

#========================================================================
#
# mplus.list.loop.labels - list the loop variables
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.loop.labels('ex8.1.gh5')
#
mplus.list.loop.labels <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if loop data exists
	if ( !("loop_data" %in% names(gh5)) ) {
		stop("mplus.list.loop.labels requires loop data.\n\nUse TYPE=PLOT2 and the PLOT/LOOP keywords in MODEL CONSTRAINT or use the MOD keyword in MODEL INDIRECT.")
	}

	cat(c("\nList of loop labels to use in the following functions:\n"))
	cat(c(" - mplus.plot.loop\n"))
	cat(c(" - mplus.get.loop.estimates\n"))
	cat(c(" - mplus.get.loop.lowerci\n"))
	cat(c(" - mplus.get.loop.upperci\n"))
	cat(c(" - mplus.get.loop.xvalues\n"))

	cat(c("\nLoop labels:\n"))

	# get the parameter statements from loop_data and lookup the indices
	statements <- mplus.get.group.attribute(file, 'loop_data', 'labels')
	statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

	nplaus <- length(statements)
	for (i in c(1:nplaus)) {
		cstr <- sprintf("[%d] %s", i, statements[i])
		cat(cstr,sep="\n")
	}
#	cat(statements,sep="\n")
	invisible(statements)
}



#========================================================================
#
# mplus.get.loop.estimates - get the estimates for the given loop label
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	loopstr - the quoted loop label
#
# eg. mplus.get.loop.estimates('ex8.1.gh5','indirect')
#
mplus.get.loop.estimates <- function(file,loopstr=1) {
	if (missing(file)) {
		stop("- - name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste(" - file does not exist:",file)
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if loop data exists
	if ( !("loop_data" %in% names(gh5)) ) {
		stop(" - requires loop data\n\nUse TYPE=PLOT2 and the PLOT/LOOP keywords in MODEL CONSTRAINT.")
	}

	if (missing(loopstr)) {
		loopstr=1
	}

	if (is.character(loopstr)) {
		labels <- mplus.get.group.attribute(file,'loop_data','labels')
		labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)
        labels <- tolower(labels)
		loopstr <- toupper(loopstr)
		loopidx <- pmatch(loopstr, labels, nomatch=0)

		if (loopidx == 0) {
			cstr <- paste(c("- unknown loop label:"),loopstr,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of the estimates dataset
		# first dimension is the number of loop labels
		# second dimension is the number of x points
		dims <- attr(gh5$loop_data$estimates,'dim')

		loopidx <- loopstr
		if (loopidx <= 0 || loopidx > dims[1]) {
			cstr <- paste(" - loop index is out of range: ",loopidx,"\n\nUse mplus.list.loop.labels to see the list of loop labels.\n")
			stop(cstr)
		}
	}

	gh5$loop_data$estimates[loopidx,]
}



#========================================================================
#
# mplus.get.loop.lowerci - get the lower CI values for the given loop label
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	loopstr - the quoted loop label
#
# eg. mplus.get.loop.lowerci('ex8.1.gh5','indirect')
#
mplus.get.loop.lowerci <- function(file,loopstr=1) {
	if (missing(file)) {
		stop("- - name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste(" - file does not exist:",file)
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if loop data exists
	if ( !("loop_data" %in% names(gh5)) ) {
		stop(" - requires loop data\n\nUse TYPE=PLOT2 and the PLOT/LOOP keywords in MODEL CONSTRAINT.")
	}

	if (missing(loopstr)) {
		loopstr=1
	}

	if (is.character(loopstr)) {
		labels <- mplus.get.group.attribute(file,'loop_data','labels')
		labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)
        labels <- tolower(labels)
		loopstr <- tolower(loopstr)
		loopidx <- pmatch(loopstr, labels, nomatch=0)

		if (loopidx == 0) {
			cstr <- paste(c("- unknown loop label:"),loopstr,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of the estimates dataset
		# first dimension is the number of loop labels
		# second dimension is the number of x points
		dims <- attr(gh5$loop_data$estimates,'dim')

		loopidx <- loopstr
		if (loopidx <= 0 || loopidx > dims[1]) {
			cstr <- paste(" - loop index is out of range: ",loopidx,"\n\nUse mplus.list.loop.labels to see the list of loop labels.\n")
			stop(cstr)
		}
	}

	gh5$loop_data$lowerci[loopidx,]
}



#========================================================================
#
# mplus.get.loop.upperci - get the upper CI values for the given loop label
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	loopstr - the quoted loop label
#
# eg. mplus.get.loop.upperci('ex8.1.gh5','indirect')
#
mplus.get.loop.upperci <- function(file,loopstr=1) {
	if (missing(file)) {
		stop("- - name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste(" - file does not exist:",file)
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if loop data exists
	if ( !("loop_data" %in% names(gh5)) ) {
		stop(" - requires loop data\n\nUse TYPE=PLOT2 and the PLOT/LOOP keywords in MODEL CONSTRAINT.")
	}

	if (missing(loopstr)) {
		loopstr=1
	}

	if (is.character(loopstr)) {
		labels <- mplus.get.group.attribute(file,'loop_data','labels')
		labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)
        labels <- tolower(labels)
		loopstr <- tolower(loopstr)
		loopidx <- pmatch(loopstr, labels, nomatch=0)

		if (loopidx == 0) {
			cstr <- paste(c("- unknown loop label:"),loopstr,"\n")
			stop(cstr)
		}
	} else {
		# get the dimensions of the estimates dataset
		# first dimension is the number of loop labels
		# second dimension is the number of x points
		dims <- attr(gh5$loop_data$estimates,'dim')

		loopidx <- loopstr
		if (loopidx <= 0 || loopidx > dims[1]) {
			cstr <- paste(" - loop index is out of range: ",loopidx,"\n\nUse mplus.list.loop.labels to see the list of loop labels.\n")
			stop(cstr)
		}
	}

	gh5$loop_data$upperci[loopidx,]
}



#========================================================================
#
# mplus.get.loop.xvalues - get the x points for the loop plots
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.get.loop.xvalues('ex8.1.gh5')
#
mplus.get.loop.xvalues <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if loop data exists
	if ( !("loop_data" %in% names(gh5)) ) {
		stop("mplus.get.loop.upperci requires loop data.\n\nUse TYPE=PLOT2 and the PLOT/LOOP keywords in MODEL CONSTRAINT.")
	}

	gh5$loop_data$xvalues
}



#========================================================================
#
# mplus.plot.loop - plot the loop label
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	loopvar - the index of the loop label
#
# eg. mplus.plot.loop('ex8.1.gh5',1)
#
mplus.plot.loop <- function(file,loopstr=1) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if loop data exists
	if ( !("loop_data" %in% names(gh5)) ) {
		stop("requires loop data.\n\nUse TYPE=PLOT2 and the PLOT/LOOP keywords in MODEL CONSTRAINT.")
	}

	if (missing(loopstr)) {
		loopstr=1
	}

	# get the dimensions of the estimates dataset
	# first dimension is the number of loop labels
	# second dimension is the number of x points
	props <- mplus.get.group.attribute(file,'loop_data','properties')

	if (is.character(loopstr)) {
		# get the parameter statements from loop_data and lookup the indices
		labels <- mplus.get.group.attribute(file, 'loop_data', 'labels')
		labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)
        labels <- tolower(labels)
		loopstr <- tolower(loopstr)
		loopidx <- pmatch(loopstr, labels, nomatch=0)

		if (loopidx == 0) {
			cstr <- paste(c("Unknown loop label:"),loopstr,"\n")
			stop(cstr)
		}
	} else {
		if (loopstr <= 0 || loopstr > props[1]) {
			cstr <- paste("Loop index is out of range: ",loopvar,"\n\nUse mplus.list.loop.labels to see the list of loop labels.\n")
			stop(cstr)
		}
		loopidx <- loopstr
	}

	labels <- mplus.get.group.attribute(file,'loop_data','labels')
	labels <- gsub("(^\\s+|\\s+$)", "", labels, perl=TRUE)

	loopvar <- mplus.get.group.attribute(file,'loop_data','loop_variable')
	loopvar <- gsub("(^\\s+|\\s+$)", "", loopvar, perl=TRUE)

	xx <- array(0,c(3,props[2]))
	xx[1,] <- mplus.get.loop.xvalues(file)
	xx[2,] <- mplus.get.loop.xvalues(file)
	xx[3,] <- mplus.get.loop.xvalues(file)

	yy <- array(0,c(3,props[2]))
	yy[1,] <- mplus.get.loop.estimates(file,loopidx)
	yy[2,] <- mplus.get.loop.lowerci(file,loopidx)
	yy[3,] <- mplus.get.loop.upperci(file,loopidx)

	# plot the loop
	cstr <- paste("Loop plot for",labels[loopidx])
	plot(xx,yy,xlab=loopvar,ylab=labels[loopidx],main=cstr,type='n')

	lines(xx[1,],yy[1,],col='red')
	lines(xx[2,],yy[2,],col='blue')
	lines(xx[3,],yy[3,],col='blue')

#	abline(v=0,untf=FALSE,col='black')
#	abline(h=0,untf=FALSE,col='black')
	grid(NULL, NULL, lty=6, col='cornsilk2')
}



######################################################################################################
# Functions for IRT plots
######################################################################################################

#========================================================================
# mplus.list.irt.variables
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.irt.variables('ex7.27.gh5')
#
mplus.list.irt.variables <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("IRT data is required.\n\nUse TYPE=PLOT2.")
	}

	cat(c("\nList of variables to use in the following functions:\n"))
	cat(c(" - mplus.compute.irt.icc\n"))
	cat(c(" - mplus.plot.irt.icc\n"))

	cat(c("\nVariables for 'uvar' argument:\n"))

	ulabels <- mplus.get.group.attribute(file,'irt_data','ulabels')
	ulabels <- gsub("(^\\s+|\\s+$)", "", ulabels, perl=TRUE)

	nvar <- length(ulabels)
	for (i in c(1:nvar)) {
		cstr <- sprintf("[%d] %s", i, ulabels[i])
		cat(cstr,sep="\n")
	}
	invisible(ulabels)
}


#========================================================================
# mplus.list.irt.xvariables
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.irt.xvariables('ex7.27.gh5')
#
mplus.list.irt.xvariables <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("IRT data is required.\n\nUse TYPE=PLOT2.")
	}

	cat(c("\nList of variables to use in the following functions:\n"))
	cat(c(" - mplus.compute.irt.icc\n"))
	cat(c(" - mplus.plot.irt.icc\n"))

	cat(c("\nVariables for the 'xvar' argument:\n"))

	flabels <- mplus.get.group.attribute(file,'irt_data','flabels')
	flabels <- gsub("(^\\s+|\\s+$)", "", flabels, perl=TRUE)

	nvar <- length(flabels)
	for (i in c(1:nvar)) {
		cstr <- sprintf("[%d] %s", i, flabels[i])
		cat(cstr,sep="\n")
	}
	invisible(flabels)
}

#========================================================================
# mplus.compute.irt.icc
#
# arguments:
#	file - the quoted name of an existing GH5 file (required)
#	group - the group number (required)
#	xvar - the variable for the x-axis, can be the variable index or quoted variable name (required)
#	uvar - the indicator variable, can be the variable index or the quoted variable name (required)
#	cat - the category number (required)
#	xvector -> the vector containing x values to use (required)
#	covariates -> the vector containing values for all the other covariates (not required, sample mean used if not given)
#
# eg. mplus.compute.irt.icc('ex7.27.gh5',1,'F','U1',1,seq(-3,3,0.2))
#
mplus.compute.irt.icc <- function(file,group,xvar,uvar,cat,xvector,covariates) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("IRT data is required.\n\nUse TYPE=PLOT2.")
	}

	#	properties[1] - number of factors
	#	properties[2] - number of factors/covariates
	#	properties[3] - number of indicators
	#	properties[4] - number of classes
	#	properties[5] - maximum number of categories 
	props <- mplus.get.group.attribute(file,'irt_data','properties')

	num_fx <- as.integer(props[2])
	num_r <- as.integer(props[3])
	max_num_cat <- as.integer(props[5])

	flabels <- mplus.get.group.attribute(file,'irt_data','flabels')
	flabels <- gsub("(^\\s+|\\s+$)", "", flabels, perl=TRUE)

	ulabels <- mplus.get.group.attribute(file,'irt_data','ulabels')
	ulabels <- gsub("(^\\s+|\\s+$)", "", ulabels, perl=TRUE)

	if (missing(xvar)) {
		stop("The x-axis variable (xvar) is required.")
	} else {
		if (is.character(xvar)) {
			xvar <- toupper(xvar)
			index <- pmatch(xvar, flabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown x-variable:  %s\n", xvar)
				stop(cstr)
			}
			fidx = index
		} else {
			if (xvar <= 0 || xvar > num_fx) {
				stop("The index for the x-variable (xvar) is out of range.")
			}
			fidx = xvar
		}
	}
	if (missing(uvar)) {
		stop("The indicator variable (uvar) is required.")
	} else {
		if (is.character(uvar)) {
			uvar <- toupper(uvar)
			index <- pmatch(uvar, ulabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown indicator:  %s\n", uvar)
				stop(cstr)
			}
			ridx = index
		} else {
			if (uvar <= 0 || uvar > num_r) {
				stop("The index for the indicator (uvar) is out of range.")
			}
			ridx = uvar
		}
	}
	if (missing(group)) {
		stop("The group index (group) is required.")
	} else {
		if (group <= 0 || group > props[4]) {
			stop("The group index (group) is out of range.")
		}
	}
	if (missing(xvector)) {
		stop("The vector (xvector) containing values for the x-axis is required.")
	}
	if (missing(covariates)) {
		means <- mplus.get.group.dataset(file,'irt_data','mean')
		covariates <- means[,group]
	} else {
		if (length(covariates) != num_fx) {
			cstr <- sprintf("The length of the covariates vector should be %d.\nFound: %d", num_fx, length(covariates))
			stop(cstr)
		}
	}

	links <- mplus.get.group.attribute(file,'categorical_data','link')
	shift <- 0.0
	for (i in c(1:num_fx)) {
		if (i != fidx) {
			shift <- shift + covariates[i]*gh5$irt_data$loading[ridx,i,group]
		}
	}

	prob <- array(0,c(length(xvector)))
	for (i in c(1:length(xvector))) {
		x <- xvector[i]
		if (cat == 1) {
			p <- gh5$irt_data$tau[cat,ridx,group] - shift - x * gh5$irt_data$loading[fidx,ridx,group]
			p <- p * gh5$irt_data$scale[ridx,group]
			prob[i] <- lin(p,links[ridx])
		} else if (cat == max_num_cat) {
			p = gh5$irt_data$tau[cat-1,ridx,group] - shift - x * gh5$irt_data$loading[fidx,ridx,group]
			p = p * gh5$irt_data$scale[ridx,group]
			prob[i] = 1.0 - lin(p,links[ridx])
		} else {
			p = gh5$irt_data$tau[cat,ridx,group] - shift - x * gh5$irt_data$loading[fidx,ridx,group]
			p = p * gh5$irt_data$scale[ridx,group]

			p2 = gh5$irt_data$tau[cat-1,ridx,group] - shift - x * gh5$irt_data$loading[fidx,ridx,group]
			p2 = p2 * gh5$irt_data$scale[ridx,group]

			prob[i] = lin(p,links[ridx]) - lin(p2,links[ridx])
		}
	}

	prob
}


#========================================================================
# mplus.compute.irt.iic
#
# arguments:
#	file - the quoted name of an existing GH5 file (required)
#	group - the group number (required)
#	xvar - the variable for the x-axis, can be the variable index or quoted variable name (required)
#	uvar - the indicator variable, can be the variable index or the quoted variable name (required)
#	xvector -> the vector containing x values to use (required)
#	covariates -> the vector containing values for all the other covariates (not required, sample mean used if not given)
#
# eg. mplus.compute.irt.iic('ex7.27.gh5',1,'F','U1',seq(-3,3,0.2))
#
mplus.compute.irt.iic <- function(file,group,xvar,uvar,xvector,covariates) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("IRT data is required.\n\nUse TYPE=PLOT2.")
	}

	#	properties[1] - number of factors
	#	properties[2] - number of factors/covariates
	#	properties[3] - number of indicators
	#	properties[4] - number of classes
	#	properties[5] - maximum number of categories 
	props <- mplus.get.group.attribute(file,'irt_data','properties')

	num_fx <- as.integer(props[2])
	num_r <- as.integer(props[3])
	max_num_cat <- as.integer(props[5])

	if (missing(xvar)) {
		stop("The x-axis variable (xvar) is required.")
	} else {
		if (is.character(xvar)) {
			index <- pmatch(xvar, flabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown x-variable:  %s\n", xvar)
				stop(cstr)
			}
			fidx = index
		} else {
			if (xvar <= 0 || xvar > num_fx) {
				stop("The index for the x-variable (xvar) is out of range.")
			}
			fidx = xvar
		}
	}
	if (missing(uvar)) {
		stop("The indicator variable (uvar) is required.")
	} else {
		if (is.character(uvar)) {
			index <- pmatch(uvar, ulabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown indicator:  %s\n", uvar)
				stop(cstr)
			}
			ridx = index
		} else {
			if (uvar <= 0 || uvar > num_r) {
				stop("The index for the indicator (uvar) is out of range.")
			}
			ridx = uvar
		}
	}
	if (missing(group)) {
		stop("The group index (group) is required.")
	} else {
		if (group <= 0 || group > props[4]) {
			stop("The group index (group) is out of range.")
		}
	}
	if (missing(xvector)) {
		stop("The vector (xvector) containing values for the x-axis is required.")
	}
	if (missing(covariates)) {
		covariates <- mplus.get.group.dataset(file,'irt_data','mean')
	} else {
		if (length(covariates) != num_fx) {
			cstr <- sprintf("The length of the covariates vector should be %d.\nFound: %d", num_fx, length(covariates))
			stop(cstr)
		}
	}

	categories <- mplus.get.group.attribute(file,'irt_data','categories')
	links <- mplus.get.group.attribute(file,'categorical_data','link')

	shift <- 0.0
	for (i in c(1:num_fx)) {
		if (i != fidx) {
			shift <- shift + covariates[i]*gh5$irt_data$loading[ridx,i,group]
		}
	}

	categories <- as.numeric(categories)

	probvec <- array(0, c(length(xvector),categories[ridx]+1))
	for (i in c(1:length(xvector))) {
		x <- xvector[i]
		probvec[1] <- 0
		for (j in c(2:c(categories[ridx]))) {
			fp = gh5$irt_data$tau[j-1,ridx,group] - shift - x * gh5$irt_data$loading[fidx,ridx,group]
			fp = fp * gh5$irt_data$scale[ridx,group]
			dp = lin(fp,links[ridx])
			probvec[i,j] <- dp
		}
		probvec[i,categories[ridx]+1]=1.0
	}

	prob <- array(0,c(length(xvector)))
	for (i in c(1:length(xvector))) {
		x <- xvector[i]
		for (j in c(2:c(categories[ridx]+1))) {
			r <- 10**(-10)
			ep = probvec[i,j] - probvec[i,j-1]
			if (ep < r) { ep <- r }
			dp = gh5$irt_data$scale[ridx,group] * gh5$irt_data$loading[fidx,ridx,group] * gh5$irt_data$scale[ridx,group] * gh5$irt_data$loading[fidx,ridx,group];
			p = (probvec[i,j] * (1-probvec[i,j])) - (probvec[i,j-1] * (1-probvec[i,j-1]))
			prob[i] <- prob[i] + p * p * dp / ep
		}
	}

	prob
}

#========================================================================
# mplus.plot.irt.icc
#
# arguments:
#	file - the quoted name of an existing GH5 file (required)
#	group - the group number (not required) -- 1 if not specified
#	xvar - the variable for the x-axis, can be the variable index or quoted variable name (not required, uses the first x)
#	uvar - the indicator variable or vector containing more than one indicator variable
#		 - can be the variable index or the quoted variable name
#		 - if not given, assume all indicator variables but cat must be given (not required)
#	cat - the category number
#		- if not given, assume all categories for the given indicator variables
#		- required if uvar not given
#	cat2 - the second category number if range of categories is desired (not required)
#	covariates -> the vector containing values for all the other covariates (not required, sample mean used if not given)
#	xrange - the type of range for the x-axis (not required)
#		- xrange=1: -1 s.d to +1 s.d of xvar
#		- xrange=2: -2 s.d to +2 s.d of xvar
#		- xrange=3: -3 s.d to +3 s.d of xvar (default)
#		- xrange=4: -4 s.d to +4 s.d of xvar
#		- xrange=5: -5 s.d to +5 s.d of xvar
#		- xrange=6: -6 s.d to +6 s.d of xvar
#	xstep - the step increment for the x-axis range (not required)
#		- xstep=1: 1.0
#		- xstep=2: 0.5
#		- xstep=3: 0.1
#		- xstep=4: 0.05
#		- xstep=5: 1/2 s.d of xvar
#		- xstep=6: 1/4 s.d of xvar
#		- xstep=7: 1/5 s.d of xvar (default)
#		- xstep=8: 1/10 s.d of xvar
#		- xstep=9: 1/20 s.d of xvar
#		- xstep=10: 1/50 s.d of xvar
#		- xstep=11: 1/100 s.d of xvar
#
# eg. mplus.plot.irt.icc('ex7.27.gh5',1,'F','U1',)
#
mplus.plot.irt.icc <- function(file,group=1,xvar=1,uvar,cat,cat2,covariates,xrange=3,xstep=7,lloc="top") {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("This function requires IRT data.\n\nUse TYPE=PLOT2.")
	}

	#	properties[1] - number of factors
	#	properties[2] - number of factors/covariates
	#	properties[3] - number of indicators
	#	properties[4] - number of classes
	#	properties[5] - maximum number of categories 
	props <- mplus.get.group.attribute(file,'irt_data','properties')

	flabels <- mplus.get.group.attribute(file,'irt_data','flabels')
	flabels <- gsub("(^\\s+|\\s+$)", "", flabels, perl=TRUE)
	ulabels <- mplus.get.group.attribute(file,'irt_data','ulabels')
	ulabels <- gsub("(^\\s+|\\s+$)", "", ulabels, perl=TRUE)

	num_fx <- as.integer(props[2])
	num_r <- as.integer(props[3])
	max_num_cat <- as.integer(props[5])

	if (is.character(xvar)) {
		xvar <- toupper(xvar)
		index <- pmatch(xvar, flabels, nomatch=0)
		if (index == 0) {
			cstr <- sprintf("Unknown variable for the x-axis:  %s\n", xvar)
			stop(cstr)
		}
		fidx = index
	} else {
		if (xvar <= 0 || xvar > num_fx) {
			stop("The index for the x-variable (xvar) is out of range.")
		}
		fidx = xvar
	}
	if (missing(uvar)) {
	} else if (length(uvar) > 1) {
		ridx <- vector()
		for (r in c(1:length(uvar))) {
			var <- uvar[r]
			if (is.character(var)) {
				var <- toupper(var)
				index <- pmatch(var, ulabels, nomatch=0)
				if (index == 0) {
					cstr <- sprintf("Unknown indicator:  %s\n", var)
					stop(cstr)
				}
				ridx[r] = index
			} else {
				if (var <= 0 || var > num_r) {
					stop("The index for the indicator in uvar is out of range.")
				}
				ridx[r] = var
			}
		}
	} else {
		if (is.character(uvar)) {
			uvar <- toupper(uvar)
			index <- pmatch(uvar, ulabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown indicator:  %s\n", uvar)
				stop(cstr)
			}
			ridx <- index
		} else {
			if (uvar <= 0 || uvar > num_r) {
				stop("The index for the indicator (uvar) is out of range.")
			}
			ridx <- uvar
		}
	}
	if (group <= 0 || group > props[4]) {
		stop("The group index (group) is out of range.")
	}
	if (missing(covariates)) {
		xmean <- mplus.get.group.dataset(file,'irt_data','mean')
		covariates <- xmean[,group]
	} else {
		if (length(covariates) != num_fx) {
			cstr <- sprintf("The length of the covariates vector should be %d.", num_fx)
			stop(cstr)
		}
	}

	categories <- mplus.get.group.attribute(file,'irt_data','categories')
	if (missing(uvar)) {
		# case 1: uvar not specified, we plot ICC for all variables.  The category number must be given.
		if (missing(cat)) {
			stop("The category number (cat) is required when plotting ICCs for all variables.")
		}
		for (i in c(1:num_r)) {
			if (cat <= 0 || cat > categories[i]) {
				cstr <- sprintf("The category number (cat) is out of range for variable %s.", ulabels[i])
				stop(cstr)
			}
		}
		if (!(missing(cat2))) {
			if (cat > cat2) {
				cstr <- sprintf("The first category number (cat2=%d) must be smaller than the second category number (cat2=%d).", cat, cat2)
				stop(cstr)
			}
			for (i in c(1:num_r)) {
				if (cat2 <= 0 || cat2 > categories[i]) {
					cstr <- sprintf("The second category number (cat2) is out of range for variable %s.", ulabels[i])
					stop(cstr)
				}
			}
		}
	} else if (length(uvar) > 1) {
		for (r in c(1:length(ridx))) {
			if (!(missing(cat))) {
				if (cat <= 0 || cat > categories[ridx[r]]) {
					cstr <- sprintf("The category (cat) is out of range for variable %s.", ulabels[ridx[r]])
					stop(cstr)
				}
			} else {
				# cat is missing but cat2 isn't!
				if (!(missing(cat2))) {
					stop("The first category (cat) is required if the second category (cat2) is given.")
				}
			}
			if (!(missing(cat2))) {
				if (cat2 <= 0 || cat2 > categories[ridx[r]]) {
					cstr <- sprintf("The category (cat2) is out of range for variable %s.", ulabels[ridx[r]])
					stop(cstr)
				}
				if (cat > cat2) {
					cstr <- sprintf("The first category (cat2=%d) must be smaller than the second category (cat2=%d).", cat, cat2)
					stop(cstr)
				}
			}
		}
	} else {
		if (!(missing(cat))) {
			if (cat <= 0 || cat > categories[ridx]) {
				cstr <- sprintf("The category (cat) is out of range for variable %s.", ulabels[ridx])
				stop(cstr)
			}
		} else {
			# cat is missing but cat2 isn't!
			if (!(missing(cat2))) {
				stop("The first category (cat) is required if the second category (cat2) is given.")
			}
		}
		if (!(missing(cat2))) {
			if (cat2 <= 0 || cat2 > categories[ridx]) {
				cstr <- sprintf("The category (cat2) is out of range for variable %s.", ulabels[ridx])
				stop(cstr)
			}
			if (cat > cat2) {
				cstr <- sprintf("The first category (cat2=%d) must be smaller than the second category (cat2=%d).", cat, cat2)
				stop(cstr)
			}
		}
	}
	if (!(missing(xrange))) {
		if (xrange <= 0 || xrange > 6) {
			stop("The xrange type should be between 1 and 6.")
		}
	}
	if (!(missing(xstep))) {
		if (xstep <= 0 || xstep > 11) {
			stop("The xstep type should be between 1 and 11.")
		}
	}

	variances <- mplus.get.group.dataset(file,'irt_data','variance')
	means <- mplus.get.group.dataset(file,'irt_data','mean')
	fsd = sqrt(variances[fidx])

	xmult <- switch(xrange, 1, 2, 3, 4, 5, 6)
	vmin = means[fidx] + (-1) * xmult * fsd
	vmax = means[fidx] + xmult * fsd

	vstep = switch(xstep, 1.0, 0.5, 0.1, 0.05, 0.5*fsd, 0.25*fsd, 0.2*fsd, 0.1*fsd, 0.05*fsd, 0.02*fsd, 0.01*fsd)
	steps <- seq(vmin,vmax,by=vstep)

    print(steps)

	# if cat is missing, then we plot all categories
	if (missing(uvar)) {
		prob <- array(0,c(num_r,length(steps)))
		xx <- array(0,c(num_r,length(steps)))
		if (missing(cat2)) {
			for (r in c(1:num_r)) {
				prob[r,] <- mplus.compute.irt.icc(file,group,fidx,r,cat,xvector=steps,covariates=covariates)
				xx[r,] <- steps
			}
		} else {
			for (r in c(1:num_r)) {
				for (c in c(cat:cat2)) {
					prob[r,] <- prob[r,] + mplus.compute.irt.icc(file,group,fidx,r,c,xvector=steps,covariates=covariates)
				}
				xx[r,] <- steps
			}
		}

		# plot the icc
		cstr <- sprintf("Item characteristic curves as a function of %s, Class %d", flabels[fidx], group)
		colors <- rainbow(num_r)
		plot(xx,prob,xlab=flabels[fidx],ylab="Probability",main=cstr,type='n')
		for (i in c(1:num_r)) {
			lines(xx[i,],prob[i,],col=colors[i])
		}

		ldesc <- array(0,c(num_r))
		lty <- array(0,c(num_r))
		lwd <- array(0,c(num_r))
		for (i in c(1:num_r)) {
			if (missing(cat2)) {
				ldesc[i] <- sprintf("%s, Category %d", ulabels[i], cat)
			} else {
				ldesc[i] <- sprintf("%s, Cat %d to %d", ulabels[i], cat, cat2)
			}
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend(lloc,ldesc,col=colors,lty=lty,lwd=lwd)
	} else if (length(ridx) > 1) {
		prob <- array(0,c(length(ridx),length(steps)))
		xx <- array(0,c(length(ridx),length(steps)))
		if (missing(cat)) {
			for (j in c(1:categories[ridx])) {
				prob[j,] <- mplus.compute.irt.icc(file,group,fidx,ridx,j,xvector=steps,covariates=covariates)
				xx[j,] <- steps
			}
		} else if (missing(cat2)) {
			for (r in c(1:length(ridx))) {
				prob[r,] <- mplus.compute.irt.icc(file,group,fidx,ridx[r],cat,xvector=steps,covariates=covariates)
				xx[r,] <- steps
			}
		} else {
			for (r in c(1:length(ridx))) {
				for (c in c(cat:cat2)) {
					prob[r,] <- prob[r,] + mplus.compute.irt.icc(file,group,fidx,ridx[r],c,xvector=steps,covariates=covariates)
				}
				xx[r,] <- steps
			}
		}

		# plot the icc
		cstr <- sprintf("Item characteristic curves as a function of %s, Class %d", flabels[fidx], group)
		colors <- rainbow(length(ridx))
		plot(xx,prob,xlab=flabels[fidx],ylab="Probability",main=cstr,type='n')
		for (i in c(1:length(ridx))) {
			lines(xx[i,],prob[i,],col=colors[i])
		}

		ldesc <- array(0,c(length(ridx)))
		lty <- array(0,c(length(ridx)))
		lwd <- array(0,c(length(ridx)))
		for (i in c(1:length(ridx))) {
			if (missing(cat2)) {
				ldesc[i] <- sprintf("%s, Category %d", ulabels[ridx[i]], cat)
			} else {
				ldesc[i] <- sprintf("%s, Cat %d to %d", ulabels[ridx[i]], cat, cat2)
			}
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend(lloc,ldesc,col=colors,lty=lty,lwd=lwd)
	} else if (missing(cat)) {
		prob <- array(0,c(categories[ridx],length(steps)))
		xx <- array(0,c(categories[ridx],length(steps)))
		for (j in c(1:categories[ridx])) {
			prob[j,] <- mplus.compute.irt.icc(file,group,fidx,ridx,j,steps,covariates)
			xx[j,] <- steps
		}

		# plot the icc
		cstr <- sprintf("Item characteristic curve for %s (all categories)\n as a function of %s, Class %d", ulabels[ridx], flabels[fidx], group)
		colors <- rainbow(categories[ridx])
		plot(xx,prob,xlab=flabels[fidx],ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		for (i in c(1:categories[ridx])) {
			lines(xx[i,],prob[i,],col=colors[i])
		}

		ldesc <- vector()
		for (i in c(1:categories[ridx])) {
			ldesc[i] <- sprintf("%s, Category %d", ulabels[ridx], i)
		}

		legend(lloc,ldesc,col=colors,lty=c(1,1,1,1,1),lwd=c(2.5,2.5,2.5,2.5,2.5))
	} else if (missing(cat2)) {
		# if cat2 is missing, then we plot only the given category

		prob <- mplus.compute.irt.icc(file,group,fidx,ridx,cat,steps,covariates)

		# plot the icc
		cstr <- sprintf("Item characteristic curve for %s (category %d)\n as a function of %s, Class %d", ulabels[ridx], cat, flabels[fidx], group)
		plot(steps,prob,xlab=flabels[fidx],ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(steps,prob,col='red')
	} else {
		# if cat and cat2 are given, then we plot the sum from cat to cat2

		prob <- array(0,c(length(steps)))
		for (c in c(cat:cat2)) {
			prob <- prob + mplus.compute.irt.icc(file,group,fidx,ridx,c,steps,covariates)
		}

		# plot the icc
		cstr <- sprintf("Item characteristic curve for %s\n(sum from category %d to category %d)\nas a function of %s, Class %d", ulabels[ridx], cat, cat2, flabels[fidx], group)

		plot(steps,prob,xlab=flabels[fidx],ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(steps,prob,col='red')
	}

    steps
}


#========================================================================
# mplus.plot.irt.iic
#
# arguments:
#	file - the quoted name of an existing GH5 file (required)
#	group - the group number (not required)
#		 - if not given, group=1 will be used
#	xvar - the variable for the x-axis, can be the variable index or quoted variable name (not required, uses the first x)
#	uvar - the indicator variable or vector containing more than one indicator variable
#		 - can be the variable index or the quoted variable name
#		 - if not given, assume all indicator variables (not required)
#	covariates -> the vector containing values for all the other covariates (not required, sample mean used if not given)
#	xrange - the type of range for the x-axis (not required)
#		- xrange=1: -1 s.d to +1 s.d of xvar
#		- xrange=2: -2 s.d to +2 s.d of xvar
#		- xrange=3: -3 s.d to +3 s.d of xvar (default)
#		- xrange=4: -4 s.d to +4 s.d of xvar
#		- xrange=5: -5 s.d to +5 s.d of xvar
#		- xrange=6: -6 s.d to +6 s.d of xvar
#	xstep - the step increment for the x-axis range (not required)
#		- xstep=1: 1.0
#		- xstep=2: 0.5
#		- xstep=3: 0.1
#		- xstep=4: 0.05
#		- xstep=5: 1/2 s.d of xvar
#		- xstep=6: 1/4 s.d of xvar
#		- xstep=7: 1/5 s.d of xvar (default)
#		- xstep=8: 1/10 s.d of xvar
#		- xstep=9: 1/20 s.d of xvar
#		- xstep=10: 1/50 s.d of xvar
#		- xstep=11: 1/100 s.d of xvar
#
# eg. mplus.plot.irt.iic('ex7.27.gh5',1,'F','U1',)
#
mplus.plot.irt.iic <- function(file,group=1,xvar=1,uvar,covariates,xrange=3,xstep=7,lloc="top") {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("This function requires IRT data.\n\nUse TYPE=PLOT2.")
	}

	#	properties[1] - number of factors
	#	properties[2] - number of factors/covariates
	#	properties[3] - number of indicators
	#	properties[4] - number of classes
	#	properties[5] - maximum number of categories 
	props <- mplus.get.group.attribute(file,'irt_data','properties')

	flabels <- mplus.get.group.attribute(file,'irt_data','flabels')
	flabels <- gsub("(^\\s+|\\s+$)", "", flabels, perl=TRUE)
	flabels <- tolower(flabels)
	ulabels <- mplus.get.group.attribute(file,'irt_data','ulabels')
	ulabels <- gsub("(^\\s+|\\s+$)", "", ulabels, perl=TRUE)
	ulabels <- tolower(ulabels)

	num_fx <- as.integer(props[2])
	num_r <- as.integer(props[3])
	max_num_cat <- as.integer(props[5])

	if (is.character(xvar)) {
		xvar <- tolower(xvar)
		index <- pmatch(xvar, flabels, nomatch=0)
		if (index == 0) {
			cstr <- sprintf("Unknown variable for the x-axis:  %s\n", xvar)
			stop(cstr)
		}
		fidx = index
	} else {
		if (xvar <= 0 || xvar > num_fx) {
			stop("The index for the x-variable (xvar) is out of range.")
		}
		fidx = xvar
	}
	if (missing(uvar)) {
	} else if (length(uvar) > 1) {
		ridx <- vector()
		for (r in c(1:length(uvar))) {
			var <- uvar[r]
			if (is.character(var)) {
				index <- pmatch(var, ulabels, nomatch=0)
				if (index == 0) {
					cstr <- sprintf("Unknown indicator:  %s\n", var)
					stop(cstr)
				}
				ridx[r] = index
			} else {
				if (var <= 0 || var > num_r) {
					stop("The index for the indicator in uvar is out of range.")
				}
				ridx[r] = var
			}
		}
	} else {
		if (is.character(uvar)) {
			uvar <- tolower(uvar)
			index <- pmatch(uvar, ulabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown indicator:  %s\n", uvar)
				stop(cstr)
			}
			ridx = index
		} else {
			if (uvar <= 0 || uvar > num_r) {
				stop("The index for the indicator (uvar) is out of range.")
			}
			ridx = uvar
		}
	}
	if (group <= 0 || group > props[4]) {
		stop("The group index (group) is out of range.")
	}
	if (missing(covariates)) {
		xmean <- mplus.get.group.dataset(file,'irt_data','mean')
		covariates <- xmean[,group]
	} else {
		if (length(covariates) != num_fx) {
			cstr <- sprintf("The length of the covariates vector should be %d.", num_fx)
			stop(cstr)
		}
	}

	categories <- mplus.get.group.attribute(file,'irt_data','categories')
	if (!(missing(xrange))) {
		if (xrange <= 0 || xrange > 6) {
			stop("The xrange type should be between 1 and 6.")
		}
	}
	if (!(missing(xstep))) {
		if (xstep <= 0 || xstep > 11) {
			stop("The xstep type should be between 1 and 11.")
		}
	}

	variances <- mplus.get.group.dataset(file,'irt_data','variance')
	means <- mplus.get.group.dataset(file,'irt_data','mean')
	fsd = sqrt(variances[fidx])

	xmult <- switch(xrange, 1, 2, 3, 4, 5, 6)
	vmin = means[fidx] + (-1) * xmult * fsd
	vmax = means[fidx] + xmult * fsd

	vstep = switch(xstep, 1.0, 0.5, 0.1, 0.05, 0.5*fsd, 0.25*fsd, 0.2*fsd, 0.1*fsd, 0.05*fsd, 0.02*fsd, 0.01*fsd)
	steps <- seq(vmin,vmax,by=vstep)

	# if cat is missing, then we plot all categories
	if (missing(uvar)) {
		prob <- array(0,c(num_r,length(steps)))
		xx <- array(0,c(num_r,length(steps)))
		for (r in c(1:num_r)) {
			prob[r,] <- mplus.compute.irt.iic(file,group,fidx,r,xvector=steps,covariates=covariates)
			xx[r,] <- steps
		}

		# plot the iic
		cstr <- sprintf("Item information curves as a function of %s, Class %d", flabels[fidx], group)
		colors <- rainbow(num_r)
		plot(xx,prob,xlab=flabels[fidx],ylab="Information",main=cstr,type='n')
		for (i in c(1:num_r)) {
			lines(xx[i,],prob[i,],col=colors[i])
		}

		ldesc <- array(0,c(num_r))
		lty <- array(0,c(num_r))
		lwd <- array(0,c(num_r))
		for (i in c(1:num_r)) {
			ldesc[i] <- sprintf("%s", ulabels[i])
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend(lloc,ldesc,col=colors,lty=lty,lwd=lwd)
	} else if (length(ridx) > 1) {
		prob <- array(0,c(length(ridx),length(steps)))
		xx <- array(0,c(length(ridx),length(steps)))
		for (r in c(1:length(ridx))) {
			prob[r,] <- mplus.compute.irt.iic(file,group,fidx,ridx[r],xvector=steps,covariates=covariates)
			xx[r,] <- steps
		}

		# plot the iic
		cstr <- sprintf("Item information curves as a function of %s, Class %d", flabels[fidx], group)
		colors <- rainbow(length(ridx))
		plot(xx,prob,xlab=flabels[fidx],ylab="Information",main=cstr,type='n')
		for (i in c(1:length(ridx))) {
			lines(xx[i,],prob[i,],col=colors[i])
		}

#		for (i in c(1:length(steps))) {
#			cstr <- sprintf("x = %0.3f, probx = %0.3f", xx[1,i], prob[1,i])
#			print(cstr)
#		}

		ldesc <- array(0,c(length(ridx)))
		lty <- array(0,c(length(ridx)))
		lwd <- array(0,c(length(ridx)))
		for (i in c(1:length(ridx))) {
			ldesc[i] <- sprintf("%s", ulabels[ridx[i]])
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend(lloc,ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		prob <- mplus.compute.irt.iic(file,group,fidx,ridx,steps,covariates)

#		for (i in c(1:length(steps))) {
#			cstr <- sprintf("x = %0.3f, probx = %0.3f", steps[i], prob[i])
#			print(cstr)
#		}

		# plot the iic
		cstr <- sprintf("Item information curve for %s as a function of %s, Class %d", ulabels[ridx], flabels[fidx], group)
		plot(steps,prob,xlab=flabels[fidx],ylab="Information",main=cstr,type='n')
		lines(steps,prob,col='red')
	}
}


#========================================================================
# mplus.plot.irt.tic
#
# arguments:
#	file - the quoted name of an existing GH5 file (required)
#	group - the group number (not required)
#		 - if not given, group=1 will be shown
#	xvar - the variable for the x-axis, can be the variable index or quoted variable name (not required, uses the first x)
#	uvar - the indicator variable or vector containing more than one indicator variable
#		 - can be the variable index or the quoted variable name
#		 - if not given, assume all indicator variables (not required)
#	covariates -> the vector containing values for all the other covariates (not required, sample mean used if not given)
#	xrange - the type of range for the x-axis (not required)
#		- xrange=1: -1 s.d to +1 s.d of xvar
#		- xrange=2: -2 s.d to +2 s.d of xvar
#		- xrange=3: -3 s.d to +3 s.d of xvar (default)
#		- xrange=4: -4 s.d to +4 s.d of xvar
#		- xrange=5: -5 s.d to +5 s.d of xvar
#		- xrange=6: -6 s.d to +6 s.d of xvar
#	xstep - the step increment for the x-axis range (not required)
#		- xstep=1: 1.0
#		- xstep=2: 0.5
#		- xstep=3: 0.1
#		- xstep=4: 0.05
#		- xstep=5: 1/2 s.d of xvar
#		- xstep=6: 1/4 s.d of xvar
#		- xstep=7: 1/5 s.d of xvar (default)
#		- xstep=8: 1/10 s.d of xvar
#		- xstep=9: 1/20 s.d of xvar
#		- xstep=10: 1/50 s.d of xvar
#		- xstep=11: 1/100 s.d of xvar
#
# eg. mplus.plot.irt.tic('ex7.27.gh5',1,'F','U1',)
#
mplus.plot.irt.tic <- function(file,group=1,xvar=1,uvar,covariates,xrange=3,xstep=7) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if irt data exists
	if ( !("irt_data" %in% names(gh5)) ) {
		stop("This function requires IRT data.\n\nUse TYPE=PLOT2.")
	}

	#	properties[1] - number of factors
	#	properties[2] - number of factors/covariates
	#	properties[3] - number of indicators
	#	properties[4] - number of classes
	#	properties[5] - maximum number of categories 
	props <- mplus.get.group.attribute(file,'irt_data','properties')

	flabels <- mplus.get.group.attribute(file,'irt_data','flabels')
	flabels <- gsub("(^\\s+|\\s+$)", "", flabels, perl=TRUE)
	flabels <- tolower(flabels)
	ulabels <- mplus.get.group.attribute(file,'irt_data','ulabels')
	ulabels <- gsub("(^\\s+|\\s+$)", "", ulabels, perl=TRUE)
	ulabels <- tolower(ulabels)

	num_fx <- as.integer(props[2])
	num_r <- as.integer(props[3])
	max_num_cat <- as.integer(props[5])

	if (is.character(xvar)) {
		index <- pmatch(xvar, flabels, nomatch=0)
		if (index == 0) {
			cstr <- sprintf("Unknown variable for the x-axis:  %s\n", xvar)
			stop(cstr)
		}
		fidx = index
	} else {
		if (xvar <= 0 || xvar > num_fx) {
			stop("The index for the x-variable (xvar) is out of range.")
		}
		fidx = xvar
	}
	if (missing(uvar)) {
	} else if (length(uvar) > 1) {
		ridx <- vector()
		for (r in c(1:length(uvar))) {
			var <- uvar[r]
			if (is.character(var)) {
				index <- pmatch(var, ulabels, nomatch=0)
				if (index == 0) {
					cstr <- sprintf("Unknown indicator:  %s\n", var)
					stop(cstr)
				}
				ridx[r] = index
			} else {
				if (var <= 0 || var > num_r) {
					stop("The index for the indicator in uvar is out of range.")
				}
				ridx[r] = var
			}
		}
	} else {
		if (is.character(uvar)) {
			index <- pmatch(uvar, ulabels, nomatch=0)
			if (index == 0) {
				cstr <- sprintf("Unknown indicator:  %s\n", uvar)
				stop(cstr)
			}
			ridx = index
		} else {
			if (uvar <= 0 || uvar > num_r) {
				stop("The index for the indicator (uvar) is out of range.")
			}
			ridx = uvar
		}
	}
	if (group <= 0 || group > props[4]) {
		stop("The group index (group) is out of range.")
	}
	if (missing(covariates)) {
		xmean <- mplus.get.group.dataset(file,'irt_data','mean')
		covariates <- xmean[,group]
	} else {
		if (length(covariates) != num_fx) {
			cstr <- sprintf("The length of the covariates vector should be %d.", num_fx)
			stop(cstr)
		}
	}

	categories <- mplus.get.group.attribute(file,'irt_data','categories')
	if (!(missing(xrange))) {
		if (xrange <= 0 || xrange > 6) {
			stop("The xrange type should be between 1 and 6.")
		}
	}
	if (!(missing(xstep))) {
		if (xstep <= 0 || xstep > 11) {
			stop("The xstep type should be between 1 and 11.")
		}
	}

	variances <- mplus.get.group.dataset(file,'irt_data','variance')
	means <- mplus.get.group.dataset(file,'irt_data','mean')
	fsd = sqrt(variances[fidx])

	xmult <- switch(xrange, 1, 2, 3, 4, 5, 6)
	vmin = means[fidx] + (-1) * xmult * fsd
	vmax = means[fidx] + xmult * fsd

	vstep = switch(xstep, 1.0, 0.5, 0.1, 0.05, 0.5*fsd, 0.25*fsd, 0.2*fsd, 0.1*fsd, 0.05*fsd, 0.02*fsd, 0.01*fsd)
	steps <- seq(vmin,vmax,by=vstep)

	# if cat is missing, then we plot all categories
	if (missing(uvar)) {
		prob <- array(0,c(length(steps)))
		for (r in c(1:num_r)) {
			prob <- prob + mplus.compute.irt.iic(file,group,fidx,r,xvector=steps,covariates=covariates)
		}
        prob <- prob + 1 / gh5$irt_data$variance[fidx,group]
		# plot the tic
		cstr <- sprintf("Total information curve as a function of %s, Class %d", flabels[fidx], group)
		plot(steps,prob,xlab=flabels[fidx],ylab="Information",main=cstr,type='n')
		lines(steps,prob,col='red')
	} else if (length(ridx) > 1) {
		prob <- array(0,c(length(steps)))
		for (r in c(1:length(ridx))) {
			prob <- prob + mplus.compute.irt.iic(file,group,fidx,ridx[r],xvector=steps,covariates=covariates)
		}

		# plot the iic
		cstr <- sprintf("Partial total information curve as a function of %s, Class %d", flabels[fidx], group)
		plot(steps,prob,xlab=flabels[fidx],ylab="Information",main=cstr,type='n')
		lines(steps,prob,col='red')
	} else {
		prob <- mplus.compute.irt.iic(file,group,fidx,ridx,steps,covariates)

		# plot the tic
		cstr <- sprintf("Partial total information curve as a function of %s, Class %d", flabels[fidx], group)
		plot(steps,prob,xlab=flabels[fidx],ylab="Information",main=cstr,type='n')
		lines(steps,prob,col='red')
	}

#	for (i in c(1:length(steps))) {
#		cstr <- sprintf("x = %0.3f, probx = %0.5f", steps[i], prob[i])
#		print(cstr)
#	}
}



######################################################################################################
# Functions for Survival plots
######################################################################################################

#========================================================================
# mplus.list.survival.variables
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.survival.variables('ex6.21.gh5')
#
mplus.list.survival.variables <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

#	cat(c("\nList of variables to use in the following functions:\n"))
#	cat(c(" - mplus.compute.irt.icc\n"))
#	cat(c(" - mplus.plot.irt.icc\n"))

	cat(c("\nList of survival variables:\n"))

	for (i in c(1:props[1])) {
		cstr <- sprintf("survival_data/survival%d", i)
		label <- mplus.get.group.attribute(file,cstr,'label')
		cstr <- sprintf("%s", label)
		cstr <- gsub("(^\\s+|\\s+$)", "", cstr, perl=TRUE)
		print(cstr)
	}
}


#========================================================================
# mplus.get.survival.kaplanmeier.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	classnum - the group number (not required)
#
# eg. mplus.get.survival.kaplanmeier.values('ex6.21.gh5','T')
#
mplus.get.survival.kaplanmeier.values <- function(file,survvar,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	if (missing(classnum)) {
		datastr <- sprintf("kaplan_meier1")
	} else {
		classes <- mplus.get.group.dataset(file,'/','model_group_labels')
		dims <- attr(classes,'dim')
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}
		datastr <- sprintf("kaplan_meier%d", classnum)
	}
	kmvals <- mplus.get.group.dataset(file,groupstr,datastr)

	if (missing(time)) {
		return(kmvals[,2])
	} else {
		return(kmvals[,1])
	}
}



#========================================================================
# mplus.compute.survival.sample.logcumulative.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	classnum - the group number (not required)
#
# eg. mplus.compute.survival.sample.logcumulative.values('ex6.21.gh5','T')
#
mplus.compute.survival.sample.logcumulative.values <- function(file,survvar,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	if (missing(classnum)) {
		datastr <- sprintf("kaplan_meier1")
	} else {
		classes <- mplus.get.group.dataset(file,'/','model_group_labels')
		dims <- attr(classes,'dim')
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}
		datastr <- sprintf("kaplan_meier%d", classnum)
	}
	kmvals <- mplus.get.group.dataset(file,groupstr,datastr)

	y <- log(-log(kmvals[,2]))
	return(y)
}


#========================================================================
# mplus.get.survival.baseline.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	survvar2 - ending survival variable for getting sequential time
#	classnum - the group number (not required)
#
# eg. mplus.get.survival.baseline.values('ex6.21.gh5','T')
#
mplus.get.survival.baseline.values <- function(file,survvar,survvar2,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	if (!(missing(survvar2))) {
		if (is.character(survvar2)) {
			surv_idx2 <- 0
			for (i in c(1:props[1])) {
				cstr <- sprintf("survival_data/survival%d", i)
				label <- mplus.get.group.attribute(file,cstr,'label')
				label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
				if (label == survvar2) {
					surv_idx2 = i
					break
				}
			}
			if (surv_idx2 == 0) {
				stop("- unknown survival variable:  ", survvar2)
			}
		} else {
			if (survvar2 <= 0 || survvar2 > props[1]) {
				stop("- index for the survival variable is out of range")
			}
			surv_idx2 = survvar2
		}
	}

	if (missing(survvar2)) {
		groupstr <- sprintf("survival_data/survival%d", surv_idx)
		if (missing(classnum)) {
			datastr <- sprintf("estimated_survival")
		} else {
			classes <- mplus.get.group.dataset(file,'/','model_group_labels')
			dims <- attr(classes,'dim')
			if (classnum <= 0 || classnum > dims[1]) {
				stop("Class number is out of range.")
			}
			datastr <- sprintf("estimated_survival%d", classnum)
		}
		esvals <- mplus.get.group.dataset(file,groupstr,datastr)

		if (missing(time)) {
			return(esvals[,2])
		} else {
			return(esvals[,1])
		}
	} else {
		# ending survival variable given so we need to link them sequentially
		ylast <- 1
		xlast <- 0
		data <- vector()
		time <- vector()
		count <- 0
		for (s in c(surv_idx:surv_idx2)) {
			groupstr <- sprintf("survival_data/survival%d", s)
			if (missing(classnum)) {
				datastr <- sprintf("estimated_survival")
			} else {
				classes <- mplus.get.group.dataset(file,'/','model_group_labels')
				dims <- attr(classes,'dim')
				if (classnum <= 0 || classnum > dims[1]) {
					stop("Class number is out of range.")
				}
				datastr <- sprintf("estimated_survival%d", classnum)
			}
			esvals1 <- mplus.get.group.dataset(file,groupstr,datastr)
			
			if (s == surv_idx) {
				count <- length(esvals1[,1])
				data[1:count] <- esvals1[,1]
				time[1:count] <- estvals[,2]
			} else {
				n <- length(estvals1[,1])
			}
		}
	}
}



#========================================================================
# mplus.compute.survival.estimated.logcumulative.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	classnum - the group number (not required)
#
# eg. mplus.compute.survival.estimated.logcumulative.values('ex6.21.gh5','T')
#
mplus.compute.survival.estimated.logcumulative.values <- function(file,survvar,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	if (missing(classnum)) {
		datastr <- sprintf("estimated_survival1")
	} else {
		classes <- mplus.get.group.dataset(file,'/','model_group_labels')
		dims <- attr(classes,'dim')
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}
		datastr <- sprintf("estimated_survival%d", classnum)
	}
	esvals <- mplus.get.group.dataset(file,groupstr,datastr)

	y <- log(-log(esvals[,2]))
	return(y)
}



#========================================================================
# mplus.get.survival.basehazard.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	classnum - the group number (not required)
#
# eg. mplus.get.survival.basehazard.values('ex6.21.gh5','T')
#
mplus.get.survival.basehazard.values <- function(file,survvar,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	if (missing(classnum)) {
		datastr <- sprintf("basehazard")
	} else {
		classes <- mplus.get.group.dataset(file,'/','model_group_labels')
		dims <- attr(classes,'dim')
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}
		datastr <- sprintf("basehazard%d", classnum)
	}
	bhvals <- mplus.get.group.dataset(file,groupstr,datastr)

	if (missing(time)) {
		return(bhvals[,2])
	} else {
		return(bhvals[,1])
	}
}


#========================================================================
# mplus.plot.survival.kaplanmeier
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.kaplanmeier('ex6.21.gh5','T')
#
mplus.plot.survival.kaplanmeier <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Kaplan-Meier curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			yall[i,1:npoints[i]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("- class number is out of range")
		}

		xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		yy <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum)

		plot(xx,yy,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(xx,yy,col='red')
	}
}



#========================================================================
# mplus.plot.survival.baseline
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.baseline('ex6.21.gh5','T')
#
mplus.plot.survival.baseline <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Estimated baseline survival curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			yall[i,1:npoints[i]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		yy <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum)

		plot(xx,yy,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(xx,yy,col='red')
	}
}


#========================================================================
# mplus.plot.survival.basehazard
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.basehazard('ex6.21.gh5','T')
#
mplus.plot.survival.basehazard <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required.\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Estimated baseline hazard curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.basehazard.values(file,surv_idx,i,0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.survival.basehazard.values(file,surv_idx,i,0)
			yall[i,1:npoints[i]] <- mplus.get.survival.basehazard.values(file,surv_idx,i)
		}
		plot(xall,yall,xlab="Time",ylab="",main=cstr,type='n')

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		xx <- mplus.get.survival.basehazard.values(file,surv_idx,classnum,0)
		yy <- mplus.get.survival.basehazard.values(file,surv_idx,classnum)

		plot(xx,yy,xlab="Time",ylab="",main=cstr,type='n')
		lines(xx,yy,col='red')
	}
}


#========================================================================
# mplus.plot.survival.sample.logcumulative
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.sample.logcumulative('ex6.21.gh5','T')
#
mplus.plot.survival.sample.logcumulative <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Sample log cumulative hazard curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			yall[i,1:npoints[i]] <- mplus.compute.survival.sample.logcumulative.values(file,surv_idx,i)
			for (j in c(1:npoints[i])) {
				if (is.infinite(yall[j])) {
					xall[j] = NA
				}
			}
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		yy <- mplus.compute.survival.sample.logcumulative.values(file,surv_idx,classnum)
		for (j in c(1:length(xx))) {
			if (is.infinite(yy[j])) {
				xx[j] = NA
			}
		}

		plot(xx,yy,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(xx,yy,col='red')
	}
}



#========================================================================
# mplus.plot.survival.estimated.logcumulative
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.estimated.logcumulative('ex6.21.gh5','T')
#
mplus.plot.survival.estimated.logcumulative <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Estimated log cumulative hazard curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			yall[i,1:npoints[i]] <- mplus.compute.survival.estimated.logcumulative.values(file,surv_idx,i)
			for (j in c(1:npoints[i])) {
				if (is.infinite(yall[j])) {
					xall[j] = NA
				}
			}
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		yy <- mplus.compute.survival.estimated.logcumulative.values(file,surv_idx,classnum)
		for (j in c(1:length(xx))) {
			if (is.infinite(yy[j])) {
				xx[j] = NA
			}
		}

		plot(xx,yy,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(xx,yy,col='red')
	}
}



#========================================================================
# mplus.plot.survival.kaplanmeier.vs.baseline
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.kaplanmeier.vs.baseline('ex6.21.gh5','T')
#
mplus.plot.survival.kaplanmeier.vs.baseline <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Kaplan-Meier curve compared with\nestimated baseline survival curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(2*dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			npoints[2*(i-1)+1] = length(xx)
			xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			npoints[2*i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(2*dims[1],maxpoints))
		yall <- array(NA, c(2*dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[2*(i-1)+1,1:npoints[2*(i-1)+1]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			yall[2*(i-1)+1,1:npoints[2*(i-1)+1]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i)

			xall[2*i,1:npoints[2*i]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			yall[2*i,1:npoints[2*i]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(2*dims[1])
		for (i in c(1:(2*dims[1]))) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(2*dims[1]))
		lty <- array(0,c(2*dims[1]))
		lwd <- array(0,c(2*dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[2*(i-1)+1] <- sprintf("KM for Class %d", i)
			lty[2*(i-1)+1] = 1
			lwd[2*(i-1)+1] = 2.5

			ldesc[2*i] <- sprintf("ES for Class %d", i)
			lty[2*i] = 1
			lwd[2*i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		npoints <- array(0, c(2))
		xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		npoints[1] = length(xx)
		xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		npoints[2] = length(xx)
		maxpoints = max(npoints)

		xall <- array(NA, c(2,maxpoints))
		yall <- array(NA, c(2,maxpoints))

		xall[1,1:npoints[1]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		yall[1,1:npoints[1]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum)

		xall[2,1:npoints[2]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		yall[2,1:npoints[2]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum)

		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(2)
		for (i in c(1:2)) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(2))
		lty <- array(0,c(2))
		lwd <- array(0,c(2))

		ldesc[1] <- sprintf("KM for Class %d", classnum)
		lty[1] = 1
		lwd[1] = 2.5

		ldesc[2] <- sprintf("ES for Class %d", classnum)
		lty[2] = 1
		lwd[2] = 2.5

		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	}
}



#========================================================================
# mplus.plot.survival.sample.vs.estimated.logcumulative
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.survival.sample.vs.estimated.logcumulative('ex6.21.gh5','T')
#
mplus.plot.survival.sample.vs.estimated.logcumulative <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Sample log cumulative hazard curve compared with\nestimated log cumulative baseline hazard curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(2*dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			npoints[2*(i-1)+1] = length(xx)
			xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			npoints[2*i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(2*dims[1],maxpoints))
		yall <- array(NA, c(2*dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[2*(i-1)+1,1:npoints[2*(i-1)+1]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,i,0)
			yall[2*(i-1)+1,1:npoints[2*(i-1)+1]] <- mplus.compute.survival.sample.logcumulative.values(file,surv_idx,i)

			xall[2*i,1:npoints[2*i]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			yall[2*i,1:npoints[2*i]] <- mplus.compute.survival.estimated.logcumulative.values(file,surv_idx,i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(2*dims[1])
		for (i in c(1:(2*dims[1]))) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(2*dims[1]))
		lty <- array(0,c(2*dims[1]))
		lwd <- array(0,c(2*dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[2*(i-1)+1] <- sprintf("LC for Class %d", i)
			lty[2*(i-1)+1] = 1
			lwd[2*(i-1)+1] = 2.5

			ldesc[2*i] <- sprintf("ELC for Class %d", i)
			lty[2*i] = 1
			lwd[2*i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		npoints <- array(0, c(2))
		xx <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		npoints[1] = length(xx)
		xx <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		npoints[2] = length(xx)
		maxpoints = max(npoints)

		xall <- array(NA, c(2,maxpoints))
		yall <- array(NA, c(2,maxpoints))

		xall[1,1:npoints[1]] <- mplus.get.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		yall[1,1:npoints[1]] <-mplus.compute.survival.sample.logcumulative.values(file,surv_idx,classnum)

		xall[2,1:npoints[2]] <- mplus.get.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		yall[2,1:npoints[2]] <- mplus.compute.survival.estimated.logcumulative.values(file,surv_idx,classnum)

		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(2)
		for (i in c(1:2)) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(2))
		lty <- array(0,c(2))
		lwd <- array(0,c(2))

		ldesc[1] <- sprintf("LC for Class %d", classnum)
		lty[1] = 1
		lwd[1] = 2.5

		ldesc[2] <- sprintf("ELC for Class %d", classnum)
		lty[2] = 1
		lwd[2] = 2.5

		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	}
}




######################################################################################################
# Functions for Discrete survival plots
######################################################################################################

#========================================================================
# mplus.list.discrete.survival.variables
#
# arguments:
#	file - the quoted name of an existing GH5 file
#
# eg. mplus.list.discrete.survival.variables('ex6.21.gh5')
#
mplus.list.discrete.survival.variables <- function(file) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- discrete survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

#	cat(c("\nList of variables to use in the following functions:\n"))
#	cat(c(" - mplus.compute.irt.icc\n"))
#	cat(c(" - mplus.plot.irt.icc\n"))

	cat(c("\nList of survival variables:\n"))

	for (i in c(1:props[1])) {
		cstr <- sprintf("discrete_survival_data/survival%d", i)
		label <- mplus.get.group.attribute(file,cstr,'label')
		cstr <- sprintf("%s", label)
		cstr <- gsub("(^\\s+|\\s+$)", "", cstr, perl=TRUE)
		print(cstr)
	}
}


#========================================================================
# mplus.get.discrete.survival.kaplanmeier.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	classnum - the group number (not required)
#
# eg. mplus.get.discrete.survival.kaplanmeier.values('ex6.21.gh5','T')
#
mplus.get.discrete.survival.kaplanmeier.values <- function(file,survvar,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("discrete_survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("discrete_survival_data/survival%d", surv_idx)
	if (missing(classnum)) {
		datastr <- sprintf("kaplan_meier1")
	} else {
		classes <- mplus.get.group.dataset(file,'/','model_group_labels')
		dims <- attr(classes,'dim')
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}
		datastr <- sprintf("kaplan_meier%d", classnum)
	}
	kmvals <- mplus.get.group.dataset(file,groupstr,datastr)

	if (missing(time)) {
		return(kmvals[,2])
	} else {
		return(kmvals[,1])
	}
}

#========================================================================
# mplus.get.discrete.survival.baseline.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	survvar2 - ending survival variable for getting sequential time
#	classnum - the group number (not required)
#
# eg. mplus.get.discrete.survival.baseline.values('ex6.21.gh5','T')
#
mplus.get.discrete.survival.baseline.values <- function(file,survvar,survvar2,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("discrete_survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	if (!(missing(survvar2))) {
		if (is.character(survvar2)) {
			surv_idx2 <- 0
			for (i in c(1:props[1])) {
				cstr <- sprintf("discrete_survival_data/survival%d", i)
				label <- mplus.get.group.attribute(file,cstr,'label')
				label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
				if (label == survvar2) {
					surv_idx2 = i
					break
				}
			}
			if (surv_idx2 == 0) {
				stop("- unknown survival variable:  ", survvar2)
			}
		} else {
			if (survvar2 <= 0 || survvar2 > props[1]) {
				stop("- index for the survival variable is out of range")
			}
			surv_idx2 = survvar2
		}
	}

	if (missing(survvar2)) {
		groupstr <- sprintf("discrete_survival_data/survival%d", surv_idx)
		if (missing(classnum)) {
			datastr <- sprintf("estimated_survival")
		} else {
			classes <- mplus.get.group.dataset(file,'/','model_group_labels')
			dims <- attr(classes,'dim')
			if (classnum <= 0 || classnum > dims[1]) {
				stop("Class number is out of range.")
			}
			datastr <- sprintf("estimated_survival%d", classnum)
		}
		esvals <- mplus.get.group.dataset(file,groupstr,datastr)

		if (missing(time)) {
			return(esvals[,2])
		} else {
			return(esvals[,1])
		}
	} else {
		# ending survival variable given so we need to link them sequentially
		ylast <- 1
		xlast <- 0
		data <- vector()
		time <- vector()
		count <- 0
		for (s in c(surv_idx:surv_idx2)) {
			groupstr <- sprintf("discrete_survival_data/survival%d", s)
			if (missing(classnum)) {
				datastr <- sprintf("estimated_survival")
			} else {
				classes <- mplus.get.group.dataset(file,'/','model_group_labels')
				dims <- attr(classes,'dim')
				if (classnum <= 0 || classnum > dims[1]) {
					stop("Class number is out of range.")
				}
				datastr <- sprintf("estimated_survival%d", classnum)
			}
			esvals1 <- mplus.get.group.dataset(file,groupstr,datastr)
			
			if (s == surv_idx) {
				count <- length(esvals1[,1])
				data[1:count] <- esvals1[,1]
				time[1:count] <- estvals[,2]
			} else {
				n <- length(estvals1[,1])
			}
		}
	}
}



#========================================================================
# mplus.get.discrete.survival.basehazard.values
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (required)
#	classnum - the group number (not required)
#
# eg. mplus.get.discrete.survival.basehazard.values('ex6.21.gh5','T')
#
mplus.get.discrete.survival.basehazard.values <- function(file,survvar,classnum,time) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	if (missing(survvar)) {
		stop("The survival variable must be given.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("discrete_survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("discrete_survival_data/survival%d", surv_idx)
	if (missing(classnum)) {
		datastr <- sprintf("basehazard")
	} else {
		classes <- mplus.get.group.dataset(file,'/','model_group_labels')
		dims <- attr(classes,'dim')
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}
		datastr <- sprintf("basehazard%d", classnum)
	}
	bhvals <- mplus.get.group.dataset(file,groupstr,datastr)

	if (missing(time)) {
		return(bhvals[,2])
	} else {
		return(bhvals[,1])
	}
}


#========================================================================
# mplus.plot.discrete.survival.kaplanmeier
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.discrete.survival.kaplanmeier('ex6.21.gh5','T')
#
mplus.plot.discrete.survival.kaplanmeier <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("discrete_survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("discrete_survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Kaplan-Meier curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,i,0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,i,0)
			yall[i,1:npoints[i]] <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("- class number is out of range")
		}

		xx <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		yy <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,classnum)

		plot(xx,yy,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(xx,yy,col='red')
	}
}



#========================================================================
# mplus.plot.discrete.survival.baseline
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.discrete.survival.baseline('ex6.21.gh5','T')
#
mplus.plot.discrete.survival.baseline <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("discrete_survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("discrete_survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Estimated baseline survival curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			npoints[i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(dims[1],maxpoints))
		yall <- array(NA, c(dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[i,1:npoints[i]] <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			yall[i,1:npoints[i]] <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))

		colors <- rainbow(dims[1])
		for (i in c(1:dims[1])) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(dims[1]))
		lty <- array(0,c(dims[1]))
		lwd <- array(0,c(dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[i] <- sprintf("Class %d", i)
			lty[i] = 1
			lwd[i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		xx <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		yy <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=classnum)

		plot(xx,yy,xlab="Time",ylab="Probability",main=cstr,type='n',ylim=c(0,1))
		lines(xx,yy,col='red')
	}
}


#========================================================================
# mplus.plot.discrete.survival.kaplanmeier.vs.baseline
#
# arguments:
#	file - the quoted name of an existing GH5 file
#	survvar - the quoted name of the survival variable or the index of the survival variable (not required)
#	classnum - the group number (not required)
#
# eg. mplus.plot.discrete.survival.kaplanmeier.vs.baseline('ex6.21.gh5','T')
#
mplus.plot.discrete.survival.kaplanmeier.vs.baseline <- function(file,survvar=1,classnum) {
	if (missing(file)) {
		stop("- name of the GH5 file is required")
	}
	if (!(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	# check if survival data exists
	if ( !("discrete_survival_data" %in% names(gh5)) ) {
		stop("- survival data is required\n\nUse TYPE=PLOT2.")
	}

	props <- mplus.get.group.attribute(file,'discrete_survival_data','properties')

	if (is.character(survvar)) {
		surv_idx <- 0
		for (i in c(1:props[1])) {
			cstr <- sprintf("discrete_survival_data/survival%d", i)
			label <- mplus.get.group.attribute(file,cstr,'label')
			label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)
			if (label == survvar) {
				surv_idx = i
				break
			}
		}
		if (surv_idx == 0) {
			stop("- unknown survival variable:  ", survvar)
		}
	} else {
		if (survvar <= 0 || survvar > props[1]) {
			stop("- index for the survival variable is out of range")
		}
		surv_idx = survvar
	}

	groupstr <- sprintf("discrete_survival_data/survival%d", surv_idx)
	label <- mplus.get.group.attribute(file,groupstr,'label')
	label <- gsub("(^\\s+|\\s+$)", "", label, perl=TRUE)

	classes <- mplus.get.group.dataset(file,'/','model_group_labels')
	dims <- attr(classes,'dim')

	cstr <- sprintf("Kaplan-Meier curve compared with\nestimated baseline survival curve for %s", label)

	if (missing(classnum)) {
		npoints <- array(0, c(2*dims[1]))
		for (i in c(1:dims[1])) {
			xx <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,i,0)
			npoints[2*(i-1)+1] = length(xx)
			xx <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			npoints[2*i] = length(xx)
		}
		maxpoints = max(npoints)

		xall <- array(NA, c(2*dims[1],maxpoints))
		yall <- array(NA, c(2*dims[1],maxpoints))

		for (i in c(1:dims[1])) {
			xall[2*(i-1)+1,1:npoints[2*(i-1)+1]] <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,i,0)
			yall[2*(i-1)+1,1:npoints[2*(i-1)+1]] <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,i)

			xall[2*i,1:npoints[2*i]] <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=i,time=0)
			yall[2*i,1:npoints[2*i]] <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=i)
		}
		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n')

		colors <- rainbow(2*dims[1])
		for (i in c(1:(2*dims[1]))) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(2*dims[1]))
		lty <- array(0,c(2*dims[1]))
		lwd <- array(0,c(2*dims[1]))
		for (i in c(1:dims[1])) {
			ldesc[2*(i-1)+1] <- sprintf("KM for Class %d", i)
			lty[2*(i-1)+1] = 1
			lwd[2*(i-1)+1] = 2.5

			ldesc[2*i] <- sprintf("ES for Class %d", i)
			lty[2*i] = 1
			lwd[2*i] = 2.5
		}
		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	} else {
		if (classnum <= 0 || classnum > dims[1]) {
			stop("Class number is out of range.")
		}

		npoints <- array(0, c(2))
		xx <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		npoints[1] = length(xx)
		xx <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		npoints[2] = length(xx)
		maxpoints = max(npoints)

		xall <- array(NA, c(2,maxpoints))
		yall <- array(NA, c(2,maxpoints))

		xall[1,1:npoints[1]] <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,classnum,0)
		yall[1,1:npoints[1]] <- mplus.get.discrete.survival.kaplanmeier.values(file,surv_idx,classnum)

		xall[2,1:npoints[2]] <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=classnum,time=0)
		yall[2,1:npoints[2]] <- mplus.get.discrete.survival.baseline.values(file,surv_idx,classnum=classnum)

		plot(xall,yall,xlab="Time",ylab="Probability",main=cstr,type='n')

		colors <- rainbow(2)
		for (i in c(1:2)) {
			lines(xall[i,],yall[i,],col=colors[i])
		}

		ldesc <- array(0,c(2))
		lty <- array(0,c(2))
		lwd <- array(0,c(2))

		ldesc[1] <- sprintf("KM for Class %d", classnum)
		lty[1] = 1
		lwd[1] = 2.5

		ldesc[2] <- sprintf("ES for Class %d", classnum)
		lty[2] = 1
		lwd[2] = 2.5

		legend("top",ldesc,col=colors,lty=lty,lwd=lwd)
	}
}





######################################################################################################
# Supporting functions
######################################################################################################


##########################################################################
#
# mplus.get.group.attribute - supporting function for getting attribute
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   groupstr - the name of the group for the attribute
#   attrstr - the name of the attribute
#
# eg. mplus.get.attribute('ex8.1.gh5','individual_data','var_names')
#
mplus.get.group.attribute <- function(file, groupstr, attrstr) {
	if ( !(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	fid <- H5Fopen(file)
	gid <- H5Gopen(fid, groupstr)
	atid <- H5Aopen(gid, attrstr)

	attr <- H5Aread(atid)

	H5Aclose(atid)
	H5Gclose(gid)
	H5Fclose(fid)

	attr <- gsub("(^\\s+|\\s+$)", "", attr, perl=TRUE)

	return(attr)
}

##########################################################################
#
# mplus.get.group.attribute - supporting function for getting attribute
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   groupstr - the name of the group for the attribute
#   attrstr - the name of the attribute
#
# eg. mplus.get.attribute('ex8.1.gh5','individual_data','var_names')
#
mplus.get.dataset.attribute <- function(file, datastr, attrstr) {
	if ( !(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	fid <- H5Fopen(file)
	did <- H5Dopen(fid, datastr)
	atid <- H5Aopen(did, attrstr)

	attr <- H5Aread(atid)

	H5Aclose(atid)
	H5Dclose(did)
	H5Fclose(fid)

	return(attr)
}

##########################################################################
#
# mplus.get.group.dataset - supporting function for getting dataset
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   groupstr - the name of the group for the attribute
#   datastr - the name of the attribute
#
# eg. mplus.get.group.dataset('ex8.1.gh5','bayesian_data','statements')
#
mplus.get.group.dataset <- function(file, groupstr, datastr) {
	if ( !(file.exists(file))) {
		cstr <- paste("- file does not exist:",file,"\n")
		stop(cstr)
	}

	gh5 <- h5dump(file, load=TRUE)

	fid <- H5Fopen(file)
	gid <- H5Gopen(fid, groupstr)
	dtid <- H5Dopen(gid, datastr)

	data <- H5Dread(dtid)

	H5Dclose(dtid)
	H5Gclose(gid)
	H5Fclose(fid)

	return(data)
}

estimate_mode <- function(x) {
	d <- density(x)
	d$x[which.max(d$y)]
}



######################################################################################################
# Math functions
######################################################################################################

lin <- function(y, link) {
	if (link == 0) {
		x <- logistic(y)
	} else {
		x <- pnorm(y, mean=0, sd=1)
	}
	x
}

logistic <- function(y) {
	if (y > 50) {
		x = 1
	} else if (y > -50) {
		x = 1 / (1 + exp(-y))
	} else {
		x = 0
	}

	x
}




