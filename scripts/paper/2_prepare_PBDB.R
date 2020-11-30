# Paleocoastlines - paper.
# 2_prepare_PBDB.R - The purpose of this script is to prepare the raw 
# Paleobiology Database download to be used with the coastlines data.
# The dataset is separated to likely marine and likely terrestrial subsets.
# These are then assigned to stratigraphic stages and 10my bins. 
# The collections are assigned to a tectonic reconstruction based on the assigned
# ages of their stage. Data belonging to the same age are assumed to be contemporary. 
# Paleogeographic collections are then reconstructed with the chronosphere and GPlates.
# exported as results. 
# 2020-08-09. 
# Ádám T. Kocsis, Erlangen, CC-BY

library(chronosphere)
library(divDyn)
library(restools)
data(stages)

# working dir
	workdir<-file.path(Sys.getenv("WorkSpace"), "2019-07-04_paleoCoastlines", sep="")
	setwd(workdir)

# load the functions
	funky()

# version
	ver <- "paper_2020-07-29"
	setver(ver)
	results()

################################################################################
# A. read the data table
	# this will likely give a warning about character encoding
	# nothing to worry about
	dat <- fetch("pbdb", datadir=file.path(workdir, "data/chronosphere"), ver="20200807")

	totalColl <- length(unique(dat$collection_no))
	stores(totalColl)

# 1. taxonomic filtering
#	# filter records not identified at least to genus
#	dat <-dat[dat$accepted_rank %in% c("genus", "species"),]
#
#	# omit non-informative genus entries
#	dat <- dat[dat$genus!="", ]

	#A. phyla
	marineNoPlant <- c("",
		"Agmata",
		"Annelida",
		"Bilateralomorpha",
		"Brachiopoda",
		"Bryozoa",
		"Calcispongea",
		"Chaetognatha",
		"Cnidaria",
		"Ctenophora",
		"Echinodermata",
		"Entoprocta",
		"Foraminifera",
		"Hemichordata",
		"Hyolitha",
		"Mollusca",
		"Nematoda",
		"Nematomorpha",
		"Nemertina",
		"Onychophora",
		"Petalonamae",
		"Phoronida",
		"Platyhelminthes",
		"Porifera",
		"Problematica",
		"Rhizopodea",
		"Rotifera",
		"Sarcomastigophora",
		"Sipuncula",
		"Uncertain",
		"Vetulicolia",
		""
	)

	# which rows?
	bByPhyla <- dat$phylum%in% marineNoPlant

	# the other
		noNeed <- dat[!bByPhyla,]
		needPhylum <- dat[bByPhyla,]

	#B. classes
	#	levels(factor(noNeed$class))
		needClass <- c(
			"Acanthodii",
			"Actinopteri",
			"Actinopterygii",
			"Agnatha",
			"Cephalaspidomorphi",
			"Chondrichthyes",
			"Cladistia",
			"Coelacanthimorpha",
			"Conodonta",
			"Galeaspida",
			"Myxini",
			"Osteichthyes",
			"Petromyzontida",
			"Plagiostomi",
			"Pteraspidomorphi",
			# here come the Arthropods
			"Artiopoda",
			"Branchiopoda",
			"Cephalocarida",
			"Copepoda",
			"Malacostraca",
			"Maxillopoda",
			"Megacheira",
			"Merostomoidea",
			"Ostracoda",
			"Paratrilobita",
			"Pycnogonida",
			"Remipedia",
			"Thylacocephala",
			"Trilobita",
			"Xiphosura"
		)
		
		# which rows?
		bNeedClass <- dat$class %in% needClass

	#C.  mammals
	#	mammals <- dat[dat$class=="Mammalia",]
	#	levels(factor(mammals$order))

		needMammalOrd <- c("Cetacea", "Sirenia")

		# which rows?
		bMammalOrder <- dat$order %in% needMammalOrd

		# the carnivores
		carnivores <- dat[dat$order=="Carnivora",]
		levels(factor(carnivores$family))

		needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")

		# which rows?
		bNeedMamFam<- dat$family%in%needFam

	# D. Reptiles
	#	reptiles <- dat[dat$class=="Reptilia",]
	#	levels(factor(reptiles$order))

		needReptOrd<-c(
			"Eosauropterygia",
			"Hupehsuchia",
			"Ichthyosauria",
			"Placodontia",
			"Sauropterygia",
			"Thalattosauria"
		)
		
		# which rows?
		bRept <- dat$order%in%needReptOrd

	# E. turtles 
	#	turtles <- dat[dat$order=="Testudines",]
	#	levels(factor(turtles$family))
	
	# Chelonioidea turtles
	needTurtleFam <- c(
		"Cheloniidae",
		"Protostegidae",
		"Dermochelyidae",
		"Dermochelyoidae",
		"Toxochelyidae",
		"Pancheloniidae"
	)

	# which rows?
	bTurtle <- dat$family%in%needTurtleFam

	# subset the data
	dat <- dat[
		bByPhyla |
		bNeedClass |
		bMammalOrder |
		bNeedMamFam |
		bRept | 
		bTurtle
		, ]



################################################################################
# 2. filter by environment
	levels(factor((dat$environment)))

	omitEnv <- c(
		"\"floodplain\"",
		"alluvial fan",
		"cave",
		"\"channel\"",
		"channel lag" ,
		"coarse channel fill",
		"crater lake",
		"crevasse splay",
		"dry floodplain",
		"delta plain",
		"dune",
		"eolian indet.",
		"fine channel fill",
		"fissure fill",
		"fluvial indet.",
		"fluvial-lacustrine indet.",
		"fluvial-deltaic indet.",
		"glacial",
		"interdune",
		"karst indet.",
		"lacustrine - large",
		"lacustrine - small",
		"lacustrine delta front",
		"lacustrine delta plain",
		"lacustrine deltaic indet.",
		"lacustrine indet.",
		"lacustrine interdistributary bay",
		"lacustrine prodelta",
		"levee",
		"loess",
		"mire/swamp",
		"pond",
		"sinkhole",
		"spring",
		"tar",
		"terrestrial indet.",
		"wet floodplain"
	)

	dat<-dat[!dat$environment%in%omitEnv, ]


# finally omit unlithified sediments
#	dat <- dat[dat$lithification1!="unlithified",]

	
# resolving remaining marine environmental variables (not for this, but for additional analyses) 
 	#load from divDyn
 	data(keys)

################################################################################
# 3. stratigraphic resolution
	# time scales
	data(stages)
	data(tens)
	
	# rename the entries
	colnames(tens)[colnames(tens)=="X10"]<-"name"
	colnames(stages)[colnames(stages)=="stage"]<-"name"

	
#-------------------------------------------------------------------------------
# A.  the bin entries

	# a. categorize interval names to bin numbers
		# categorize is the new function of the package
		tenMin<-categorize(dat[,"early_interval"],keys$tenInt)
		tenMax<-categorize(dat[,"late_interval"],keys$tenInt)

		# convert to numeric
		tenMin<-as.numeric(tenMin)
		tenMax<-as.numeric(tenMax)

	# b. resolve max-min interval uncertainty
	# final variable (empty)
		dat$ten <- rep(NA, nrow(dat))

	# use entries, where
		tenCondition <- c(
		# the early and late interval fields indicate the same bin
			which(tenMax==tenMin),
		# or the late_interval field is empty
			which(tenMax==-1))

	# in these entries, use the bin indicated by the early_interval
		dat$ten[tenCondition] <- tenMin[tenCondition]

#####################################-------------------------------------------
# do the same for the stages
# B. the stg entries (lookup)
		stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
		stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

		# convert to numeric
		stgMin<-as.numeric(stgMin)
		stgMax<-as.numeric(stgMax)

	# empty container
		dat$stg <- rep(NA, nrow(dat))

	# select entries, where
		stgCondition <- c(
		# the early and late interval fields indicate the same stg
			which(stgMax==stgMin),
		# or the late_intervarl field is empty
			which(stgMax==-1))

	# in these entries, use the stg indicated by the early_interval
		dat$stg[stgCondition] <- stgMin[stgCondition]

	# terrible in the pre-Silurian
# using the online items
	# additional treatment required for Cambrian
		# load data
		load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))
		# correct it with this function
		source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

	# and the Ordovician
		# load data
		load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
		# correct it with this function
		source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")
	
	dat$marine <- TRUE
	marine <- dat

# 2.  likely terrestrial subset
# read the data table
	# same as before - disregard warnings
	dat <- fetch("pbdb", datadir=file.path(workdir, "data/chronosphere"), ver="20200807")

################################################################################
# 1. filter only by environment
	levels(factor((dat$environment)))

	omitEnv <- c(
		"\"floodplain\"",
		"alluvial fan",
		"cave",
		"\"channel\"",
		"channel lag" ,
		"coarse channel fill",
		"crater lake",
		"crevasse splay",
		"dry floodplain",
		"delta plain",
		"dune",
		"eolian indet.",
		"fine channel fill",
		"fissure fill",
		"fluvial indet.",
		"fluvial-lacustrine indet.",
		"fluvial-deltaic indet.",
		"glacial",
		"interdune",
		"karst indet.",
		"lacustrine - large",
		"lacustrine - small",
		"lacustrine delta front",
		"lacustrine delta plain",
		"lacustrine deltaic indet.",
		"lacustrine indet.",
		"lacustrine interdistributary bay",
		"lacustrine prodelta",
		"levee",
		"loess",
		"mire/swamp",
		"pond",
		"sinkhole",
		"spring",
		"tar",
		"terrestrial indet.",
		"wet floodplain"
	)

	dat<-dat[dat$environment%in%omitEnv, ]

################################################################################
# 2. stratigraphic resolution
	# time scales
	data(stages)
	data(tens)
	
	# rename the entries
	colnames(tens)[colnames(tens)=="X10"]<-"name"
	colnames(stages)[colnames(stages)=="stage"]<-"name"

	
#-------------------------------------------------------------------------------
# A.  the bin entries
	data(keys)
	# a. categorize interval names to bin numbers
		# categorize is the new function of the package
		tenMin<-categorize(dat[,"early_interval"],keys$tenInt)
		tenMax<-categorize(dat[,"late_interval"],keys$tenInt)

		# convert to numeric
		tenMin<-as.numeric(tenMin)
		tenMax<-as.numeric(tenMax)

	# b. resolve max-min interval uncertainty
	# final variable (empty)
		dat$ten <- rep(NA, nrow(dat))

	# use entries, where
		tenCondition <- c(
		# the early and late interval fields indicate the same bin
			which(tenMax==tenMin),
		# or the late_interval field is empty
			which(tenMax==-1))

	# in these entries, use the bin indicated by the early_interval
		dat$ten[tenCondition] <- tenMin[tenCondition]

#####################################-------------------------------------------
# do the same for the stages
# B. the stg entries (lookup)
		stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
		stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

		# convert to numeric
		stgMin<-as.numeric(stgMin)
		stgMax<-as.numeric(stgMax)

	# empty container
		dat$stg <- rep(NA, nrow(dat))

	# select entries, where
		stgCondition <- c(
		# the early and late interval fields indicate the same stg
			which(stgMax==stgMin),
		# or the late_intervarl field is empty
			which(stgMax==-1))

	# in these entries, use the stg indicated by the early_interval
		dat$stg[stgCondition] <- stgMin[stgCondition]

	# terrible in the pre-Silurian
# using the online items
	# additional treatment required for Cambrian
		# load data
		load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))
		# correct it with this function
		source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

	# and the Ordovician
		# load data
		load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
		# correct it with this function
		source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")

# export this part
		dat$marine <- FALSE
		terrestrial <- dat

# bind the two together
dat <- rbind(marine, terrestrial)

#####################################-------------------------------------------
# use chronosphere to reconstruct points
	# check invalide coordinates
	bInvalid <- dat$lng>180 | dat$lng< (-180) | dat$lat>90 | dat$lat< (-90)
	dat <- dat[!bInvalid, ]

	# create a collection table
	collTab <- unique(dat[, c("collection_no", "collection_name", "lng", "lat", "paleolng", "paleolat", "stg", "cc", "marine")])

	# drop non-stage level entries
	collTab <- collTab[!is.na(collTab$stg),]

	# rename old paleocoordinates
	colnames(collTab)[colnames(collTab)=="paleolng"] <- "dlPaleolng"
	colnames(collTab)[colnames(collTab)=="paleolat"] <- "dlPaleolat"
 	
 	# the shapefiles
		# match order to stages
		demOrd <- dem1400[matchtime(names(dem1400), stages$mid)]
		
	# reconstrcution age 
		collTab$mapage <- names(demOrd)[collTab$stg]

	# download model - but use the one provided by chris for this!
	mod <- fetch("paleomap", "model", ver="v19o_r1c", datadir=file.path(workdir, "data/chronosphere"))

	# reconstruct coordinates
	paleoColls <- reconstruct(collTab[, c("lng", "lat")], 
		age=collTab[, "mapage"], enumerate=FALSE, verbose=T, model=mod, plateperiod=FALSE)
	colnames(paleoColls) <- c("paleolng", "paleolat")

	# final dataframe
	paleoColls <- cbind(collTab, paleoColls)

	# store the total dataset
	stores(paleoColls)











