### Purpose (by section
### 1. This script loads an Excel file containing ServCat output and removes duplicate rows
### 2. Look 
### Author: A. Allstadt, 7/21/16

### Load library to load old Excel files (*.xls).
### If this library is not already installed, run commented line below first. 
### install.packages("gdata"). Entering "?gdata" will bring up the help file. 
library(gdata)

### Wrapper function to properly capitalize items in a list
toProper <- function(text) {
	s <- strsplit(text, " ")[[1]]
	paste(toupper(substring(s, 1,1)), substring(s, 2),
			sep="", collapse=" ")
}

###################################################################################
### Section 1 - Load file, remove duplicate rows, contained in variable "in.data" #
###################################################################################

### Load the file into a data.frame
filename = "C:/Users/aallstadt/Desktop/AA ad hoc grouped bio 1_partial edit.xls"
raw.data = read.xls(filename, stringsAsFactors = F);

### Find rows denoting groups, remove these rows as there is already a column for them
inds = which(raw.data$Survey.Name == "")
raw.data = raw.data[-1*inds,]

### Show column names
names(raw.data)

### Size of raw data (rows, columns)
dim(raw.data)

### Remove duplicate rows
in.data = raw.data[!duplicated(raw.data),]

### Set all capitalizations to "Title Case", see how many unique surveys remain after that
#in.data$Survey.Name = sapply(in.data$Survey.Name, FUN = toProper)
#in.data = in.data[!duplicated(in.data),]

### Size with duplicates removed (rows, columns)
dim(in.data)

##################################################
### Section 2 - Approx matching of survey names  #
##################################################

### Get unique values of the column "Survey.Name"
### Sort so they are easier to compare
surveys = sort(unique(in.data$Survey.Name))
surveys;

if(F){ # Old stuff

### Take spaces out of surveys
ns.surveys = surveys;
for (i in 1:length(surveys)) {
	ns.surveys[i] = gsub("[[:space:]]", "", ns.surveys[i]) 
}
species.vals = list()
max.len  = -1;
which.max = -1;
for (i in 1:length(surveys)){
	txt = ns.surveys[i];
	lst = ns.surveys[-i];
	species.vals[which.[i]] = lst[agrep(txt, lst, max.distance = 1)]	
	if (length(species.vals[[i]]) > max.len) {
		max.len = length(species.vals[[i]])
		which.max = i;
	}
}
}

species.vals = list()
max.len  = -1;
which.max = -1;
for (i in 1:length(surveys)){
	txt = surveys[i];
	lst = surveys[-i];
	#species.vals[[i]] = lst[agrep(txt, lst, max.distance = 1)]	
	species.vals[[i]] = lst[which(adist(txt, lst, ignore.case=F) <= 0.25 * nchar(txt))]
	if (length(species.vals[[i]]) > max.len) {
		max.len = length(species.vals[[i]])
		which.max = i;
	}
}

### Convert to data.frame
out.dat = data.frame(matrix(NA, length(surveys), max.len+1))
names(out.dat) = c("Original", paste0("Match", 1:max.len))
for (i in 1:length(surveys)){
	len = length(species.vals[[i]])
	if (len>0){
		out.dat[i,2:(1+len)] = unlist(species.vals[[i]])
		out.dat[i,1] = surveys[i]
	}
}
out.dat = out.dat[!is.na(out.dat[,1]),]


### Write an output CSV file
out.filename = "C:/Users/aallstadt/Desktop/AA_ApproxSurveyNames.xls"
WriteXLS(out.dat, out.filename, row.names = F)




