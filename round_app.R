#------------------------------------------------------------------------------#
# Create an application to round up the scoring and marks from the students
# 
# created on "Wed Jan 10 17:51:43 2024"
# by Francesco Pupillo
#------------------------------------------------------------------------------#
library(readxl)
library(openxlsx)
#library(writexl)
#library(xlsx)

# source helper functions
for (func in list.files("helper_functions")){
source(paste0("helper_functions/", func))
}

# read the first file
#input1 <- read_excel("8721_rep_cijferlijst.xlsx")
#input1 <- read.xlsx("8814_rep_cijferlijst.xlsx")


# read the second
#input2name<-"500193-B-6_20240122.xlsx"
#input2 <-read.xlsx(input2name)

#names(input2)[c(3,4,9,10)]<-" "

round_app<-function(input1, input2){
  
  input1 <- read_excel(input1)
  input2 <- read_excel(input2)
  
  input1<-as.data.frame(input1)

 input2<-as.data.frame(input2)

# check whether the columns correspond
if(ncol(input1) > ncol(input2)) {
stop("you submitted the files in the wrong order! 
   Please reload the page and submit the file with the scores first and
                              the empty file after")
}

# -----------------------------------------------------------------------------#
# get the headers in file 1
coord1<-getHeadercoor(df = input1)

# make it the header
names(input1)<-as.character(input1[coord1[1],])

# fix na
for (name in 1:ncol(input1)){
  if(is.na(names(input1)[name])) {names(input1)[name]<-"no_name"}
}

# delete all the rows before
input1<-input1[-(1:coord1[1]),]

# convert the Resultaat as numeric
input1$Resultaat<-as.numeric(input1$Resultaat)
  
# convert and append to the dataset for easy lookup functioning
input1$conv_mark<-round(input1$Resultaat)

# -----------------------------------------------------------------------------#
# get the headers in file 2
coord2<-getHeadercoor(df = input2)

# get current names
input2_header<-names(input2)

# store the old file
input2_old<-input2

# make it thinput2# make it the header
names(input2)<-as.character(input2[coord2[1],])

# fix na
for (name in 1:ncol(input2)){
  if(is.na(names(input2)[name])) {names(input2)[name]<-"no_name"}
}

# before deleting all the rows, cached them in a variable
del_rows<-input2[(1:coord2[1]),]

# delete all the rows before
input2<-input2[-(1:coord2[1]),]

# -----------------------------------------------------------------------------#

# create a file check as well
file_check<-input2[, c("Studentnummer", "Resultaat")]

names(file_check)[2]<-"converted"
file_check$original<-NA

# run a loop and attach the mark that corresponds to the student nummer
for (n in 1:nrow(input2)){
  
  if (all(is.na(input1$Resultaat[input1$`S Nummer`== 
                      input2$Studentnummer[n]]))){ # if we do not have the mark 
                                                  # for that student
    
   input2$Resultaat[n]<-"" # leave it empty
   
   file_check$original[n]<-""
  
} else{
  
  # find the mark in df1 that correspond to studentnummer in df2
 c_mark<-input1$conv_mark[input1$`S Nummer`==  
            input2$Studentnummer[n]]
 
 # remove NAs
 c_mark<-c_mark[!is.na(c_mark)]

 input2$Resultaat[n]<-c_mark
 
 file_check$converted[n]<-c_mark
 original<-input1$Resultaat[input1$`S Nummer`==  
                                         input2$Studentnummer[n]]
 
 file_check$original[n]<-original[!is.na(original)]
 
}
}

# now attach the rows at the top of the input 2
input2<-rbind(del_rows, input2)

# reassign the old names
names(input2)<-input2_header

# write
names(input2)[c(3,4,9,10)]<-c(" ", "  ", "   ", "    ")

# convert the resultaat and the anr to numeric
# first, get the row number
nrow<-which(input2$Cursus=="Studentnummer")

input2$Cursus<-list(input2$Cursus[1:nrow],as.numeric(input2$Cursus[(rown+1):nrow(input2)]))

write.xlsx(x = input2, file = paste0("output_files/", input2name), format_headers = T)

# write the check_file
file_check$converted<-as.character(file_check$converted)


write.csv(file_check,"output_files/check_file.csv", row.names = F )

return(list("file"= read.xlsx( paste0("output_files/", input2name)), 
            "check_file" = file_check))
}

#round_app(input1, input2)
