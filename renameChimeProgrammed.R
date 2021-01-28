
## This script takes as input the following:
# The path to the directory where the files to be renamed reside
# The AudioMoth unit name in format s2l[am/lg]###
# If an offset is needed, the starting date as yymmdd
# If an offset is needed, the starting time as hhmm
# The path to the local directory where the processed folder and files will be saved e.g., "c:/tempS2L/"
# The path to the temp local directory where the zip file with raw data will be saved (automatically erased after upload to Google Drive)
# The path to the Google Drive folder where the renamed files will be saved, using File Stream e.g., "G:/My Drive/s2l/"
# The path to the Google Drive folder where the zip file with raw data will be archived, using File Stream

## This script then renames the files as needed and provides the following outputs:
# A folder within the local directory, named: unitname_deploydate, unitname is s2l[am/lg]###, and deploydate is yymmdd
# Within that folder, all files renamed as: unitname_deploydate_yyyy-mm-dd_hh-mm.WAV
# A zip file with the raw data (original file names) archived in the specified Google Drive folder
# All renamed files uploaded to the specified Google Drive folder


########################
# DEPENDENCIES
########################
library(zip); library(plyr)

checkInputPaths<-function(impPath,type){
	if(substr(impPath,nchar(impPath),nchar(impPath))!="/"){
		impPath<-paste0(impPath,"/")
	}
	if(!dir.exists(impPath) & type %in% c("goog")){
		stop(paste("The directory",impPath,"cannot be reached or does not exist. Please make sure you have mapped and can access the Google drive from the File Explorer.",
						"\n If the folder does not exist in the Google drive, create this directory and try again."))
	}
	if(!dir.exists(impPath) & type %in% c("local")){
		stop(paste("The directory",impPath,"does not exist. Please create it and try again."))
	}
	if(!dir.exists(impPath) & type=="src"){
		stop(paste("The directory",impPath,"does not exist. Please provide the correct data directory and try again."))
	}
	if(type=="src" & NROW(list.files(impPath))<50){
		stop(paste0("WARNING: there are fewer than 50 files in ", impPath,". Is this source directory correct?"))
	}
	return(impPath)
}

checkARUname<-function(amn){
	if(tolower(substr(amn,1,3))!="s2l"){
		stop("Incorrect ARU name - must begin with 's2l'")
	}
	if(!tolower(substr(amn,4,5)) %in% c("am","lg")){
		stop("Incorrect ARU name - must have unit type: lg or am")
	}
	if(nchar(amn)!=8){
		stop("Incorrect ARU name - more than 8 characters long. Expect: 8.")
	}
	if(!is.numeric(as.numeric(substr(amn,6,8)))){
		stop("Incorrect ARU name - last 3 characters must be a number")
	}
	return(tolower(amn))
}

getNewNames<-function(wavNames,sourcePath,mothUnitName,deployDate,localDestFolder,unzipGooglePath,offsetDate,offsetTime){
	
	if(offsetDate!=""){
		firstFile<-sort(wavNames)[1]
		firstDate<-substr(firstFile,1,nchar(firstFile)-4)
		start<-as.POSIXlt(firstDate,format="%Y%m%d_%H%M%S")
		end<-as.POSIXlt(paste0("20",offsetDate,"_",offsetTime,"00"),format="%Y%m%d_%H%M%S")
		diff<-end - start
	}else{
		diff<-0
	}
	
	#loop through the files, apply offset, get new name
	resdf<-ldply(wavNames,function(nn,sourcePath,mothUnitName,deployDate,localDestFolder,unzipGooglePath,diff){
				currDate<-as.POSIXlt(substr(nn,1,nchar(nn)-4),format="%Y%m%d_%H%M%S")
				newDate<-currDate + diff
				oldFilePath<-paste0(sourcePath,nn)
				newFileLocalPath<-paste0(localDestFolder,mothUnitName,"_",deployDate,"_",format(newDate,"%Y-%m-%d_%H-%M"),".WAV")
				newFileGooglePath<-paste0(unzipGooglePath,mothUnitName,"_",deployDate,"_",format(newDate,"%Y-%m-%d_%H-%M"),".WAV")
				tdf<-data.frame(srcFile=oldFilePath,destFileLocal=newFileLocalPath,destFileGoogle=newFileGooglePath)
				return(tdf)
			},sourcePath=sourcePath,mothUnitName=mothUnitName,deployDate=deployDate,localDestFolder=localDestFolder,unzipGooglePath=unzipGooglePath,diff=diff)
	resdf$srcFile<-as.character(resdf$srcFile)
	resdf$destFileLocal<-as.character(resdf$destFileLocal)
	resdf$destFileGoogle<-as.character(resdf$destFileGoogle)
	return(resdf)
}

########################
# INPUTS
########################
sourcePath<-checkInputPaths("D:/",type="src")  #Careful: ends with /
mothUnitName<-checkARUname("s2lam001")
offsetDate<-""		#set to "" if no offset needed
offsetTime<-""			#set to "" if no offset needed
localDestPath<-checkInputPaths("C:/tmp/audio/",type="local") # Mandatory: you need a local copy of renamed data to upload to Arbimon. 
localZipPath<-checkInputPaths("C:/tmp/",type="local")   # Mandatory: Local zip folder destination - need this to compress locally before uploading. 
unzipGooglePath<-checkInputPaths("G:/Shared drives/temp/",type="goog") # Mandatory: the Google destination drive for unzipped files
zipGooglePath<-checkInputPaths("G:/Shared drives/temp/",type="goog") # Mandatory: the Google destination drive for the zipped folder
numToSkipStart<-1	#Number of files at the start of survey to skip renaming and uploading (but still preserved in the zip)
numToSkipEnd<-0 	#Number of files at the tail end of survey to skip renaming and uploading (but still preserved in the zip)

#check the first 6 files in sourcePath - see if it is correct.
head(listfiles(sourcePath))

## generate essentials from the above
# get the file names
wavNames<-list.files(sourcePath,pattern=".WAV")
# get the deploy date
if(offsetDate!=""){
	deployDate<-offsetDate
}else{
	firstFile<-sort(wavNames)[1]
	deployDate<-substr(firstFile,3,8)
}

########################
# Create local target directories
########################
# create the local target directory 
localDestFolder<-paste0(localDestPath,mothUnitName,"_",deployDate,"/")
unlink(localDestFolder, recursive=TRUE)
dir.create(localDestFolder)

# create the zip target directory 
zipTargetDir<-paste0(localZipPath,mothUnitName,"_",deployDate)
unlink(zipTargetDir, recursive=TRUE)
dir.create(zipTargetDir)

########################
# ZIP output
########################
zfn<-paste0(zipTargetDir,"/",mothUnitName,"_",deployDate,".zip")
setwd(sourcePath)
zip(zipfile=zfn,files=list.files())

########################
# Copy ZIP to google drive
########################
zft<-paste0(zipGooglePath,mothUnitName,"_",deployDate,".zip")
bb<-file.copy(from=zfn,to=zft)
# then delete from source
if(bb==TRUE){file.remove(zfn);unlink(zipTargetDir, recursive=TRUE)}

########################
# RENAME & COPY output
########################
newNamesdf<-getNewNames(wavNames,sourcePath=sourcePath,mothUnitName=mothUnitName,deployDate=deployDate,localDestFolder=localDestFolder,unzipGooglePath=unzipGooglePath,offsetDate=offsetDate,offsetTime=offsetTime)
newNamesdf<-newNamesdf[order(newNamesdf$srcFile),]
sks<-1;if(numToSkipStart>0){sks<-numToSkipStart+1}
skd<-nrow(newNamesdf);if(numToSkipEnd>0){skd<-numToSkipEnd-1}
if(numToSkipStart>0 | numToSkipEnd>0){
	newNamesdf<-newNamesdf[c(sks:skd),]
}

l_ply(1:nrow(newNamesdf),function(rr,newNamesdf){
			src<-newNamesdf[rr,"srcFile"]
			destloc<-newNamesdf[rr,"destFileLocal"]
			destgoo<-newNamesdf[rr,"destFileGoogle"]
			file.copy(from=src,to=destloc)
			file.copy(from=src,to=destgoo)
		},newNamesdf=newNamesdf)

print("DONE!")
