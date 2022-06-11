library(wrassp)
library(stringr) 
library(readr)

library(tuneR)

library(signal)

library(oce)

library(seewave)
rootPath="/home/mrigank/Public/WINTER_SEMESTER_2021_22/CSE3506_ESSENTIAL_OF_DATA_ANALYTICS/CSE3506_PROJECT/Basic-Arabic-Vocal-Emotions-Dataset"
listFiles=list.files(rootPath,recursive=TRUE)

#** The above code will help in recursing through each sub folder
#*in the directory and then using it. I will extract the features of audio files
#*from the .wav files.
#*


path = rootPath
num_files = 0
for (i in listFiles){
  len = str_length(i)
  if(len-2>0) {
    format_file = substr(i, len-2, len)
    if(format_file == "wav") {
      num_files = num_files+1 }
    }
  }
print(num_files)
number_of_cep_features = 20
col_names <- vector(mode = "character", length = number_of_cep_features)

for (i in 1:number_of_cep_features){
  col_names[i]=paste("mfcc",as.character(i),sep = "")
}
col_names[number_of_cep_features+1]="class"
matrix_features = matrix(nrow=num_files,ncol=number_of_cep_features+1)
colnames(matrix_features)=col_names

for (i in 1:num_files){
  audio = readWave(paste(rootPath,listFiles[i],sep ="/"))
  pos = str_locate_all(pattern = "-", listFiles[i])
  class = substr(listFiles[i],pos[[1]][4]+1,pos[[1]][4]+1)
  class = as.integer(class)
  mfcc = melfcc(audio, sr = audio@samp.rate,
                wintime = 0.025,        # STAGE 2 Window length
                hoptime = 0.005,        # STAGE 2 Successive windown inbetween
                numcep = 20,           # By default it will be 12 features -  # Create equal number of frames - Equal array for every diffrent length audio file
                sumpower = TRUE,        # frequence scale transformation based on powerspectrum
                nbands = 40,            # Number of spectra bands, filter banks
                bwidth = 1,             # Width of spectral bands
                preemph = 0.60,         # STAGE 1 pre Emphasis
                # frames_in_rows = TRUE
                ) 
  for (j in 1:length(col_names)-1){
    matrix_features[i,j]=mean(mfcc[,j],na.rm = TRUE)
  }
  matrix_features[i,number_of_cep_features+1]=class
}

write.csv( matrix_features, "MFCC_FEATURES_EXTRACTED.csv")



# LPC FEATURES

library(phonTools)



col_names <- vector(mode = "character", length = number_of_cep_features)

for (i in 1:number_of_cep_features){
  col_names[i]=paste("lpc",as.character(i),sep = "")
}
col_names[number_of_cep_features+1]="class"
matrix_features_lpc = matrix(nrow=num_files,ncol=number_of_cep_features+1)
colnames(matrix_features_lpc)=col_names

for (i in 1:num_files){
  audio = readWave(paste(rootPath,listFiles[i],sep ="/"))
  pos = str_locate_all(pattern = "-", listFiles[i])
  class = substr(listFiles[i],pos[[1]][4]+1,pos[[1]][4]+1)
  class = as.integer(class)
  lpc = lpc(audio@left, fs=audio@samp.rate, order=number_of_cep_features, show=FALSE)
  for (j in 1:length(col_names)-1){
    matrix_features_lpc[i,j]= lpc[j+1]
  }
  matrix_features_lpc[i,number_of_cep_features+1]=class
}

write.csv( matrix_features_lpc, "LPC_FEATURES_EXTRACTED.csv")


# MFCC + LPC Combined

number_of_cep_features = 40
col_names <- vector(mode = "character", length = number_of_cep_features)

for (i in 1:number_of_cep_features/2){
  col_names[i]=paste("mfcc",as.character(i),sep = "")
}
for (i in ((number_of_cep_features/2)+1):number_of_cep_features){
  col_names[i]=paste("lpc",as.character(i),sep = "")
}

col_names[number_of_cep_features+1]="class"
matrix_features_comb = matrix(nrow=num_files,ncol=number_of_cep_features+1)
colnames(matrix_features_comb)=col_names
for (i in 1:num_files){
  audio = readWave(paste(rootPath,listFiles[i],sep ="/"))
  pos = str_locate_all(pattern = "-", listFiles[i])
  class = substr(listFiles[i],pos[[1]][4]+1,pos[[1]][4]+1)
  class = as.integer(class)
  mfcc = melfcc(audio, sr = audio@samp.rate,
                wintime = 0.025,        # STAGE 2 Window length
                hoptime = 0.005,        # STAGE 2 Successive windown inbetween
                numcep = 20,           # By default it will be 12 features -  # Create equal number of frames - Equal array for every diffrent length audio file
                sumpower = TRUE,        # frequence scale transformation based on powerspectrum
                nbands = 40,            # Number of spectra bands, filter banks
                bwidth = 1,             # Width of spectral bands
                preemph = 0.60,         # STAGE 1 pre Emphasis
                # frames_in_rows = TRUE
  ) 
  lpc = lpc(audio@left, fs=audio@samp.rate, order=number_of_cep_features, show=FALSE)
  for (j in 1:number_of_cep_features/2-1){
    matrix_features_comb[i,j]=mean(mfcc[,j],na.rm = TRUE)
  }
  k = 0
  for (j in ((number_of_cep_features/2)+1):number_of_cep_features){
    matrix_features_comb[i,j]= lpc[k+1]
    k=k+1
  }
  matrix_features_comb[i,number_of_cep_features+1]=class
}
write.csv( matrix_features_lpc, "LPC_MFCC_FEATURES_EXTRACTED.csv")
