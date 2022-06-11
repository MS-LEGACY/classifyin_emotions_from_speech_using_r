library(shiny)
library(shinythemes)
library(wrassp)
library(wrassp)
library(stringr) 
library(readr)

library(tuneR)

library(signal)

library(oce)

library(seewave)
library(readr)

library(tuneR)

library(signal)

library(oce)

library(seewave)


ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",  # <--- To use a theme, uncomment this
                  "VOICE EMOTION DETECTOR MRIGANK",
                  tabPanel("Detect Emotion",
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Voice Sample (WAV Format)", accept = ".wav"),
    ),
    mainPanel(
      textOutput("printing_wait"),textOutput("noise_removal"), plotOutput("plot"),plotOutput("plot2"),htmlOutput('contents')
    )
  )
)
))

server <- function(input, output) {
    
    output$printing_wait = renderText({"Reading Audio File ......"})
    output$noise_removal = renderText({"Removing Silence From Audio ......"})
    file_audio = reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "wav", "Please Upload a WAV format Audio File"))
    return (file$datapath) })
    
    plot_image = function(b) 
    {
    par(mfrow = c(2,1))
    audio =  readWave(file_audio())
    s1 = (audio@left) / 2^(audio@bit - 1)
    timearray = (0:(length(audio@left)-1))/audio@samp.rate
    length(timearray)
    plot(timearray, s1, type = 'l', col = 'blue')
    return (plot(audio@left[10:40008], type ='l', 
                                    col = 'seagreen', xlab = 'Elements / Time', ylab = 'Freq', main = 'Audio Frequency Wave'))
    }
    output$plot <- renderPlot({plot_image()})
    spectrogram = function(a,nfft,fs,window,overlap) {
      
      spec = specgram(x = a,
                      n = nfft,
                      Fs = fs,
                      window = window,
                      overlap = overlap)
      # str(spec)
      P = abs(spec$S)
      P = P/max(P)    
      P = 10*log10(P)
      t = spec$t
      imagep(x = t,
             y = spec$f,
             z = t(P),
             col = oce.colorsViridis,
             ylab = 'Frequency in Hz',
             xlab = 'Time in Sec',
             main = 'Spectrogram',
             drawPalette = T,
             decimate = F)
      
    }
    plot_spectogram = function()
    {
      audio = readWave(file_audio())
      print(audio)
      sr = audio@samp.rate
      mfcc.m = melfcc(audio, sr = sr,
                      wintime = 0.015,        # STAGE 2 Window length
                      hoptime = 0.005,        # STAGE 2 Successive windown inbetween
                      # numcep = 3,           # By default it will be 12 features -  # Create equal number of frames - Equal array for every diffrent length audio file
                      sumpower = TRUE,        # frequence scale transformation based on powerspectrum
                      nbands = 40,            # Number of spectra bands, filter banks
                      bwidth = 1,             # Width of spectral bands
                      preemph = 0.95,         # STAGE 1 pre Emphasis
                      # frames_in_rows = TRUE
      )
      image(mfcc.m)
      image(as.matrix(audio@left))
      
      dur = length(mfcc.m)/audio@samp.rate
      fs = audio@samp.rate
      nfft = 512
      window = 1500
      overlap = 500
      spectrogram(as.matrix(audio@left),nfft,fs,window,overlap)
    }
    
    output$plot2 <- renderPlot({plot_spectogram()})
    
    model_output = function(audio)
    {
      library(caret)
      data_voice = read.csv('/home/mrigank/Public/WINTER_SEMESTER_2021_22/CSE3506_ESSENTIAL_OF_DATA_ANALYTICS/CSE3506_PROJECT/LPC_MFCC_FEATURES_EXTRACTED.csv',row.names=1)
      # str(data_voice)
      data_voice$class = as.factor(data_voice$class)
      levels(data_voice$class)
      
      train_ind = createDataPartition(data_voice$class,p=0.75,list=FALSE)
      train_data = data_voice[train_ind,]
      test_data = data_voice[-train_ind,]
      control_cv = trainControl(method="cv",number=10)
      metric = "Accuracy"

      # 
      # ### RANDOM FOREST 
      
      set.seed(130)
      fit.rf = train(class~ .,data=data_voice,method="rf",metric=metric,trControl=control_cv)
      # predictions = predict(fit.rf,test_data)
      # confusionMatrix(predictions,test_data$class)
      class = predict(fit.rf,audio)
      if (class==0)
      {
        return(includeHTML("OUTPUT_0.html"))
          
      }
      else if(class==1){
        return (includeHTML("OUTPUT_1.html"))
      }
        else
        {
          return (includeHTML("OUTPUT_2.html"))
        }
          
      
    }
    features_extract <- function()
    {
      audio_file = file_audio()

      num_files = 1
      number_of_cep_features = 20
      col_names <- vector(mode = "character", length = number_of_cep_features)
      
      for (i in 1:number_of_cep_features){
        col_names[i]=paste("mfcc",as.character(i),sep = "")
      }
      col_names[number_of_cep_features+1]="class"
      matrix_features = matrix(nrow=num_files,ncol=number_of_cep_features+1)
      colnames(matrix_features)=col_names
      
      for (i in 1:num_files){
        audio = readWave(audio_file)
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
      }
      
      # write.csv( matrix_features, "MFCC_FEATURES_EXTRACTED.csv")
      
      
      
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
        audio = readWave(audio_file)
        lpc = lpc(audio@left, fs=audio@samp.rate, order=number_of_cep_features, show=FALSE)
        for (j in 1:length(col_names)-1){
          matrix_features_lpc[i,j]= lpc[j+1]
        }
       
      }
     
      return (matrix_features_lpc)
    }
    output$contents <- renderUI({model_output(features_extract())})
    

}

shinyApp(ui, server)