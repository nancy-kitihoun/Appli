library(shiny)
library(shinydashboard)
library(DT)
library(reshape)
library(funModeling)
library(GGally)
library(dplyr)
library(datasets)
library(ggplot2)
library(e1071)
library(randomForest)
library(tree)
library(caret)
library(parallelSVM)
library(gbm)

options(shiny.maxRequestSize=150*1024^2)


plot_num_density <- function (data, path_out = NA) 
{
    wide_data = suppressMessages(melt(data))
    p = ggplot(data = wide_data,  aes(x = value)) + 
        geom_density( na.rm = T) + 
        facet_wrap(~variable, 
                   scales = "free") + aes(fill = variable) + guides(fill = FALSE)
    if (!is.na(path_out)) {
        export_plot(p, path_out, "density")
    }
    plot(p)
}

tabl=function(data,b){
    
    set.seed(123)
    attach(data)
    da1=data[Class==1,]
    
    dao=data[Class==0,]
    no=nrow(dao)
    
    p=which(Class==0)
    
    ind=sample(p, no*b)
    part=data[ind,]
    
    new=rbind(part,da1)
    neawdat=sample(1:nrow(new),nrow(new))
    newdata=new[neawdat,]
    attach(newdata)
    newdata
}

# b pouvent aller de 0,001 à 0.005

# ind=sample(2, nrow(data()), replace=TRUE ,prob=c(0.7, 0.3))
# 
# train=data[ind==1,]
# test=data[ind==2,]
# 
# 
# und=reactive({
#   s=ovun.sample(Class~., data=train(), method="under", N=1000)
#   s$data
#   
# })  

shinyServer(function(input, output,session) {
    
    #importation 
    
    #saveRDS(data,file='C:/Users/camille/Desktop/Documents/GitHub/Application/genre/creditcard.rds')
    
 data=reactive({
      readRDS(file='D:/Github/Appli/genre/genre/creditcard.rds')
})
        
        
        
    })
    set.seed(1234)
    
    output$dataTable = DT::renderDataTable( {
        datatable(data(), extensions = 'FixedColumns',editable = 'cell',
                  rownames = F, options = list(pageLength = 5, dom = 'tp',scrollX = TRUE,
                                               fixedColumns = TRUE),
                  selection = list(mode = "single",
                                   target = "column", selected = 4)
        )
    })
    output$Des1 =DT::renderDataTable({ 
        datatable(profiling_num(data()),extensions = 'FixedColumns',editable = 'cell',
                  rownames = F, options = list(pageLength = 5, dom = 'tp',scrollX = TRUE,
                                               fixedColumns = TRUE),
                  selection = list(mode = "single",
                                   target = "column", selected = 4) )})
    
    output$Des2 =DT::renderDataTable({ 
        datatable(df_status(data()),extensions = 'FixedColumns',editable = 'cell',
                  rownames = F, options = list(pageLength = 5, dom = 'tp',scrollX = TRUE,
                                               fixedColumns = TRUE),
                  selection = list(mode = "single",
                                   target = "column", selected = 4) )})
    
    m=reactive({
        switch(input$variable1,
               
               "Time"= Time , "V1"=V1 ,"V2"=V2, "V3"=V3 ,"V4"=V4, "V5"=V5,"V6"=V6,"V7"=V7 ,"V8"=V8 ,
               "V9"=V9,"V10"=V10,"V11"=V11 , "V12"=V12,"V13"=V13,"V14"=V14  , 
               "V15"=V15, "V16"=V16   , "V17"=V17   , "V18"=V18 ,   "V19"=V19  ,  "V20"=V20  ,  "V21"=V21  ,  
               "V22"=V22, "V23"=V23, "V24"=V24, "V25"=V25 , "V26"=V26, "V27"=V27  ,  "V28"=V28 , "Amount"=Amount)
        
        
        
    })
    
    n=reactive({
        switch(input$variable2,
               
               "Time"= Time , "V1"=V1 ,"V2"=V2, "V3"=V3 ,"V4"=V4, "V5"=V5,"V6"=V6,"V7"=V7 ,"V8"=V8 ,
               "V9"=V9,"V10"=V10,"V11"=V11 , "V12"=V12,"V13"=V13,"V14"=V14  , 
               "V15"=V15, "V16"=V16   , "V17"=V17   , "V18"=V18 ,   "V19"=V19  ,  "V20"=V20  ,  "V21"=V21  ,  
               "V22"=V22, "V23"=V23, "V24"=V24, "V25"=V25 , "V26"=V26, "V27"=V27  ,  "V28"=V28 , "Amount"=Amount)
        
        
        
    })
    
    
    output$Grap1 <- renderPlot({ggplot(data(), aes(x=m(), y=n()))})
    output$Grap2<- renderPlot({freq(data()[,31])})
    output$tabCor <-renderPlot({ ggcorr(data()[,-31])})
    
    # pour une page qui affiche tout un graph complet 
    
    
    
    output$Grap3<- renderPlot({plot_num_density(data()[,c(-31)])})
    
    # partie analyse
    # svm
    train=reactive({
        new=tabl(data(),0.002)
        ind=sample(1:nrow(new),nrow(new)*0.7)
        tr=new[ind,]
        tr
    })
    
    test=reactive({
        new=tabl(data(),0.002)
        ind=sample(1:nrow(new),nrow(new)*0.7)
        tes=new[-ind,]
        tes
        
    })
    
    svm=reactive({
        
        s=parallelSVM(Class~.,data=train(), kernel="linear",scale=F, type= 'C')
        s
    })
    
    output$svm=renderPrint(svm())
    
    
    
    predicte=reactive({
        
        svm.pred=predict(svm(),test()[,-31])
        #svm.pred=as.factor(svm.pred1)
        t=confusionMatrix(svm.pred,test())
        t
    })
    
    output$performence =renderPrint({predicte()})
    
    
    #roc=reactive({Sys.sleep(1)
    #roc.curve(predicte(),curve = TRUE,max.compute = TRUE, 
    #  min.compute = TRUE, rand.compute = TRUE)})
    
    # output$ROC=reactive({renderPlot( {plot(roc(),max.plot = TRUE, 
    #                                       min.plot = TRUE, rand.plot = TRUE, fill.area = TRUE)})
    # })
    
    #output$ROC= renderPlot( {plot(roc(),max.plot = TRUE, min.plot = TRUE, rand.plot = TRUE, fill.area = TRUE)})
    
    p= roc.curve(met.pred,test$Class,curve = TRUE,max.compute = TRUE, 
                 min.compute = TRUE, rand.compute = TRUE)
    plot(p,max.plot = TRUE, min.plot = TRUE, rand.plot = TRUE, fill.area = TRUE)
    
    #abre de classification
    tre=reactive({
        tree(Class~., train())})
    
    output$tree=renderPrint({ summary(tre())})
    
    output$abre= renderPlot({plot(tre())
        text(tre(), pretty=0)
        
    })
    
    
    
    # gradient boosting
    boost=reactive({
        trai=as.data.frame(train())
        g=gbm(class~., data=trai, distribution="gaussian", n.trees= 500, interaction=4)
        g})
    
    output$gradient=renderPrint(summary(boost()))
    
    
})

