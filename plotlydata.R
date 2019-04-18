library(ggplot2)
library(shiny)
library(ggpubr)
library(stringr)
library(plotly)

options(shiny.maxRequestSize=50*1024^2)
sliderMax <- 1
sliderMin <- 0
pulse.x.CH2.2.idx<-vector("numeric",length = 0)
# Define UI for data upload app ----
ui <- fluidPage(
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      h3("设备数据分析"),
      fluidRow(     
        column(3,
               
               radioButtons("disp", "显示:",
                            choices = c(合图 = "One",分图 = "Many"),selected = "Many")
        ),
        column(9,
               fileInput("file1", "请选择CSV文件",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
        )
      ),
      fluidRow(     
        
        column(6,
               uiOutput("checkboxGroupInput"),
               actionButton('btn', '刷新', icon=icon('refresh'))
        ),
        column(6,
               uiOutput("ddl_YoffsetCH"),
               uiOutput("numericInput_Yoffset")
        )
      ),
      
      tableOutput("values")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(     
        column(9,
               # uiOutput("slider"),
               absolutePanel(
                 top = 0, left=10, width=1, draggable = TRUE, style="float:left; padding: 0px;border: 5px solid #666666;opacity:0.5;",
                 HTML( '<div style="float:left;margin-top: 30px;width: 1px;height: 1000px; background: #666666;"></div>' )
               ),
               # plotOutput("sepratePlot",width = "100%", height = "900px",brush='pl_brush'),
               plotlyOutput("plotlyCH1.1", height = "1000px")
        ),
        column(3,
               uiOutput("sliderRegion"),
               tabsetPanel(
                 type = "pills",
                 tabPanel("初始显示",tableOutput("InitValues")),
                 navbarMenu(
                   title = "分通道",
                   tabPanel("CH1.1.V.物料感应"),
                   tabPanel("CH1.2.V.植入上感应"),
                   tabPanel("CH1.3.V.植入下感应"),
                   tabPanel("CH1.4.V.检测1感应"),
                   tabPanel("CH2.1.V.检测2感应"),
                   tabPanel("CH2.2.V.中间盘感应",tableOutput("SenseValues")),
                   tabPanel("CH2.3.V.POS PIN 感应"),
                   tabPanel("CH2.4.V.POS PIN 位置感应"),
                   tabPanel("CH3.1.V.分离感应"),
                   tabPanel("CH3.2.V.中间盘动作",tableOutput("ActionValues")),
                   tabPanel("CH3.3.V.植入动作"),
                   tabPanel("CH3.4.V.上烙铁动作"),
                   tabPanel("CH4.1.V.POS PIN 动作"),
                   tabPanel("CH4.2.V.分离动作"),
                   tabPanel("CH4.3.V.检测1动作"),
                   tabPanel("CH4.4.V.检测2动作")),
                 tabPanel("区域数据",tableOutput("RegionValues"))
               )
        )
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  csvdata <- reactive({ 
    # print(input$obs)
    req(input$file1)
    df1<-read.csv(input$file1$datapath,
                  header = TRUE,
                  sep = ',',
                  quote = '""',skip = 100,nrows = 1)
    # print(length(df1))
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ',',
                   quote = '""',skip = 6+length(df1)-1)
  })
  # pts <- reactiveValues(sel=rep(FALSE, nrow(csvdata())))
  
  output$slider <- renderUI({
    df<-csvdata()
    vectorTime=df$Time.s.
    sliderMax<-max(vectorTime)
    sliderMin<-min(vectorTime)
    sliderInput(inputId="slider",NULL,  min = sliderMin,max = sliderMax, width = 1000,value = c(sliderMin,sliderMax), step = 0.0005)
  })
  output$sliderRegion <- renderUI({
    df<-csvdata()
    vectorTime=df$Time.s.
    sliderMax<-max(vectorTime)
    sliderMin<-min(vectorTime)
    sliderInput("sliderRegion", NULL, min=sliderMin,max=sliderMax,value = c(sliderMin,sliderMax))
  })
  output$checkboxGroupInput <- renderUI({
    df<-csvdata()
    vectorCHs<- colnames(df)
    CHs.16<-c("CH1.1.V.","CH1.2.V.","CH1.3.V.","CH1.4.V.","CH2.1.V.","CH2.2.V.","CH2.3.V.","CH2.4.V.","CH3.1.V.","CH3.2.V.","CH3.3.V.","CH3.4.V.","CH4.1.V.","CH4.2.V.","CH4.3.V.","CH4.4.V.")
    CHs.16_Remarks<-c("物料感应","植入上感应","植入下感应","检测1感应","检测2感应","中间盘感应","POS PIN 感应","POS PIN 位置感应","分离感应","中间盘动作","植入动作","上烙铁动作","POS PIN 动作","分离动作","检测1动作","检测2动作")
    
    CHs.FullName<-vector(mode="character",length=0)
    for (variable in vectorCHs) {
      idx<-which(CHs.16 == variable)
      tempCHs.FullName<-paste(variable,CHs.16_Remarks[idx])
      CHs.FullName<-c(CHs.FullName,tempCHs.FullName)
    }
    # print(CHs.FullName)
    checkboxGroupInput("SelectedCHs", "请勾选通道:",choices = CHs.FullName[-1],selected=CHs.FullName[-1],inline = FALSE)
  })
  output$ddl_YoffsetCH <- renderUI({
    df<-csvdata()
    vectorCHs<- colnames(df)[-1]
    selectInput("ddloffsetCH", "请设置通道Y偏移量:（合图时）", 
                choices = vectorCHs)
  })
  output$numericInput_Yoffset <- renderUI({
    numericInput("numYoffset", "Y 偏移量：", 1)
  })
  
  InitValues <- reactive({
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    #处理动作波形CH3.2
    pulse.x.CH3.2<- funGetPluseX(vectorTime,vectorCH3.2,2,10)[[1]]
    Cycle.Qty.CH3.2 <- funGetCycleQty(pulse.x.CH3.2,8)
    Cycle.Each.TimeSpan <- funGet.Cycle.Each.TimeSpan(Cycle.Qty.CH3.2,pulse.x.CH3.2,8)
    Cycle.diff.SortedIdx <- funGet.timespan.Outliers(Cycle.Each.TimeSpan)[[2]]
    #速度v,1分钟有多少个周期，60000/平均周期
    v<-60/mean(Cycle.Each.TimeSpan)
    
    # Compose data frame
    data.frame(
      Item = c("速度",
               "周期",
               "异常周期"),
      Value = as.character(c(
        signif(v,7),
        signif(mean(Cycle.Each.TimeSpan),7),
        str_c(Cycle.diff.SortedIdx,collapse=',')
      ), 
      stringsAsFactors=FALSE)
    )
  })
  ActionSignalValues <- reactive({
    
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    
    #处理动作波形CH3.2
    pulse.x.CH3.2<- funGetPluseX(vectorTime,vectorCH3.2,2,10)[[1]]
    #  #处理感应波形CH2.2
    pulse.x.CH2.2<- funGetPluseX(vectorTime,vectorCH2.2,7,10)[[1]]
    pulse.x.CH2.2.idx<-funGetPluseX(vectorTime,vectorCH2.2,7,10)[[2]]
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    #周期总数=动作波形突变点数/8的商
    Cycle.Qty.CH3.2 <- funGetCycleQty(pulse.x.CH3.2,8)
    # print(Cycle.Qty.CH3.2)
    #CH3.2异常周期（初始显示）
    Cycle.Each.TimeSpan <- funGet.Cycle.Each.TimeSpan(Cycle.Qty.CH3.2,pulse.x.CH3.2,8)
    Cycle.diff.SortedIdx <- funGet.timespan.Outliers(Cycle.Each.TimeSpan)[[2]]
    #CH3.2异常周期（初始显示）
    
    #CH3.2高电平所有点
    highPoints.CH3.2<-vectorCH3.2[vectorCH3.2>4]#c(df$CH2.2.V.[time.Idxeffect[1]:time.Idxeffect[2]],df$CH2.2.V.[time.Idxeffect[3]:time.Idxeffect[4]])
    #CH3.2低电平所有点
    lowPoints.CH3.2<-vectorCH3.2[vectorCH3.2<1]
    
    list.funGet.8pulse.timespan<-funGet.8pulse.timespan(pulse.x.CH3.2,8,pulse.x.CH2.2,2,Cycle.Qty.CH3.2)
    # high.First.y.Points<-vector(mode="numeric",length=0)
    high.1st.timespan <- list.funGet.8pulse.timespan[[1]]
    high.1stEnd2ndStart.timespan <- list.funGet.8pulse.timespan[[2]]
    high.2nd.timespan <- list.funGet.8pulse.timespan[[3]]
    high.1stStart2ndStart.timespan <- list.funGet.8pulse.timespan[[4]]
    high.2ndStart3rdStart.timespan <- list.funGet.8pulse.timespan[[5]]
    high.3rdStart4thStart.timespan <- list.funGet.8pulse.timespan[[6]]
    thisCycleEnd_nextCycleStart.timespan <- list.funGet.8pulse.timespan[[7]]
    Start.CH3.2_CH2.2.timespan <- list.funGet.8pulse.timespan[[8]]
    End.CH3.2_CH2.2.timespan <- list.funGet.8pulse.timespan[[9]]
    
    Start.CH3.2_CH2.2.timespan.diff.SortedIdx<- funGet.timespan.Outliers(Start.CH3.2_CH2.2.timespan)[[2]]
    End.CH3.2_CH2.2.timespan.diff.SortedIdx<-funGet.timespan.Outliers(End.CH3.2_CH2.2.timespan)[[2]]
    
    # Compose data frame
    data.frame(
      CH3_2 = c(
        "周期时间",
        "高电平均值",
        "第一次高电平持续时间均值",
        "单周期内第一次高电平结束到第二次高电平起始时间差记录均值",
        "第二次高电平持续时间均值",
        "单周期内第一次高电平与第二次高电平时间差记录",
        "单周期内第二次高电平与第三次高电平时间差记录",
        "单周期内第三次高电平与第四次高电平时间差记录",
        "最后一次动作信号结束到下次信号起始时间",
        "单次关联动作时序起始位置差均值",
        "单次关联动作时序起始位置差最大值",
        "单次关联动作时序起始位置差最小值",
        "单次关联动作时序起始异常位置",
        "单次关联动作时序结束位置差均值",
        "单次关联动作时序结束位置差最大值",
        "单次关联动作时序位置位置差最小值",
        "单次关联动作时序结束异常位置"
      ),
      CH3_2_Value = as.character(c(
        signif(mean(Cycle.Each.TimeSpan),7),
        signif(mean(highPoints.CH3.2),7),
        signif(mean(high.1st.timespan),7),
        signif(mean(high.1stEnd2ndStart.timespan),7),
        signif(mean(high.2nd.timespan),7),
        signif(mean(high.1stStart2ndStart.timespan),7),
        signif(mean(high.2ndStart3rdStart.timespan),7),
        signif(mean(high.3rdStart4thStart.timespan),7),
        signif(mean(thisCycleEnd_nextCycleStart.timespan),7),
        signif(mean(Start.CH3.2_CH2.2.timespan),7),
        signif(max(Start.CH3.2_CH2.2.timespan),7),
        signif(min(Start.CH3.2_CH2.2.timespan),7),
        str_c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=','),
        signif(mean(End.CH3.2_CH2.2.timespan),7),
        signif(max(End.CH3.2_CH2.2.timespan),7),
        signif(min(End.CH3.2_CH2.2.timespan),7),
        str_c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=',')
      ), 
      stringsAsFactors=FALSE)
      
    )
  }) 
  SenseSignalValues <- reactive({
    
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    
    #处理动作波形CH3.2
    pulse.x.CH3.2<- funGetPluseX(vectorTime,vectorCH3.2,2,10)[[1]]
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    Cycle.Qty.CH3.2<-funGetCycleQty(pulse.x.CH3.2,8)
    # print(Cycle.Qty.CH3.2)
    
    #  #处理感应波形CH2.2
    pulse.x.CH2.2<- funGetPluseX(vectorTime,vectorCH2.2,7,10)[[1]]
    pulse.x.CH2.2.idx<-funGetPluseX(vectorTime,vectorCH2.2,7,10)[[2]]
    # print(paste("CH2.2 x节点：",pulse.x.CH2.2))
    # print(paste("嘿嘿嘿嘿",pulse.x.CH2.2.idx))
    Cycle.Qty.CH2.2<-funGetCycleQty(pulse.x.CH2.2,2)
    # print(Cycle.Qty.CH2.2)
    #处理感应波形CH2.2
    
    highPoints.CH2.2 <- funGet.HighLow.2Pulse.PerCycle(vectorCH2.2,pulse.x.CH2.2.idx,Cycle.Qty.CH2.2)[[1]]
    lowPoints.CH2.2 <- funGet.HighLow.2Pulse.PerCycle(vectorCH2.2,pulse.x.CH2.2.idx,Cycle.Qty.CH2.2)[[2]]
    
    list.pulse.timespan.CH2.2 <- funGet.pulse.timespan(pulse.x.CH3.2,8,pulse.x.CH2.2,2,Cycle.Qty.CH2.2)
    # cnt <- 0
    high.CH2.2.timespan<-list.pulse.timespan.CH2.2[[1]]
    low.CH2.2.timespan<-list.pulse.timespan.CH2.2[[2]]
    Start.CH3.2_CH2.2.timespan<-list.pulse.timespan.CH2.2[[3]]
    End.CH3.2_CH2.2.timespan<-list.pulse.timespan.CH2.2[[4]]
    
    Start.CH3.2_CH2.2.timespan.diff.SortedIdx<- funGet.timespan.Outliers(Start.CH3.2_CH2.2.timespan)[[2]]
    End.CH3.2_CH2.2.timespan.diff.SortedIdx<-funGet.timespan.Outliers(End.CH3.2_CH2.2.timespan)[[2]]
    
    #CH2.2总周期时间段
    TotalCycle.TimeSpan.CH2.2<-pulse.x.CH2.2[2*Cycle.Qty.CH2.2+1]-pulse.x.CH2.2[1]
    #CH2.2平均周期
    Cycle.avg.CH2.2<-TotalCycle.TimeSpan.CH2.2/Cycle.Qty.CH2.2
    # print(Cycle.avg.CH2.2)
    
    highPoints.CH2.2.Max5Point.vectorTime <- funGet.Y.Outliers(highPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    high.CH2.2.timespan.diff.SortedIdx <- funGet.timespan.Outliers(high.CH2.2.timespan)[[2]]
    # print(highPoints.CH2.2.Max5Point.vectorTime)
    
    lowPoints.CH2.2.Max5Point.vectorTime<- funGet.Y.Outliers(lowPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    low.CH2.2.timespan.diff.SortedIdx <- funGet.timespan.Outliers(low.CH2.2.timespan)[[2]]
    
    # Compose data frame
    data.frame(
      CH2_2 = c(
        "周期均值", 
        "高电平均值",
        "高电平最大值",
        "高电平最小值",
        "单次高电平异常位置",
        "低电平均值", 
        "低电平最大值",
        "低电平最小值",
        "单次低电平异常位置",
        "单次高电平持续时间均值",
        "单次高电平持续时间最大值",
        "单次高电平持续时间最小值",
        "单次高电平持续时间异常位置",
        "单次低电平持续时间均值",
        "单次低电平持续时间最大值",
        "单次低电平持续时间最小值",
        "单次低电平持续时间异常位置",
        "单次关联动作时序起始位置差均值",
        "单次关联动作时序起始位置差最大值",
        "单次关联动作时序起始位置差最小值",
        "单次关联动作时序起始异常位置",
        "单次关联动作时序结束位置差均值",
        "单次关联动作时序结束位置差最大值",
        "单次关联动作时序位置位置差最小值",
        "单次关联动作时序结束异常位置"
      ),
      CH2_2_Value = as.character(c(
        signif(Cycle.avg.CH2.2,7),#nrow(csvdata()),#1,#max(csvdata$Time.s.), 
        signif(mean(highPoints.CH2.2),7),
        signif(max(highPoints.CH2.2),7),
        signif(min(highPoints.CH2.2),7),
        str_c(highPoints.CH2.2.Max5Point.vectorTime,collapse=','),
        signif(mean(lowPoints.CH2.2),7),
        signif(max(lowPoints.CH2.2),7),
        signif(min(lowPoints.CH2.2),7),
        str_c(lowPoints.CH2.2.Max5Point.vectorTime,collapse=','),
        signif(mean(high.CH2.2.timespan),7),
        signif(max(high.CH2.2.timespan),7),
        signif(min(high.CH2.2.timespan),7),
        str_c(high.CH2.2.timespan.diff.SortedIdx,collapse=','),
        signif(mean(low.CH2.2.timespan),7),
        signif(max(low.CH2.2.timespan),7),
        signif(min(low.CH2.2.timespan),7),
        str_c(low.CH2.2.timespan.diff.SortedIdx,collapse=','),
        signif(mean(Start.CH3.2_CH2.2.timespan),7),
        signif(max(Start.CH3.2_CH2.2.timespan),7),
        signif(min(Start.CH3.2_CH2.2.timespan),7),
        str_c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=','),
        signif(mean(End.CH3.2_CH2.2.timespan),7),
        signif(max(End.CH3.2_CH2.2.timespan),7),
        signif(min(End.CH3.2_CH2.2.timespan),7),
        str_c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=',')
      ), 
      stringsAsFactors=FALSE)
      
    )
  }) 
  RegionSelectedValues <- reactive({
    Region.Selected <- input$sliderRegion
    # print(paste("哈哈哈哈哈",Region.Selected))
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    
    vectorCH2.2 <- funGetRegion.SelectedData(vectorTime,vectorCH2.2,Region.Selected)
    vectorCH3.2 <- funGetRegion.SelectedData(vectorTime,vectorCH3.2,Region.Selected)
    
    #判断区间是否有一个周期
    pulse.x.CH3.2<- funGetPluseX(vectorTime,vectorCH3.2,2,10)[[1]]
    Cycle.Qty.CH3.2<-funGetCycleQty(pulse.x.CH3.2,8)
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    # print(Cycle.Qty.CH3.2)
    
    pulse.x.CH2.2<- funGetPluseX(vectorTime,vectorCH2.2,7,10)[[1]]
    pulse.x.CH2.2.idx<-funGetPluseX(vectorTime,vectorCH2.2,7,10)[[2]]
    # print(paste("CH2.2 x节点：",pulse.x.CH2.2))
    # print(paste("嘿嘿嘿嘿",pulse.x.CH2.2.idx))
    Cycle.Qty.CH2.2<-funGetCycleQty(pulse.x.CH2.2,2)
    # print(Cycle.Qty.CH2.2)
    #处理感应波形CH2.2
    
    list.HighLow.2Pulse.PerCycle <- funGet.HighLow.2Pulse.PerCycle(vectorCH2.2,pulse.x.CH2.2.idx,Cycle.Qty.CH2.2)
    highPoints.CH2.2 <- list.HighLow.2Pulse.PerCycle[[1]]
    lowPoints.CH2.2 <- list.HighLow.2Pulse.PerCycle[[2]]
    
    list.pulse.timespan.CH2.2 <- funGet.pulse.timespan(pulse.x.CH3.2,8,pulse.x.CH2.2,2,Cycle.Qty.CH2.2)
    
    high.CH2.2.timespan<-list.pulse.timespan.CH2.2[[1]]
    low.CH2.2.timespan<-list.pulse.timespan.CH2.2[[2]]
    Start.CH3.2_CH2.2.timespan<-list.pulse.timespan.CH2.2[[3]]
    End.CH3.2_CH2.2.timespan<-list.pulse.timespan.CH2.2[[4]]
    
    highPoints.CH2.2.Max5Point.vectorTime <- funGet.Y.Outliers(highPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    list.highPoints.CH2.2.timespan.Outliers <- funGet.timespan.Outliers(high.CH2.2.timespan)
    high.CH2.2.Timespan.diff.Max <- list.highPoints.CH2.2.timespan.Outliers[[1]][1]
    
    lowPoints.CH2.2.Max5Point.vectorTime<- funGet.Y.Outliers(lowPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    list.lowPoints.CH2.2.timespan.Outliers <- funGet.timespan.Outliers(low.CH2.2.timespan)
    low.CH2.2.timespan.diff.Max <- list.lowPoints.CH2.2.timespan.Outliers[[1]][1]
    
    list.Start.CH3.2_CH2.2.timespan.Outliers <- funGet.timespan.Outliers(Start.CH3.2_CH2.2.timespan)
    Start.CH3.2_CH2.2.timespan.diff.Max <-list.Start.CH3.2_CH2.2.timespan.Outliers[[1]][1]
    
    list.End.CH3.2_CH2.2.timespan.Outliers <- funGet.timespan.Outliers(End.CH3.2_CH2.2.timespan)
    End.CH3.2_CH2.2.timespan.diff.Max <-list.End.CH3.2_CH2.2.timespan.Outliers[[1]][1]
    
    list.8pulse.timespan <- funGet.8pulse.timespan(pulse.x.CH3.2,8,pulse.x.CH2.2,2,Cycle.Qty.CH3.2)
    high.1stStart2ndStart.timespan <- list.8pulse.timespan[[4]]
    high.2ndStart3rdStart.timespan <- list.8pulse.timespan[[5]]
    high.3rdStart4thStart.timespan <- list.8pulse.timespan[[6]]
    thisCycleEnd_nextCycleStart.timespan <- list.8pulse.timespan[[7]]
    
    # Compose data frame
    data.frame(
      CH2_2 = c(
        "单次高电平持续时间", 
        "单次高电平最大差值",
        "单次低电平持续时间",
        "单次低电平最大差值",
        "关联信号单周期时序起始位置差均值",
        "关联信号单周期时序起始位置差与均值最大差值", 
        "关联信号单周期时序结束位置差均值",
        "关联信号单周期时序结束位置差与均值最大差值", 
        "动作信号单周期内第一次高电平与第二次高电平时间差均值",
        "动作信号单周期内第二次高电平与第三次高电平时间差均值",
        "动作信号单周期内第三次高电平与第四次高电平时间差均值",
        "动作信号最后一次动作信号结束到下次信号起始时间差均值"
      ),
      CH2_2_Value = as.character(c(
        signif(mean(high.CH2.2.timespan),7),
        signif(mean(high.CH2.2.Timespan.diff.Max),7),
        signif(mean(low.CH2.2.timespan),7),
        signif(mean(low.CH2.2.timespan.diff.Max),7),
        # paste("时序起始位置差",str_c(mean(Start.CH3.2_CH2.2.timespan),collapse=',')),
        signif(mean(Start.CH3.2_CH2.2.timespan),7),
        signif(mean(Start.CH3.2_CH2.2.timespan.diff.Max),7),
        signif(mean(End.CH3.2_CH2.2.timespan),7),
        signif(mean(End.CH3.2_CH2.2.timespan.diff.Max),7),
        signif(mean(high.1stStart2ndStart.timespan),7),
        signif(mean(high.2ndStart3rdStart.timespan),7),
        signif(mean(high.3rdStart4thStart.timespan),7),
        signif(mean(thisCycleEnd_nextCycleStart.timespan),7)
      ), 
      stringsAsFactors=FALSE)
    )
  })
  
  # Show the values using an HTML table
  output$InitValues <- renderTable({
    InitValues()
  })
  output$ActionValues <- renderTable({
    ActionSignalValues()
  })
  output$SenseValues <- renderTable({
    SenseSignalValues()
  })
  
  output$RegionValues<-renderTable({
    RegionSelectedValues()
  })
  
  # Show plot
  # output$sepratePlot <- renderPlot({
  #   df<-csvdata()
  #   vectorCHs<- colnames(df)[-1]
  #   # print(vectorCHs)
  #   if(input$disp == "One") {
  #     # ggplot(df, aes(df$Time.s.)) + 
  #     #   geom_line(aes(y = df$CH2.2.V., colour = "var0")) + 
  #     #   geom_line(aes(y = df$CH3.2.V., colour = "var1"))+
  #     #   scale_x_continuous(limits=input$slider)
  #     Y.offset<-vector(mode="numeric",length=length(vectorCHs))
  #     CHIdx<-which(vectorCHs==input$ddloffsetCH)
  #     Y.offset[CHIdx]<-input$numYoffset
  #     # print(Y.offset)
  #     
  #     SelectedCHs <- c(input$SelectedCHs)
  #     SelectedCHs<-substring(SelectedCHs, 1, 8)
  #     p<- ggplot(df, aes(df$Time.s.))+scale_x_continuous(limits=input$slider)
  #     for (v in SelectedCHs) {
  #       idx<-which(vectorCHs==v)#获取勾选的通道的索引
  #       nam <- paste("plot", vectorCHs[idx], sep = ".")
  #       Yoffset<-Y.offset[idx]
  #       # print(Yoffset)
  #       YPoints<-df[,idx+1]+Yoffset
  #       pcolour<-as.character(vectorCHs[idx]) 
  #       nam<- funGetgeom_line(YPoints,pcolour)
  #       p<-p+nam
  #       print(p)
  #     }
  #   }
  #   else {
  #     SelectedCHs <- c(input$SelectedCHs)
  #     SelectedCHs<-substring(SelectedCHs, 1, 8)
  #     # print(SelectedCHs)
  #     plotlist1<-list()
  #     for (v in SelectedCHs) {
  #       idx<-which(vectorCHs==v)#获取勾选的通道的索引
  #       nam <- paste("plot", SelectedCHs[idx], sep = ".")
  #       nam<- funGetPlot(df,idx+1,input$slider,SelectedCHs[idx])
  #       print(nam)#这是一个超级大坑，一定要print出来，否则报错
  #       plotlist1[[idx]]<- nam
  #     }
  #     x = length(plotlist1)
  #     cols = round(sqrt(x),0)
  #     rows = ceiling(x/cols)
  #     cols=1
  #     rows=length(plotlist1)
  #     bl <- (length(plotlist1)==0)
  #     if(bl)
  #     {}
  #     else
  #     {
  #       ggarrange(plotlist = plotlist1, ncol=cols, nrow = rows)
  #     }
  #     # ggarrange(plotlist = plotlist1, ncol=cols, nrow = rows)
  #     # do.call("ggarrange", c(plist, ncol=nCol))
  #     # ggarrange(plist,ncol=1,nrow=2,labels=c("A","B"))
  #   }
  # })
  SelectedCHsData <- reactive({
    input$btn
    df<- csvdata()
    vectorCHs<- colnames(df)
    if(length(isolate(input$SelectedCHs))==0)#第一次加载时显示所有通道
    {
      df
    }
    else
    {
      print(length(isolate(input$SelectedCHs))) 
      SelectedCHs <- c(isolate(input$SelectedCHs))
      SelectedCHs <- substring(SelectedCHs, 1, 8)
      
      SelectedCHs.idx <- vector(mode="numeric",length = 0)
      for (variable in SelectedCHs) {
        idx <- which(vectorCHs == variable)
        SelectedCHs.idx <- c(SelectedCHs.idx,idx)
        print(SelectedCHs.idx)
      }
      df<- df[,c(1,SelectedCHs.idx)]
    }
  })
  output$plotlyCH1.1 <- renderPlotly({
    df<-SelectedCHsData()
    vars <- setdiff(names(df), "Time.s.")
    plots <- lapply(vars, function(var) {
      plot_ly(df, x = ~df$Time.s., y = as.formula(paste0("~", var))) %>%
        add_lines(name = var)
    })
    subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

  })
}
# Create a function to print squares of numbers in sequence.
funGetPlot <- function(df,n,slideID,YText) {
  # df<-csvdata()
  ggplot(data = df, mapping = aes(x = df$Time.s., y = df[,n])) + 
    geom_line()+ labs(x="Time[s]",y=YText)+#+geom_point() 
    scale_x_continuous(limits=slideID)
  # theme(
  #    # panel.background = element_rect(fill = "transparent",colour = NA), 
  #    # panel.grid.minor = element_blank(), 
  #    # panel.grid.major = element_blank(),
  #    plot.background = element_rect(fill = "transparent",colour = NA))
}	

funGetgeom_line <- function(n,CH) {
  geom_line(aes(y = n, colour = CH ))
}	

funGetCycleQty <- function(vector,pulseQtyperCycle){
  # print(vector)
  # print(paste("xiiiiiiii",pulseQtyperCycle)) 
  if((length(vector)) %% pulseQtyperCycle == 0)
  {
    Cycle.Qty<-(length(vector)) %/% pulseQtyperCycle-1
  }
  else
  {
    Cycle.Qty<-(length(vector)) %/% pulseQtyperCycle
  }
}

funGetRegion.SelectedData<-function(vectorX,vectorY,Region.Selected){
  Region.min <- Region.Selected[1]
  Region.max <- Region.Selected[2]
  Region.min.idx<-which(vectorX == Region.min)
  Region.max.idx<-which(vectorX == Region.max)
  vectorY<-vectorY[Region.min.idx:Region.max.idx]
}

#vectorCH3.2,Threshold.Y.1st=2,Threshold.X.2nd=10
#vectorCH2.2,Threshold.Y.1st=2,Threshold.X.2nd=10
funGetPluseX<-function(vectorX,vector.Y,Threshold.Y.1st,Threshold.X.2nd)
{
  vector.Y.1stDiff<-vector.Y[-1]-vector.Y[-length(vector.Y)]
  vector.Y.1stDiff.filtered<- vector.Y.1stDiff[abs(vector.Y.1stDiff) > Threshold.Y.1st] #
  vector.X.1stDiff<-which(abs(vector.Y.1stDiff) > Threshold.Y.1st)
  
  vector.X.2ndDiff<-vector.X.1stDiff[-1]-vector.X.1stDiff[-length(vector.X.1stDiff)]
  Pulse.X.idx<-vector.X.1stDiff[vector.X.2ndDiff>Threshold.X.2nd]+1
  Pulse.X<-vectorX[Pulse.X.idx]
  Pulse.X <- Pulse.X[!is.na(Pulse.X)]
  outPara<-list(Pulse.X,Pulse.X.idx)
}
funGet.HighLow.2Pulse.PerCycle<-function(vectorY,vectorX.idx,Cycle.Qty)
{
  highPoints<-vector(mode="numeric",length=0)
  lowPoints<-vector(mode="numeric",length=0)
  if(vectorY[vectorX.idx[2]]-vectorY[vectorX.idx[1]] < 0)#识别高低电平变化规律
  {
    #获取所有高电平向量、所有低电平向量
    vidx<-1
    while (vidx <= Cycle.Qty) {
      # print(vidx)
      temphighPoints<-vectorY[(vectorX.idx[2*vidx-1]+3):(vectorX.idx[2*vidx]-3)]#往区间内两端各收3个点过滤误差值
      highPoints <- c(highPoints,temphighPoints)
      
      templowPoints<-vectorY[(vectorX.idx[2*vidx]+3):(vectorX.idx[2*vidx+1]-3)]
      lowPoints <- c(lowPoints,templowPoints)
      vidx = vidx + 1
    }
    # print(paste("第一次电平转换为低——高电平"))
  }
  else
  {
    vidx<-1
    while (vidx <= Cycle.Qty) {
      # print(vidx)
      temphighPoints<-vectorY[(vectorX.idx[2*vidx]+3):(vectorX.idx[2*vidx+1]-3)]
      highPoints <- c(highPoints,temphighPoints)
      templowPoints <- vectorY[(vectorX.idx[2*vidx-1]+3):(vectorX.idx[2*vidx]-3)]
      lowPoints <- c(lowPoints,templowPoints)
      vidx = vidx + 1
    }
    # print(paste("第一次电平转换为高——低电平"))
  }
  outPara<-list(highPoints,lowPoints)
}
funGet.Y.Outliers<- function(highPoints,vectorX,vectorY)
{
  highPoints.diff<-abs(highPoints - mean(highPoints))
  highPoints.diff.DecSorted<-highPoints.diff[order(highPoints.diff,decreasing=TRUE)[1:3]]
  
  highPoints.diff.SortedIdx<-vector(mode="numeric",length=0)
  vectorY.Sorted.Idx<-vector(mode="numeric",length=0)
  for (variable in highPoints.diff.DecSorted) {
    temphighPoints.diff.SortedIdx<-which(highPoints.diff==variable)
    highPoints.diff.SortedIdx<-c(highPoints.diff.SortedIdx,temphighPoints.diff.SortedIdx)
    
    tempvectorY.SortedIdx<-which(vectorY==highPoints[temphighPoints.diff.SortedIdx])
    vectorY.Sorted.Idx<-c(vectorY.Sorted.Idx,tempvectorY.SortedIdx)
  }
  # print(highPoints.diff.SortedIdx)
  highPoints.Max5Point.vectorX<-vectorX[vectorY.Sorted.Idx][1:3]
  highPoints.Max5Point.vectorY<-highPoints[highPoints.diff.SortedIdx]
  
  # print(highPoints.Max5Point.vectorY)
  outPara<-list(highPoints.Max5Point.vectorX,highPoints.Max5Point.vectorY)
  
}
funGet.timespan.Outliers<-function(vectorTimespan)
{
  vectorTimespan.diff<-abs(vectorTimespan - mean(vectorTimespan))
  # print(high.CH2.2.timespan.diff)
  # print(sort(Cycle.diff, decreasing = TRUE))
  vectorTimespan.diff.DecSorted = sort(vectorTimespan.diff, decreasing = TRUE)
  vectorTimespan.diff.SortedIdx <- vector(mode="numeric",length=0)
  for (variable in vectorTimespan.diff.DecSorted) {
    tempvectorTimespan.diff.SortedIdx <- which(vectorTimespan.diff == variable)
    vectorTimespan.diff.SortedIdx <- c(vectorTimespan.diff.SortedIdx,tempvectorTimespan.diff.SortedIdx)
  }
  vectorTimespan.diff.SortedIdx<-vectorTimespan.diff.SortedIdx[1:length(vectorTimespan)]
  
  outParas<-list(vectorTimespan.diff.DecSorted,vectorTimespan.diff.SortedIdx)
  # print(vectorTimespan.diff.SortedIdx)
}
funGet.pulse.timespan<-function(pulse.X.CHaction,pulse.QtyperCycel.action,pulse.X.CHsense,pulse.QtyperCycel.sense,Cycle.Qty.CHsense)
{
  cnt <- 0
  
  Start.CHaction_CHsense.timespan<-vector(mode="numeric",length=0)
  End.CHaction_CHsense.timespan<-vector(mode="numeric",length=0)
  
  high.CHsense.timespan<-vector(mode="numeric",length=0)
  low.CHsense.timespan<-vector(mode="numeric",length=0)
  while (cnt < Cycle.Qty.CHsense) {
    # print(v)
    # print(which(vectorTime==pulse.x.CH2.2[1]))
    # p1<-which(vectorTime==pulse.x.CH3.2[4*cnt+1])
    # print(p1)
    # p2<-which(vectorTime==pulse.x.CH3.2[4*cnt+2])
    # print(p2)
    # high.First.y.Points<-c(high.First.y.Points
    #                        ,vectorCH3.2[p1:p2])
    tempStart.CHaction_CHsense.timespan<-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+1]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+1]
    Start.CHaction_CHsense.timespan<-c(Start.CHaction_CHsense.timespan,tempStart.CHaction_CHsense.timespan)
    
    tempEnd.CHaction_CHsense.timespan<-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+2]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+8]
    End.CHaction_CHsense.timespan<-c(End.CHaction_CHsense.timespan,tempEnd.CHaction_CHsense.timespan)
    
    temphigh.CHsense.timespan<-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+2]-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+1]
    high.CHsense.timespan<-c(high.CHsense.timespan,temphigh.CHsense.timespan)
    
    templow.CHsense.timespan<-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+3]-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+2]
    low.CHsense.timespan<-c(low.CHsense.timespan,templow.CHsense.timespan)
    
    cnt = cnt + 1
  }
  outPara<-list(high.CHsense.timespan,low.CHsense.timespan,Start.CHaction_CHsense.timespan,End.CHaction_CHsense.timespan)
}
funGet.8pulse.timespan<-function(pulse.X.CHaction,pulse.QtyperCycel.action,pulse.X.CHsense,pulse.QtyperCycel.sense,Cycle.Qty.CHaction)
{
  cnt <- 0
  high.First.y.Points<-vector(mode="numeric",length=0)
  high.1st.timespan<-vector(mode="numeric",length=0)
  high.1stEnd2ndStart.timespan<-vector(mode="numeric",length=0)
  high.2nd.timespan<-vector(mode="numeric",length=0)
  high.1stStart2ndStart.timespan<-vector(mode="numeric",length=0)
  high.2ndStart3rdStart.timespan<-vector(mode="numeric",length=0)
  high.3rdStart4thStart.timespan<-vector(mode="numeric",length=0)
  
  thisCycleEnd_nextCycleStart.timespan<-vector(mode="numeric",length=0)
  
  Start.CHaction_CHsense.timespan<-vector(mode="numeric",length=0)
  End.CHaction_CHsense.timespan<-vector(mode="numeric",length=0)
  
  while (cnt < Cycle.Qty.CHaction) {
    tempHigh.1st.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+2]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+1]
    high.1st.timespan<-c(high.1st.timespan,tempHigh.1st.timespan)
    
    temphigh.1stEnd2ndStart.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+3]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+2]
    high.1stEnd2ndStart.timespan<-c(high.1stEnd2ndStart.timespan,temphigh.1stEnd2ndStart.timespan)
    
    temphigh.2nd.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+4]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+3]
    high.2nd.timespan<-c(high.2nd.timespan,temphigh.2nd.timespan)
    
    temphigh.1stStart2ndStart.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+3]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+1]
    high.1stStart2ndStart.timespan<-c(high.1stStart2ndStart.timespan,temphigh.1stStart2ndStart.timespan)
    
    temphigh.2ndStart3rdStart.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+5]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+3]
    high.2ndStart3rdStart.timespan<-c(high.2ndStart3rdStart.timespan,temphigh.2ndStart3rdStart.timespan)
    
    temphigh.3rdStart4thStart.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+7]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+5]
    high.3rdStart4thStart.timespan<-c(high.3rdStart4thStart.timespan,temphigh.3rdStart4thStart.timespan)
    
    tempthisCycleEnd_nextCycleStart.timespan<-pulse.X.CHaction[pulse.QtyperCycel.action*(cnt+1)+1]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+8]
    thisCycleEnd_nextCycleStart.timespan<-c(thisCycleEnd_nextCycleStart.timespan,tempthisCycleEnd_nextCycleStart.timespan)
    
    tempStart.CHaction_CHsense.timespan<-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+1]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+1]
    Start.CHaction_CHsense.timespan<-c(Start.CHaction_CHsense.timespan,tempStart.CHaction_CHsense.timespan)
    
    tempEnd.CHaction_CHsense.timespan<-pulse.X.CHsense[pulse.QtyperCycel.sense*cnt+2]-pulse.X.CHaction[pulse.QtyperCycel.action*cnt+8]
    End.CHaction_CHsense.timespan<-c(End.CHaction_CHsense.timespan,tempEnd.CHaction_CHsense.timespan)
    
    cnt = cnt + 1
  }
  outPara<-list(high.1st.timespan,high.1stEnd2ndStart.timespan,high.2nd.timespan,high.1stStart2ndStart.timespan,high.2ndStart3rdStart.timespan,high.3rdStart4thStart.timespan,thisCycleEnd_nextCycleStart.timespan,Start.CHaction_CHsense.timespan,End.CHaction_CHsense.timespan)
}
funGet.Cycle.Each.TimeSpan<-function(Cycle.Qty,pulse.x.CH,pulse.QtyperCycel)
{
  Cycle.Each.TimeSpan <- vector(mode="numeric",length=0)
  i<-0
  while (i < Cycle.Qty) {
    tempCycle.Each.TimeSpan <- pulse.x.CH[pulse.QtyperCycel*i+(pulse.QtyperCycel+1)]-pulse.x.CH[pulse.QtyperCycel*i+1]
    Cycle.Each.TimeSpan <- c(Cycle.Each.TimeSpan,tempCycle.Each.TimeSpan)
    i = i + 1
  }
  Cycle.Each.TimeSpan <- Cycle.Each.TimeSpan[!is.na(Cycle.Each.TimeSpan)]
}
# Create Shiny app ----
shinyApp(ui, server)


