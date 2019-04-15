library(ggplot2)
library(shiny)
library(ggpubr)
library(stringr)
options(shiny.maxRequestSize=50*1024^2)
sliderMax <- 1
sliderMin <- 0
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
               uiOutput("checkboxGroupInput")
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
               uiOutput("slider"),
               absolutePanel(
                 top = 0, left=10, width=1, draggable = TRUE, style="float:left; padding: 0px;border: 5px solid #666666;opacity:0.5;",
                 HTML( '<div style="float:left;margin-top: 30px;width: 1px;height: 1000px; background: #666666;"></div>' )
               ),
               plotOutput("sepratePlot",width = "100%", height = "900px",brush='pl_brush')
        ),
        column(3,
               tabsetPanel(
                 type = "pills",
                 tabPanel("初始显示", icon=icon("home"),tableOutput("InitValues")),
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
                 tabPanel("区域数据",uiOutput("sliderRegion"))
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
    print(CHs.FullName)
    checkboxGroupInput("icons", "请勾选通道:",choices=CHs.FullName[-1],selected=CHs.FullName[-1],inline = FALSE)
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
    vCH3.2.Next<-vectorCH3.2[-1]
    vCH3.2<-vectorCH3.2[-length(vectorCH3.2)]
    vCH3.2.Idx1DValue<-vCH3.2.Next-vCH3.2
    #print(vCH3.2.Idx1DValue)
    vCH3.2.Idx1DEffective<-vCH3.2.Idx1DValue[abs(vCH3.2.Idx1DValue)>2]
    time.Idx1DEffectiveCH3.2<-which(abs(vCH3.2.Idx1DValue)>2)
    # print(vCH3.2.Idx1DEffective)
    # print(time.Idx1DEffectiveCH3.2)
    
    time.Idx1DEffectiveCH3.22<-time.Idx1DEffectiveCH3.2[-1]-time.Idx1DEffectiveCH3.2[-length(time.Idx1DEffectiveCH3.2)]
    pulse.x.CH3.2.idx<-time.Idx1DEffectiveCH3.2[time.Idx1DEffectiveCH3.22>10]+1
    # print(pulse.x.CH3.2.idx)
    pulse.x.CH3.2<-vectorTime[pulse.x.CH3.2.idx]
    
    # print(time.Idxeffect3.2)
    # print(which(time.Idx1DEffectiveCH3.22>10))
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    
    #处理动作波形CH3.2
    #周期总数=动作波形突变点数/8的商
    Cycle.Qty<-funGetCycleQty(pulse.x.CH3.2,8)
    #处理动作波形CH3.2
    
    # print(Cycle.Qty)
    
    #CH3.2总周期时间段
    TotalCycle.TimeSpan.CH3.2<-pulse.x.CH3.2[8*Cycle.Qty+1]-pulse.x.CH3.2[1]
    #CH3.2平均周期
    Cycle.avg.CH3.2<-TotalCycle.TimeSpan.CH3.2/Cycle.Qty
    #CH3.2异常周期（初始显示）
    #step1：获取每个周期的时间段，和时间间隔
    i<-0
    Cycle.Each.TimeSpan<-vector(mode="numeric",length=0)
    while (i < Cycle.Qty) {
      tempCycle.Each.TimeSpan<-pulse.x.CH3.2[8*i+9]-pulse.x.CH3.2[8*i+1]
      Cycle.Each.TimeSpan<-c(Cycle.Each.TimeSpan,tempCycle.Each.TimeSpan)
      
      i = i + 1
    }
    Cycle.Each.TimeSpan<-Cycle.Each.TimeSpan[!is.na(Cycle.Each.TimeSpan)]
    print(Cycle.Each.TimeSpan)
    #step2:按周期差值排序，获取到排序的索引
    Cycle.diff<-abs(Cycle.Each.TimeSpan-Cycle.avg.CH3.2) 
    # print(Cycle.diff)
    # print(sort(Cycle.diff, decreasing = TRUE))
    Cycle.diff.DecSorted=sort(Cycle.diff, decreasing = TRUE)
    Cycle.diff.SortedIdx<-vector(mode="numeric",length=0)
    for (variable in Cycle.diff.DecSorted) {
      tempCycle.diff.SortedIdx<-which(Cycle.diff==variable)
      Cycle.diff.SortedIdx <- c(Cycle.diff.SortedIdx,tempCycle.diff.SortedIdx)
    }
    print(Cycle.diff.SortedIdx)
    
    #速度v,1分钟有多少个周期，60000/平均周期
    v<-60/Cycle.avg.CH3.2
    # print(v)
    #CH3.2异常周期（初始显示）
    
    # Compose data frame
    data.frame(
      Item = c("速度",
               "周期",
               "异常周期"),
      Value = as.character(c(
        signif(v,7),
        signif(Cycle.avg.CH3.2,7),
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
    vCH3.2.Next<-vectorCH3.2[-1]
    vCH3.2<-vectorCH3.2[-length(vectorCH3.2)]
    vCH3.2.Idx1DValue<-vCH3.2.Next-vCH3.2
    #print(vCH3.2.Idx1DValue)
    vCH3.2.Idx1DEffective<-vCH3.2.Idx1DValue[abs(vCH3.2.Idx1DValue)>2]
    time.Idx1DEffectiveCH3.2<-which(abs(vCH3.2.Idx1DValue)>2)
    # print(vCH3.2.Idx1DEffective)
    # print(time.Idx1DEffectiveCH3.2)
    
    time.Idx1DEffectiveCH3.22<-time.Idx1DEffectiveCH3.2[-1]-time.Idx1DEffectiveCH3.2[-length(time.Idx1DEffectiveCH3.2)]
    pulse.x.CH3.2.idx<-time.Idx1DEffectiveCH3.2[time.Idx1DEffectiveCH3.22>10]+1
    # print(pulse.x.CH3.2.idx)
    pulse.x.CH3.2<-vectorTime[pulse.x.CH3.2.idx]
    
    # print(time.Idxeffect3.2)
    # print(which(time.Idx1DEffectiveCH3.22>10))
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    
    #处理感应波形CH2.2
    df.Next<-vectorCH2.2[-1]
    df<-vectorCH2.2[-length(vectorCH2.2)-1]
    df.Idx1DValue<-df.Next-df
    df.Idx1DEffective<-df.Idx1DValue[abs(df.Idx1DValue)>7]
    time.Idx1DEffective<-which(abs(df.Idx1DValue)>7)
    print(df.Idx1DEffective)
    
    time.Idx1DEffective2<-time.Idx1DEffective[-1]-time.Idx1DEffective[-length(time.Idx1DEffective)]
    pulse.x.CH2.2.idx<-time.Idx1DEffective[time.Idx1DEffective2 > 10] + 1
    pulse.x.CH2.2<-vectorTime[pulse.x.CH2.2.idx]
    pulse.x.CH2.2<-pulse.x.CH2.2[!is.na(pulse.x.CH2.2)]

    #处理动作波形CH3.2
    #周期总数=动作波形突变点数/8的商
    Cycle.Qty<-funGetCycleQty(pulse.x.CH3.2,8)
    
    # print(Cycle.Qty)
    
    #CH3.2总周期时间段
    TotalCycle.TimeSpan.CH3.2<-pulse.x.CH3.2[8*Cycle.Qty+1]-pulse.x.CH3.2[1]
    #CH3.2平均周期
    Cycle.avg.CH3.2<-TotalCycle.TimeSpan.CH3.2/Cycle.Qty

    #CH3.2异常周期（初始显示）
    #step1：获取每个周期的时间段，和时间间隔
    i<-0
    Cycle.Each.TimeSpan<-vector(mode="numeric",length=0)
    while (i < Cycle.Qty) {
      tempCycle.Each.TimeSpan<-pulse.x.CH3.2[8*i+9]-pulse.x.CH3.2[8*i+1]
      Cycle.Each.TimeSpan<-c(Cycle.Each.TimeSpan,tempCycle.Each.TimeSpan)
      
      i = i + 1
    }
    Cycle.Each.TimeSpan<-Cycle.Each.TimeSpan[!is.na(Cycle.Each.TimeSpan)]
    print(Cycle.Each.TimeSpan)
    #step2:按周期差值排序，获取到排序的索引
    Cycle.diff<-abs(Cycle.Each.TimeSpan-Cycle.avg.CH3.2) 
    # print(Cycle.diff)
    # print(sort(Cycle.diff, decreasing = TRUE))
    Cycle.diff.DecSorted=sort(Cycle.diff, decreasing = TRUE)
    Cycle.diff.SortedIdx<-vector(mode="numeric",length=0)
    for (variable in Cycle.diff.DecSorted) {
      tempCycle.diff.SortedIdx<-which(Cycle.diff==variable)
      Cycle.diff.SortedIdx <- c(Cycle.diff.SortedIdx,tempCycle.diff.SortedIdx)
    }
    print(Cycle.diff.SortedIdx)
    
    #CH3.2异常周期（初始显示）

    #CH3.2高电平所有点
    highPoints.CH3.2<-vectorCH3.2[vectorCH3.2>4]#c(df$CH2.2.V.[time.Idxeffect[1]:time.Idxeffect[2]],df$CH2.2.V.[time.Idxeffect[3]:time.Idxeffect[4]])
    #CH3.2低电平所有点
    lowPoints.CH3.2<-vectorCH3.2[vectorCH3.2<1]
    #CH3.2高电平均值
    high.CH3.2.avg<-mean(highPoints.CH3.2)
    #第一次高电平持续时间均值
    cnt <- 0
    high.First.y.Points<-vector(mode="numeric",length=0)
    high.1st.timespan<-vector(mode="numeric",length=0)
    high.1stEnd2ndStart.timespan<-vector(mode="numeric",length=0)
    high.2nd.timespan<-vector(mode="numeric",length=0)
    high.1stStart2ndStart.timespan<-vector(mode="numeric",length=0)
    high.2ndStart3rdStart.timespan<-vector(mode="numeric",length=0)
    high.3rdStart4thStart.timespan<-vector(mode="numeric",length=0)
    
    thisCycleEnd_nextCycleStart.timespan<-vector(mode="numeric",length=0)
    
    Start.CH3.2_CH2.2.timespan<-vector(mode="numeric",length=0)
    End.CH3.2_CH2.2.timespan<-vector(mode="numeric",length=0)
    
    high.CH2.2.timespan<-vector(mode="numeric",length=0)
    low.CH2.2.timespan<-vector(mode="numeric",length=0)
    while (cnt < Cycle.Qty) {
      # print(v)
      # print(which(vectorTime==pulse.x.CH2.2[1]))
      # p1<-which(vectorTime==pulse.x.CH3.2[4*cnt+1])
      # print(p1)
      # p2<-which(vectorTime==pulse.x.CH3.2[4*cnt+2])
      # print(p2)
      # high.First.y.Points<-c(high.First.y.Points
      #                        ,vectorCH3.2[p1:p2])
      
      tempHigh.1st.timespan<-pulse.x.CH3.2[8*cnt+2]-pulse.x.CH3.2[8*cnt+1]
      high.1st.timespan<-c(high.1st.timespan,tempHigh.1st.timespan)
      
      temphigh.1stEnd2ndStart.timespan<-pulse.x.CH3.2[8*cnt+3]-pulse.x.CH3.2[8*cnt+2]
      high.1stEnd2ndStart.timespan<-c(high.1stEnd2ndStart.timespan,temphigh.1stEnd2ndStart.timespan)
      
      temphigh.2nd.timespan<-pulse.x.CH3.2[8*cnt+4]-pulse.x.CH3.2[8*cnt+3]
      high.2nd.timespan<-c(high.2nd.timespan,temphigh.2nd.timespan)
      
      temphigh.1stStart2ndStart.timespan<-pulse.x.CH3.2[8*cnt+3]-pulse.x.CH3.2[8*cnt+1]
      high.1stStart2ndStart.timespan<-c(high.1stStart2ndStart.timespan,temphigh.1stStart2ndStart.timespan)
      
      temphigh.2ndStart3rdStart.timespan<-pulse.x.CH3.2[8*cnt+5]-pulse.x.CH3.2[8*cnt+3]
      high.2ndStart3rdStart.timespan<-c(high.2ndStart3rdStart.timespan,temphigh.2ndStart3rdStart.timespan)
      
      temphigh.3rdStart4thStart.timespan<-pulse.x.CH3.2[8*cnt+7]-pulse.x.CH3.2[8*cnt+5]
      high.3rdStart4thStart.timespan<-c(high.3rdStart4thStart.timespan,temphigh.3rdStart4thStart.timespan)
      
      tempthisCycleEnd_nextCycleStart.timespan<-pulse.x.CH3.2[8*(cnt+1)+1]-pulse.x.CH3.2[8*cnt+8]
      thisCycleEnd_nextCycleStart.timespan<-c(thisCycleEnd_nextCycleStart.timespan,tempthisCycleEnd_nextCycleStart.timespan)
      
      tempStart.CH3.2_CH2.2.timespan<-pulse.x.CH2.2[2*cnt+1]-pulse.x.CH3.2[8*cnt+1]
      Start.CH3.2_CH2.2.timespan<-c(Start.CH3.2_CH2.2.timespan,tempStart.CH3.2_CH2.2.timespan)
      
      tempEnd.CH3.2_CH2.2.timespan<-pulse.x.CH2.2[2*cnt+2]-pulse.x.CH3.2[8*cnt+8]
      End.CH3.2_CH2.2.timespan<-c(End.CH3.2_CH2.2.timespan,tempEnd.CH3.2_CH2.2.timespan)
      
      temphigh.CH2.2.timespan<-pulse.x.CH2.2[2*cnt+2]-pulse.x.CH2.2[2*cnt+1]
      high.CH2.2.timespan<-c(high.CH2.2.timespan,temphigh.CH2.2.timespan)
      
      templow.CH2.2.timespan<-pulse.x.CH2.2[2*cnt+3]-pulse.x.CH2.2[2*cnt+2]
      low.CH2.2.timespan<-c(low.CH2.2.timespan,templow.CH2.2.timespan)
      
      cnt = cnt + 1
    }
    # print(high.First.y.Points)
    # print(high.1st.timespan)
    #CH3.2第一次高电平持续时间均值
    high.1st.timespan.avg<-mean(high.1st.timespan)
    print(high.1st.timespan.avg)
    #CH3.2单周期内第一次高电平结束到第二次高电平起始时间差记录均值
    high.1stEnd2ndStart.timespan.avg<-mean(high.1stEnd2ndStart.timespan)
    print(high.1stEnd2ndStart.timespan.avg)
    #CH3.2第二次高电平持续时间均值
    high.2nd.timespan.avg<-mean(high.2nd.timespan)
    print(high.2nd.timespan.avg)
    #CH3.2单周期内第一次高电平开始与第二次高电平开始时间差记录均值
    high.1stStart2ndStart.timespan.avg<-mean(high.1stStart2ndStart.timespan)
    print(high.1stStart2ndStart.timespan.avg)
    #CH3.2单周期内第二次高电平开始与第三次高电平开始时间差记录均值
    high.2ndStart3rdStart.timespan.avg<-mean(high.2ndStart3rdStart.timespan)
    print(high.2ndStart3rdStart.timespan.avg)
    #CH3.2单周期内第三次高电平开始与第四次高电平开始时间差记录均值
    high.3rdStart4thStart.timespan.avg<-mean(high.3rdStart4thStart.timespan)
    print(high.3rdStart4thStart.timespan.avg)
    #CH3.2最后一次动作信号结束到下次信号起始时间
    thisCycleEnd_nextCycleStart.timespan.avg<-mean(thisCycleEnd_nextCycleStart.timespan)
    print(thisCycleEnd_nextCycleStart.timespan.avg)
    
    #单次关联动作时序起始位置差均值、最大值、最小值
    Start.CH3.2_CH2.2.timespan.avg<-mean(Start.CH3.2_CH2.2.timespan)
    print(Start.CH3.2_CH2.2.timespan.avg)
    Start.CH3.2_CH2.2.timespan.max<-max(Start.CH3.2_CH2.2.timespan)
    print(Start.CH3.2_CH2.2.timespan.max)
    Start.CH3.2_CH2.2.timespan.min<-min(Start.CH3.2_CH2.2.timespan)
    print(Start.CH3.2_CH2.2.timespan.min)
    
    #单次关联动作时序起始异常位置
    
    print(Start.CH3.2_CH2.2.timespan)
    #step2:按周期差值排序，获取到排序的索引
    Start.CH3.2_CH2.2.timespan.diff<-abs(Start.CH3.2_CH2.2.timespan - Start.CH3.2_CH2.2.timespan.avg) 
    print(Start.CH3.2_CH2.2.timespan.diff)
    # print(sort(Cycle.diff, decreasing = TRUE))
    Start.CH3.2_CH2.2.timespan.diff.DecSorted=sort(Start.CH3.2_CH2.2.timespan.diff, decreasing = TRUE)
    Start.CH3.2_CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
    for (variable in Start.CH3.2_CH2.2.timespan.diff.DecSorted) {
      tempStart.CH3.2_CH2.2.timespan.diff.SortedIdx<-which(Start.CH3.2_CH2.2.timespan.diff==variable)
      Start.CH3.2_CH2.2.timespan.diff.SortedIdx<-c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,tempStart.CH3.2_CH2.2.timespan.diff.SortedIdx)
    }
    print(Start.CH3.2_CH2.2.timespan.diff.SortedIdx)
    
    #单次关联动作时序起始位置差均值、最大值、最小值
    End.CH3.2_CH2.2.timespan.avg<-mean(End.CH3.2_CH2.2.timespan)
    # print(End.CH3.2_CH2.2.timespan.avg)
    End.CH3.2_CH2.2.timespan.max<-max(End.CH3.2_CH2.2.timespan)
    # print(End.CH3.2_CH2.2.timespan.max)
    End.CH3.2_CH2.2.timespan.min<-min(End.CH3.2_CH2.2.timespan)
    # print(End.CH3.2_CH2.2.timespan.min)
    #单次关联动作时序结束异常位置
    
    print(End.CH3.2_CH2.2.timespan)
    #step2:按周期差值排序，获取到排序的索引
    End.CH3.2_CH2.2.timespan.diff<-abs(End.CH3.2_CH2.2.timespan - End.CH3.2_CH2.2.timespan.avg) 
    print(End.CH3.2_CH2.2.timespan.diff)
    # print(sort(Cycle.diff, decreasing = TRUE))
    End.CH3.2_CH2.2.timespan.diff.DecSorted=sort(End.CH3.2_CH2.2.timespan.diff, decreasing = TRUE)
    End.CH3.2_CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
    for (variable in End.CH3.2_CH2.2.timespan.diff.DecSorted) {
      tempEnd.CH3.2_CH2.2.timespan.diff.SortedIdx<-which(End.CH3.2_CH2.2.timespan.diff==variable)
      End.CH3.2_CH2.2.timespan.diff.SortedIdx<-c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,tempEnd.CH3.2_CH2.2.timespan.diff.SortedIdx)
    }
    print(End.CH3.2_CH2.2.timespan.diff.SortedIdx)
    
    # # #CH3.2总周期时间段
    # # TotalCycle.TimeSpan.CH3.2<-pulse.x.CH3.2[8*Cycle.Qty+1]-pulse.x.CH3.2[1]
    # # #CH3.2平均周期
    # # Cycle.avg.CH3.2<-TotalCycle.TimeSpan.CH3.2/Cycle.Qty
    # # print(Cycle.avg.CH3.2)
    # 
    # 
    # #CH2.2总周期时间段
    # TotalCycle.TimeSpan.CH2.2<-pulse.x.CH2.2[2*Cycle.Qty+1]-pulse.x.CH2.2[1]
    # 
    # # print(pulse.x.CH2.2)
    # # print(TotalCycle.TimeSpan.CH2.2)
    # 
    # # print(Cycle.Qty)
    # #CH2.2平均周期
    # Cycle.avg.CH2.2<-TotalCycle.TimeSpan.CH2.2/Cycle.Qty
    # # print(Cycle.avg.CH2.2)
    # # AverageCycle<-(pulse.x.CH2.2[length(pulse.x.CH2.2)]-pulse.x.CH2.2[1])/((length(pulse.x.CH2.2)-1)/2)
    # # print(cycle)
    # # print(max(df.Idx1DValue))
    # # print(min(df.Idx1DValue))
    # 
    # 
    # # highPoints<-highPoints#c(df$CH2.2.V.[time.Idxeffect[1]:time.Idxeffect[2]],df$CH2.2.V.[time.Idxeffect[3]:time.Idxeffect[4]])
    # 
    # lowPoints<-vectorCH2.2[vectorCH2.2 < 1]
    # 
    # highMean<-mean(highPoints.CH2.2)
    # highMax<-max(highPoints.CH2.2)
    # highMin<-min(highPoints.CH2.2)
    # 
    # # print(highPoints)
    # # #单次高电平异常位置
    # # #step2:按周期差值排序，获取到排序的索引
    # highPoints.CH2.2.diff<-abs(highPoints.CH2.2 - highMean)
    # highPoints.CH2.2.diff.DecSorted<-highPoints.CH2.2.diff[order(highPoints.CH2.2.diff,decreasing=TRUE)[1:3]]
    # 
    # highPoints.CH2.2.diff.SortedIdx<-vector(mode="numeric",length=0)
    # vectorCH2.2.Sorted.Idx<-vector(mode="numeric",length=0)
    # for (variable in highPoints.CH2.2.diff.DecSorted) {
    #   temphighPoints.CH2.2.diff.SortedIdx<-which(highPoints.CH2.2.diff==variable)
    #   highPoints.CH2.2.diff.SortedIdx<-c(highPoints.CH2.2.diff.SortedIdx,temphighPoints.CH2.2.diff.SortedIdx)
    #   
    #   tempvectorCH2.2SortedIdx<-which(vectorCH2.2==highPoints.CH2.2[temphighPoints.CH2.2.diff.SortedIdx])
    #   vectorCH2.2.Sorted.Idx<-c(vectorCH2.2.Sorted.Idx,tempvectorCH2.2SortedIdx)
    # }
    # print(highPoints.CH2.2.diff.SortedIdx)
    # highPoints.CH2.2.Max5Point.vectorTime<-vectorTime[vectorCH2.2.Sorted.Idx][1:3]
    # highPoints.CH2.2.Max5Point.vectorCH2.2<-highPoints.CH2.2[highPoints.CH2.2.diff.SortedIdx]
    # 
    # print(highPoints.CH2.2.Max5Point.vectorCH2.2)
    # 
    # lowMean<-mean(lowPoints.CH2.2)
    # lowMax<-max(lowPoints.CH2.2)
    # lowMin<-min(lowPoints.CH2.2)
    # 
    # # print(lowPoints.CH2.2)
    # # #单次低电平异常位置
    # #step2:按周期差值排序，获取到排序的索引
    # lowPoints.CH2.2.diff<-abs(lowPoints.CH2.2 - lowMean)
    # # print(lowPoints.CH2.2.diff)
    # # print(sort(Cycle.diff, decreasing = TRUE))
    # lowPoints.CH2.2.diff.DecSorted=lowPoints.CH2.2.diff[order(lowPoints.CH2.2.diff,decreasing=TRUE)[1:3]]
    # lowPoints.CH2.2.diff.SortedIdx<-vector(mode="numeric",length=0)
    # vectorCH2.2.Sorted.Idx<-vector(mode="numeric",length=0)
    # for (variable in lowPoints.CH2.2.diff.DecSorted) {
    #   templowPoints.CH2.2.diff.SortedIdx<-which(lowPoints.CH2.2.diff==variable)
    #   lowPoints.CH2.2.diff.SortedIdx<-c(lowPoints.CH2.2.diff.SortedIdx,templowPoints.CH2.2.diff.SortedIdx)
    #   
    #   tempvectorCH2.2SortedIdx<-which(vectorCH2.2==lowPoints.CH2.2[templowPoints.CH2.2.diff.SortedIdx])
    #   vectorCH2.2.Sorted.Idx<-c(vectorCH2.2.Sorted.Idx,tempvectorCH2.2SortedIdx)
    # }
    # lowPoints.CH2.2.Max5Point.vectorTime<-vectorTime[vectorCH2.2.Sorted.Idx][1:3]
    # lowPoints.CH2.2.Max5Point.vectorCH2.2<-lowPoints.CH2.2[lowPoints.CH2.2.diff.SortedIdx]
    # # print(lowPoints.CH2.2.diff.SortedIdx)
    # # print(vectorTime[vectorCH2.2.Sorted.Idx])
    # # print(lowPoints.CH2.2.diff.DecSorted)
    # # print(lowPoints.CH2.2[lowPoints.CH2.2.diff.SortedIdx])
    # 
    # # #单次高电平持续时间异常位置
    # # #step2:按周期差值排序，获取到排序的索引
    # # print(high.CH2.2.timespan)
    # high.CH2.2.timespan.diff<-abs(high.CH2.2.timespan - mean(high.CH2.2.timespan))
    # # print(high.CH2.2.timespan.diff)
    # # print(sort(Cycle.diff, decreasing = TRUE))
    # high.CH2.2.timespan.diff.DecSorted=sort(high.CH2.2.timespan.diff, decreasing = TRUE)
    # high.CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
    # for (variable in high.CH2.2.timespan.diff.DecSorted) {
    #   temphigh.CH2.2.timespan.diff.SortedIdx<-which(high.CH2.2.timespan.diff==variable)
    #   high.CH2.2.timespan.diff.SortedIdx<-c(high.CH2.2.timespan.diff.SortedIdx,temphigh.CH2.2.timespan.diff.SortedIdx)
    # }
    # high.CH2.2.timespan.diff.SortedIdx<-high.CH2.2.timespan.diff.SortedIdx[1:length(high.CH2.2.timespan)]
    # print(high.CH2.2.timespan.diff.SortedIdx)
    # 
    # # #单次低电平持续时间异常位置
    # #step2:按周期差值排序，获取到排序的索引
    # # print(low.CH2.2.timespan)
    # low.CH2.2.timespan.diff<-abs(low.CH2.2.timespan - mean(low.CH2.2.timespan))
    # low.CH2.2.timespan.diff.DecSorted=sort(low.CH2.2.timespan.diff, decreasing = TRUE)
    # low.CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
    # for (variable in low.CH2.2.timespan.diff.DecSorted) {
    #   templow.CH2.2.timespan.diff.SortedIdx<-which(low.CH2.2.timespan.diff==variable)
    #   low.CH2.2.timespan.diff.SortedIdx<-c(low.CH2.2.timespan.diff.SortedIdx,templow.CH2.2.timespan.diff.SortedIdx)
    #   
    # }
    # low.CH2.2.timespan.diff.SortedIdx<-low.CH2.2.timespan.diff.SortedIdx[1:length(low.CH2.2.timespan)]
    # print(low.CH2.2.timespan.diff.SortedIdx)
    # # #单次低电平持续时间异常位置
    
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
        "最后一次动作信号结束到下次信号起始时间"
        # "单次关联动作时序起始位置差均值",
        # "单次关联动作时序起始位置差最大值",
        # "单次关联动作时序起始位置差最小值",
        # "单次关联动作时序起始异常位置",
        # "单次关联动作时序结束位置差均值",
        # "单次关联动作时序结束位置差最大值",
        # "单次关联动作时序位置位置差最小值",
        # "单次关联动作时序结束异常位置"
      ),
      CH3_2_Value = as.character(c(
        signif(Cycle.avg.CH3.2,7),
        signif(high.CH3.2.avg,7),
        signif(high.1st.timespan.avg,7),
        signif(high.1stEnd2ndStart.timespan.avg,7),
        signif(high.2nd.timespan.avg,7),
        signif(high.1stStart2ndStart.timespan.avg,7),
        signif(high.2ndStart3rdStart.timespan.avg,7),
        signif(high.3rdStart4thStart.timespan.avg,7),
        signif(thisCycleEnd_nextCycleStart.timespan.avg,7)
        # signif(Start.CH3.2_CH2.2.timespan.avg,7),
        # signif(Start.CH3.2_CH2.2.timespan.max,7),
        # signif(Start.CH3.2_CH2.2.timespan.min,7),
        # str_c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=','),
        # signif(End.CH3.2_CH2.2.timespan.avg,7),
        # signif(End.CH3.2_CH2.2.timespan.max,7),
        # signif(End.CH3.2_CH2.2.timespan.min,7),
        # str_c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=',')
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
    vCH3.2.Next<-vectorCH3.2[-1]
    vCH3.2<-vectorCH3.2[-length(vectorCH3.2)]
    vCH3.2.Idx1DValue<-vCH3.2.Next-vCH3.2
    #print(vCH3.2.Idx1DValue)
    vCH3.2.Idx1DEffective<-vCH3.2.Idx1DValue[abs(vCH3.2.Idx1DValue)>2]
    time.Idx1DEffectiveCH3.2<-which(abs(vCH3.2.Idx1DValue)>2)
    # print(vCH3.2.Idx1DEffective)
    # print(time.Idx1DEffectiveCH3.2)
    
    time.Idx1DEffectiveCH3.22<-time.Idx1DEffectiveCH3.2[-1]-time.Idx1DEffectiveCH3.2[-length(time.Idx1DEffectiveCH3.2)]
    pulse.x.CH3.2.idx<-time.Idx1DEffectiveCH3.2[time.Idx1DEffectiveCH3.22>10]+1
    # print(pulse.x.CH3.2.idx)
    pulse.x.CH3.2<-vectorTime[pulse.x.CH3.2.idx]
    
    # print(time.Idxeffect3.2)
    # print(which(time.Idx1DEffectiveCH3.22>10))
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    
    #处理动作波形CH3.2
    #周期总数=动作波形突变点数/8的商
    Cycle.Qty<-funGetCycleQty(pulse.x.CH3.2,8)

    # print(Cycle.Qty)
    
    #CH3.2总周期时间段
    TotalCycle.TimeSpan.CH3.2<-pulse.x.CH3.2[8*Cycle.Qty+1]-pulse.x.CH3.2[1]
    #CH3.2平均周期
    Cycle.avg.CH3.2<-TotalCycle.TimeSpan.CH3.2/Cycle.Qty
    
    #处理感应波形CH2.2
   df.Next<-vectorCH2.2[-1]
   df<-vectorCH2.2[-length(vectorCH2.2)-1]
   df.Idx1DValue<-df.Next-df
   df.Idx1DEffective<-df.Idx1DValue[abs(df.Idx1DValue)>7]
   time.Idx1DEffective<-which(abs(df.Idx1DValue)>7)
   print(df.Idx1DEffective)
  
   time.Idx1DEffective2<-time.Idx1DEffective[-1]-time.Idx1DEffective[-length(time.Idx1DEffective)]
   pulse.x.CH2.2.idx<-time.Idx1DEffective[time.Idx1DEffective2 > 10] + 1
   pulse.x.CH2.2<-vectorTime[pulse.x.CH2.2.idx]
   pulse.x.CH2.2<-pulse.x.CH2.2[!is.na(pulse.x.CH2.2)]
   Cycle.Qty.CH2.2<-funGetCycleQty(pulse.x.CH2.2,2)
   # print(Cycle.Qty.CH2.2)
   
   # print(pulse.x.CH2.2.idx)
   # print(length(pulse.x.CH2.2.idx))
   
   # print(paste("CH2.2 x节点：",pulse.x.CH2.2))
   highPoints.CH2.2<-vector(mode="numeric",length=0)
   lowPoints.CH2.2<-vector(mode="numeric",length=0)
   if(vectorCH2.2[pulse.x.CH2.2.idx[2]]-vectorCH2.2[pulse.x.CH2.2.idx[1]]<0)#识别高低电平变化规律
   {
     #获取所有高电平向量、所有低电平向量
     vidx<-1
     while (vidx <= Cycle.Qty.CH2.2) {
       print(vidx)
       temphighPoints.CH2.2<-vectorCH2.2[(pulse.x.CH2.2.idx[2*vidx-1]+3):(pulse.x.CH2.2.idx[2*vidx]-3)]#往区间内两端各收3个点过滤误差值
       highPoints.CH2.2 <- c(highPoints.CH2.2,temphighPoints.CH2.2)
       
       templowPoints.CH2.2<-vectorCH2.2[(pulse.x.CH2.2.idx[2*vidx]+3):(pulse.x.CH2.2.idx[2*vidx+1]-3)]
       # if(vidx>105)
       # {
       #   print(vidx)
       #   print(templowPoints.CH2.2)
       # }
       lowPoints.CH2.2 <- c(lowPoints.CH2.2,templowPoints.CH2.2)
       vidx = vidx + 1
     }
     # print("end")
   }
   else
   {
     vidx<-1
     while (vidx <= Cycle.Qty) {
       # print(vidx)
       temphighPoints.CH2.2<-vectorCH2.2[(pulse.x.CH2.2.idx[2*vidx]+3):(pulse.x.CH2.2.idx[2*vidx+1]-3)]
       highPoints.CH2.2 <- c(highPoints.CH2.2,temphighPoints.CH2.2)
       
       templowPoints.CH2.2<-vectorCH2.2[(pulse.x.CH2.2.idx[2*vidx-1]+3):(pulse.x.CH2.2.idx[2*vidx]-3)]
       lowPoints.CH2.2 <- c(lowPoints.CH2.2,templowPoints.CH2.2)
       vidx = vidx + 1
     }
     print(paste("怎么回事"))
   }
   # print(highPoints.CH2.2)
   print(mean(highPoints.CH2.2))
   print(mean(lowPoints.CH2.2))
   
   # print(time.Idxeffect)
   # print(which(time.Idx1DEffective2>10))
   #处理感应波形CH2.2
   
   # print(Cycle.avg.CH3.2)
   #CH3.2异常周期（初始显示）
   #step1：获取每个周期的时间段，和时间间隔
   i<-0
   Cycle.Each.TimeSpan<-vector(mode="numeric",length=0)
   while (i < Cycle.Qty) {
     tempCycle.Each.TimeSpan<-pulse.x.CH3.2[8*i+9]-pulse.x.CH3.2[8*i+1]
     Cycle.Each.TimeSpan<-c(Cycle.Each.TimeSpan,tempCycle.Each.TimeSpan)
     
     i = i + 1
   }
   Cycle.Each.TimeSpan<-Cycle.Each.TimeSpan[!is.na(Cycle.Each.TimeSpan)]
   print(Cycle.Each.TimeSpan)
   #step2:按周期差值排序，获取到排序的索引
   Cycle.diff<-abs(Cycle.Each.TimeSpan-Cycle.avg.CH3.2) 
   # print(Cycle.diff)
   # print(sort(Cycle.diff, decreasing = TRUE))
   Cycle.diff.DecSorted=sort(Cycle.diff, decreasing = TRUE)
   Cycle.diff.SortedIdx<-vector(mode="numeric",length=0)
   for (variable in Cycle.diff.DecSorted) {
     tempCycle.diff.SortedIdx<-which(Cycle.diff==variable)
     Cycle.diff.SortedIdx <- c(Cycle.diff.SortedIdx,tempCycle.diff.SortedIdx)
   }
   print(Cycle.diff.SortedIdx)
   
   #CH3.2异常周期（初始显示）
   
   
   #速度v,1分钟有多少个周期，60000/平均周期
   v<-60/Cycle.avg.CH3.2
   # print(v)
   #CH3.2高电平所有点
   highPoints.CH3.2<-vectorCH3.2[vectorCH3.2>4]#c(df$CH2.2.V.[time.Idxeffect[1]:time.Idxeffect[2]],df$CH2.2.V.[time.Idxeffect[3]:time.Idxeffect[4]])
   #CH3.2低电平所有点
   lowPoints.CH3.2<-vectorCH3.2[vectorCH3.2<1]
   #CH3.2高电平均值
   high.CH3.2.avg<-mean(highPoints.CH3.2)
   #第一次高电平持续时间均值
   cnt <- 0
   high.First.y.Points<-vector(mode="numeric",length=0)
   high.1st.timespan<-vector(mode="numeric",length=0)
   high.1stEnd2ndStart.timespan<-vector(mode="numeric",length=0)
   high.2nd.timespan<-vector(mode="numeric",length=0)
   high.1stStart2ndStart.timespan<-vector(mode="numeric",length=0)
   high.2ndStart3rdStart.timespan<-vector(mode="numeric",length=0)
   high.3rdStart4thStart.timespan<-vector(mode="numeric",length=0)
   
   thisCycleEnd_nextCycleStart.timespan<-vector(mode="numeric",length=0)
   
   Start.CH3.2_CH2.2.timespan<-vector(mode="numeric",length=0)
   End.CH3.2_CH2.2.timespan<-vector(mode="numeric",length=0)
   
   high.CH2.2.timespan<-vector(mode="numeric",length=0)
   low.CH2.2.timespan<-vector(mode="numeric",length=0)
   while (cnt < Cycle.Qty) {
     # print(v)
     # print(which(vectorTime==pulse.x.CH2.2[1]))
     # p1<-which(vectorTime==pulse.x.CH3.2[4*cnt+1])
     # print(p1)
     # p2<-which(vectorTime==pulse.x.CH3.2[4*cnt+2])
     # print(p2)
     # high.First.y.Points<-c(high.First.y.Points
     #                        ,vectorCH3.2[p1:p2])
     
     tempHigh.1st.timespan<-pulse.x.CH3.2[8*cnt+2]-pulse.x.CH3.2[8*cnt+1]
     high.1st.timespan<-c(high.1st.timespan,tempHigh.1st.timespan)
     
     temphigh.1stEnd2ndStart.timespan<-pulse.x.CH3.2[8*cnt+3]-pulse.x.CH3.2[8*cnt+2]
     high.1stEnd2ndStart.timespan<-c(high.1stEnd2ndStart.timespan,temphigh.1stEnd2ndStart.timespan)
     
     temphigh.2nd.timespan<-pulse.x.CH3.2[8*cnt+4]-pulse.x.CH3.2[8*cnt+3]
     high.2nd.timespan<-c(high.2nd.timespan,temphigh.2nd.timespan)
     
     temphigh.1stStart2ndStart.timespan<-pulse.x.CH3.2[8*cnt+3]-pulse.x.CH3.2[8*cnt+1]
     high.1stStart2ndStart.timespan<-c(high.1stStart2ndStart.timespan,temphigh.1stStart2ndStart.timespan)
     
     temphigh.2ndStart3rdStart.timespan<-pulse.x.CH3.2[8*cnt+5]-pulse.x.CH3.2[8*cnt+3]
     high.2ndStart3rdStart.timespan<-c(high.2ndStart3rdStart.timespan,temphigh.2ndStart3rdStart.timespan)
     
     temphigh.3rdStart4thStart.timespan<-pulse.x.CH3.2[8*cnt+7]-pulse.x.CH3.2[8*cnt+5]
     high.3rdStart4thStart.timespan<-c(high.3rdStart4thStart.timespan,temphigh.3rdStart4thStart.timespan)
     
     tempthisCycleEnd_nextCycleStart.timespan<-pulse.x.CH3.2[8*(cnt+1)+1]-pulse.x.CH3.2[8*cnt+8]
     thisCycleEnd_nextCycleStart.timespan<-c(thisCycleEnd_nextCycleStart.timespan,tempthisCycleEnd_nextCycleStart.timespan)
     
     tempStart.CH3.2_CH2.2.timespan<-pulse.x.CH2.2[2*cnt+1]-pulse.x.CH3.2[8*cnt+1]
     Start.CH3.2_CH2.2.timespan<-c(Start.CH3.2_CH2.2.timespan,tempStart.CH3.2_CH2.2.timespan)
     
     tempEnd.CH3.2_CH2.2.timespan<-pulse.x.CH2.2[2*cnt+2]-pulse.x.CH3.2[8*cnt+8]
     End.CH3.2_CH2.2.timespan<-c(End.CH3.2_CH2.2.timespan,tempEnd.CH3.2_CH2.2.timespan)
     
     temphigh.CH2.2.timespan<-pulse.x.CH2.2[2*cnt+2]-pulse.x.CH2.2[2*cnt+1]
     high.CH2.2.timespan<-c(high.CH2.2.timespan,temphigh.CH2.2.timespan)
     
     templow.CH2.2.timespan<-pulse.x.CH2.2[2*cnt+3]-pulse.x.CH2.2[2*cnt+2]
     low.CH2.2.timespan<-c(low.CH2.2.timespan,templow.CH2.2.timespan)
     
     cnt = cnt + 1
   }
   # print(high.First.y.Points)
   # print(high.1st.timespan)
   #CH3.2第一次高电平持续时间均值
   high.1st.timespan.avg<-mean(high.1st.timespan)
   print(high.1st.timespan.avg)
   #CH3.2单周期内第一次高电平结束到第二次高电平起始时间差记录均值
   high.1stEnd2ndStart.timespan.avg<-mean(high.1stEnd2ndStart.timespan)
   print(high.1stEnd2ndStart.timespan.avg)
   #CH3.2第二次高电平持续时间均值
   high.2nd.timespan.avg<-mean(high.2nd.timespan)
   print(high.2nd.timespan.avg)
   #CH3.2单周期内第一次高电平开始与第二次高电平开始时间差记录均值
   high.1stStart2ndStart.timespan.avg<-mean(high.1stStart2ndStart.timespan)
   print(high.1stStart2ndStart.timespan.avg)
   #CH3.2单周期内第二次高电平开始与第三次高电平开始时间差记录均值
   high.2ndStart3rdStart.timespan.avg<-mean(high.2ndStart3rdStart.timespan)
   print(high.2ndStart3rdStart.timespan.avg)
   #CH3.2单周期内第三次高电平开始与第四次高电平开始时间差记录均值
   high.3rdStart4thStart.timespan.avg<-mean(high.3rdStart4thStart.timespan)
   print(high.3rdStart4thStart.timespan.avg)
   #CH3.2最后一次动作信号结束到下次信号起始时间
   thisCycleEnd_nextCycleStart.timespan.avg<-mean(thisCycleEnd_nextCycleStart.timespan)
   print(thisCycleEnd_nextCycleStart.timespan.avg)
   
   #单次关联动作时序起始位置差均值、最大值、最小值
   Start.CH3.2_CH2.2.timespan.avg<-mean(Start.CH3.2_CH2.2.timespan)
   print(Start.CH3.2_CH2.2.timespan.avg)
   Start.CH3.2_CH2.2.timespan.max<-max(Start.CH3.2_CH2.2.timespan)
   print(Start.CH3.2_CH2.2.timespan.max)
   Start.CH3.2_CH2.2.timespan.min<-min(Start.CH3.2_CH2.2.timespan)
   print(Start.CH3.2_CH2.2.timespan.min)
   
   #单次关联动作时序起始异常位置
   
   print(Start.CH3.2_CH2.2.timespan)
   #step2:按周期差值排序，获取到排序的索引
   Start.CH3.2_CH2.2.timespan.diff<-abs(Start.CH3.2_CH2.2.timespan - Start.CH3.2_CH2.2.timespan.avg) 
   print(Start.CH3.2_CH2.2.timespan.diff)
   # print(sort(Cycle.diff, decreasing = TRUE))
   Start.CH3.2_CH2.2.timespan.diff.DecSorted=sort(Start.CH3.2_CH2.2.timespan.diff, decreasing = TRUE)
   Start.CH3.2_CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
   for (variable in Start.CH3.2_CH2.2.timespan.diff.DecSorted) {
     tempStart.CH3.2_CH2.2.timespan.diff.SortedIdx<-which(Start.CH3.2_CH2.2.timespan.diff==variable)
     Start.CH3.2_CH2.2.timespan.diff.SortedIdx<-c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,tempStart.CH3.2_CH2.2.timespan.diff.SortedIdx)
   }
   print(Start.CH3.2_CH2.2.timespan.diff.SortedIdx)
   
   #单次关联动作时序起始位置差均值、最大值、最小值
   End.CH3.2_CH2.2.timespan.avg<-mean(End.CH3.2_CH2.2.timespan)
   # print(End.CH3.2_CH2.2.timespan.avg)
   End.CH3.2_CH2.2.timespan.max<-max(End.CH3.2_CH2.2.timespan)
   # print(End.CH3.2_CH2.2.timespan.max)
   End.CH3.2_CH2.2.timespan.min<-min(End.CH3.2_CH2.2.timespan)
   # print(End.CH3.2_CH2.2.timespan.min)
   #单次关联动作时序结束异常位置
   
   print(End.CH3.2_CH2.2.timespan)
   #step2:按周期差值排序，获取到排序的索引
   End.CH3.2_CH2.2.timespan.diff<-abs(End.CH3.2_CH2.2.timespan - End.CH3.2_CH2.2.timespan.avg) 
   print(End.CH3.2_CH2.2.timespan.diff)
   # print(sort(Cycle.diff, decreasing = TRUE))
   End.CH3.2_CH2.2.timespan.diff.DecSorted=sort(End.CH3.2_CH2.2.timespan.diff, decreasing = TRUE)
   End.CH3.2_CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
   for (variable in End.CH3.2_CH2.2.timespan.diff.DecSorted) {
     tempEnd.CH3.2_CH2.2.timespan.diff.SortedIdx<-which(End.CH3.2_CH2.2.timespan.diff==variable)
     End.CH3.2_CH2.2.timespan.diff.SortedIdx<-c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,tempEnd.CH3.2_CH2.2.timespan.diff.SortedIdx)
   }
   print(End.CH3.2_CH2.2.timespan.diff.SortedIdx)
   
   # #CH3.2总周期时间段
   # TotalCycle.TimeSpan.CH3.2<-pulse.x.CH3.2[8*Cycle.Qty+1]-pulse.x.CH3.2[1]
   # #CH3.2平均周期
   # Cycle.avg.CH3.2<-TotalCycle.TimeSpan.CH3.2/Cycle.Qty
   # print(Cycle.avg.CH3.2)
   
   
   #CH2.2总周期时间段
   TotalCycle.TimeSpan.CH2.2<-pulse.x.CH2.2[2*Cycle.Qty+1]-pulse.x.CH2.2[1]
   
   # print(pulse.x.CH2.2)
   # print(TotalCycle.TimeSpan.CH2.2)
   
   # print(Cycle.Qty)
   #CH2.2平均周期
   Cycle.avg.CH2.2<-TotalCycle.TimeSpan.CH2.2/Cycle.Qty
   # print(Cycle.avg.CH2.2)
   # AverageCycle<-(pulse.x.CH2.2[length(pulse.x.CH2.2)]-pulse.x.CH2.2[1])/((length(pulse.x.CH2.2)-1)/2)
   # print(cycle)
   # print(max(df.Idx1DValue))
   # print(min(df.Idx1DValue))
   
   
   # highPoints<-highPoints#c(df$CH2.2.V.[time.Idxeffect[1]:time.Idxeffect[2]],df$CH2.2.V.[time.Idxeffect[3]:time.Idxeffect[4]])
   
   lowPoints<-vectorCH2.2[vectorCH2.2 < 1]
   
   highMean<-mean(highPoints.CH2.2)
   highMax<-max(highPoints.CH2.2)
   highMin<-min(highPoints.CH2.2)
   
   # print(highPoints)
   # #单次高电平异常位置
   # #step2:按周期差值排序，获取到排序的索引
   highPoints.CH2.2.diff<-abs(highPoints.CH2.2 - highMean)
   highPoints.CH2.2.diff.DecSorted<-highPoints.CH2.2.diff[order(highPoints.CH2.2.diff,decreasing=TRUE)[1:3]]

   highPoints.CH2.2.diff.SortedIdx<-vector(mode="numeric",length=0)
   vectorCH2.2.Sorted.Idx<-vector(mode="numeric",length=0)
   for (variable in highPoints.CH2.2.diff.DecSorted) {
     temphighPoints.CH2.2.diff.SortedIdx<-which(highPoints.CH2.2.diff==variable)
     highPoints.CH2.2.diff.SortedIdx<-c(highPoints.CH2.2.diff.SortedIdx,temphighPoints.CH2.2.diff.SortedIdx)
     
     tempvectorCH2.2SortedIdx<-which(vectorCH2.2==highPoints.CH2.2[temphighPoints.CH2.2.diff.SortedIdx])
     vectorCH2.2.Sorted.Idx<-c(vectorCH2.2.Sorted.Idx,tempvectorCH2.2SortedIdx)
   }
   print(highPoints.CH2.2.diff.SortedIdx)
   highPoints.CH2.2.Max5Point.vectorTime<-vectorTime[vectorCH2.2.Sorted.Idx][1:3]
   highPoints.CH2.2.Max5Point.vectorCH2.2<-highPoints.CH2.2[highPoints.CH2.2.diff.SortedIdx]
   
   print(highPoints.CH2.2.Max5Point.vectorCH2.2)
   
   lowMean<-mean(lowPoints.CH2.2)
   lowMax<-max(lowPoints.CH2.2)
   lowMin<-min(lowPoints.CH2.2)
   
   # print(lowPoints.CH2.2)
   # #单次低电平异常位置
   #step2:按周期差值排序，获取到排序的索引
   lowPoints.CH2.2.diff<-abs(lowPoints.CH2.2 - lowMean)
   # print(lowPoints.CH2.2.diff)
   # print(sort(Cycle.diff, decreasing = TRUE))
   lowPoints.CH2.2.diff.DecSorted=lowPoints.CH2.2.diff[order(lowPoints.CH2.2.diff,decreasing=TRUE)[1:3]]
   lowPoints.CH2.2.diff.SortedIdx<-vector(mode="numeric",length=0)
   vectorCH2.2.Sorted.Idx<-vector(mode="numeric",length=0)
   for (variable in lowPoints.CH2.2.diff.DecSorted) {
     templowPoints.CH2.2.diff.SortedIdx<-which(lowPoints.CH2.2.diff==variable)
     lowPoints.CH2.2.diff.SortedIdx<-c(lowPoints.CH2.2.diff.SortedIdx,templowPoints.CH2.2.diff.SortedIdx)
     
     tempvectorCH2.2SortedIdx<-which(vectorCH2.2==lowPoints.CH2.2[templowPoints.CH2.2.diff.SortedIdx])
     vectorCH2.2.Sorted.Idx<-c(vectorCH2.2.Sorted.Idx,tempvectorCH2.2SortedIdx)
   }
   lowPoints.CH2.2.Max5Point.vectorTime<-vectorTime[vectorCH2.2.Sorted.Idx][1:3]
   lowPoints.CH2.2.Max5Point.vectorCH2.2<-lowPoints.CH2.2[lowPoints.CH2.2.diff.SortedIdx]
   # print(lowPoints.CH2.2.diff.SortedIdx)
   # print(vectorTime[vectorCH2.2.Sorted.Idx])
   # print(lowPoints.CH2.2.diff.DecSorted)
   # print(lowPoints.CH2.2[lowPoints.CH2.2.diff.SortedIdx])
   
   # #单次高电平持续时间异常位置
   # #step2:按周期差值排序，获取到排序的索引
   # print(high.CH2.2.timespan)
   high.CH2.2.timespan.diff<-abs(high.CH2.2.timespan - mean(high.CH2.2.timespan))
   # print(high.CH2.2.timespan.diff)
   # print(sort(Cycle.diff, decreasing = TRUE))
   high.CH2.2.timespan.diff.DecSorted=sort(high.CH2.2.timespan.diff, decreasing = TRUE)
   high.CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
   for (variable in high.CH2.2.timespan.diff.DecSorted) {
     temphigh.CH2.2.timespan.diff.SortedIdx<-which(high.CH2.2.timespan.diff==variable)
     high.CH2.2.timespan.diff.SortedIdx<-c(high.CH2.2.timespan.diff.SortedIdx,temphigh.CH2.2.timespan.diff.SortedIdx)
   }
   high.CH2.2.timespan.diff.SortedIdx<-high.CH2.2.timespan.diff.SortedIdx[1:length(high.CH2.2.timespan)]
   print(high.CH2.2.timespan.diff.SortedIdx)
   
   # #单次低电平持续时间异常位置
   #step2:按周期差值排序，获取到排序的索引
   # print(low.CH2.2.timespan)
   low.CH2.2.timespan.diff<-abs(low.CH2.2.timespan - mean(low.CH2.2.timespan))
   low.CH2.2.timespan.diff.DecSorted=sort(low.CH2.2.timespan.diff, decreasing = TRUE)
   low.CH2.2.timespan.diff.SortedIdx<-vector(mode="numeric",length=0)
   for (variable in low.CH2.2.timespan.diff.DecSorted) {
     templow.CH2.2.timespan.diff.SortedIdx<-which(low.CH2.2.timespan.diff==variable)
     low.CH2.2.timespan.diff.SortedIdx<-c(low.CH2.2.timespan.diff.SortedIdx,templow.CH2.2.timespan.diff.SortedIdx)
     
   }
   low.CH2.2.timespan.diff.SortedIdx<-low.CH2.2.timespan.diff.SortedIdx[1:length(low.CH2.2.timespan)]
   print(low.CH2.2.timespan.diff.SortedIdx)
   # #单次低电平持续时间异常位置
   
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
               "单次低电平持续时间异常位置"#,
               # "单次关联动作时序起始位置差均值",
               # "单次关联动作时序起始位置差最大值",
               # "单次关联动作时序起始位置差最小值",
               # "单次关联动作时序起始异常位置",
               # "单次关联动作时序结束位置差均值",
               # "单次关联动作时序结束位置差最大值",
               # "单次关联动作时序位置位置差最小值",
               # "单次关联动作时序结束异常位置"
               ),
      CH2_2_Value = as.character(c(

        signif(Cycle.avg.CH2.2,7),#nrow(csvdata()),#1,#max(csvdata$Time.s.), 
        signif(highMean,7),
        signif(highMax,7),
        signif(highMin,7),
         str_c(highPoints.CH2.2.Max5Point.vectorTime,collapse=','),
        signif(lowMean,7),
        signif(lowMax,7),
        signif(lowMin,7),
         str_c(lowPoints.CH2.2.Max5Point.vectorTime,collapse=','),
        signif(mean(high.CH2.2.timespan),7),
        signif(max(high.CH2.2.timespan),7),
        signif(min(high.CH2.2.timespan),7),
        str_c(high.CH2.2.timespan.diff.SortedIdx,collapse=','),
        signif(mean(low.CH2.2.timespan),7),
        signif(max(low.CH2.2.timespan),7),
        signif(min(low.CH2.2.timespan),7),
        str_c(low.CH2.2.timespan.diff.SortedIdx,collapse=',')#,
        # signif(Start.CH3.2_CH2.2.timespan.avg,7),
        # signif(Start.CH3.2_CH2.2.timespan.max,7),
        # signif(Start.CH3.2_CH2.2.timespan.min,7),
        # str_c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=','),
        # signif(End.CH3.2_CH2.2.timespan.avg,7),
        # signif(End.CH3.2_CH2.2.timespan.max,7),
        # signif(End.CH3.2_CH2.2.timespan.min,7),
        # str_c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=',')
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
  
  # Show plot
  output$sepratePlot <- renderPlot({
    df<-csvdata()
    vectorCHs<- colnames(df)[-1]
    # print(vectorCHs)
     if(input$disp == "One") {
       # ggplot(df, aes(df$Time.s.)) + 
       #   geom_line(aes(y = df$CH2.2.V., colour = "var0")) + 
       #   geom_line(aes(y = df$CH3.2.V., colour = "var1"))+
       #   scale_x_continuous(limits=input$slider)
       Y.offset<-vector(mode="numeric",length=length(vectorCHs))
       CHIdx<-which(vectorCHs==input$ddloffsetCH)
       Y.offset[CHIdx]<-input$numYoffset
       # print(Y.offset)
       
       icons <- c(input$icons)
       icons<-substring(icons, 1, 8)
       p<- ggplot(df, aes(df$Time.s.))+scale_x_continuous(limits=input$slider)
       for (v in icons) {
         idx<-which(vectorCHs==v)#获取勾选的通道的索引
         nam <- paste("plot", vectorCHs[idx], sep = ".")
         Yoffset<-Y.offset[idx]
         # print(Yoffset)
         YPoints<-df[,idx+1]+Yoffset
         pcolour<-as.character(vectorCHs[idx]) 
         nam<- funGetgeom_line(YPoints,pcolour)
         p<-p+nam
         print(p)
       }
    }
    else {
      icons <- c(input$icons)
      icons<-substring(icons, 1, 8)
      # print(icons)
      plotlist1<-list()
      for (v in icons) {
        idx<-which(vectorCHs==v)#获取勾选的通道的索引
        nam <- paste("plot", icons[idx], sep = ".")
        nam<- funGetPlot(df,idx+1,input$slider,icons[idx])
        print(nam)#这是一个超级大坑，一定要print出来，否则报错
        plotlist1[[idx]]<- nam
      }
      x = length(plotlist1)
      cols = round(sqrt(x),0)
      rows = ceiling(x/cols)
      cols=1
      rows=length(plotlist1)
      bl <- (length(plotlist1)==0)
      if(bl)
      {}
      else
      {
        ggarrange(plotlist = plotlist1, ncol=cols, nrow = rows)
      }
      # ggarrange(plotlist = plotlist1, ncol=cols, nrow = rows)
      # do.call("ggarrange", c(plist, ncol=nCol))
      # ggarrange(plist,ncol=1,nrow=2,labels=c("A","B"))
    }
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
  print(paste("xiiiiiiii",pulseQtyperCycle)) 
  if((length(vector)) %% pulseQtyperCycle == 0)
  {
    Cycle.Qty<-(length(vector)) %/% pulseQtyperCycle-1
  }
  else
  {
    Cycle.Qty<-(length(vector)) %/% pulseQtyperCycle
  }
}
# Create Shiny app ----
shinyApp(ui, server)


