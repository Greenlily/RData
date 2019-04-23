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
      
      tableOutput("timeMapData")
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
                   tabPanel("CH2.2.V.中间盘感应",tableOutput("CH2.2Values")),
                   tabPanel("CH2.3.V.POS PIN 感应"),
                   tabPanel("CH2.4.V.POS PIN 位置感应"),
                   tabPanel("CH3.1.V.分离感应"),
                   tabPanel("CH3.2.V.中间盘动作",tableOutput("CH3.2Values")),
                   tabPanel("CH3.3.V.植入动作"),
                   tabPanel("CH3.4.V.上烙铁动作"),
                   tabPanel("CH4.1.V.POS PIN 动作"),
                   tabPanel("CH4.2.V.分离动作",tableOutput("CH4.2Values")),
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
  
  timeMapData <- reactive({
    data.frame(
      Item = c("t1",
               "t2",
               "t3",
               "t4",
               "t5",
               "t6",
               "t7",
               "t8",
               "t9",
               "t10",
               "t11",
               "t12",
               "t13",
               "t14",
               "t15",
               "t16",
               "t17",
               "t18",
               "t19",
               "t20",
               "t21",
               "t22",
               "t23",
               "t24",
               "t25",
               "t26",
               "t27",
               "t28",
               "t29",
               "t30",
               "t31",
               "t32",
               "t33",
               "t34",
               "t35",
               "t36",
               "t37",
               "t38",
               "t39",
               "t40",
               "t41",
               "t42",
               "t43",
               "t44"
               ),
      Value = as.character(c(
        "分离动作下降状态持续时间",
        "物料感应感应到有料到分离动作上升时间",
        "分离动作上升状态持续时间",
        "中间盘动作结束到下次分离下降动作时间",
        "分离动作下降开始到分离下限感应到时间",
        "分离电磁铁吸合状态持续时间(分离针在下限状态）",
        "分离电磁铁断电状态持续时间（分离针在上限状态）",
        "分离感应到到物料感应到持续时间",
        "物料感应到持续时间",
        "物料感应到到中间盘开始动作时间",
        
        "中间盘动作开始到中间盘感应到动作的时间",
        "中间盘感应到状态持续时间",
        "中间盘波形可调电阻VR1",
        "中间盘波形可调电阻VR2",
        "中间盘波形可调电阻VR3",
        "检测部上升感应结束到中间盘下次动作时间",
        "中间盘感应到结束到检测部动作开始时间",
        "检测电磁铁可调电阻",
        "检测电磁铁可调电阻",
        "检测电磁铁可调电阻",
        
        "检测部吸合动作开始到感应到检测吸合上升到位时间",
        "检测部感应到上升状态持续时间",
        "中间盘感应到动作结束到植入电磁铁断电时间",
        "植入上限感应不到状态持续时间",
        "中间盘感应到动作结束到植入电磁铁吸合通电时间",
        "吸合动作开始到植入上限感应到时间",
        "植入上限感应到到中间盘二次动作时间",
        "植入电磁铁波形可调电阻VR5",
        "植入电磁铁波形可调电阻VR6",
        "植入电磁铁波形可调电阻VR7",
        
        "中间盘感应到结束到植入下限感应到时间",
        "植入下限感应到状态持续时间",
        "植入下限感应到状态持续时间",
        "V-NOCH上限感应不到持续时间",
        "V-NOCH下降动作持续时间",
        "V-NOCH下降动作开始到POS光纤感应不到时间（纸带开始转动）",
        "纸带开始转动到植入离开下限位置时间",
        "纸带持续转动时间",
        "POS 光感感应到到V-NOCH吸合动作结束时间",
        "V-NOCH吸合动作结束到上烙铁断电下压时间",
        
        "上烙铁持续下压时间",
        "中间盘动作结束到下次开始",
        "植入动作结束到下次开始",
        "V-NOCH 动作结束到下次开始"
      ), 
      stringsAsFactors=FALSE)
    )
  })
  InitValues <- reactive({
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    #处理动作波形CH3.2
    pulse.x.CH3.2<- funGetPulseX(vectorTime,vectorCH3.2,2,10,8)[[1]]
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
  CH3.2Data <- reactive({
    
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    
    #处理动作波形CH3.2
    pulse.x.CH3.2<- funGetPulseX(vectorTime,vectorCH3.2,2,10,8)[[1]]
    #  #处理感应波形CH2.2
    pulse.x.CH2.2<- funGetPulseX(vectorTime,vectorCH2.2,7,10,2)[[1]]
    pulse.x.CH2.2.idx<-funGetPulseX(vectorTime,vectorCH2.2,7,10,2)[[2]]
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
        # "高电平均值",
        # "第一次高电平持续时间均值",
        # "单周期内第一次高电平结束到第二次高电平起始时间差记录均值",
        # "第二次高电平持续时间均值",
        "t13",
        "t14",
        "t15",
        "t42",
        "t11均值",
        "t11最大值",
        "t11最小值",
        "t11异常位置",
        "单次关联动作时序结束位置差均值",
        "单次关联动作时序结束位置差最大值",
        "单次关联动作时序位置位置差最小值",
        "单次关联动作时序结束异常位置"
      ),
      CH3_2_Value = as.character(c(
        signif(mean(Cycle.Each.TimeSpan),7),
        # signif(mean(highPoints.CH3.2),7),
        # signif(mean(high.1st.timespan),7),
        # signif(mean(high.1stEnd2ndStart.timespan),7),
        # signif(mean(high.2nd.timespan),7),
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
  CH2.2Data <- reactive({
    
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V.
    vectorTime=df$Time.s.
    
    #处理动作波形CH3.2
    pulse.x.CH3.2<- funGetPulseX(vectorTime,vectorCH3.2,2,10,8)[[1]]
    pulse.x.CH3.2.idx<-funGetPulseX(vectorTime,vectorCH3.2,2,10,8)[[2]]
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    Cycle.Qty.CH3.2<-funGetCycleQty(pulse.x.CH3.2,8)
    # print(Cycle.Qty.CH3.2)
    
    #  #处理感应波形CH2.2
    pulse.x.CH2.2<- funGetPulseX(vectorTime,vectorCH2.2,7,10,2)[[1]]
    pulse.x.CH2.2.idx<-funGetPulseX(vectorTime,vectorCH2.2,7,10,2)[[2]]
    # print(paste("CH2.2 x节点：",pulse.x.CH2.2))
    # print(paste("嘿嘿嘿嘿",pulse.x.CH2.2.idx))
    Cycle.Qty.CH2.2<-funGetCycleQty(pulse.x.CH2.2,2)
    # print(Cycle.Qty.CH2.2)
    #处理感应波形CH2.2
    
    highPoints.CH2.2 <- funGet.HighLow.2Pulse.PerCycle(vectorCH2.2,pulse.x.CH2.2.idx,Cycle.Qty.CH2.2)[[1]]
    lowPoints.CH2.2 <- funGet.HighLow.2Pulse.PerCycle(vectorCH2.2,pulse.x.CH2.2.idx,Cycle.Qty.CH2.2)[[2]]
  
    list.2pulse.timespan.CH2.2 <- funGet.2pulse.timespan(vectorCH2.2,pulse.x.CH2.2.idx,pulse.x.CH2.2,2,Cycle.Qty.CH2.2)
    # cnt <- 0
    high.CH2.2.timespan <- list.2pulse.timespan.CH2.2[[1]]
    
    print(paste("high.CH2.2.timespan:",high.CH2.2.timespan))
    low.CH2.2.timespan<-list.2pulse.timespan.CH2.2[[2]]
    # Start.CH3.2_CH2.2.timespan<-list.2pulse.timespan.CH2.2[[3]]
    # End.CH3.2_CH2.2.timespan<-list.2pulse.timespan.CH2.2[[4]]
    
    # Start.CH3.2_CH2.2.timespan.diff.SortedIdx<- funGet.timespan.Outliers(Start.CH3.2_CH2.2.timespan)[[2]]
    # End.CH3.2_CH2.2.timespan.diff.SortedIdx<-funGet.timespan.Outliers(End.CH3.2_CH2.2.timespan)[[2]]
    
    #CH2.2总周期时间段
    TotalCycle.TimeSpan.CH2.2<-pulse.x.CH2.2[2*Cycle.Qty.CH2.2+1]-pulse.x.CH2.2[1]
    #CH2.2平均周期
    Cycle.avg.CH2.2 <- TotalCycle.TimeSpan.CH2.2/Cycle.Qty.CH2.2
    # print(Cycle.avg.CH2.2)
    # 
    highPoints.CH2.2.Max5Point.vectorTime <- funGet.Y.Outliers(highPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    high.CH2.2.timespan.diff.SortedIdx <- funGet.timespan.Outliers(high.CH2.2.timespan)[[2]]
    # # print(highPoints.CH2.2.Max5Point.vectorTime)
    # 
    lowPoints.CH2.2.Max5Point.vectorTime<- funGet.Y.Outliers(lowPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    low.CH2.2.timespan.diff.SortedIdx <- funGet.timespan.Outliers(low.CH2.2.timespan)[[2]]
    
    # Compose data frame
    data.frame(
      CH2_2 = c(
        "周期均值", 
        # "高电平均值",
        # "高电平最大值",
        # "高电平最小值",
        # "单次高电平异常位置",
        # "低电平均值", 
        # "低电平最大值",
        # "低电平最小值",
        # "单次低电平异常位置",
        "t12均值",
        "t12最大值",
        "t12最小值",
        "t12异常位置",
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
        # signif(mean(highPoints.CH2.2),7),
        # signif(max(highPoints.CH2.2),7),
        # signif(min(highPoints.CH2.2),7),
        # str_c(highPoints.CH2.2.Max5Point.vectorTime,collapse=','),
        # signif(mean(lowPoints.CH2.2),7),
        # signif(max(lowPoints.CH2.2),7),
        # signif(min(lowPoints.CH2.2),7),
        # str_c(lowPoints.CH2.2.Max5Point.vectorTime,collapse=','),
        signif(mean(high.CH2.2.timespan),7),
        signif(max(high.CH2.2.timespan),7),
        signif(min(high.CH2.2.timespan),7),
        str_c(high.CH2.2.timespan.diff.SortedIdx,collapse=','),
        signif(mean(low.CH2.2.timespan),7),
        signif(max(low.CH2.2.timespan),7),
        signif(min(low.CH2.2.timespan),7),
        str_c(low.CH2.2.timespan.diff.SortedIdx,collapse=',')#,
        # signif(mean(Start.CH3.2_CH2.2.timespan),7),
        # signif(max(Start.CH3.2_CH2.2.timespan),7),
        # signif(min(Start.CH3.2_CH2.2.timespan),7),
        # str_c(Start.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=','),
        # signif(mean(End.CH3.2_CH2.2.timespan),7),
        # signif(max(End.CH3.2_CH2.2.timespan),7),
        # signif(min(End.CH3.2_CH2.2.timespan),7),
        # str_c(End.CH3.2_CH2.2.timespan.diff.SortedIdx,collapse=',')
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
    pulse.x.CH3.2<- funGetPulseX(vectorTime,vectorCH3.2,2,10,8)[[1]]
    Cycle.Qty.CH3.2<-funGetCycleQty(pulse.x.CH3.2,8)
    # print(paste("CH3.2 x节点：",pulse.x.CH3.2) )
    # print(Cycle.Qty.CH3.2)
    
    pulse.x.CH2.2<- funGetPulseX(vectorTime,vectorCH2.2,7,10,2)[[1]]
    pulse.x.CH2.2.idx<-funGetPulseX(vectorTime,vectorCH2.2,7,10,2)[[2]]
    # print(paste("CH2.2 x节点：",pulse.x.CH2.2))
    # print(paste("嘿嘿嘿嘿",pulse.x.CH2.2.idx))
    Cycle.Qty.CH2.2<-funGetCycleQty(pulse.x.CH2.2,2)
    # print(Cycle.Qty.CH2.2)
    #处理感应波形CH2.2
    
    list.HighLow.2Pulse.PerCycle <- funGet.HighLow.2Pulse.PerCycle(vectorCH2.2,pulse.x.CH2.2.idx,Cycle.Qty.CH2.2)
    highPoints.CH2.2 <- list.HighLow.2Pulse.PerCycle[[1]]
    lowPoints.CH2.2 <- list.HighLow.2Pulse.PerCycle[[2]]
    
    list.2pulse.timespan.CH2.2 <- funGet.2pulse.timespan(vectorCH2.2,pulse.x.CH2.2.idx,pulse.x.CH2.2,2,Cycle.Qty.CH2.2)
    
    high.CH2.2.timespan<-list.2pulse.timespan.CH2.2[[1]]
    low.CH2.2.timespan<-list.2pulse.timespan.CH2.2[[2]]
    # Start.CH3.2_CH2.2.timespan<-list.2pulse.timespan.CH2.2[[3]]
    # End.CH3.2_CH2.2.timespan<-list.2pulse.timespan.CH2.2[[4]]
    
    highPoints.CH2.2.Max5Point.vectorTime <- funGet.Y.Outliers(highPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    list.highPoints.CH2.2.timespan.Outliers <- funGet.timespan.Outliers(high.CH2.2.timespan)
    high.CH2.2.Timespan.diff.Max <- list.highPoints.CH2.2.timespan.Outliers[[1]][1]
    
    lowPoints.CH2.2.Max5Point.vectorTime<- funGet.Y.Outliers(lowPoints.CH2.2,vectorTime,vectorCH2.2)[[1]]
    list.lowPoints.CH2.2.timespan.Outliers <- funGet.timespan.Outliers(low.CH2.2.timespan)
    low.CH2.2.timespan.diff.Max <- list.lowPoints.CH2.2.timespan.Outliers[[1]][1]
    

    
    list.8pulse.timespan <- funGet.8pulse.timespan(pulse.x.CH3.2,8,pulse.x.CH2.2,2,Cycle.Qty.CH3.2)
    high.1stStart2ndStart.timespan <- list.8pulse.timespan[[4]]
    high.2ndStart3rdStart.timespan <- list.8pulse.timespan[[5]]
    high.3rdStart4thStart.timespan <- list.8pulse.timespan[[6]]
    thisCycleEnd_nextCycleStart.timespan <- list.8pulse.timespan[[7]]
    Start.CH3.2_CH2.2.timespan <- list.8pulse.timespan[[8]]
    End.CH3.2_CH2.2.timespan <- list.8pulse.timespan[[9]]
    
     list.Start.CH3.2_CH2.2.timespan.Outliers <- funGet.timespan.Outliers(Start.CH3.2_CH2.2.timespan)
     Start.CH3.2_CH2.2.timespan.diff.Max <-list.Start.CH3.2_CH2.2.timespan.Outliers[[1]][1]

     list.End.CH3.2_CH2.2.timespan.Outliers <- funGet.timespan.Outliers(End.CH3.2_CH2.2.timespan)
     End.CH3.2_CH2.2.timespan.diff.Max <-list.End.CH3.2_CH2.2.timespan.Outliers[[1]][1]
    
    # Start.CH3.2_CH2.2.timespan.diff.SortedIdx<- funGet.timespan.Outliers(Start.CH3.2_CH2.2.timespan)[[2]]
    # End.CH3.2_CH2.2.timespan.diff.SortedIdx<-funGet.timespan.Outliers(End.CH3.2_CH2.2.timespan)[[2]]
    # Compose data frame
    data.frame(
      Item = c(
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
      Value = as.character(c(
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
  
  InitData <- reactive({
    df<-csvdata()
    vectorCHs<- colnames(df)
    vectorTime=df$Time.s.
    vectorCH4.2=df$CH4.2.V
    vectorCH3.1=df$CH3.1.V
    vectorCH1.1=df$CH1.1.V
    vectorCH2.2=df$CH2.2.V
    vectorCH3.2=df$CH3.2.V
    vectorCH2.1=df$CH2.1.V
    vectorCH4.4=df$CH4.4.V#4脉冲波需要清洗数据
    vectorCH1.2=df$CH1.2.V
    vectorCH3.3=df$CH3.3.V#4脉冲波需要清洗数据
    vectorCH1.3=df$CH1.3.V
    vectorCH2.3=df$CH2.3.V
    vectorCH4.1=df$CH4.1.V
    vectorCH2.4=df$CH2.4.V
    vectorCH4.2=df$CH4.2.V
   
    
    
    list.pulse.x.CH4.2 <- funGetPulseX(vectorTime,vectorCH4.2,4,5,2)
    list.pulse.x.CH3.1 <- funGetPulseX(vectorTime,vectorCH3.1,4,5,2)
    list.pulse.x.CH1.1 <- funGetPulseX(vectorTime,vectorCH1.1,4,1500,2)
    list.pulse.x.CH3.2 <- funGetPulseX(vectorTime,vectorCH3.2,2,10,8)
    list.pulse.x.CH2.2 <- funGetPulseX(vectorTime,vectorCH2.2,7,10,2)
    list.pulse.x.CH2.1 <- funGetPulseX(vectorTime,vectorCH2.1,7,10,2)
    list.pulse.x.CH4.4 <- funGetPulseX(vectorTime,vectorCH4.4,2,5,4)
    
    list.pulse.x.CH1.2 <- funGetPulseX(vectorTime,vectorCH1.2,7,10,2)
    list.pulse.x.CH3.3 <- funGetPulseX(vectorTime,vectorCH3.3,2,5,4)
    list.pulse.x.CH1.3 <- funGetPulseX(vectorTime,vectorCH1.3,7,10,2)
    list.pulse.x.CH2.3 <- funGetPulseX(vectorTime,vectorCH2.3,7,10,2)
    list.pulse.x.CH4.1 <- funGetPulseX(vectorTime,vectorCH4.1,2,5,2)
    list.pulse.x.CH2.4 <- funGetPulseX(vectorTime,vectorCH2.4,7,10,2)
    list.pulse.x.CH4.2 <- funGetPulseX(vectorTime,vectorCH4.2,3,10,2)

    pulse.x.CH4.2 <- list.pulse.x.CH4.2[[1]]
    pulse.x.CH3.1 <- list.pulse.x.CH3.1[[1]]
    pulse.x.CH1.1 <- list.pulse.x.CH1.1[[1]]
    pulse.x.CH3.2 <- list.pulse.x.CH3.2[[1]]
    pulse.x.CH2.2 <- list.pulse.x.CH2.2[[1]]
    pulse.x.CH2.1 <- list.pulse.x.CH2.1[[1]]
    pulse.x.CH4.4 <- list.pulse.x.CH4.4[[1]]
    
    pulse.x.CH1.2 <- list.pulse.x.CH1.2[[1]]
    pulse.x.CH3.3 <- list.pulse.x.CH3.3[[1]]
    pulse.x.CH1.3 <- list.pulse.x.CH1.3[[1]]
    pulse.x.CH2.3 <- list.pulse.x.CH2.3[[1]]
    pulse.x.CH4.1 <- list.pulse.x.CH4.1[[1]]
    pulse.x.CH2.4 <- list.pulse.x.CH2.4[[1]]
    pulse.x.CH4.2 <- list.pulse.x.CH4.2[[1]]
    
    
    Pulse.X.idx.CH4.2 <-list.pulse.x.CH4.2[[2]]
    Pulse.X.idx.CH3.1 <-list.pulse.x.CH3.1[[2]]
    Pulse.X.idx.CH1.1 <-list.pulse.x.CH1.1[[2]]
    Pulse.X.idx.CH3.2 <-list.pulse.x.CH3.2[[2]]
    Pulse.X.idx.CH2.2 <-list.pulse.x.CH2.2[[2]]
    Pulse.X.idx.CH2.1 <-list.pulse.x.CH2.1[[2]]
    Pulse.X.idx.CH4.4 <-list.pulse.x.CH4.4[[2]]
    
    Pulse.X.idx.CH1.2 <-list.pulse.x.CH1.2[[2]]
    Pulse.X.idx.CH3.3 <-list.pulse.x.CH3.3[[2]]
    Pulse.X.idx.CH1.3 <-list.pulse.x.CH1.3[[2]]
    Pulse.X.idx.CH2.3 <-list.pulse.x.CH2.3[[2]]
    Pulse.X.idx.CH4.1 <-list.pulse.x.CH4.1[[2]]
    Pulse.X.idx.CH2.4 <-list.pulse.x.CH2.4[[2]]
    Pulse.X.idx.CH4.2 <-list.pulse.x.CH4.2[[2]]
    
    Cycle.Qty.CH2.2 <- funGetCycleQty(pulse.x.CH2.2,2)
    Cycle.Qty.CH3.2 <- funGetCycleQty(pulse.x.CH3.2,8)
    Cycle.Qty.CH4.4 <- funGetCycleQty(pulse.x.CH4.4,4)
    
    
    #过滤时序脉冲，清洗掉动作波脉冲发生前的感应波脉冲
    # print(pulse.x.CH3.1 > pulse.x.CH4.2[1])
    Pulse.X.idx.CH3.1 <- Pulse.X.idx.CH3.1[pulse.x.CH3.1 > pulse.x.CH4.2[1]]
    pulse.x.CH3.1 <- pulse.x.CH3.1[pulse.x.CH3.1 > pulse.x.CH4.2[1]]
    # print(pulse.x.CH3.1)
    # print(Pulse.X.idx.CH3.1)
    
    # print(Pulse.X.idx.CH3.1)
    Pulse.X.idx.CH1.1  <- Pulse.X.idx.CH1.1[pulse.x.CH1.1 > pulse.x.CH3.1[1]]
    pulse.x.CH1.1 <- pulse.x.CH1.1[pulse.x.CH1.1 > pulse.x.CH3.1[1]]
    
    Cycle.Qty.CH4.2 <- funGetCycleQty(pulse.x.CH4.2,2)
    list.2pulse.timespan.CH4.2 <- funGet.2pulse.timespan(vectorCH4.2,Pulse.X.idx.CH4.2,pulse.x.CH4.2,2,Cycle.Qty.CH4.2)
    t1 <- list.2pulse.timespan.CH4.2[[2]]
    t3 <- list.2pulse.timespan.CH4.2[[1]]
    
    Cycle.Qty.CH3.1 <- funGetCycleQty(pulse.x.CH3.1,2)
    list.2pulse.timespan.CH3.1 <- funGet.2pulse.timespan(vectorCH3.1,Pulse.X.idx.CH3.1,pulse.x.CH3.1,2,Cycle.Qty.CH3.1)
    t6 <- list.2pulse.timespan.CH3.1[[1]]
    t7 <- list.2pulse.timespan.CH3.1[[2]]
    
    Cycle.Qty.CH1.1 <- funGetCycleQty(pulse.x.CH1.1,2)
    list.2pulse.timespan.CH1.1 <- funGet.2pulse.timespan(vectorCH1.1,Pulse.X.idx.CH1.1,pulse.x.CH1.1,2,Cycle.Qty.CH1.1)
    t9 <- list.2pulse.timespan.CH1.1[[1]]
    
    Cycle.Qty.CH2.1 <- funGetCycleQty(pulse.x.CH2.1,2)
    list.2pulse.timespan.CH2.1 <- funGet.2pulse.timespan(vectorCH2.1,Pulse.X.idx.CH2.1,pulse.x.CH2.1,2,Cycle.Qty.CH2.1)
    t22 <- list.2pulse.timespan.CH2.1[[1]]
    
    Cycle.Qty.CH1.2 <- funGetCycleQty(pulse.x.CH1.2,2)
    list.2pulse.timespan.CH1.2 <- funGet.2pulse.timespan(vectorCH1.2,Pulse.X.idx.CH1.2,pulse.x.CH1.2,2,Cycle.Qty.CH1.2)
    t24 <- list.2pulse.timespan.CH1.2[[1]]
    
    Cycle.Qty.CH3.3 <- funGetCycleQty(pulse.x.CH3.3,4)
    list.4pulse.timespan.CH3.3 <- funGet.4pulse.timespan(vectorCH3.3,Pulse.X.idx.CH3.3,pulse.x.CH3.3,4,Cycle.Qty.CH3.3)
    t28 <- list.4pulse.timespan.CH3.3[[1]]
    t29 <- list.4pulse.timespan.CH3.3[[2]]
    t30 <- list.4pulse.timespan.CH3.3[[3]]
    
    Cycle.Qty.CH1.3 <- funGetCycleQty(pulse.x.CH1.3,2)
    list.2pulse.timespan.CH1.3 <- funGet.2pulse.timespan(vectorCH1.3,Pulse.X.idx.CH1.3,pulse.x.CH1.3,2,Cycle.Qty.CH1.3)
    t32 <- list.2pulse.timespan.CH1.3[[2]]
    
    Cycle.Qty.CH2.3 <- funGetCycleQty(pulse.x.CH2.3,2)
    list.2pulse.timespan.CH2.3 <- funGet.2pulse.timespan(vectorCH2.3,Pulse.X.idx.CH2.3,pulse.x.CH2.3,2,Cycle.Qty.CH2.3)
    t34 <- list.2pulse.timespan.CH2.3[[1]]
    
    Cycle.Qty.CH4.1 <- funGetCycleQty(pulse.x.CH4.1,2)
    list.2pulse.timespan.CH4.1 <- funGet.2pulse.timespan(vectorCH4.1,Pulse.X.idx.CH4.1,pulse.x.CH4.1,2,Cycle.Qty.CH4.1)
    t35 <- list.2pulse.timespan.CH4.1[[2]]
    
    Cycle.Qty.CH2.4 <- funGetCycleQty(pulse.x.CH2.4,2)
    list.2pulse.timespan.CH2.4 <- funGet.2pulse.timespan(vectorCH2.4,Pulse.X.idx.CH2.4,pulse.x.CH2.4,2,Cycle.Qty.CH2.4)
    t38 <- list.2pulse.timespan.CH2.4[[1]]
    
    Cycle.Qty.CH4.2 <- funGetCycleQty(pulse.x.CH4.2,2)
    list.2pulse.timespan.CH4.2 <- funGet.2pulse.timespan(vectorCH4.2,Pulse.X.idx.CH4.2,pulse.x.CH4.2,2,Cycle.Qty.CH4.2)
    t41 <- list.2pulse.timespan.CH4.2[[1]]
    
    t2 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH1.1,Pulse.X.idx.CH1.1,vectorCH1.1,2,pulse.x.CH4.2,vectorCH4.2, 2,Cycle.Qty.CH4.2)
    t4 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.2,Pulse.X.idx.CH2.2,vectorCH2.2,2,pulse.x.CH4.2,vectorCH4.2, 2,Cycle.Qty.CH4.2)
    t5 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH4.2,Pulse.X.idx.CH4.2,vectorCH4.2,2,pulse.x.CH2.2,vectorCH2.2, 2,Cycle.Qty.CH2.2)
    t8 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH3.1,Pulse.X.idx.CH3.1,vectorCH3.1,2,pulse.x.CH1.1,vectorCH1.1, 2,Cycle.Qty.CH1.1)
    t10 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH1.1,Pulse.X.idx.CH1.1,vectorCH1.1,2,pulse.x.CH3.2,vectorCH3.2, 8,Cycle.Qty.CH3.2)
    t11 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH3.2,Pulse.X.idx.CH3.2,vectorCH3.2,8,pulse.x.CH2.2,vectorCH2.2, 2,Cycle.Qty.CH2.2)
    t16 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.1,Pulse.X.idx.CH2.1,vectorCH2.1,2,pulse.x.CH3.2,vectorCH3.2, 8,Cycle.Qty.CH3.2)
    t17 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.2,Pulse.X.idx.CH2.2,vectorCH2.2,2,pulse.x.CH4.4,vectorCH4.4, 4,Cycle.Qty.CH4.4)
    t21 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH4.4,Pulse.X.idx.CH4.4,vectorCH4.4,4,pulse.x.CH2.1,vectorCH2.1, 2,Cycle.Qty.CH2.1)
    t23 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.2,Pulse.X.idx.CH2.2,vectorCH2.2,2,pulse.x.CH1.2,vectorCH1.2, 2,Cycle.Qty.CH1.2)
    t25 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.2,Pulse.X.idx.CH2.2,vectorCH2.2,2,pulse.x.CH3.3,vectorCH3.3, 4,Cycle.Qty.CH3.3)
    t26 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH3.3,Pulse.X.idx.CH3.3,vectorCH3.3,4,pulse.x.CH1.2,vectorCH1.2, 2,Cycle.Qty.CH1.2)
    t27 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH1.2,Pulse.X.idx.CH1.2,vectorCH1.2,2,pulse.x.CH3.2,vectorCH3.2, 8,Cycle.Qty.CH3.2)
    t31 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.2,Pulse.X.idx.CH2.2,vectorCH2.2,2,pulse.x.CH1.3,vectorCH1.3, 2,Cycle.Qty.CH1.3)
    t33 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH1.3,Pulse.X.idx.CH1.3,vectorCH1.3,2,pulse.x.CH2.3,vectorCH2.3, 2,Cycle.Qty.CH2.3)
    t36 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH4.1,Pulse.X.idx.CH4.1,vectorCH4.1,2,pulse.x.CH2.4,vectorCH2.4, 2,Cycle.Qty.CH2.4)
    t37 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH2.4,Pulse.X.idx.CH2.4,vectorCH2.4,2,pulse.x.CH1.3,vectorCH1.3, 2,Cycle.Qty.CH1.3)
    t39 <- funGet.pulse.timespan.2Wave.2(pulse.x.CH2.4,Pulse.X.idx.CH2.4,vectorCH2.4,2,pulse.x.CH4.1,vectorCH4.1, 2,Cycle.Qty.CH4.1)
    t40 <- funGet.pulse.timespan.2Wave.1(pulse.x.CH4.1,Pulse.X.idx.CH4.1,vectorCH4.1,2,pulse.x.CH4.2,vectorCH4.2, 2,Cycle.Qty.CH4.2)
    
    Cycle.Qty.CH2.2 <- funGetCycleQty(pulse.x.CH2.2,2)
    list.2pulse.timespan.CH2.2 <- funGet.2pulse.timespan(vectorCH2.2,Pulse.X.idx.CH2.2,pulse.x.CH2.2,2,Cycle.Qty.CH2.2)
    t12 <- list.2pulse.timespan.CH2.2[[1]]
    # t7 <- list.2pulse.timespan.CH3.1[[2]]
    
    list.8pulse.timespan <- funGet.8pulse.timespan(pulse.x.CH3.2,8,pulse.x.CH2.2,2,Cycle.Qty.CH3.2)
    t13 <- list.8pulse.timespan[[4]]
    t14 <- list.8pulse.timespan[[5]]
    t15 <- list.8pulse.timespan[[6]]
    t42 <- list.8pulse.timespan[[7]]
    
    list.4pulse.timespan.CH4.4 <- funGet.4pulse.timespan(vectorCH4.4,Pulse.X.idx.CH4.4,pulse.x.CH4.4,4,Cycle.Qty.CH4.4)
    t18 <- list.4pulse.timespan.CH4.4[[1]]
    t19 <- list.4pulse.timespan.CH4.4[[2]]
    t20 <- list.4pulse.timespan.CH4.4[[3]]
    
    
    
    
    Cycle.Each.TimeSpan <- funGet.Cycle.Each.TimeSpan(Cycle.Qty.CH3.2,pulse.x.CH3.2,8)
    #速度v,1分钟有多少个周期，60000/平均周期
    v<-60/mean(Cycle.Each.TimeSpan)
    # Compose data frame
    data.frame(
      Item = c(
        "速度",
        "周期",
        "t1", 
        "t2",
        "t3",
        "t4",
        "t5",
        "t6", 
        "t7",
        "t8",
        "t9",
        "t10",
        "t11",
        "t12",
        "t13",
        "t14",
        "t15",
        "t16",
        "t17",
        "t18",
        "t19",
        "t20",
        "t21", 
        "t22",
        "t23",
        "t24",
        "t25",
        "t26",
        "t27",
        "t28",
        "t29",
        "t30",
        "t31",
        "t32",
        "t33",
        "t34",
        "t35",
        "t36",
        "t37",
        "t38",
        "t39",
        "t40",
        "t41",
        "t42"#,
        # "t43",
        # "t44"
      ),
      Value.ms = as.character(c(
        signif(v,7),
        signif(mean(Cycle.Each.TimeSpan*1000),7),
        signif(mean(t1*1000),7),
        signif(mean(t2*1000),7),
        signif(mean(t3*1000),7),
        signif(mean(t4*1000),7),
        signif(mean(t5*1000),7),
        signif(mean(t6*1000),7),
        signif(mean(t7*1000),7),
        signif(mean(t8*1000),7),
        signif(mean(t9*1000),7),
        signif(mean(t10*1000),7),
        signif(mean(t11*1000),7),
        signif(mean(t12*1000),7),
        signif(mean(t13*1000),7),
        signif(mean(t14*1000),7),
        signif(mean(t15*1000),7),
        signif(mean(t16*1000),7),
        signif(mean(t17*1000),7),
        signif(mean(t18*1000),7),
        signif(mean(t19*1000),7),
        signif(mean(t20*1000),7),
        signif(mean(t21*1000),7),
        signif(mean(t22*1000),7),
        signif(mean(t23*1000),7),
        signif(mean(t24*1000),7),
        signif(mean(t25*1000),7),
        signif(mean(t26*1000),7),
        signif(mean(t27*1000),7),
        signif(mean(t28*1000),7),
        signif(mean(t29*1000),7),
        signif(mean(t30*1000),7),
        signif(mean(t31*1000),7),
        signif(mean(t32*1000),7),
        signif(mean(t33*1000),7),
        signif(mean(t34*1000),7),
        signif(mean(t35*1000),7),
        signif(mean(t36*1000),7),
        signif(mean(t37*1000),7),
        signif(mean(t38*1000),7),
        signif(mean(t39*1000),7),
        signif(mean(t40*1000),7),
        signif(mean(t41*1000),7),
        signif(mean(t42*1000),7)#,
        # signif(mean(t43*1000),7),
        # signif(mean(t44*1000),7)
      ), 
      stringsAsFactors=FALSE)
    )
  })
  
  # Show the values using an HTML table
  output$InitValues <- renderTable({
    InitData()
  })
  output$CH3.2Values <- renderTable({
    CH3.2Data()
  })
  output$CH2.2Values <- renderTable({
    CH2.2Data()
  })
  output$CH4.2Values<- renderTable({
    CH4.2Data()
  })
  output$RegionValues<-renderTable({
    RegionSelectedValues()
  })
  output$timeMapData<-renderTable({
    timeMapData()
  })
  
  
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
      # print(length(isolate(input$SelectedCHs))) 
      SelectedCHs <- c(isolate(input$SelectedCHs))
      SelectedCHs <- substring(SelectedCHs, 1, 8)
      
      SelectedCHs.idx <- vector(mode="numeric",length = 0)
      for (variable in SelectedCHs) {
        idx <- which(vectorCHs == variable)
        SelectedCHs.idx <- c(SelectedCHs.idx,idx)
        # print(SelectedCHs.idx)
      }
      df<- df[,c(1,SelectedCHs.idx)]
    }
  })
  output$plotlyCH1.1 <- renderPlotly({
    df<-SelectedCHsData()
    
    if(input$disp == "One") {
      
    }
    else
    {
      vars <- setdiff(names(df), "Time.s.")
      plots <- lapply(vars, function(var) {
        plot_ly(df, x = ~df$Time.s., y = as.formula(paste0("~", var))) %>%
          add_lines(name = var)
      })
      subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)
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
#vectorCH2.2,Threshold.Y.1st=7,Threshold.X.2nd=10
#vectorCH4.2,Threshold.Y.1st=10,Threshold.X.2nd=10
funGetPulseX <- function(vectorX,vector.Y,Threshold.Y.1st,Threshold.X.2nd,pulse.QtyperCycel)
{
  vector.Y.1stDiff<-vector.Y[-1]-vector.Y[-length(vector.Y)]
  vector.Y.1stDiff.filtered<- vector.Y.1stDiff[abs(vector.Y.1stDiff) > Threshold.Y.1st] #
  vector.X.1stDiff<-which(abs(vector.Y.1stDiff) > Threshold.Y.1st)
  
  vector.X.2ndDiff<-vector.X.1stDiff[-1]-vector.X.1stDiff[-length(vector.X.1stDiff)]
  Pulse.X.idx<-vector.X.1stDiff[vector.X.2ndDiff > Threshold.X.2nd] + 1
  Pulse.X <- vectorX[Pulse.X.idx]
  Pulse.X <- Pulse.X[!is.na(Pulse.X)]
  
  
  if(pulse.QtyperCycel==4)#4脉冲波需要清洗数据,将第一个不足4脉冲的波形过滤掉
  {
    print("4脉冲波")
    # print()
    print(Pulse.X)
    print(Pulse.X.idx)
    print(Pulse.X[4])
    print(Pulse.X[1] + 0.007)
     # Pulse.X[4] > Pulse.X[1] + 0.007
   if(Pulse.X[4] - Pulse.X[1] > 0.007)
   {
    # print()
    Pulse.X <- Pulse.X[which(Pulse.X > (Pulse.X[1] + 0.007))]
   }
    # print(Pulse.X)
   
  }
  else
  {
    print("不是4脉冲波")
  }
  print(Pulse.X)
  
  outPara<-list(Pulse.X,Pulse.X.idx)
}
funGetPulseX.curve<-function(vector.X,vector.Y,Threshold.Y.1st,Threshold.X.2nd)
{
  # Slope<
  vector.Y.1stDiff<-vector.Y[-1]-vector.Y[-length(vector.Y)]
  vector.X.1stDiff<-vector.X[-1]-vector.X[-length(vector.X)]
  Slope <- vector.Y.1stDiff/vector.X.1stDiff
  Slope.diff <- Slope[-1]-Slope[-length(Slope)]
  Slope.Slope <- Slope.diff/vector.X.1stDiff
  slope.diff <- abs(Slope[-1]-Slope[-length(Slope)])
  slope.diff.ord<-slope.diff[order(slope.diff,decreasing=TRUE)[1:100]]
  print(slope.diff.ord)
  
  print(paste("二阶导",Slope.Slope))
  print(paste("二阶导",which(Slope.Slope==0)))
  print(paste("二阶导000",vector.X[which(Slope.Slope==0)]))
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

funGet.2pulse.timespan<-function(vectorY,vectorX.idx, pulse.X.CHn,pulse.QtyperCycel,Cycle.Qty)
{
  cnt <- 0
  high.CHn.timespan<-vector(mode="numeric",length=0)
  low.CHn.timespan<-vector(mode="numeric",length=0)
  while (cnt < Cycle.Qty) {
    # print(v)
    # print(which(vectorTime==pulse.x.CH2.2[1]))
    # p1<-which(vectorTime==pulse.x.CH3.2[4*cnt+1])
    # print(p1)
    # p2<-which(vectorTime==pulse.x.CH3.2[4*cnt+2])
    # print(p2)
    # high.First.y.Points<-c(high.First.y.Points
    #                        ,vectorCH3.2[p1:p2])
    if(vectorY[vectorX.idx[2]]-vectorY[vectorX.idx[1]] < 0)#识别高低电平变化规律
    {
      # print(paste("第一次电平转换为低——高电平"))
      temphigh.CHn.timespan<-pulse.X.CHn[pulse.QtyperCycel*cnt+2]-pulse.X.CHn[pulse.QtyperCycel*cnt+1]
      high.CHn.timespan<-c(high.CHn.timespan,temphigh.CHn.timespan)
      
      templow.CHn.timespan<-pulse.X.CHn[pulse.QtyperCycel*cnt+3]-pulse.X.CHn[pulse.QtyperCycel*cnt+2]
      low.CHn.timespan<-c(low.CHn.timespan,templow.CHn.timespan)
    }
    else
    {
      # print(paste("第一次电平转换为高——低电平"))
      templow.CHn.timespan<-pulse.X.CHn[pulse.QtyperCycel*cnt+2]-pulse.X.CHn[pulse.QtyperCycel*cnt+1]
      low.CHn.timespan<-c(low.CHn.timespan,templow.CHn.timespan)
      
      temphigh.CHn.timespan<-pulse.X.CHn[pulse.QtyperCycel*cnt+3]-pulse.X.CHn[pulse.QtyperCycel*cnt+2]
      high.CHn.timespan<-c(high.CHn.timespan,temphigh.CHn.timespan)
    }
    cnt = cnt + 1
  }
  outPara<-list(high.CHn.timespan,low.CHn.timespan)#,Start.CHaction_CHsense.timespan,End.CHaction_CHsense.timespan)
}
#t.type:计算时序差值类型，1：上升沿到上升沿；2、下降沿到下降沿；3、下降沿到上升沿；4、上升沿到下降沿
funGet.pulse.timespan.2Wave.1 <- function(pulse.X.1st,pulse.X.1st.idx,pulse.Y.1st,pulse.QtyperCycel.1st,pulse.X.2nd,pulse.Y.2nd, pulse.QtyperCycel.2nd,Cycle.Qty.2nd)
{
  t <- vector(mode="numeric",length = 0)
  L_H.Pulse.idx.1st <- vector(mode="numeric",length = 0)
  cnt <- 0
  #获取先脉冲的波上升沿时序
  if(pulse.Y.1st[pulse.X.1st.idx[2]]-pulse.Y.1st[pulse.X.1st.idx[1]] < 0)#识别高低电平变化规律
  {
    L_H.Pulse.idx.1st <- 1
  }
  else
  {
    L_H.Pulse.idx.1st <- 2
    # L_H.Pulse.idx.2nd <- which(pulse.X.2nd > pulse.X.1st[1])[1]
  }
  # print(paste("L_H.Pulse.idx.1st:",L_H.Pulse.idx.1st))
  L_H.Pulse.idx.2nd <- which(pulse.X.2nd > pulse.X.1st[L_H.Pulse.idx.1st])[1]
  # print(L_H.Pulse.idx.2nd)
  # print(paste("第一次电平转换为低——高电平"))
  while (cnt < Cycle.Qty.2nd) {
    t.temp <- pulse.X.2nd[pulse.QtyperCycel.2nd*cnt + L_H.Pulse.idx.2nd] - pulse.X.1st[pulse.QtyperCycel.1st*cnt + L_H.Pulse.idx.1st]
    # print(t.temp)
    t <- c(t,t.temp)
    
    cnt = cnt + 1
  }
  t
}
funGet.pulse.timespan.2Wave.2 <- function(pulse.X.1st,pulse.X.1st.idx,pulse.Y.1st,pulse.QtyperCycel.1st,pulse.X.2nd,pulse.Y.2nd, pulse.QtyperCycel.2nd,Cycle.Qty.2nd)
{
  t <- vector(mode="numeric",length = 0)
  H_L.Pulse.idx.1st <- vector(mode="numeric",length = 0)
  cnt <- 0
  #获取先脉冲的波上升沿时序
  if(pulse.Y.1st[pulse.X.1st.idx[2]]-pulse.Y.1st[pulse.X.1st.idx[1]] < 0)#识别高低电平变化规律
  {
    H_L.Pulse.idx.1st <- 2
  }
  else
  {
    H_L.Pulse.idx.1st <- 1
  }
  # print(paste("H_L.Pulse.idx.1st:",H_L.Pulse.idx.1st))
  H_L.Pulse.idx.2nd <- which(pulse.X.2nd > pulse.X.1st[H_L.Pulse.idx.1st])[1]
  # print(paste("第一次电平转换为低——高电平"))
  while (cnt < Cycle.Qty.2nd) {
    t.temp <- pulse.X.2nd[pulse.QtyperCycel.2nd*cnt + H_L.Pulse.idx.2nd] - pulse.X.1st[pulse.QtyperCycel.1st*cnt + H_L.Pulse.idx.1st]
    # print(t.temp)
    t <- c(t,t.temp)
    cnt = cnt + 1
  }
  t<- t[!is.na(t)]
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
#对于4个脉冲的波形，会传入经过预处理的pulse.X.CHn，去除第一个不足4个脉冲的波形，所有默认传入的都是有完整4个脉冲的波形，否则识别 t18,t19,t20太复杂
funGet.4pulse.timespan<-function(vectorY,vectorX.idx, pulse.X.CHn,pulse.QtyperCycel,Cycle.Qty)
{
  cnt <- 0
  # high.First.y.Points<-vector(mode="numeric",length=0)
  high.1st.timespan <- vector(mode="numeric",length=0)
  low.1st.timespan <- vector(mode="numeric",length=0)
  high.2nd.timespan <- vector(mode="numeric",length=0)
  while (cnt < Cycle.Qty) {
    temphigh.1st.timespan <- pulse.X.CHn[pulse.QtyperCycel*cnt+2]-pulse.X.CHn[pulse.QtyperCycel*cnt+1]
    high.1st.timespan <- c(high.1st.timespan,temphigh.1st.timespan)
      
    templow.1st.timespan <- pulse.X.CHn[pulse.QtyperCycel*cnt+3]-pulse.X.CHn[pulse.QtyperCycel*cnt+2]
    low.1st.timespan <- c(low.1st.timespan,templow.1st.timespan)
      
    temphigh.2nd.timespan <- pulse.X.CHn[pulse.QtyperCycel*cnt+4]-pulse.X.CHn[pulse.QtyperCycel*cnt+3]
    high.2nd.timespan <- c(high.2nd.timespan,temphigh.2nd.timespan)
   
    cnt = cnt + 1
  }
  outPara<-list(high.1st.timespan,low.1st.timespan,high.2nd.timespan)
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


