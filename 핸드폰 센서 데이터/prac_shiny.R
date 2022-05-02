#setwd("C:\\Users\\kimeu\\Desktop\\수업\\4학년 1학기\\비정형데이터분석\\자료\\A_DeviceMotion_data")
#library(shiny)
#library(tidyverse)
#library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("전처리",
             menuSubItem("통계", tabName = 'statistics'),
             menuSubItem("피크", tabName = 'peak'),
             menuSubItem("변화분석", tabName = 'change_point'),
             menuSubItem("PCA", tabName = 'PCA')),
    menuItem("모델사용", tabName = 'modeling')
    
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "statistics",
            fluidPage(
              titlePanel("통계"),
              fluidRow(
                column(
                  width = 3,
                  selectInput(inputId = 'statistics_input',
                              label = '통계치를 선택해 주세요',
                              choices = c("평균","최소값","최대값","표준편차","왜도","rms","rss","첨도"),
                              selected = NULL),
                  submitButton(text = "변경사항을 적용합니다.")
                ),
                column(
                  width = 4,
                  plotOutput(outputId = 'sta_plot_1')
                ),
                column(
                  width = 4,
                  plotOutput(outputId = 'sta_plot_2')
                )
              ),
              fluidRow(
                dataTableOutput(outputId = "print_sta_data")
              )
            )
    ),
    
    tabItem(tabName = 'peak',
            fluidPage(
              titlePanel("피크 분석"),
              fluidRow(
                column(
                  width = 4,
                  sliderInput(inputId = "threshold",
                              label = "임계치를 선택해주세요",
                              value = 3,
                              min = 1,
                              max = 5,
                              step = 1),
                  textInput(inputId = "peak_data",
                            label = "확인하고 싶은 데이터를 입력하세요",
                            value = 'dws_1/sub_1.csv',
                            placeholder = 'dws_1/sub_1.csv'),
                  submitButton(text = '변경사항을 적용합니다.'),
                  helpText("조금만 기다리면 나옵니다.")
                  
                ),
                column(
                  width = 6,
                  plotOutput(outputId = 'peak_plot')
                )
              ),
              fluidRow(
                column(
                  width = 10,
                  DT::dataTableOutput(outputId = "peak_DT")
                )
              )
            )
    ),
    
    tabItem(tabName = 'change_point',
            fluidPage(
              titlePanel("변화 분석"),
              fluidRow(
                column(
                  width = 3,
                  h2("무엇인가 쓰기"),
                  submitButton(text = '변경사항을 적용합니다.')
                )
              )
            )
    ),
    
    tabItem(tabName = "PCA",
            fluidPage(
              titlePanel("PCA"),
              fluidRow(
                column(
                  width = 8,
                  plotOutput(outputId = 'screeplot')
                ),
                column(
                  width = 12,
                  verbatimTextOutput(outputId = "PCA_print")
                )
              )
            )
    ),
    
    tabItem(tabName = 'modeling',
            fluidPage(
              titlePanel("결과"),
              fluidRow(
                column(
                  width = 3,
                  radioButtons(inputId = 'sel_model',
                               label = '사용할 모델을 선택하세요',
                               choices = c("RF"),
                               selected = NULL),
                  helpText('모델 특성을 선택해 주세요'),
                  selectInput(inputId = "PCA_sta",
                              label = 'PCA와 통계중 하나만 선택해 주세요.',
                              choices = c("통계","PCA"),
                              multiple = FALSE,
                              selected = NULL),
                  sliderInput(inputId = 'threshold_model',
                              label = ' 피크의 임계치를 선택해 주세요',
                              value = 3,
                              min = 1,
                              max = 5,
                              step = 1),
                  selectInput(inputId = "sel_ch",
                              label = '변화 분석을 사용할지 말지 선택해 주세요',
                              choices = c('yes', 'no'),
                              multiple = FALSE,
                              selected = NULL),
                  sliderInput(inputId = 'fold',
                              label = '교차 검증 횟수를 선택하세요',
                              value = 10,
                              min = 1,
                              max = 20,
                              step = 1),
                  helpText('훈련할땐 교차검증이 사용되지 않습니다.'),
                  submitButton(text = '변경사항을 적용합니다.')
                ),
                column(
                  width = 8,
                  uiOutput(outputId = 'mainUI')
                )
              )
            )
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = '비정형 과제'),
  sidebar,
  body
)

server <- function(input, output, sesstion){
  
  # 통계 분석
  statistics_data <- reactive({
    x <- HAR_summary
    x
  })
  
  output$sta_plot_1 <- renderPlot({
    if(input$statistics_input == "평균"){
      boxplot(HAR_summary$maguserAcceleration_fn1, main = "maguserAcceleration 평균의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "최소값"){
      boxplot(HAR_summary$maguserAcceleration_fn2, main = "maguserAcceleration 최소값의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "최대값"){
      boxplot(HAR_summary$maguserAcceleration_fn3, main = "maguserAcceleration 최대값의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "표준편차"){
      boxplot(HAR_summary$maguserAcceleration_fn4, main = "maguserAcceleration 표준편차의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "왜도"){
      boxplot(HAR_summary$maguserAcceleration_fn5, main = "maguserAcceleration 왜도의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "rms"){
      boxplot(HAR_summary$maguserAcceleration_fn6, main = "maguserAcceleration rms의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "rss"){
      boxplot(HAR_summary$maguserAcceleration_fn7, main = "maguserAcceleration rss의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "첨도"){
      boxplot(HAR_summary$maguserAcceleration_fn9, main = "maguserAcceleration 첨도의 박스 그래프", ylab = '값')
    }
  })
  
  output$sta_plot_2 <- renderPlot({
    if(input$statistics_input == "평균"){
      boxplot(HAR_summary$magrotationRate_fn1, main = "magrotationRate 평균의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "최소값"){
      boxplot(HAR_summary$magrotationRate_fn2, main = "magrotationRate 최소값의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "최대값"){
      boxplot(HAR_summary$magrotationRate_fn3, main = "magrotationRate 최대값의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "표준편차"){
      boxplot(HAR_summary$magrotationRate_fn4, main = "magrotationRate 표준편차의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "왜도"){
      boxplot(HAR_summary$magrotationRate_fn5, main = "magrotationRate 왜도의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "rms"){
      boxplot(HAR_summary$magrotationRate_fn6, main = "magrotationRate rms의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "rss"){
      boxplot(HAR_summary$magrotationRate_fn7, main = "magrotationRate rss의 박스 그래프", ylab = '값')
    }
    if(input$statistics_input == "첨도"){
      boxplot(HAR_summary$magrotationRate_fn9, main = "magrotationRate 첨도의 박스 그래프", ylab = '값')
    }
  })
  
  output$print_sta_data <- renderDataTable(
    statistics_data()
  )
  
  
  
  
  # 피크 분석
  peak_data <- reactive({
    temp <- input$threshold
    k <- make_peak(temp)
    k
  })
  
  output$peak_plot <- renderPlot({
    x <- get(input$peak_data)
    plot(1:length(x$magrotationRate),x$magrotationRate,'l', 
         main = 'MagrotationRate', xlab = 'Index',ylab = 'peak')
    point_temp <- findpeaks(x$magrotationRate, threshold = input$threshold)
    points(point_temp[,2],point_temp[,1])
  })
  
  output$peak_DT <- DT::renderDataTable({
    peak_data()
  })
  
  
  # PCA
  pca_data <- reactive({
    x <- HAR_summary %>% select(1,2,12,9,17,3,4)
    x
  })
  
  output$screeplot <- renderPlot(
    plot(HAR_summary.pca$sdev,type = 'o',main = 'ScreePlot')
  )
  
  output$PCA_print <- renderPrint(
    HAR_summary.pca
  )
  
  # modeling
  total_data <- reactive({
    x <- data.frame()
    if(input$PCA_sta == 'PCA'){
      x <- pca_data()
    }
    if(input$PCA_sta == '통계'){
      x <- statistics_data()
    }
    x <- cbind(x,make_peak(input$threshold_model))
    result <- x %>% select(-d)
    result
  })
  
  m <- reactive(RF(as.factor(activity)~., total_data()))
  e <- reactive({
    evaluate_Weka_classifier(m(),numFolds = input$fold, complexity = TRUE, class = TRUE)
  })
  
  output$mainUI <- renderUI({
    tabsetPanel(
      tabPanel(title = '훈련',
               renderPrint(summary(m()))),
      tabPanel(title = '테스트',
               renderPrint(e()))
    )
  })
}


shinyApp(ui = ui, server = server)
