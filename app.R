library(shiny)
library(fmsb)
library(shinythemes)

df <- read.csv("tallot.csv")

df_id <- df$id
df_a <- df$para_a
df_b <- df$para_b
df_c <- df$para_c
df_d <- df$para_d
df_e <- df$para_e
df_sentence <- as.character(df$sentence)
df_sentence2 <- as.character(df$sentence2)


num <- 1

ui <- fluidPage(
  theme = shinytheme("cyborg"),#黒画面
  
  titlePanel("タロットゲーム"),
  tabsetPanel(type = "tabs",
              
              tabPanel("game",
                       
                       # fluidRow(
                       #   column(2,uiOutput("fuga1")),
                       #   column(2,uiOutput("fuga2")),
                       #   column(2,uiOutput("fuga3")),
                       #   column(2,uiOutput("fuga4")),
                       #   column(2,uiOutput("fuga5")),
                       #   column(2,uiOutput("fuga6"))
                       # ),
                       # fluidRow(
                       #   column(1),
                       #   column(2,uiOutput("fuga7")),
                       #   column(2,uiOutput("fuga8")),
                       #   column(2,uiOutput("fuga9")),
                       #   column(2,uiOutput("fuga10")),
                       #   column(3,uiOutput("fuga11"))
                       # )
                       
                       
                       fluidRow(
                         column(5),
                         column(1,uiOutput("fuga1")),
                         column(6)
                       ),
                       fluidRow(
                         column(3),
                         column(1,uiOutput("fuga2")),
                         column(3),
                         column(1,uiOutput("fuga3")),
                         column(4)
                       ),
                       fluidRow(
                         column(2),
                         column(1,uiOutput("fuga4")),
                         column(5),
                         column(1,uiOutput("fuga5")),
                         column(3)
                       ),
                       fluidRow(
                         column(1),
                         column(1,uiOutput("fuga6")),
                         column(3),
                         column(1,uiOutput("fuga7")),
                         column(3),
                         column(1,uiOutput("fuga8")),
                         column(2)
                       ),
                       fluidRow(
                         column(2),
                         column(1,uiOutput("fuga9")),
                         column(5),
                         column(1,uiOutput("fuga10")),
                         column(3)
                       ),
                       fluidRow(
                         column(3),
                         column(1,uiOutput("fuga11")),
                         column(3),
                         column(1,uiOutput("fuga12")),
                         column(4)
                       ),
                       fluidRow(
                         column(5),
                         column(1,uiOutput("fuga13")),
                         column(6)
                       )
                       
                       
                       
              ),
              tabPanel("結果",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("sentence"),
                           textOutput("sentence2")
                         ),
                         mainPanel(
                           plotOutput("radarPlot")
                         )
                       )
              )
  )
)

server <- function(input, output) {
  
  
  #最大、最小データの準備
  maxmin <- data.frame(
    para_a = c(100, 0),
    para_b = c(100, 0),
    para_c = c(100, 0),
    para_d = c(100, 0),
    para_e = c(100, 0)
  )
  
  
  
  ##########################################
  output$fuga1 <- renderUI({make_ui(1)})
  output$fuga2 <- renderUI({make_ui(2)})
  output$fuga3 <- renderUI({make_ui(3)})
  output$fuga4 <- renderUI({make_ui(4)})
  output$fuga5 <- renderUI({make_ui(5)})
  output$fuga6 <- renderUI({make_ui(6)})
  output$fuga7 <- renderUI({make_ui(7)})
  output$fuga8 <- renderUI({make_ui(8)})
  output$fuga9 <- renderUI({make_ui(9)})
  output$fuga10 <- renderUI({make_ui(10)})
  output$fuga11 <- renderUI({make_ui(11)})
  output$fuga12 <- renderUI({make_ui(12)})
  output$fuga13 <- renderUI({make_ui(13)})
  
  make_ui <- function(x){
    text = paste("button",x,sep = "")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_",0,".jpg",sep = ""),
               height = "300px",width = "150px"
      )
    )
  }
  
  observeEvent(input$button1,{
    ran_make()
    output$fuga1 <- renderUI({turn_card(1)})
    change()
  })
  observeEvent(input$button2,{
    ran_make()
    output$fuga2 <- renderUI({turn_card(2)})
    change()
  })
  observeEvent(input$button3,{
    ran_make()
    output$fuga3 <- renderUI({turn_card(3)})
    change()
  })
  observeEvent(input$button4,{
    ran_make()
    output$fuga4 <- renderUI({turn_card(4)})
    change()
  })
  observeEvent(input$button5,{
    ran_make()
    output$fuga5 <- renderUI({turn_card(5)})
    change()
  })
  observeEvent(input$button6,{
    ran_make()
    output$fuga6 <- renderUI({turn_card(6)})
    change()
  })
  observeEvent(input$button7,{
    ran_make()
    output$fuga7 <- renderUI({turn_card(7)})
    change()
  })
  observeEvent(input$button8,{
    ran_make()
    output$fuga8 <- renderUI({turn_card(8)})
    change()
  })
  observeEvent(input$button9,{
    ran_make()
    output$fuga9 <- renderUI({turn_card(9)})
    change()
  })
  observeEvent(input$button10,{
    ran_make()
    output$fuga10 <- renderUI({turn_card(10)})
    change()
  })
  observeEvent(input$button11,{
    ran_make()
    output$fuga11 <- renderUI({turn_card(11)})
    change()
  })
  observeEvent(input$button12,{
    ran_make()
    output$fuga12 <- renderUI({turn_card(12)})
    change()
  })
  observeEvent(input$button13,{
    ran_make()
    output$fuga13 <- renderUI({turn_card(13)})
    change()
  })
  
  ran_make <- function(){
    x <- runif(1,1,20)
    num <<- floor(x)
  }
  
  turn_card <- function(x){
    text = paste("button",x,sep="")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_", num ,".jpg",sep = ""),
               height = "300px",
               width = "150px"
      )
    )
  }
  
  change <- function(){
    
    #描写データの準備
    dat <- data.frame(
      para_a = df_a[num],
      para_b = df_b[num],
      para_c = df_c[num],
      para_d = df_d[num],
      para_e = df_e[num]
    )
    dat <- rbind(maxmin, dat) #データの結合
    VLabel <- c("a","b","c","d","e") #ラベルの名前
    
    output$radarPlot <- renderPlot({
      radarchart(dat, 
                 axistype = 0,#ラベル表示無し
                 seg = 1,#分割数
                 plty = 16,#線の種類(丸ぽち無し)
                 pcol=7,#線の色 (黄色)
                 plwd=1,　#ラインの太さ 
                 vlcex = 2,# ラベルの大きさ
                 pty=32,#データ点をプロットしない
                 centerzero = TRUE,#ゼロ真ん中
                 vlabels = VLabel,#ラベルの名前
                 pdensity=100,　#塗りつぶす（斜線の）程度
                 pangle=180,　#塗りつぶす斜線の傾き
                 pfcol=7,　#塗りつぶす色(黄色)
                 cglcol="pink",#軸の色
                 title = "Luck")
      
    })
    output$sentence <- renderUI({
      h1(df_sentence[num])
    })
    output$sentence2 <- renderText({
      df_sentence2[num]
    })
  }
}

shinyApp(ui = ui, server = server)



