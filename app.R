library(shiny)
library(fmsb)
library(shinythemes)

#csv読み込み
df <- read.csv("tallot.csv")
#csv各値を変数に
df_id <- df$id
df_a <- df$para_a
df_b <- df$para_b
df_c <- df$para_c
df_d <- df$para_d
df_e <- df$para_e
df_sentence <- as.character(df$sentence)
df_sentence2 <- as.character(df$sentence2)
#最大、最小データの準備
maxmin <- data.frame(
  para_a = c(100, 0),
  para_b = c(100, 0),
  para_c = c(100, 0),
  para_d = c(100, 0),
  para_e = c(100, 0)
)
#カード指定の変数
num <- 1

ui <- fluidPage(
  #黒画面
  theme = shinytheme("cyborg"),
  
  titlePanel("タロットゲーム"),
  tabsetPanel(type = "tabs",id="tallot_tab",selected = "Start",
              tabPanel(title = "Start",value = "panel_1",
                       actionButton("start_button","Start"),
                       actionButton("staff_button","Staff_list")
              ),
              tabPanel(title = "Game",value = "panel_2",
                       fluidRow(
                         column(5),column(1,uiOutput("fuga1")),
                         column(6)
                       ),
                       fluidRow(
                         column(3),column(1,uiOutput("fuga2")),
                         column(3),column(1,uiOutput("fuga3")),
                         column(4)
                       ),
                       fluidRow(
                         column(2),column(1,uiOutput("fuga4")),
                         column(5),column(1,uiOutput("fuga5")),
                         column(3)
                       ),
                       fluidRow(
                         column(1), column(1,uiOutput("fuga6")),
                         column(3),column(1,uiOutput("fuga7")),
                         column(3),column(1,uiOutput("fuga8")),
                         column(2)
                       ),
                       fluidRow(
                         column(2),column(1,uiOutput("fuga9")),
                         column(5),column(1,uiOutput("fuga10")),
                         column(3)
                       ),
                       fluidRow(
                         column(3),column(1,uiOutput("fuga11")),
                         column(3),column(1,uiOutput("fuga12")),
                         column(4)
                       ),
                       fluidRow(
                         column(5),column(1,uiOutput("fuga13")),
                         column(6)
                       )
              ),
              tabPanel(title = "Result",value = "panel_3",
                       sidebarLayout(
                         sidebarPanel(
                           plotOutput("radarPlot")
                         ),
                         mainPanel(
                           uiOutput("sentence"),
                           uiOutput("sentence2"),
                           uiOutput("tweet"),
                           fluidRow(
                             column(9),column(3,actionButton("Back_to_start_button1","Back"))
                           )
                         )
                       )
              ),
              tabPanel(title = "Staff List",value="panel_4",
                       h2("Illustrator", align = "center"),
                       h4("ロジ まつり",align="center"),
                       h4("かずみ サ^ｎ ろろ オダマキ れしぃ やまろ fuchi ゆーが okanon ここなつ ",align="center"),
                       h4("てぃらみー エディ ごぼぬん 藤汐 aozam サカモト ", align = "center"),
                       h2("Designer", align = "center"),
                       h4("okanon fuchi", align = "center"),
                       h2("Programmer", align = "center"),
                       h4("ミノエル れしぃ comane fuchi", align = "center"),
                       h2("Tea lady(Boss)", align = "center"),
                       h4("葵", align = "center"),
                       fluidRow(
                         column(11),column(1,actionButton("Back_to_start_button2","Back"))
                       )
              )
  )
)

server <- function(input, output,session) {
  
  #スタート画面に飛ばす
  updateTabsetPanel(session, "tallot_tab",
                    selected = paste("panel_", 1,sep = "")
  )
  
  ##################  スタート画面   ########################
  #スタートボタンを押した際にゲーム画面に遷移
  observeEvent(input$start_button, {
    updateTabsetPanel(session, "tallot_tab",
                      selected = paste("panel_", 2,sep = "")
    )
    all_reverse()
  })
  #スタッフボタンを押した際にスタッフ画面に遷移
  observeEvent(input$staff_button, {
    updateTabsetPanel(session, "tallot_tab",
                      selected = paste("panel_", 4,sep = "")
    )
  })
  
  ##################  ゲーム画面   ########################
  #全てのカードを裏返す関数
  all_reverse <- function(){
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
    
  }
  
  all_reverse()
  
  #カードを裏返す関数
  make_ui <- function(x){
    text = paste("button",x,sep = "")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_","00",".jpg",sep = ""),
               height = "300px",width = "150px"
      )
      #実際はこれ
      # tags$img(src = paste("tallot_",00,".jpg",sep = ""),
      #          height = "300px",width = "150px"
      # )
      
    )
  }
  #各カードのボタンが押された際に乱数生成=>画像変換=>結果描写
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
  #カード選択用乱数生成関数
  ran_make <- function(){
    x <- floor(runif(1,0,20))#実際は1,1,23
    ran <- floor(runif(1,1,3))
    ifelse(ran == 1,num <<- x,num <<-x + 100)
    
  }
  #カードを裏返す関数
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
  
  ##################  結果画面   ########################
  #結果画面のレーダーチャートとその他を作る関数
  change <- function(){
    
    #描写データの準備
    dat <- data.frame(
      para_a = df_a[df$id == num],
      para_b = df_b[df$id == num],
      para_c = df_c[df$id == num],
      para_d = df_d[df$id == num],
      para_e = df_e[df$id == num]
    )
    dat <- rbind(maxmin, dat) #データの結合
    VLabel <- c("a","b","c","d","e") #ラベルの名前！！！（決まり次第ここを変える）
    #レーダーチャート作成
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
                 # title = "Luck"
                 title = df_sentence[df$id == num]
      )
      
    })
    output$sentence <- renderUI({
      h1(df_sentence[df$id == num])
    })
    output$sentence2 <- renderUI({
      h5(df_sentence2[df$id == num])
    })
    output$tweet <- renderUI({
      actionButton("Tweet_button","Tweet",onclick = "window.open('https://twitter.com/login?lang=ja', '_blank')")
    })
  }
  #Backボタンが押された際にカードを裏返してStart画面へ
  observeEvent(input$Back_to_start_button1, {
    updateTabsetPanel(session, "tallot_tab",
                      selected = paste("panel_", 1,sep = "")
    )
    all_reverse()
  })
  
  
  ##################  スタッフ画面   ########################
  #Backボタンが押された際にStart画面へ
  observeEvent(input$Back_to_start_button2, {
    updateTabsetPanel(session, "tallot_tab",
                      selected = paste("panel_", 1,sep = "")
    )
  })
}
shinyApp(ui = ui, server = server)


