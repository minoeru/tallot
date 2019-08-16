library(shiny)
library(fmsb)
library(shinythemes)
library(shinyjs)

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

#csv読み込み
df_ex <- read.csv("tallot_ex.csv")
#csv各値を変数に
df_id_ex <- df_ex$id
df_a_ex <- df_ex$para_a
df_b_ex <- df_ex$para_b
df_c_ex <- df_ex$para_c
df_d_ex <- df_ex$para_d
df_e_ex <- df_ex$para_e
df_sentence_ex <- as.character(df_ex$sentence)
df_sentence2_ex <- as.character(df_ex$sentence2)

#最大、最小データの準備
maxmin <- data.frame(
  para_a = c(100, 0),
  para_b = c(100, 0),
  para_c = c(100, 0),
  para_d = c(100, 0),
  para_e = c(100, 0)
)
#カード指定の変数
card_num <<- 1

ui <- fluidPage(
  
  useShinyjs(),
  
  #黒画面
  theme = shinytheme("cyborg"),
  
  titlePanel("タロットゲーム"),
  tabsetPanel(type = "tabs",id="tallot_tab",selected = "Start",
              tabPanel(title = "Start",value = "panel_1",
                       actionButton("start_button","Start"),
                       actionButton("staff_button","StaffList"),
                       actionButton("minigame_button","MiniGame")
              ),
              tabPanel(title = "Game",value = "panel_2",
                       fluidRow(
                         column(1,offset = 5,uiOutput("card1")),
                         column(6)
                       ),
                       fluidRow(
                         column(1,offset = 3 ,uiOutput("card2")),
                         column(1,offset = 3,uiOutput("card3")),
                         column(4)
                       ),
                       fluidRow(
                         column(1,offset = 1,uiOutput("card4")),
                         column(1,offset = 7,uiOutput("card5")),
                         column(2)
                       ),
                       fluidRow(
                         column(1,uiOutput("card6")),
                         column(1,offset = 4,uiOutput("card7")),
                         column(1,offset = 4,uiOutput("card8")),
                         column(1)
                       ),
                       fluidRow(
                         column(1,offset = 1,uiOutput("card9")),
                         column(1,offset = 7,uiOutput("card10")),
                         column(2)
                       ),
                       fluidRow(
                         column(1,offset = 3,uiOutput("card11")),
                         column(1,offset = 3,uiOutput("card12")),
                         column(4)
                       ),
                       fluidRow(
                         column(1,offset = 5,uiOutput("card13")),
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
                             column(3,offset = 9,actionButton("Back_to_start_button1","Back"))
                           )
                         )
                       )
              ),
              tabPanel(title = "Mini Game",value = "panel_5",
                       fluidRow(
                         column(3,actionButton("Player1_turn","1")),
                         column(2,uiOutput("player1_point")),
                         column(2,uiOutput("MiniGame_start")),
                         column(2,uiOutput("player2_point")),
                         column(3,actionButton("Player2_turn","2"))
                       ),
                       fluidRow(
                         column(2,uiOutput("mini1")),
                         column(2,uiOutput("mini2")),
                         column(2,uiOutput("mini3")),
                         column(2,uiOutput("mini4")),
                         column(2,uiOutput("mini5")),
                         column(2,uiOutput("mini6"))
                       ),
                       fluidRow(
                         column(2,uiOutput("mini7")),
                         column(2,uiOutput("mini8")),
                         column(2,uiOutput("mini9")),
                         column(2,uiOutput("mini10")),
                         column(2,uiOutput("mini11")),
                         column(2,uiOutput("mini12"))
                       ),
                       fluidRow(
                         column(2,uiOutput("mini13")),
                         column(2,uiOutput("mini14")),
                         column(2,uiOutput("mini15")),
                         column(2,uiOutput("mini16")),
                         column(2,uiOutput("mini17")),
                         column(2,uiOutput("mini18"))
                       ),
                       fluidRow(
                         column(2,uiOutput("mini19")),
                         column(2,uiOutput("mini20")),
                         column(2,uiOutput("mini21")),
                         column(2,uiOutput("mini22")),
                         column(2,uiOutput("mini23")),
                         column(2,uiOutput("mini24"))
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
                       h4("ミノエル れしぃ comame fuchi", align = "center"),
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
  updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 1,sep = "") )
  
  ##################  スタート画面   ########################
  #スタートボタンを押した際にゲーム画面に遷移
  observeEvent(input$start_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 2,sep = "") )
    reverseAll()
  })
  #スタッフボタンを押した際にスタッフ画面に遷移
  observeEvent(input$staff_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 4,sep = "") )
  })
  #ミニゲームボタンを押した際にミニゲーム画面に遷移
  observeEvent(input$minigame_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 5,sep = "") )
    reverseAllMini()
  })
  
  ##################  ゲーム画面   ########################
  #全てのカードを裏返す関数
  reverseAll <- function(){
    output$card1 <- renderUI({makeUi(1)})
    output$card2 <- renderUI({makeUi(2)})
    output$card3 <- renderUI({makeUi(3)})
    output$card4 <- renderUI({makeUi(4)})
    output$card5 <- renderUI({makeUi(5)})
    output$card6 <- renderUI({makeUi(6)})
    output$card7 <- renderUI({makeUi(7)})
    output$card8 <- renderUI({makeUi(8)})
    output$card9 <- renderUI({makeUi(9)})
    output$card10 <- renderUI({makeUi(10)})
    output$card11 <- renderUI({makeUi(11)})
    output$card12 <- renderUI({makeUi(12)})
    output$card13 <- renderUI({makeUi(13)})
    #重複対策
    tallot_data_sets <<- c(0:21)
    #めくる枚数とめくった番号の合計
    turn_num <<- 0
    sum_num <<- 0
  }
  
  #カードを裏面で生成する関数
  makeUi <- function(x){
    text <- paste("button",x,sep = "")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_","00",".jpg",sep = ""),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      #実際はこれ
      # tags$img(src = paste("tallot_",00,".png",sep = ""),height = "200px",width = "100px")
    )
  }
  
  #カード選択用乱数生成関数
  makeRan <- function(){
    #全てのカードが出たら最後のカードから変更させない
    if(sum(tallot_data_sets) <= -22){
      card_num <<- 00
      return()
    }
    x <- floor(runif(1,0,22))
    if(tallot_data_sets[x+1] != x) Recall()
    else{
      dual <- sum( (df_id %% 100) - x == 0 )
      ran <- floor(runif(1,0,dual))
      card_num <<- x + ran * 100
      tallot_data_sets[x+1] <<- -1
    }
  }
  
  #カードを裏返す関数
  turnCard <- function(x){
    turn_num <<- turn_num + 1
    sum_num <<- sum_num + (card_num %% 100)
    text <- paste("button",x,sep="")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_", card_num ,".jpg",sep = ""),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      # tags$img(src = paste("tallot_", card_num ,".png",sep = ""),height = "200px",width = "100px")
    )
  }
  
  #各カードのボタンが押された際に乱数生成=>画像変換=>結果描写
  observeEvent(input$button1,{
    makeRan()
    output$card1 <- renderUI({turnCard(1)})
    makeChart()
  })
  observeEvent(input$button2,{
    makeRan()
    output$card2 <- renderUI({turnCard(2)})
    makeChart()
  })
  observeEvent(input$button3,{
    makeRan()
    output$card3 <- renderUI({turnCard(3)})
    makeChart()
  })
  observeEvent(input$button4,{
    makeRan()
    output$card4 <- renderUI({turnCard(4)})
    makeChart()
  })
  observeEvent(input$button5,{
    makeRan()
    output$card5 <- renderUI({turnCard(5)})
    makeChart()
  })
  observeEvent(input$button6,{
    makeRan()
    output$card6 <- renderUI({turnCard(6)})
    makeChart()
  })
  observeEvent(input$button7,{
    makeRan()
    output$card7 <- renderUI({turnCard(7)})
    makeChart()
  })
  observeEvent(input$button8,{
    makeRan()
    output$card8 <- renderUI({turnCard(8)})
    makeChart()
  })
  observeEvent(input$button9,{
    makeRan()
    output$card9 <- renderUI({turnCard(9)})
    makeChart()
  })
  observeEvent(input$button10,{
    makeRan()
    output$card10 <- renderUI({turnCard(10)})
    makeChart()
  })
  observeEvent(input$button11,{
    makeRan()
    output$card11 <- renderUI({turnCard(11)})
    makeChart()
  })
  observeEvent(input$button12,{
    makeRan()
    output$card12 <- renderUI({turnCard(12)})
    makeChart()
  })
  observeEvent(input$button13,{
    makeRan()
    output$card13 <- renderUI({turnCard(13)})
    makeChart()
  })
  
  #前処理
  reverseAll()
  
  ##################  結果画面   ########################
  #結果画面のレーダーチャートとその他を作る関数
  makeChart <- function(){
    if(turn_num == 0){
      #描写データの準備
      dat <- data.frame(
        para_a = df_a[df$id == card_num],
        para_b = df_b[df$id == card_num],
        para_c = df_c[df$id == card_num],
        para_d = df_d[df$id == card_num],
        para_e = df_e[df$id == card_num]
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
                   title = df_sentence[df$id == card_num]
        )
      })
      output$sentence <- renderUI({ h1(df_sentence[df$id == card_num]) })
      output$sentence2 <- renderUI({ h5(df_sentence2[df$id == card_num]) })
      output$tweet <- renderUI({ actionButton("Tweet_button","Tweet",onclick = "window.open('https://twitter.com/login?lang=ja', '_blank')") })
    }
    else{
      #描写データの準備
      dat <- data.frame(
        para_a = df_a_ex[df_ex$id == sum_num %% 100],
        para_b = df_b_ex[df_ex$id == sum_num %% 100],
        para_c = df_c_ex[df_ex$id == sum_num %% 100],
        para_d = df_d_ex[df_ex$id == sum_num %% 100],
        para_e = df_e_ex[df_ex$id == sum_num %% 100]
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
                   title = "Luck"
        )
      })
      output$sentence <- renderUI({ h1(df_sentence_ex[df_ex$id == sum_num %% 100]) })
      output$sentence2 <- renderUI({ h5(df_sentence2_ex[df_ex$id == sum_num %% 100]) })
      output$tweet <- renderUI({ actionButton("Tweet_button","Tweet",onclick = "window.open('https://twitter.com/login?lang=ja', '_blank')") })
    }
  }
  #Backボタンが押された際にカードを裏返してStart画面へ
  observeEvent(input$Back_to_start_button1, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 1,sep = "") )
    reverseAll()
  })
  
  ##################  ミニゲーム画面   ########################
  
  #カードを裏面で生成する関数
  makeUiMini <- function(x){
    text <- paste("button_mini",x,sep = "")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_","00",".jpg",sep = ""),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      #実際はこれ
      # tags$img(src = paste("tallot_",00,".jpg",sep = ""),height = "200px",width = "100px")
    )
  }
  
  #値を初期値に戻す関数
  resetPoint <- function(){
    #プレイヤーのフラグと各得点
    play_turn_flag <<- 0
    p_points <<- c(1:2) - c(1:2)
    turned_count <<- 0
    open_tmp <<- 0
    #UI作成
    output$player1_point <- renderUI({h1(p_points[1])})
    output$player2_point <- renderUI({h1(p_points[2])})
    output$MiniGame_start <- renderUI({actionButton("MiniGame_start","Start")})
    #対象の役生成
    moto_data <<- unique( floor( runif(10000,0,22) ) )#ランダムに0~21生成
    moto_data <<- append(moto_data[1:12],moto_data[1:12])#12個ずつ一組作る
    moto_data <<- moto_data[order(rnorm(length(moto_data)))]#ランダムに並べ替える
  }
  
  #カードを裏面に戻す関数
  returnCard <- function(x){
    switch(x,               
           "1"  = output$mini1 <- renderUI({makeUiMini(1)}),
           "2"  = output$mini2 <- renderUI({makeUiMini(2)}),
           "3"  = output$mini3 <- renderUI({makeUiMini(3)}),
           "4"  = output$mini4 <- renderUI({makeUiMini(4)}),
           "5"  = output$mini5 <- renderUI({makeUiMini(5)}),
           "6"  = output$mini6 <- renderUI({makeUiMini(6)}),
           "7"  = output$mini7 <- renderUI({makeUiMini(7)}),
           "8"  = output$mini8 <- renderUI({makeUiMini(8)}),
           "9"  = output$mini9 <- renderUI({makeUiMini(9)}),
           "10" = output$mini10 <- renderUI({makeUiMini(10)}),
           "11" = output$mini11 <- renderUI({makeUiMini(11)}),
           "12" = output$mini12 <- renderUI({makeUiMini(12)}),
           "13" = output$mini13 <- renderUI({makeUiMini(13)}),
           "14" = output$mini14 <- renderUI({makeUiMini(14)}),
           "15" = output$mini15 <- renderUI({makeUiMini(15)}),
           "16" = output$mini16 <- renderUI({makeUiMini(16)}),
           "17" = output$mini17 <- renderUI({makeUiMini(17)}),
           "18" = output$mini18 <- renderUI({makeUiMini(18)}),
           "19" = output$mini19 <- renderUI({makeUiMini(19)}),
           "20" = output$mini20 <- renderUI({makeUiMini(20)}),
           "21" = output$mini21 <- renderUI({makeUiMini(21)}),
           "22" = output$mini22 <- renderUI({makeUiMini(22)}),
           "23" = output$mini23 <- renderUI({makeUiMini(23)}),
           "24" = output$mini24 <- renderUI({makeUiMini(24)})
    )  
  }
  #全てのカードを裏返す関数
  reverseAllMini <- function(x) {
    for(i in 1:24)returnCard(i)
    resetPoint()
  }
  
  #カードを裏返す関数
  turnCardMini <- function(x){
    print(moto_data)
    text <- paste("button_mini",x,sep="")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_", moto_data[x] ,".jpg",sep = ""),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      # tags$img(src = paste("tallot_", num ,".png",sep = ""),height = "200px",width = "100px")
    )
  }
  
  #カードが揃っているか確認する関数
  checkMini <- function(x){
    turned_count <<- turned_count + 1
    if(turned_count == 1){
      open_tmp <<- x
    } else{
      if(moto_data[open_tmp] == moto_data[x]){
        p_points[play_turn_flag] <<- p_points[play_turn_flag] + 1
        output$player1_point <- renderUI({h1(p_points[1])})
        output$player2_point <- renderUI({h1(p_points[2])})
      } else{
        #カードを裏返す
        delay(1000, returnCard(open_tmp) )
        delay(1000, returnCard(x))
      }
      turned_count <<- 0
    }
  }
  
  #ターンチェンジ
  observeEvent(input$Player1_turn,{ if(play_turn_flag == 2) play_turn_flag <<- 1 })
  observeEvent(input$Player2_turn,{ if(play_turn_flag == 1) play_turn_flag <<- 2 })
  observeEvent(input$MiniGame_start,{ play_turn_flag <<- 1 })
  
  #各カードの反転
  observeEvent(input$button_mini1,{
    output$mini1 <- renderUI({turnCardMini(1)})
    checkMini(1)
  })
  observeEvent(input$button_mini2,{
    output$mini2 <- renderUI({turnCardMini(2)})
    checkMini(2)
  })
  observeEvent(input$button_mini3,{
    output$mini3 <- renderUI({turnCardMini(3)})
    checkMini(3)
  })
  observeEvent(input$button_mini4,{
    output$mini4 <- renderUI({turnCardMini(4)})
    checkMini(4)
  })
  observeEvent(input$button_mini5,{
    output$mini5 <- renderUI({turnCardMini(5)})
    checkMini(5)
  })
  observeEvent(input$button_mini6,{
    output$mini6 <- renderUI({turnCardMini(6)})
    checkMini(6)
  })
  observeEvent(input$button_mini7,{
    output$mini7 <- renderUI({turnCardMini(7)})
    checkMini(7)
  })
  observeEvent(input$button_mini8,{
    output$mini8 <- renderUI({turnCardMini(8)})
    checkMini(8)
  })
  observeEvent(input$button_mini9,{
    output$mini9 <- renderUI({turnCardMini(9)})
    checkMini(9)
  })
  observeEvent(input$button_mini10,{
    output$mini10 <- renderUI({turnCardMini(10)})
    checkMini(10)
  })
  observeEvent(input$button_mini11,{
    output$mini11 <- renderUI({turnCardMini(11)})
    checkMini(11)
  })
  observeEvent(input$button_mini12,{
    output$mini12 <- renderUI({turnCardMini(12)})
    checkMini(12)
  })
  observeEvent(input$button_mini13,{
    output$mini13 <- renderUI({turnCardMini(13)})
    checkMini(13)
  })
  observeEvent(input$button_mini14,{
    output$mini14 <- renderUI({turnCardMini(14)})
    checkMini(14)
  })
  observeEvent(input$button_mini15,{
    output$mini15 <- renderUI({turnCardMini(15)})
    checkMini(15)
  })
  observeEvent(input$button_mini16,{
    output$mini16 <- renderUI({turnCardMini(16)})
    checkMini(16)
  })
  observeEvent(input$button_mini17,{
    output$mini17 <- renderUI({turnCardMini(17)})
    checkMini(17)
  })
  observeEvent(input$button_mini18,{
    output$mini18 <- renderUI({turnCardMini(18)})
    checkMini(18)
  })
  observeEvent(input$button_mini19,{
    output$mini19 <- renderUI({turnCardMini(19)})
    checkMini(19)
  })
  observeEvent(input$button_mini20,{
    output$mini20 <- renderUI({turnCardMini(20)})
    checkMini(20)
  })
  observeEvent(input$button_mini21,{
    output$mini21 <- renderUI({turnCardMini(21)})
    checkMini(21)
  })
  observeEvent(input$button_mini22,{
    output$mini22 <- renderUI({turnCardMini(22)})
    checkMini(22)
  })
  observeEvent(input$button_mini23,{
    output$mini23 <- renderUI({turnCardMini(23)})
    checkMini(23)
  })
  observeEvent(input$button_mini24,{
    output$mini24 <- renderUI({turnCardMini(24)})
    checkMini(24)
  })
  
  #前処理
  reverseAllMini()
  
  ##################  スタッフ画面   ########################
  #Backボタンが押された際にStart画面へ
  observeEvent(input$Back_to_start_button2, {
    updateTabsetPanel(session, "tallot_tab",selected = paste("panel_", 1,sep = ""))
  })
}
shinyApp(ui = ui, server = server)