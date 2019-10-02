library(shiny)
library(fmsb)
library(shinythemes)
library(shinyjs)
library(rsconnect)

#csv読み込み
df <- read.csv("tallot_2.csv",encoding = "UTF-8")
#csv各値を変数に
df_id <- df$id
df_a <- df$para_a
df_b <- df$para_b
df_c <- df$para_c
df_d <- df$para_d
df_e <- df$para_e
df_sentence <- as.character(df$sentence)
df_sentence2 <- as.character(df$sentence2)
df_sentence3 <- as.character(df$sentence3)
df_illustrator <- as.character(df$illustrator)

#csv読み込み
df_ex <- read.csv("tallot_2.csv",encoding = "UTF-8")
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
card_num <- 1

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Noto+Sans+JP&display=swap")),
  
  # Shinyjsを使用できるようにする
  useShinyjs(),
  #黒画面
  theme = shinytheme("cyborg"),
  
  titlePanel("タロットゲーム"),
  tabsetPanel(type = "tabs",id="tallot_tab",selected = "Start",
              tabPanel(title = "Start",value = "panel_1",
                       uiOutput("title_ui", align = "center"),
                       actionButton("start_button","START READING"),
                       actionButton("gallary_button","GALLERY"),
                       actionButton("staff_button","CREDIT")
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
                           uiOutput("cardname"),
                           uiOutput("sentence"),
                           uiOutput("illust"),
                           uiOutput("tweet", align = "center"),
                           actionButton("Back_to_start_button","ホームに戻る",align = "center")
                         )
                       )
              ),
              tabPanel(title = "Mini Game",value = "panel_5",
                       fluidRow(
                         column(1,uiOutput("player1_point")),
                         column(10, uiOutput("player_turn")),
                         column(1,uiOutput("player2_point"))
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
              tabPanel(title = "Gallery",value = "panel_6",
                       uiOutput("gallery_ui", align = "center"),
                       fluidRow(
                         column(6,uiOutput("gallary1")),
                         column(6,uiOutput("gallary2"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary3")),
                         column(6,uiOutput("gallary4"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary5")),
                         column(6,uiOutput("gallary6"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary7")),
                         column(6,uiOutput("gallary8"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary9")),
                         column(6,uiOutput("gallary10"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary11")),
                         column(6,uiOutput("gallary12"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary13")),
                         column(6,uiOutput("gallary14"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary15")),
                         column(6,uiOutput("gallary16"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary17")),
                         column(6,uiOutput("gallary18"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary19")),
                         column(6,uiOutput("gallary20"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary21")),
                         column(6,uiOutput("gallary22"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary23")),
                         column(6,uiOutput("gallary24"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary25")),
                         column(6,uiOutput("gallary26"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary27")),
                         column(6,uiOutput("gallary28"))
                       ),
                       fluidRow(
                         column(6,uiOutput("gallary29")),
                         column(6,uiOutput("gallary30"))
                       ),
                       actionButton("Back_to_start_button2","ホームに戻る",align = "center")
              ),
              tabPanel(title = "Staff List",value="panel_4",
                       uiOutput("credit_ui", align = "center"),
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
                       actionButton("Back_to_start_button3","ホームに戻る",align = "center")
              )
  )
)

server <- function(input, output,session) {
  
  ###タイトル的なやつ
  output$title_ui <- renderUI({
    tags$object(
      id = "title",
      class = "img",
      tags$img(src = "title.png",height = "100px",width = "400px")
    )
  })
  
  output$gallery_ui <- renderUI({
    tags$object(
      id = "gallery",
      class = "img",
      tags$img(src = "gallery_t.png",height = "100px",width = "400px")
    )
  })
  
  output$credit_ui <- renderUI({
    tags$object(
      id = "credit",
      class = "img",
      tags$img(src = "credit_t.png",height = "100px",width = "400px")
    )
  })
  
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
  #ギャラリーボタンを押した際にミニゲーム画面に遷移
  observeEvent(input$gallary_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 6,sep = "") )
    reverseAllMini()
  })
  
  ##################  ゲーム画面   ########################
  #全てのカードを裏返す関数
  reverseAll <- function(){
    lapply(1:13 , function(x) output[[paste0("card",x)]] <- renderUI({makeUi(x)}))
    #重複対策
    tallot_data_sets <<- c(0:21)
    #めくる枚数とめくった番号の合計
    turn_num <<- 0
    sum_num <<- 0
    #再反転対策
    reverse_protect <<- c(1:13)
  }
  
  #カードを裏面で生成する関数
  makeUi <- function(x){
    text <- paste("button",x,sep = "")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_","00",".jpg",sep = ""),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      #実際はこれ tags$img(src = paste("mt_",00,".png",sep = ""),height = "200px",width = "100px")
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
    reverse_protect[x] <<- 0
    text <- paste0("button",x)
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste0("cc_", card_num ,".jpg"),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      # tags$img(src = paste0("tallot_", card_num ,".png"),height = "200px",width = "100px")
    )
  }
  
  #各カードのボタンが押された際に乱数生成=>画像変換=>結果描写
  lapply(1:13, function(x){
    observeEvent(input[[paste0("button",x)]],{
      if(reverse_protect[x] == x){
        makeRan()
        output[[paste0("card",x)]] <- renderUI({turnCard(x)})
        makeChart()
      }
    })
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
      VLabel <- c("General","Love","Money","Personal","work") #ラベルの名前
      #レーダーチャート作成
      output$radarPlot <- renderPlot({
        radarchart(dat, 
                   axistype = 0,#ラベル表示無し
                   seg = 5,#分割数
                   plty = 16,#線の種類(丸ぽち無し)
                   pcol="white",#線の色
                   plwd=2,　#ラインの太さ 
                   vlcex = 1,# ラベルの大きさ
                   pty=32,#データ点をプロットしない
                   centerzero = TRUE,#ゼロ真ん中
                   vlabels = VLabel,#ラベルの名前
                   pdensity=0,　#塗りつぶす（斜線の）程度
                   pangle=180,　#塗りつぶす斜線の傾き
                   pfcol=7,　#塗りつぶす色
                   cglcol="white",#軸の色
                   # title = "Luck"
                   title = df_sentence[df$id == card_num]
        )
      },bg="#a285b3")
      output$cardname <- renderUI({ h1(df_sentence2[df$id == card_num]) })
      output$sentence <- renderUI({ h5(df_sentence3[df$id == card_num]) })
      output$illust <- renderUI({ h3(df_illustrator[df$id == card_num]) })
      output$tweet <- renderUI({ actionButton("Tweet_button","結果をツイートする",onclick = "https://twitter.com/compose/tweet', '_blank')") })
      # output$tweet_button_ui <- renderUI({
      #   tags$button(
      #     id = "Tweet_button",
      #     class = "btn action-button",
      #     onclick = "window.open('https://twitter.com/compose/tweet', '_blank')",
      #     tags$img(src = "tweet.png",height = "50px",width = "300px"),
      #     style="background-color: #a285b3; border-color: #a285b3"
      #   )
      # })
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
      VLabel <- c("General","Love","Money","Personal","work") #ラベルの名前
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
  observeEvent(input$Back_to_start_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste0("panel_", 1) )
    reverseAll()
  })
  
  ##################  ミニゲーム画面   ########################
  
  #カードを裏面で生成する関数
  makeUiMini <- function(x){
    text <- paste0("button_mini",x)
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste0("cc_","00",".jpg"),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      #tags$img(src = paste0("tallot_",00,".jpg"),height = "200px",width = "100px")
    )
  }
  
  #値を初期値に戻す関数
  resetPoint <- function(){
    #プレイヤーのフラグと各得点
    play_turn_flag <<- 1
    p_points <<- c(1:2) - c(1:2)
    turned_count <<- 0
    open_tmp <<- 0
    reverse_protect_mini <<- c(1:24)
    #UI作成
    output$player1_point <- renderUI({h1(p_points[1],align="center")})
    output$player2_point <- renderUI({h1(p_points[2],align="center")})
    output$player_turn <- renderUI({h1(paste0("プレイヤー",play_turn_flag,"のターン"),align = "center")})
    #対象の役生成
    moto_data <<- unique( floor( runif(10000,0,22) ) )#ランダムに0~21生成
    moto_data <<- append(moto_data[1:12],moto_data[1:12])#12個ずつ一組作る
    moto_data <<- moto_data[order(rnorm(length(moto_data)))]#ランダムに並べ替える
  }
  
  #カードを裏面に戻す関数
  returnCard <- function(x){output[[paste0("mini",x)]] <- renderUI({makeUiMini(x)})}
  
  #全てのカードを裏返す関数
  reverseAllMini <- function(x) {
    lapply(1:24, function(x) returnCard(x))
    resetPoint()
  }
  
  #カードを裏返す関数
  turnCardMini <- function(x){
    print(moto_data)
    text <- paste0("button_mini",x)
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste0("cc_", moto_data[x] ,".jpg"),height = "200px",width = "100px"),
      style="color: #000000; background-color: #000000; border-color: #000000"
      # tags$img(src = paste0("tallot_", num ,".png"),height = "200px",width = "100px")
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
        output$player1_point <- renderUI({h1(p_points[1],align="center")})
        output$player2_point <- renderUI({h1(p_points[2],align="center")})
        #反転済みカードを押せなくする
        reverse_protect_mini[open_tmp] <<- 0
        reverse_protect_mini[x] <<- 0
      } else{
        #カードを裏返す
        delay(500, returnCard(open_tmp) )
        delay(500, returnCard(x))
        changeTurn()
      }
      if(p_points[1] + p_points[2] == 12){
        checkWinner()
      } 
      turned_count <<- 0
    }
  }
  
  #ターンチェンジを行う関数
  changeTurn <- function(){
    ifelse(play_turn_flag == 1,play_turn_flag<<- 2,play_turn_flag <<- 1)
    output$player_turn <- renderUI({h1( paste0("プレイヤー",play_turn_flag,"のターン"),align="center")})
  }
  #勝利者を決める関数
  checkWinner <- function(){
    if(p_points[1] > p_points[2]) output$player_turn <- renderUI({h1( paste0("プレイヤー1の勝利！"),align="center")})
    else if(p_points[1] < p_points[2]) output$player_turn <- renderUI({h1( paste0("プレイヤー2の勝利！"),align="center")})
    else output$player_turn <- renderUI({h1( paste0("引き分け"),align="center")})
  }
  
  #各カードの反転
  lapply(1:24, function(x){
    observeEvent(input[[paste0("button_mini",x)]],{
      if(reverse_protect_mini[x] == x){
        output[[paste0("mini",x)]] <- renderUI({turnCardMini(x)})
        checkMini(x)
      }
    })
  })
  
  #前処理
  reverseAllMini()
  
  ##################  ギャラリー画面   ########################
  
  observeEvent(input$Back_to_start_button2, {
    updateTabsetPanel(session, "tallot_tab",selected = paste0("panel_",1))
  })
  
  ##################  スタッフ画面   ########################
  #Backボタンが押された際にStart画面へ
  observeEvent(input$Back_to_start_button3, {
    updateTabsetPanel(session, "tallot_tab",selected = paste0("panel_",1))
  })
}
shinyApp(ui = ui, server = server)