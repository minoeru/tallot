library(shiny)
library(fmsb)
library(shinyjs)
# library(rsconnect)

#csv読み込み
df <- read.csv("tallot.csv",fileEncoding = "UTF-8")
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
  
  tabsetPanel(type = "tabs",id="tallot_tab",selected = "Start",
              tabPanel(title = "Start",value = "panel_1",
                       uiOutput("title_ui", align = "center"),
                       # actionButton("start_button","START READING"),
                       # actionButton("gallery_button","START READING"),
                       # actionButton("staff_button","START READING"),
                       uiOutput("start_button_ui"),
                       uiOutput("slash_ui"),
                       uiOutput("gallery_button_ui"),
                       uiOutput("staff_button_ui")
              ),
              tabPanel(title = "Game",value = "panel_2",
                       uiOutput("mainCard"),
                       uiOutput("mob1"),
                       uiOutput("mob2"),
                       uiOutput("mob3"),
                       uiOutput("mob4"),
                       uiOutput("mob5"),
                       uiOutput("mob6"),
                       uiOutput("mob7"),
                       uiOutput("mob8"),
                       uiOutput("mob9"),
                       uiOutput("mob10"),
                       uiOutput("mob11"),
                       uiOutput("mob12"),
                       uiOutput("put_button_ui",align = "center")
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
                           # actionButton("Back_to_start_button","ホームに戻る",align = "center")
                           uiOutput("Back_to_start_button_ui",align = "center")
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
                         column(2,offset = 1,uiOutput("gallery0")),
                         column(2,uiOutput("gallery200")),
                         column(2,uiOutput("gallery1")),
                         column(2,uiOutput("gallery2")),
                         column(3,uiOutput("gallery202"))
                       ),
                       fluidRow(
                         column(2,offset = 1,uiOutput("gallery3")),
                         column(2,uiOutput("gallery203")),
                         column(2,uiOutput("gallery4")),
                         column(2,uiOutput("gallery5")),
                         column(3,uiOutput("gallery6"))
                       ),
                       fluidRow(
                         column(2,offset = 1,uiOutput("gallery7")),
                         column(2,uiOutput("gallery207")),
                         column(2,uiOutput("gallery8")),
                         column(2,uiOutput("gallery9")),
                         column(3,uiOutput("gallery10"))
                       ),
                       fluidRow(
                         column(2,offset = 1,uiOutput("gallery11")),
                         column(2,uiOutput("gallery12")),
                         column(2,uiOutput("gallery13")),
                         column(2,uiOutput("gallery213")),
                         column(3,uiOutput("gallery14"))
                       ),
                       fluidRow(
                         column(2,offset = 1,uiOutput("gallery15")),
                         column(2,uiOutput("gallery16")),
                         column(2,uiOutput("gallery17")),
                         column(2,uiOutput("gallery18")),
                         column(3,uiOutput("gallery19"))
                       ),
                       fluidRow(
                         column(2,offset = 1,uiOutput("gallery20")),
                         column(2,uiOutput("gallery220")),
                         column(2,uiOutput("gallery21")),
                         column(2,uiOutput("gallery00"))
                       ),
                       uiOutput("Back_to_start_button_ui2",align = "center")
                       # actionButton("Back_to_start_button2","ホームに戻る",align = "center")
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
                       h2("Tea lady(Master)", align = "center"),
                       h4("葵", align = "center"),
                       uiOutput("Back_to_start_button_ui3",align = "center")
                       # actionButton("Back_to_start_button3","ホームに戻る",align = "center")
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
  
  #各種ボタン
  
  output$start_button_ui <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "start_button",
               class = "btn action-button",
               "START READING"
             )
    )
  })
  
  output$gallery_button_ui <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "gallery_button",
               class = "btn action-button",
               "GALLERY"
             )
    )
  })
  
  output$staff_button_ui <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "staff_button",
               class = "btn action-button",
               "CREDIT"
             )
    )
  })
  
  output$put_button_ui <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "put_button",
               class = "btn action-button",
               "カードをめくる"
             )
    )
  })
  
  
  output$Back_to_start_button_ui <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "Back_to_start_button",
               class = "btn action-button",
               "ホームに戻る"
             )
    )
  })
  
  output$Back_to_start_button_ui2 <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "Back_to_start_button2",
               class = "btn action-button",
               "ホームに戻る"
             )
    )
  })
  
  output$Back_to_start_button_ui3 <- renderUI({
    tags$div(class = "btn-container",
             tags$button(
               id = "Back_to_start_button3",
               class = "btn action-button",
               "ホームに戻る"
             )
    )
  })
  
  #変な線
  output$slash_ui <- renderUI({
    tags$div(class = "btn-container",
             tags$div(class = "btn-slash"
             )
    )
  })
  
  #スタート画面に飛ばす
  updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 1,sep = "") )
  
  ##################  スタート画面   ########################
  #スタートボタンを押した際にゲーム画面に遷移
  observeEvent(input$start_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 2,sep = "") )
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
  observeEvent(input$gallery_button, {
    updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 6,sep = "") )
    reverseAllMini()
  })
  
  ##################  ゲーム画面   ########################

  
  
  #カードをめくるボタンが 押された時の処理
  observeEvent(input$put_button, {
    #裏面カード生成
    lapply(1:12, function(x) {
      output[[paste0("mob",x)]] <- renderUI({makeCards(x)})
    })
    
    #カードをめくるボタン消滅
    output$put_button_ui <- renderUI({})
    #乱数生成
    makeRan()
    #グラフにプロット
    makeChart()
    #ams後に表のカード生成
    delay(3000, output$mainCard <- renderUI({
      tags$div(class = "btn-container",
               tags$object(
                 id = "object",
                 class = "img",
                 tags$img(src = paste0("cc_",card_num,".jpg"),height = "200px",width = "100px")
               )
      )
    })
    )
    #bms後にページ遷移
    delay(6000, updateTabsetPanel( session, "tallot_tab",selected = paste("panel_", 3,sep = "") ))
    #cms後にカード消滅ボタン復活
    delay(7000,allDelete())
  })
  #カードの生成用変数
  card_num <<- 0
  
  #カード消滅兼ボタン復活用関数
  allDelete <- function(){
    card_num <<- 0
    lapply(1:12, function(x) {
      output[[paste0("mob",x)]] <- renderUI({})
    })
    output$mainCard <- renderUI({})
    output$put_button_ui <- renderUI({
      tags$div(class = "btn-container",
               tags$button(
                 id = "put_button",
                 class = "btn action-button",
                 "カードをめくる"
               )
      )
    })
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
  
  makeCards <- function(x){
    tags$div(class = "btn-container",
             tags$object(
               id = "object",
               class = "img",
               tags$img(src = "cc_00.jpg",height = "200px",width = "100px")
             )
    )
  }
  
  
  
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
      output$tweet <- renderUI({ 
        tags$div(class = "btn-container",
                 tags$button(
                   id = "Tweet_button",
                   class = "btn action-button",
                   "結果をツイートする",
                   onclick = "https://twitter.com/compose/tweet', '_blank'"
                 )
        )
      })
      
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

  #ギャラリーを精製する関数
  makeGarraly <- function(x){
    text <- paste0("gallerys",x)
    tags$object(
      id = text,
      class = "img",
      tags$img(src = paste0("cc_",x,".jpg"),height = "200px",width = "100px")
    )
  }
  
  makeGarraly2 <- function(x){output[[paste0("gallery",x)]] <- renderUI({makeGarraly(x)})}
  
  lapply(0:21, function(x) makeGarraly2(x))
  
  #エクストラ
  makeGarraly2(200)
  makeGarraly2(202)
  makeGarraly2(203)
  makeGarraly2(207)
  makeGarraly2(213)
  makeGarraly2(220)
  
  #裏面
  output$gallery00 <- renderUI({
    tags$object(
      id = "gallery00",
      class = "img",
      tags$img(src = "cc_00.jpg",height = "200px",width = "100px")
    )
  })
  
  #Backボタンが押された際にStart画面へ
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