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
                         column(1,offset = 5,uiOutput("fuga1")),
                         column(6)
                       ),
                       fluidRow(
                         column(1,offset = 3 ,uiOutput("fuga2")),
                         column(1,offset = 3,uiOutput("fuga3")),
                         column(4)
                       ),
                       fluidRow(
                         column(1,offset = 1,uiOutput("fuga4")),
                         column(1,offset = 7,uiOutput("fuga5")),
                         column(2)
                       ),
                       fluidRow(
                         column(1,uiOutput("fuga6")),
                         column(1,offset = 4,uiOutput("fuga7")),
                         column(1,offset = 4,uiOutput("fuga8")),
                         column(1)
                       ),
                       fluidRow(
                         column(1,offset = 1,uiOutput("fuga9")),
                         column(1,offset = 7,uiOutput("fuga10")),
                         column(2)
                       ),
                       fluidRow(
                         column(1,offset = 3,uiOutput("fuga11")),
                         column(1,offset = 3,uiOutput("fuga12")),
                         column(4)
                       ),
                       fluidRow(
                         column(1,offset = 5,uiOutput("fuga13")),
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
  
  
  
  #重複対策
  tallot_data_sets <<- c(0:21)
  #めくる枚数とめくった番号の合計
  turn_num <<- 0
  sum_num <<- 0
  
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
    tallot_data_sets <<- c(0:21)
    turn_num <<- 0
    sum_num <<- 0
  }
  
  all_reverse()
  
  #カードを裏返す関数
  make_ui <- function(x){
    text = paste("button",x,sep = "")
    tags$head(
      tags$style(HTML("#button1{background-color:red}"))
    )
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_","00",".jpg",sep = ""),
               height = "200px",width = "100px"
      ),
      style="color: #000000; background-color: #000000; border-color: #000000"
      
      #実際はこれ
      # tags$img(src = paste("tallot_",00,".png",sep = ""),
      #          height = "200px",width = "100px"
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
    #全てのカードが出たら最後のカードから変更させない
    if(sum(tallot_data_sets) <= -22){
      num <<- 00
      return()
    }
    x <- floor(runif(1,0,22))
    if(tallot_data_sets[x+1] != x){
      Recall()
    }
    else{
      dual = sum( (df_id %% 100) - x == 0 )
      ran <- floor(runif(1,0,dual))
      num <<- x + ran * 100
      tallot_data_sets[x+1] <<- -1
    }
  }
  
  #カードを裏返す関数
  turn_card <- function(x){
    turn_num <<- turn_num + 1
    sum_num <<- sum_num + (num %% 100)
    text = paste("button",x,sep="")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_", num ,".jpg",sep = ""),
               height = "200px",
               width = "100px"
      ),
      style="color: #000000; background-color: #000000; border-color: #000000"
      # tags$img(src = paste("tallot_", num ,".png",sep = ""),
      #          height = "200px",
      #          width = "100px"
      # )
    )
  }
  
  ##################  結果画面   ########################
  #結果画面のレーダーチャートとその他を作る関数
  change <- function(){
    if(turn_num == 0){
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
      output$sentence <- renderUI({
        h1(df_sentence_ex[df_ex$id == sum_num %% 100])
      })
      output$sentence2 <- renderUI({
        h5(df_sentence2_ex[df_ex$id == sum_num %% 100])
      })
      output$tweet <- renderUI({
        actionButton("Tweet_button","Tweet",onclick = "window.open('https://twitter.com/login?lang=ja', '_blank')")
      })
    }
    
  }
  #Backボタンが押された際にカードを裏返してStart画面へ
  observeEvent(input$Back_to_start_button1, {
    updateTabsetPanel(session, "tallot_tab",
                      selected = paste("panel_", 1,sep = "")
    )
    all_reverse()
  })
  
  
  ##################  ミニゲーム画面   ########################
  
  #プレイヤーのフラグと各得点
  play_turn_flag <<- 0
  p_points <<- c(1:2) - c(1:2)
  turned_number <<- 0
  open_tmp <<- 0
  #UI作成
  output$player1_point <- renderUI({h1(p_points[1])})
  output$player2_point <- renderUI({h1(p_points[2])})
  output$MiniGame_start <- renderUI({actionButton("MiniGame_start","Start")})
  #対象の役生成
  moto <<- unique( floor( runif(10000,0,22) ) )#ランダムに0~21生成
  print(moto)
  moto <<- append(moto[1:12],moto[1:12])#12個ずつ一組作る
  print(moto)
  moto <<- moto[order(rnorm(length(moto)))]#ランダムに並べ替える
  print(moto)
  #裏面生成
  
  
  all_reverse_mini <- function(){
    output$mini1 <- renderUI({make_ui_mini(1)})
    output$mini2 <- renderUI({make_ui_mini(2)})
    output$mini3 <- renderUI({make_ui_mini(3)})
    output$mini4 <- renderUI({make_ui_mini(4)})
    output$mini5 <- renderUI({make_ui_mini(5)})
    output$mini6 <- renderUI({make_ui_mini(6)})
    output$mini7 <- renderUI({make_ui_mini(7)})
    output$mini8 <- renderUI({make_ui_mini(8)})
    output$mini9 <- renderUI({make_ui_mini(9)})
    output$mini10 <- renderUI({make_ui_mini(10)})
    output$mini11 <- renderUI({make_ui_mini(11)})
    output$mini12 <- renderUI({make_ui_mini(12)})
    output$mini13 <- renderUI({make_ui_mini(13)})
    output$mini14 <- renderUI({make_ui_mini(14)})
    output$mini15 <- renderUI({make_ui_mini(15)})
    output$mini16 <- renderUI({make_ui_mini(16)})
    output$mini17 <- renderUI({make_ui_mini(17)})
    output$mini18 <- renderUI({make_ui_mini(18)})
    output$mini19 <- renderUI({make_ui_mini(19)})
    output$mini20 <- renderUI({make_ui_mini(20)})
    output$mini21 <- renderUI({make_ui_mini(21)})
    output$mini22 <- renderUI({make_ui_mini(22)})
    output$mini23 <- renderUI({make_ui_mini(23)})
    output$mini24 <- renderUI({make_ui_mini(24)})
  }
  
  all_reverse_mini()
  
  make_ui_mini <- function(x){
    text = paste("button_mini",x,sep = "")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_","00",".jpg",sep = ""),
               height = "200px",width = "100px"
      ),
      style="color: #000000; background-color: #000000; border-color: #000000"
      #実際はこれ
      # tags$img(src = paste("tallot_",00,".jpg",sep = ""),
      #          height = "300px",width = "150px"
      # )
    )
  }
  
  observeEvent(input$Player1_turn,{
    if(play_turn_flag == 2){
      play_turn_flag <<- 1
    }
  })
  observeEvent(input$Player2_turn,{
    if(play_turn_flag == 1){
      play_turn_flag <<- 2
    }
  })
  observeEvent(input$MiniGame_start,{
    play_turn_flag <<- 1
  })
  
  #各カードの反転
  observeEvent(input$button_mini1,{
    output$mini1 <- renderUI({turn_card_mini(1)})
    check_mini(1)
  })
  observeEvent(input$button_mini2,{
    output$mini2 <- renderUI({turn_card_mini(2)})
    check_mini(2)
  })
  observeEvent(input$button_mini3,{
    output$mini3 <- renderUI({turn_card_mini(3)})
    check_mini(3)
  })
  observeEvent(input$button_mini4,{
    output$mini4 <- renderUI({turn_card_mini(4)})
    check_mini(4)
  })
  observeEvent(input$button_mini5,{
    output$mini5 <- renderUI({turn_card_mini(5)})
    check_mini(5)
  })
  observeEvent(input$button_mini6,{
    output$mini6 <- renderUI({turn_card_mini(6)})
    check_mini(6)
  })
  observeEvent(input$button_mini7,{
    output$mini7 <- renderUI({turn_card_mini(7)})
    check_mini(7)
  })
  observeEvent(input$button_mini8,{
    output$mini8 <- renderUI({turn_card_mini(8)})
    check_mini(8)
  })
  observeEvent(input$button_mini9,{
    output$mini9 <- renderUI({turn_card_mini(9)})
    check_mini(9)
  })
  observeEvent(input$button_mini10,{
    output$mini10 <- renderUI({turn_card_mini(10)})
    check_mini(10)
  })
  observeEvent(input$button_mini11,{
    output$mini11 <- renderUI({turn_card_mini(11)})
    check_mini(11)
  })
  observeEvent(input$button_mini12,{
    output$mini12 <- renderUI({turn_card_mini(12)})
    check_mini(12)
  })
  observeEvent(input$button_mini13,{
    output$mini13 <- renderUI({turn_card_mini(13)})
    check_mini(13)
  })
  observeEvent(input$button_mini14,{
    output$mini14 <- renderUI({turn_card_mini(14)})
    check_mini(14)
  })
  observeEvent(input$button_mini15,{
    output$mini15 <- renderUI({turn_card_mini(15)})
    check_mini(15)
  })
  observeEvent(input$button_mini16,{
    output$mini16 <- renderUI({turn_card_mini(16)})
    check_mini(16)
  })
  observeEvent(input$button_mini17,{
    output$mini17 <- renderUI({turn_card_mini(17)})
    check_mini(17)
  })
  observeEvent(input$button_mini18,{
    output$mini18 <- renderUI({turn_card_mini(18)})
    check_mini(18)
  })
  observeEvent(input$button_mini19,{
    output$mini19 <- renderUI({turn_card_mini(19)})
    check_mini(19)
  })
  observeEvent(input$button_mini20,{
    output$mini20 <- renderUI({turn_card_mini(20)})
    check_mini(20)
  })
  observeEvent(input$button_mini21,{
    output$mini21 <- renderUI({turn_card_mini(21)})
    check_mini(21)
  })
  observeEvent(input$button_mini22,{
    output$mini22 <- renderUI({turn_card_mini(22)})
    check_mini(22)
  })
  observeEvent(input$button_mini23,{
    output$mini23 <- renderUI({turn_card_mini(23)})
    check_mini(23)
  })
  observeEvent(input$button_mini24,{
    output$mini24 <- renderUI({turn_card_mini(24)})
    check_mini(24)
  })
  
  
  turn_card_mini <- function(x){
    print(moto)
    text = paste("button_mini",x,sep="")
    tags$button(
      id = text,
      class = "btn action-button",
      tags$img(src = paste("cc_", moto[x] ,".jpg",sep = ""),
               height = "200px",
               width = "100px"
      ),
      style="color: #000000; background-color: #000000; border-color: #000000"
      # tags$img(src = paste("tallot_", num ,".png",sep = ""),
      #          height = "200px",
      #          width = "100px"
      # )
    )
  }
  
  check_mini <- function(x){
    turned_number <<- turned_number + 1
    if(turned_number == 1){
      open_tmp <<- moto[x]
    }
    else if(turned_number == 2){
      if(open_tmp == moto[x]){
        p_points[turned_number] <<- p_points[turned_number] + 1
      }
    }
  }
  
  
  
  
  ##################  スタッフ画面   ########################
  #Backボタンが押された際にStart画面へ
  observeEvent(input$Back_to_start_button2, {
    updateTabsetPanel(session, "tallot_tab",selected = paste("panel_", 1,sep = ""))
  })
}
shinyApp(ui = ui, server = server)


