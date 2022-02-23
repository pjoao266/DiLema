library(shiny)
dia_inicio = "2022-02-23"

dicionario = readRDS('dados/palavras.RDS')
palavras_elegiveis = readRDS('dados/palavras_elegiveis.RDS')
source('functions/functions.R')
shinyServer(function(input, output) {
  
  aux = reactiveValues(n_palavras = 2,
                       max_tentativa=7,
                       n_tentativa = 1,
                       day = 1+interval(dia_inicio, today()) %/% days(1),
                       palavra_escolhida = NULL,
                       matches_letras = list(),
                       info_tentativas = list(),
                       acertou = c(),
                       acabou = FALSE,
                       palavra_existe = T,
                       lines = NULL,
                       letras = character(0))
  
  observeEvent(aux$day,{
    #set.seed(aux$day)
    aux$palavra_escolhida= sample(palavras_elegiveis, aux$n_palavras)
    for (k in 1:aux$n_palavras) {
      aux$info_tentativas[[k]] = list()
      aux$matches_letras[[k]] = list()
      aux$acertou[k] = 0
    }
  })
  
  observeEvent(aux$palavra_escolhida,print(aux$palavra_escolhida))
  
  
  
  reset_game <- function() {
    #set.seed(aux$day)
    aux$palavra_escolhida= sample(palavras_elegiveis, aux$n_palavras)
    aux$acertou = c()
    aux$letras = character(0)
    aux$info_tentativas = list()
    aux$matches_letras = list()
    for (k in 1:aux$n_palavras) {
      aux$info_tentativas[[k]] = list()
      aux$matches_letras[[k]] = list()
      aux$acertou[k] = 0
    }
    aux$n_tentativa = 1
    aux$palavra_existe = T
    aux$acabou = FALSE
  }
  
  
  observeEvent(input$Enter, {
    if(!aux$palavra_existe){
      aux$letras = character(0)
      aux$palavra_existe = T
    }
    else{
      palpite <- paste(aux$letras, collapse = "")
      aux$palavra_existe = T
      if (!palpite %in% dicionario){
        aux$palavra_existe = F
        return()
      }
      
      for(k in 1:aux$n_palavras){
        
        check_result <- check_word(palpite, aux$palavra_escolhida[k])
        aux$info_tentativas[[k]][[aux$n_tentativa]] = check_result
        tamanho_palavra <- nchar(aux$palavra_escolhida[k])
        matches_letras = aux$matches_letras[[k]]
        for (i in 1:tamanho_palavra) {
          letra = check_result$letters[i]
          match_atual = check_result$matches[i]
          match_letra_antigo = matches_letras[[letra]]
          
          if (is.null(match_letra_antigo)) {
            matches_letras[[letra]] =  match_atual
          } else {
            if (match_atual == "correct" && match_letra_antigo %in% c("not-in-word", "in-word")) {
              matches_letras[[letra]] = match_atual
            } else if (match_atual == "in-word" && match_letra_antigo == "not-in-word") {
              matches_letras[[letra]] =  match_atual
            }
          }
        }
        aux$matches_letras[[k]] = matches_letras
        if(aux$acertou[k]==0){
          if (isTRUE(check_result$win)) {
            aux$acertou[k] = aux$n_tentativa
          }
        }
      }
      if(sum(aux$acertou>0)==aux$n_palavras || aux$n_tentativa==aux$max_tentativa){
        aux$acabou = T
      }
      aux$n_tentativa = aux$n_tentativa + 1
      aux$letras = character(0)
    }
  })
  
  output$current_guess <- renderUI({
    if (aux$acabou) return()
    req(aux$palavra_escolhida)
    palavras = list()
    for (k in 1:aux$n_palavras) {
      letras_atuais <- aux$letras
      tamanho_palavra <- nchar(aux$palavra_escolhida[k])
      
      painel_tentativa = list()
      for (i in 1:aux$max_tentativa) {
        pos_acerto = ifelse(aux$acertou[k]==0,aux$max_tentativa+1,aux$acertou[k]) 
        
        if(i < aux$n_tentativa & i <= pos_acerto){
          infos_tent_i = aux$info_tentativas[[k]][[i]]
          letras = infos_tent_i$letters
          tipo = infos_tent_i$matches
          letras_div = list()
          for (j in 1:tamanho_palavra) {
            letras_div[[j]] = div(str_to_upper(letras[j]), class = paste("letter", tipo[j]))
          }
          painel_tentativa[[i]] = div(
            class = "word",
            letras_div)
        }
        else if(i == aux$n_tentativa & i <= pos_acerto){
          if(length(letras_atuais) < tamanho_palavra) {
            letras_atuais[(length(letras_atuais)+1) : tamanho_palavra] <- ""
          }
          classes = "letter guess guess_atual"
          if(!aux$palavra_existe){
            classes = paste0(classes, ' erro')
          }
          painel_tentativa[[i]] = div(
            class = "word",
            lapply(letras_atuais, function(letter) {
              div(toupper(letter), class =classes)
            }))
        }
        else if(i>aux$n_tentativa | i > pos_acerto){
          letras = character(tamanho_palavra)
          painel_tentativa[[i]] = div(
            class = "word",
            lapply(letras, function(letter) {
              div(toupper(letter), class ="letter guess")
            }))
        }
      }
      palavras[[k]] = div(painel_tentativa, class='palavra')
    }
    
    div(palavras,class='caixa_palavras')
  })
  
  output$new_game_ui <- renderUI({
    if (!aux$acabou)
      return()
    div(actionButton("new_game", "New Game"),
        actionButton("copy", "Copy"))
  })
  
  observeEvent(input$new_game, {
    reset_game()
  })
  observeEvent(input$copy,{
    acertou = ifelse(aux$acertou == 0, aux$max_tentativa, aux$acertou) 
    max_tent = max(acertou)
    linha = c()
    for (i in 1:max_tent) {
      linha[i] = ""
      for (k in 1:aux$n_palavras) {
        tamanho_palavra = nchar(aux$palavra_escolhida[k])
        if(i<=acertou[k]){
          for (j in 1:tamanho_palavra) {
            emoji = switch(aux$info_tentativas[[k]][[i]]$matches[[j]],
                           "correct" = "🟩",
                           "in-word" = "🟨",
                           "not-in-word" = "⬛")
            linha[i] = paste0(linha[i],emoji)
          }
        }else{
          for (j in 1:tamanho_palavra) {
            linha[i]=paste0(linha[i], "⬜")
          }
        }
        if(k!=aux$n_palavras){
          linha[i]=paste0(linha[i], "    ")
        }
      }
    }
    cores = paste(linha,collapse = '\\n')
    
    if(!0 %in% aux$acertou){
      acertos = paste0(aux$acertou, "\\\\", aux$max_tentativa, collapse=" & ")
    }else{
      texto_acertos = c()
      for (i in 1:aux$n_palavras) {
        if(aux$acertou[i]==0){
          texto_acertos[i] = 'fail'
        }else{
          texto_acertos[i] = paste0(aux$acertou[i], "\\\\", aux$max_tentativa)
        }
        acertos = paste0(texto_acertos, collapse=" & ")
      }
      
    }
    linha_inicial = paste0('joguei odilema.herokuapp.com #',aux$day,' ',acertos)
    lines = paste(linha_inicial,cores, sep='\\n \\n')
    runjs(paste0('copyTextToClipboard("',lines,'");'))
  })
  
  keys <- list(
    c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"),
    c("A", "S", "D", "F", "G", "H", "J", "K", "L"),
    c("Enter", "Z", "X", "C", "V", "B", "N", "M", "Back")
  )
  
  output$keyboard <- renderUI({
    if (aux$acabou) return()
    cont_rows = 0
    keyboard_list = list()
    for (row_key in keys) {
      cont_rows = cont_rows+1
      row_keys = list()
      cont = 0
      for(key in row_key){
        cont=cont+1
        class <- "key"
        key_lower <- tolower(key)
        palavras = ""
        if(aux$n_tentativa!=1){
          matches_letras = aux$matches_letras
          for (k in 1:aux$n_palavras) {
            if (!is.null(matches_letras[[k]][[key_lower]])) {
              palavras<- c(palavras, matches_letras[[k]][[key_lower]])
            }
          }
          palavras = case_when(palavras=='not-in-word'~'not',
                               palavras=='in-word'~'in',
                               palavras=='correct'~'cor',
                               T~'')
          
          palavras = paste0(palavras[palavras!=''], collapse='-')
        }
        if (key %in% c("Enter", "Back")) {
          class <- c(class, "wide-key")
        }
        class = c(class,palavras)
        row_keys[[cont]] = actionButton(key, key, class = paste0(class,collapse = ' '))
      }
      keyboard_list[[cont_rows]] = div(row_keys,class = "keyboard-row")
      
    }
    div(keyboard_list,class = "keyboard")
  })
  
  # Add listeners for each key, except Enter and Back
  lapply(unlist(keys, recursive = FALSE), function(key) {
    if (key %in% c("Enter", "Back")) return()
    observeEvent(input[[key]], {
      if(!aux$palavra_existe){
        aux$letras = character(0)
        aux$palavra_existe = T
      }
      if (aux$acabou)
        return()
      if (length(aux$letras) >= 5)
        return()
      aux$letras = c(aux$letras, tolower(key))
    })
  })
  
  observeEvent(input$Back, {
    if(!aux$palavra_existe){
      aux$letras = character(0)
      aux$palavra_existe = T
    }
    
    if(length(aux$letras) > 0) {
      aux$letras = aux$letras[-length(aux$letras)]
    }
  })
  
  output$endgame <- renderUI({
    if (!aux$acabou)
      return()
    line_list = list()
    div_list=list()
    i_list = list()
    acertou = ifelse(aux$acertou == 0, aux$max_tentativa, aux$acertou) 
    max_tent = max(acertou)
    for (i in 1:max_tent) {
      
      line_list[[i]] = list()
      for (k in 1:aux$n_palavras) {
        line_list[[i]][[k]] = list()
        tamanho_palavra = nchar(aux$palavra_escolhida[k])
        if(i<=acertou[k]){
          for (j in 1:tamanho_palavra) {
            line_list[[i]][[k]][[j]] = switch(aux$info_tentativas[[k]][[i]]$matches[[j]],
                                              "correct" = "🟩",
                                              "in-word" = "🟨",
                                              "not-in-word" = "⬛"
            )
          }
        }else{
          for (j in 1:tamanho_palavra) {
            line_list[[i]][[k]][[j]]= "⬜"
          }
        }
      }
      cont_line = 0
      i_list[[i]] = list()
      for (k in 1:aux$n_palavras) {
        cont_line =  cont_line + 1
        i_list[[i]][[cont_line]] = span(paste0(line_list[[i]][[k]],collapse = ''))
        if(k!=aux$n_palavras){
          cont_line = cont_line + 1
          i_list[[i]][[cont_line]] = span(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
        }
      }
      
      div_list[[i]] = div(i_list[[i]])
    }
    if(0 %in% aux$acertou){
      palavras = list()
      for (i in 1:aux$n_palavras) {
        palavras[[i]] = aux$palavra_escolhida[i]
      }
      title = p(h2('Perdeu!!',class='red-text'),
                h3(palavras))
    }else{
      for (i in 1:aux$n_palavras) {
        palavras[[i]] = aux$palavra_escolhida[i]
      }
      title = p(h2('Ganhou!!',class='green-text'),
                h3(palavras))
    }
    
    div(list(title,div(class = "endgame-content",div_list)))
  })
  
})

