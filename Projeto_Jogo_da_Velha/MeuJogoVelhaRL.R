JogoTTT<-function(jogada) 
{ 
  verificarVit<-function() 
  { 
    vitoria<-paste(estadoj,collapse = ""); 
    if( 
      (substr(vitoria,1,3)%in%c("XXX","BBB"))|(substr(vitoria,4,6)%in%c("XXX","BBB")|(substr(vitoria,7,9)%in%c("XXX","BBB"))|#horizontal
      (paste(substr(vitoria,1,1),substr(vitoria,4,4),substr(vitoria,7,7),sep = "")%in%c("XXX","BBB"))|#verticais...
      (paste(substr(vitoria,2,2),substr(vitoria,5,5),substr(vitoria,8,8),sep = "")%in%c("XXX","BBB"))| 
      (paste(substr(vitoria,3,3),substr(vitoria,6,6),substr(vitoria,9,9),sep = "")%in%c("XXX","BBB"))|#. 
      (paste(substr(vitoria,1,1),substr(vitoria,5,5),substr(vitoria,9,9),sep = "")%in%c("XXX","BBB"))|#diagonais 
      (paste(substr(vitoria,3,3),substr(vitoria,5,5),substr(vitoria,7,7),sep = "")%in%c("XXX","BBB")) 
    ))warning("vitoria!")
  } 
  if(jogada==0){ 
    estadoj<<-c(replicate(9,"."))#preenche o vetor posicao com pontos 
    plot(0,type='n',axes=FALSE,ann=FALSE) 
    #grade do jogo da velha 
    abline(v=.87) 
    abline(v=1.14) 
    abline(h=-.34) 
    abline(h=.32) 
    #IA vai jogar em.... 
    PA<-sample(9,1) 
    estadoj[PA]<<-"X" #manter o escopo do jogo, nao esquecer que o game ja comecou 
    
  }else{ 
    #pessoa joga 
    if(estadoj[jogada]!="."){ 
      stop("ilegal") #tipo break
      } 
    estadoj[jogada]<<-"B" 
    verificarVit(); 
    #IA consulta o RF pra te destruir... 
    jogada<-ttt$Policy[paste(estadoj,collapse = "")]#transforma em string o estado atual do game e passa pro policy 
    jogada<-as.integer(substr(jogada,2,2))#pega a sugestao e transforma em inteiro (jogada)
    if(is.na(jogada)|estadoj[jogada]!="."){ 
       print(paste0("ilegal: ",ifelse(is.na(jogada),"NaN","Invalida, "),paste(estadoj,collapse = ""))) 
       jogada<-regexpr(".",paste0(estadoj,collapse = ""),fixed = T)[1]#simplesmente joga na prox opcao disponivel
    } 
    #IA joga novamente 
    estadoj[jogada]<<-"X" 
    verificarVit();
  } 
  text(0.735,.8,labels=ifelse(estadoj[1]=="."," ",estadoj[1]),cex=7) 
  text(1,.8,labels=ifelse(estadoj[2]=="."," ",estadoj[2]),cex=7) 
  text(1.257,.8,labels=ifelse(estadoj[3]=="."," ",estadoj[3]),cex=7)
  text(0.735,0,labels=ifelse(estadoj[4]=="."," ",estadoj[4]),cex=7) 
  text(1,0,labels=ifelse(estadoj[5]=="."," ",estadoj[5]),cex=7) 
  text(1.257,0,labels=ifelse(estadoj[6]=="."," ",estadoj[6]),cex=7) 
  text(0.735,-.73,labels=ifelse(estadoj[7]=="."," ",estadoj[7]),cex=7)
  text(1,-.73,labels=ifelse(estadoj[8]=="."," ",estadoj[8]),cex=7) 
  text(1.257,-.73,labels=ifelse(estadoj[9]=="."," ",estadoj[9]),cex=7)
  #endgame 
  fim<-regexpr(".",paste0(estadoj,collapse = ""),fixed = T)[1] 
  if(fim==-1){ 
    warning("Fim de jogo!") 
  } 
  } 

 
