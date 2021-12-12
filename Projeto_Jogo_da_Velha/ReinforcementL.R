library(ReinforcementLearning)
set.seed(1)
ambiente<-gridworldEnvironment
print(ambiente)
estados<-c("s1","s2","s3","s4")
açoes<-c("up","down","left","right") 
controle1<-list(alpha=0.1,gamma=0.5,epsilon=0.1)
dados<-sampleExperience(N=1000,env = ambiente,states = estados,actions = açoes)
interaçao<-ReinforcementLearning(dados,s="State",a="Action",r="Reward",s_new ="NextState",control = controle1) 
#alpha-taxa de aprendizado (0 a 1) 
#gamma-fator de desconto (importância de futuras recompensas)
#epsilon-parâmetro de exploração (0 a 1) 
#iter-interações 
plot(interaçao)
computePolicy(interaçao) 
summary(interaçao) 
#reaprender... 
dados_novos<-sampleExperience(N=1000,env = ambiente,states = estados,actions = açoes,actionSelect="epsilon-greedy",model=interaçao,control=controle1)
interaçaoReforged<-ReinforcementLearning(dados_novos,s="State",a="Action",r="Reward",s_new = "NextState",control = controle1,model = interaçao) 
plot(interaçaoReforged) 
summary(interaçaoReforged) 
computePolicy(interaçaoReforged)
#######################################################################
##Jogo da velha 
##Posições legais - 5479, porém apenas 765 se tirarmos as posi. de reflexão e rotação 
controle2<-list(alpha=0.3,gamma=0.4,epsilon=0.2) #poderia fazer o epsilon ir decaindo.. DeepMind do Google 
ttt<-ReinforcementLearning(tictactoe,s="State",a="Action",r="Reward",s_new = "NextState",iter = 2,control = controle2)
computePolicy(ttt) 
plot(ttt) 
summary(ttt)
unseen<-data.frame(State=c("........."),stringsAsFactors = FALSE)
predict(ttt,unseen$State) 
#não tenho a função de tictactoe infelizmente... Como encontrar seu environment???
tttReforged<-ReinforcementLearning(tictactoe,s="State",a="Action",r="Reward",s_new = "NextState",iter = 2,control=controle2,model = ttt)
summary(tttReforged)
