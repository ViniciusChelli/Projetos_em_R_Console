library(ReinforcementLearning)
set.seed(1)
ambiente<-gridworldEnvironment
print(ambiente)
estados<-c("s1","s2","s3","s4")
a�oes<-c("up","down","left","right") 
controle1<-list(alpha=0.1,gamma=0.5,epsilon=0.1)
dados<-sampleExperience(N=1000,env = ambiente,states = estados,actions = a�oes)
intera�ao<-ReinforcementLearning(dados,s="State",a="Action",r="Reward",s_new ="NextState",control = controle1) 
#alpha-taxa de aprendizado (0 a 1) 
#gamma-fator de desconto (import�ncia de futuras recompensas)
#epsilon-par�metro de explora��o (0 a 1) 
#iter-intera��es 
plot(intera�ao)
computePolicy(intera�ao) 
summary(intera�ao) 
#reaprender... 
dados_novos<-sampleExperience(N=1000,env = ambiente,states = estados,actions = a�oes,actionSelect="epsilon-greedy",model=intera�ao,control=controle1)
intera�aoReforged<-ReinforcementLearning(dados_novos,s="State",a="Action",r="Reward",s_new = "NextState",control = controle1,model = intera�ao) 
plot(intera�aoReforged) 
summary(intera�aoReforged) 
computePolicy(intera�aoReforged)
#######################################################################
##Jogo da velha 
##Posi��es legais - 5479, por�m apenas 765 se tirarmos as posi. de reflex�o e rota��o 
controle2<-list(alpha=0.3,gamma=0.4,epsilon=0.2) #poderia fazer o epsilon ir decaindo.. DeepMind do Google 
ttt<-ReinforcementLearning(tictactoe,s="State",a="Action",r="Reward",s_new = "NextState",iter = 2,control = controle2)
computePolicy(ttt) 
plot(ttt) 
summary(ttt)
unseen<-data.frame(State=c("........."),stringsAsFactors = FALSE)
predict(ttt,unseen$State) 
#n�o tenho a fun��o de tictactoe infelizmente... Como encontrar seu environment???
tttReforged<-ReinforcementLearning(tictactoe,s="State",a="Action",r="Reward",s_new = "NextState",iter = 2,control=controle2,model = ttt)
summary(tttReforged)
