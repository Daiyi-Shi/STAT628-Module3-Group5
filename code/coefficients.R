#meaningful words
environment=c("clean","dirty","accommodate","mess","messy","filthy","sit","wifi")
quality=c("fresh","cold","hot","original","stale","scratch","grub","fly","hair","health","smell","undercooked","raw","skimpy","skimp")
taste=c("delicious","yum","yummy","tasty","bland","mediocre","plain","average","juicy","authentic","delish","creative","consistent","tasteless","gross","nom")
type=c("whip","brisket","pizza","goat","sweet","olive","bison","crisp","vanilla","salmon")
service=c("service","friendly","employee","manager","busy","helpful","polite","patient","management","courteous","rude","unfriendly","behavior","dumb")
speed=c("quick","quickly","hour","minute","fast","slow","wait","soon","efficient","speedy")
price=c("expensive","worth","affordable","overprice","upcharge","charge","overcharge")
others=c("drive","deliver","curb","university","rain")

word=rownames(fastfood_model_summary2)
coefficients=fastfood_model2$coefficients[-1]

#find all the coeffients of the meaningful word
coe_environment=c()
for (i in 1:length(environment)){
  coe_environment=c(coe_environment,coefficients[which(word==environment[i])])
}

coe_quality=c()
for (i in 1:length(quality)){
  coe_quality=c(coe_quality,coefficients[which(word==quality[i])])
}

coe_taste=c()
for (i in 1:length(taste)){
  coe_taste=c(coe_taste,coefficients[which(word==taste[i])])
}

coe_service=c()
for (i in 1:length(service)){
  coe_service=c(coe_service,coefficients[which(word==service[i])])
}

coe_type=c()
for (i in 1:length(type)){
  coe_type=c(coe_type,coefficients[which(word==type[i])])
}

coe_speed=c()
for (i in 1:length(speed)){
  coe_speed=c(coe_speed,coefficients[which(word==speed[i])])
}

coe_price=c()
for (i in 1:length(price)){
  coe_price=c(coe_price,coefficients[which(word==price[i])])
}

coe_others=c()
for (i in 1:length(others)){
  coe_others=c(coe_others,coefficients[which(word==others[i])])
}

coe_list=list(environment=coe_environment,quality=coe_quality,taste=coe_taste,type=coe_type,service=coe_service,speed=coe_speed,price=coe_price,others=coe_others)



