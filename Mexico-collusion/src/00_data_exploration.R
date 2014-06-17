library(ProjectTemplate)
reload.project()

ls()
data <- mex.clean

data$X <- NULL

head(data)

contractors <- data %.% group_by(dependance_id,contractor_id, contractor_name) %.% 
  summarise(n.contracts=n(), 
         n.amount=sum(amount, na.rm=T )) %.% arrange(n.contracts, n.amount)
contractors  <- as.data.frame(contractors)

ggplot(contractors ,aes( y= n.amount, x=n.contracts)) +   geom_point() 
#contractors$contractor_id <- as.character(contractors$contractor_id)

ggplot( contractors, aes( x= contractor_id, y=n.amount )) + geom_point() 

ggplot( contractors, aes( x= contractor_id, y=n.contracts )) + geom_point() +
  coord_flip()


tail(contractors,200)

ggplot(contractors, aes(x=c))
contractors[184,]

filter(data, contractor_id==919023988)









###suppliers
table(data$currency)
head(data)

tot <- sum( filter(data,currency=='MXN')$amount )

data$contractor_name
table(data$dependance_id)
suppliers <- data %.%filter(currency=='MXN')%.% group_by(supplier,contractor_id, dependance_id) %.% 
  summarise(n.contracts=n(), 
            n.amount=sum(amount, na.rm=T ),
            p.amount=100*sum(amount, na.rm=T )/tot )%.%
  arrange(-p.amount )
write.csv(head(data.frame(suppliers), 10), file='top-10.csv')


suppliers$n.amount.scaled <- scale(suppliers$n.amount)

ggplot(suppliers ,aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point() + 
 scale_x_log10() 

ggplot(suppliers ,
       aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point() 

ggplot(filter(suppliers,n.contracts<=20 & n.amount.scaled <10),
              aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts), size=5)) +   geom_point() 
    
ggplot(filter(suppliers,n.amount.scaled >10 ),
       aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point()



top.10  <- head(data.frame(suppliers), 15)

nrow(data)
top.10$contractor_id  <- factor(top.10$contractor_id, levels= as.character(unique(top.10$contractor_id) ))

ggplot(top.10, aes(y=p.amount, x=contractor_id,
                   colour=dependance_id , group= factor(contractor_id) , fill= dependance_id )) +   
  geom_bar(stat="identity") + scale_x_discrete(   ) +
  coord_flip() + xlab('Contractor ID number') + ylab('Percentage of total amounts awarded ') + 
  labs(fill="Buyer's State", colour="Buyer's State") + 
  ggtitle('Collusion in Mexico (50254 contracts, 100%) \n Top 15 contracts sum up to 30%')

sum(top.10$p.amount)
ggsave('top-15.pdf', width=8, height=4)

top.10



ggplot(top.10, aes(y=n.amount.scaled, x=contractor_id,
                   colour=dependance_id , group= factor(contractor_id) , fill= dependance_id )) +   
  geom_bar(stat="identity") + scale_x_discrete(   ) +
  coord_flip() + xlab('Contractor ID number') + ylab('Scaled total amounts awarded') + 
  labs(fill="Buyer's State", colour="Buyer's State") + 
  ggtitle('Collusion in Mexico (50254 contracts, 100%) \n Top 15 contracts sum up to 30%')

