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

suppliers <- data %.%filter(currency=='MXN')%.% group_by(supplier,contractor_id, dependance_id) %.% 
  summarise(n.contracts=n(),
            n.amount=sum(amount, na.rm=T ),
            p.amount=100*sum(amount, na.rm=T )/tot )%.%
  arrange(-p.amount )
write.csv(head(data.frame(suppliers), 10), file='top-10.csv')

#suppliers$n.amount.scaled <- scale(suppliers$n.amount)

ggplot(suppliers ,aes( y= n.amount, x=n.contracts, colour=factor(n.contracts))) +   geom_point() + 
  scale_x_log10() 

head(suppliers)
ggplot(suppliers ,
       aes( y= n.amount, x=n.contracts, colour=factor(dependance_id) ) ) +   geom_point(size=6) + 
  ylab('Amount awarded') + xlab('Number of contracts') + labs(colour="Buyer's State")+
  ggtitle('Amount vs #Contracts per supplier') #+guides(colour=FALSE)
ggsave('amount-contract.pdf', height = 9, width = 15 )

# ggplot(filter(suppliers,n.contracts<=20 & n.amount.scaled <10),
#               aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts), size=5)) +   geom_point() 
#     
# ggplot(filter(suppliers,n.amount.scaled >10 ),
#        aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point()

top.10  <- head(data.frame(suppliers), 15)
top.10
nrow(data)
top.10$contractor_id  <- factor(top.10$contractor_id, 
                                levels= as.character(unique(top.10$contractor_id) ))
top.10
ggplot(top.10, aes(y=p.amount, x=contractor_id,
                   colour=dependance_id , group= factor(contractor_id) , fill= dependance_id )) +   
  geom_bar(stat="identity") + scale_x_discrete(   ) +
  coord_flip() + xlab('Contractor ID number') + ylab('Percentage of total amounts awarded ') + 
  labs(fill="Buyer's State", colour="Buyer's State") + 
  ggtitle('Collusion in Mexico (50254 contracts, 100%) \n Top 15 contracts sum up to 30%')

sum(top.10$p.amount)
ggsave('top-15.pdf', width=8, height=4)

top.10


many.15 <- as.data.frame(head(arrange(suppliers, -n.contracts),15))

many.15$contractor_id  <- factor(many.15$contractor_id, 
                                levels= as.character(unique(many.15$contractor_id) ))
many.15$sup <- gsub(many.15$supplier, pattern = 'SA DE CV', replacement = '')
many.15$sup  <- gsub(many.15$sup, pattern = 'S.A. DE C.V.', replacement = '')

many.15
ggplot(many.15, aes(y=n.contracts, x=sup,
                    colour=dependance_id , group= dependance_id , size= n.amount ) )+   
  geom_point() + scale_x_discrete(   ) + coord_flip() +
 xlab('Supplier name') + ylab('Number of contracts awarded') + 
  labs(colour="Buyer's State", size="Amount awarded") + 
  ggtitle('Collusion in Mexico (50254 contracts, 100%) \n Top 15 contracts sum up to 19%')

sum(many.15$p.amount)
100*sum(many.15$n.contracts)/50254
ggsave('many-15.pdf', height = 4, width = 12)
