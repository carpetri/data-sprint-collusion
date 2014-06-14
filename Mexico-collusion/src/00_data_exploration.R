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


suppliers <- data %.%filter(currency=='MXN')%.% group_by(supplier) %.% 
  summarise(n.contracts=n(), 
            n.amount=sum(amount, na.rm=T )
            p.amount=sum(amount, na.rm=T )/ ) %.% arrange(n.contracts, n.amount )
tail(suppliers, 50)

suppliers$n.amount.scaled <- scale(suppliers$n.amount)

ggplot(suppliers ,aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point() + 
 scale_x_log10() 

ggplot(suppliers ,
       aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point() 

ggplot(filter(suppliers,n.contracts<=20 & n.amount.scaled <10),
              aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts), size=5)) +   geom_point() 
    
ggplot(filter(suppliers,n.amount.scaled >10 ),
       aes( y= n.amount.scaled, x=n.contracts, colour=factor(n.contracts))) +   geom_point()



