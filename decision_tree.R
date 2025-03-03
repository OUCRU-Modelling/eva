## 
library(rpart)
library(rpart.plot)

train_data <- ipsum[,c("district","edu","employ","son_in_house","dau_in_house",
                       "reli","tv","radio","computer","phone","refrig","washer",
                       "watheat","aircon","motorcyc","bicycle","car")]


train_data <- cbind(train_data[,1:6],
                    train_data[,-c(1:6)] %>%
              rowwise() %>%
              mutate(highest_property = names(.)[max(which(c_across(everything()) == 1))]) %>%
              ungroup()) %>% na.omit()

train_data$quantile  <-  
  case_when(train_data$highest_property == "car" ~ "Richest, 25%",
            train_data$highest_property == "bicycle" | 
              train_data$highest_property == "watheat" | 
              train_data$highest_property == "aircon"  ~ "Middle, 25%",
            train_data$highest_property == "washer" | 
              train_data$highest_property == "refrig"| 
              train_data$highest_property == "computer"  ~ "Lower Middle, 25%",
            train_data$highest_property == "phone" | 
              train_data$highest_property == "motorcyc" | 
              train_data$highest_property == "tv"   ~ "Poorest, 25%") %>% 
  factor(levels = c("Richest, 25%",
                    "Middle, 25%",
                    "Lower Middle, 25%",
                    "Poorest, 25%")
  )

# 

sorted$cover <- ifelse(sorted$mean >= 0.85,1,0)

train_data <- left_join(train_data,sorted, by = join_by(district == district))

train_data <- train_data[,c(1:6,19,21)]
colnames(train_data)

library(gtsummary)
library(broom.helpers)

train_data %>% filter(employ != "NIU" & reli != "NIU" & edu != "NIU" &
                      son_in_house != "NIU" & dau_in_house != "NIU") %>% 
glm(cover ~ edu + employ + reli + quantile, 
    data = ., family = binomial) %>%
  tbl_regression(
    exponentiate = TRUE,
    # label = list(edu ~ "Sex", age ~ "Age (years)", outcome ~ "Outcome"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>% 
  bold_p(t = 0.05)


tree <- rpart(cover ~ edu + quantile,data = train_data %>% filter(edu != "NIU"),
              method = "class",minsplit = 4,cp = -1)

rpart.plot(tree)

ggsave("./tree.svg",
       width = 10,height = 7)

## neural network

library(neuralnet)
library(caret)

NN <- neuralnet(cover ~ edu + quantile,train_data,hidden = c(5,3))

