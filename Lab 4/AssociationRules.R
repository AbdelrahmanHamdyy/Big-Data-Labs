#Name: Abdelrahman Noaman Loqman, Code:9202851, Sec:2, BN:4
#Name: Abdelrahman Hamdy Ahmed, Code:9202833, Sec:1, BN:38
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 1: First of all, start by cleaning the workspace and setting the working directory.
    rm(list=ls())
# Step 2: Load the libraries arules and arulesViz
#install.packages(c("arules","arulesViz"))
    library(arules)
    library(arulesViz)
# Step 3: Load the transactions in the file AssociationRules.csv using the function read.transactions. Make sure you don’t include the header line in the dataset.
    transactions <- read.transactions("AssociationRules.csv", header = FALSE)
# Step 4: Display the transactions in a readable format using the function inspect. Display only the first 100 transactions.
    inspect(transactions[1:100])
# Step 5: What are the most frequent two items in the dataset? What are their frequencies? 
    #Hint: use the function itemFrequency 
    # item_frequency <- sort(itemFrequency(transactions), decreasing = TRUE)
    # top_items <- head(item_frequency, 2)
    # print(top_items)
    #or use the function summary.
    summary(transactions)
    # and from both functions we got the following
    # item3 is the most frequent item with 4948 occurrence
    # item5 is the second most frequent item with 3699 occurrence

# Step 6: Plot the 5 most frequent items of the transactions using the function itemFrequencyPlot
    itemFrequencyPlot(transactions, topN = 5)
# Step 7: Generate the association rules from the transactions using the apriori algorithm. Set the minimum support = 0.01, minimum confidence = 0.5, minimum cardinality (number of items in the rule) = 2. Use the function apriori
    assoc_rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.5, minlen=2))
# Step 8: Now, sort the generated rules by support. Search the function sort found in the arules package. Show only the first 6 rules.
    rules_by_support = sort(assoc_rules, descreasing=TRUE, by="support")
    inspect(rules_by_support[1:6])
    #         lhs     rhs      support confidence coverage   lift   count
    # [1] {item5}  => {item13} 0.1877  0.5074344  0.3699   1.025534 1877
    # [2] {item30} => {item13} 0.1748  0.5284160  0.3308   1.067938 1748 
    # [3] {item58} => {item13} 0.1478  0.5220770  0.2831   1.055127 1478
    # [4] {item21} => {item13} 0.1391  0.5023474  0.2769   1.015253 1391
    # [5] {item84} => {item13} 0.1239  0.5238901  0.2365   1.058792 1239
    # [6] {item42} => {item13} 0.1200  0.5237887  0.2291   1.058587 1200 
# Step 9: Sort the generated rules by confidence. Show only the first 6 rules.
    rules_by_confidence = sort(assoc_rules, descreasing=TRUE, by="confidence")
    inspect(rules_by_confidence[1:6])
    #         lhs                       rhs    support confidence coverage lift     
    # [1] {item15, item49, item56} => {item30} 0.0101  1.0000000  0.0101    3.022975
    # [2] {item49, item56, item84} => {item30} 0.0100  1.0000000  0.0100    3.022975
    # [3] {item49, item56}         => {item30} 0.0105  0.9905660  0.0106    2.994456
    # [4] {item15, item49, item84} => {item30} 0.0100  0.9803922  0.0102    2.963701
    # [5] {item30, item49, item56} => {item15} 0.0101  0.9619048  0.0105    9.240199
    # [6] {item15, item30, item49} => {item56} 0.0101  0.9619048  0.0105   16.584565
    #     count
    # [1] 101
    # [2] 100
    # [3] 105
    # [4] 100
    # [5] 101
    # [6] 101
# Step 10: Sort the generated rules by lift. Show only the first 6 rules.
    rules_by_lift = sort(assoc_rules, descreasing=TRUE, by="lift")
    inspect(rules_by_lift[1:6])
    #         lhs                         rhs  support confidence coverage lift    
    # [1] {item15, item30, item56} => {item49} 0.0101  0.7709924  0.0131   19.42046
    # [2] {item30, item56, item84} => {item49} 0.0100  0.7407407  0.0135   18.65846
    # [3] {item15, item30, item49} => {item56} 0.0101  0.9619048  0.0105   16.58456
    # [4] {item15, item56}         => {item49} 0.0101  0.6121212  0.0165   15.41867
    # [5] {item15, item49}         => {item56} 0.0101  0.8632479  0.0117   14.88358
    # [6] {item30, item49, item84} => {item56} 0.0100  0.8000000  0.0125   13.79310
    #     count
    # [1] 101
    # [2] 100
    # [3] 101
    # [4] 101
    # [5] 101
    # [6] 100
# Step 11: Plot the generated rules with support as x-axis, confidence as y-axis and lift as shading. Use the function plot in arules package.
    plot(assoc_rules, measure = c("support", "confidence"), shading = "lift", col = c("red", "lightblue"))
# Step 12: Based on (8-11), Can you tell now what are the most interesting rules that are really useful and provide a real business value and an insight to the concerned corporate?
    # The most interesting rules are those with high lift values.
    # But why did we choose it?
    # we may answer it in two ways the first one of why we didn't choose confidence? and this is because Confidence is able to identify trustworthy rules, but it cannot tell whether a rule is coincidental.
    #so for example if we have 100 transaction and only 2 of them contains item 2, item 5 and item2's support is 2 but item 5 support is 100
    #them the confidence won't take in consideration the value of support (item 5) and the value of confidence will be 100% which means they are totally associated with each other but this is not a wise choice
    # but lift will take in consideration that item 5 is repeated many times like in all the transactions available