#(2)
total_people = 6 + 10 + 16 + 8 + 20 + 10
#probability of male or republican and not independent
p_male_rep_not_ind = (10 + 16 + 10)/total_people
p_male_rep_not_ind

# probability of female given not democrat
p_female_not_dem = (8 + 10)/ total_people
p_female_not_dem

# there are 70 total people. If 
n_males = 6 + 10 + 16
n_females = 8 + 20 + 10
males_in_each_party = n_males/3
females_in_each_party = n_females/3
print(paste('Males in each party: ',males_in_each_party))
print(paste('Females in each party: ',females_in_each_party))

#(4) #probability that x = 2, 3, or 4
(2/15) + (1/5) + (4/15)

#(6)
mat = rbind(cbind(9,3),cbind(2,4))
# matrix transpose
t(mat)
# determinant of matrix
det(mat)

#(7)
infection = function(n_runs){
    for (i in length(n_runs)){
        base_rate = 0.05
        # hospital and patient number combinations
        n_hospital = 100
        n_people = c(25, 50, 100)
        base_rate = 0.05
        percent_no_cases = c()
        #hospital and patient number combinations
        n_hospital = 100
        n_people = c(25, 50)

        for (people in length(n_people)){
            # obtain number of cases for n_hospitals testing n_people, given the base_rate
            tests = rbinom(n_hospital, n_people[people], base_rate)
            hosps_with_no_cases = 0
            for (test in tests){
                if (test == 0 ){
                    hosps_with_no_cases = hosps_with_no_cases + 1

                }
            }
            percent_no_cases = c(percent_no_cases, hosps_with_no_cases / n_hospital)
            
        }
    }
    print(percent_no_cases)
    return(mean(percent_no_cases))
}

x = infection(100000)
# number of hospitals who will find 0 cases
x*300

