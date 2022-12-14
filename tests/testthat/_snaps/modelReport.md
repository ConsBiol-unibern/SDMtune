# Settings are correct

              
      * Model type: Maxent
      * Train locations: 5400
          * presence: 400
          * absence/background: 5000
      * Test locations: 5400
          * presence: 400
          * absence/background: 5000
      * Continuous variables: bio1, bio12, bio16, bio17, bio5, bio6, bio7, bio8
      * Categorical variables: biome
      * Output type: cloglog
      * Feature Class combination: lqph
      * Regularization multiplier: 1
      * Do clamping for predictions: TRUE,
      * Extra arguments: removeduplicates=false, addsamplestobackground=false

---

        
      * Model type: Maxnet
      * Train locations: 5400
          * presence: 400
          * absence/background: 5000
      * Continuous variables: bio1, bio12, bio16, bio17, bio5, bio6, bio7, bio8
      * Categorical variables: biome
      * Output type: cloglog
      * Feature Class combination: lqph
      * Regularization multiplier: 1

---

        
      * Model type: ANN
      * Train locations: 5400
          * presence: 400
          * absence/background: 5000
      * Continuous variables: bio1, bio12, bio16, bio17
      * Categorical variables: 
      * Size: 10
      * Decay: 0
      * Rang: 0.7
      * Maxit: 100

---

        
      * Model type: BRT
      * Train locations: 5400
          * presence: 400
          * absence/background: 5000
      * Continuous variables: bio1, bio12, bio16, bio17
      * Categorical variables: ,
      * Distribution: bernoulli
      * Number of trees: 200
      * Interaction depth: 1
      * Shrinkage: 0.2
      * Bag fraction: 0.5

---

        
      * Model type: RF
      * Train locations: 5400
          * presence: 400
          * absence/background: 5000
      * Continuous variables: bio1, bio12, bio16, bio17
      * Categorical variables: ,
      * Mtry: 2
      * Number of trees: 200
      * Node size: 1

# The function raises errors

    ! Argument `factors` is deprecated
    x Please check terra documentation to see how to use factors.

