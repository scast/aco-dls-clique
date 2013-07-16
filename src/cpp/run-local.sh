#!/bin/bash
for i in {1..3}
do
    ./local-go ../../instances/DIMACS_subset_ascii/C125.9.clq 1 100000 5 >> C125.9_5.result
    ./local-go ../../instances/DIMACS_subset_ascii/keller4.clq 1 100000 5 >> keller4_5.result
    ./local-go ../../instances/DIMACS_subset_ascii/brock200_2.clq 2 100000 5 >> brock200_2_5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat300-1.clq 1 100000 5 >> p_hat300-1-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/brock200_4.clq 2 100000 5 >> brock200-4-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/gen200_p0.9_44.clq 1 100000 5 >> gen200_p0.9_44-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/gen200_p0.9_55.clq 1 100000 5 >> gen200_p0.9_55-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/hamming8-4.clq 5 100000 5 >> hamming8-4-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat300-2.clq 1 100000 5 >> p_hat300-2-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/C250.9.clq 1 100000 5 >> C250.9-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat300-3.clq 1 100000 5 >> p_hat300-3-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/brock400_4.clq 23 100000 5 >> brock400_4-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/brock400_2.clq 23 100000 5 >> brock400_2-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat700-1.clq 1 100000 5 >> p_hat700-1-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/DSJC500_5.clq 2 100000 5 >> DSJC500_5-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/MANN_a27.clq 3 100000 5 >> MANN_a27-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_55.clq 1 100000 5 >> gen400_p0.9_55-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_75.clq 1 100000 5 >> gen400_p0.9_75-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_65.clq 1 100000 5 >> gen400_p0.9_65-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/C500.9.clq 1 100000 5 >> C500.9-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat700-2.clq 1 100000 5 >> p_hat700-2-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat700-3.clq 1 100000 5 >> p_hat700-3-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/brock800_4.clq 45 100000 5 >> brock800_4-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/brock800_2.clq 45 100000 5 >> brock800_2-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/keller5.clq 1  100000 5 >> keller5-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/DSJC1000_5.clq 15 100000 5 >> DSJC1000_5-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat1500-1.clq 1 100000 5 >> p_hat1500-1-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/hamming10-4.clq 5 100000 5 >> hamming10-4-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/C1000.9.clq 1 100000 5 >> C1000.9-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/MANN_a45.clq 3 100000 5 >> MANN_a45-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat1500-2.clq 1 100000 5 >> p_hat1500-2-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat1500-3.clq 1 100000 5 >> p_hat1500-3-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/C2000.5.clq 1 100000 5 >> C2000.5-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/C2000.9.clq 1 100000 5 >> C2000.9-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/C4000.5.clq 1 100000 5 >> C4000.5-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/keller6.clq 1 100000 5 >> keller6-5.result
    ./local-go ../../instances/DIMACS_subset_ascii/MANN_a81.clq 3 100000 5 >> MANN_a81-5.result
done
