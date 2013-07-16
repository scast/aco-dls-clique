#!/bin/bash
for i in {1..5}
do
    ./local-go ../../instances/DIMACS_subset_ascii/C125.9.clq 1 100000 10 > C125.9
    ./local-go ../../instances/DIMACS_subset_ascii/keller4.clq 1 100000 10 > keller4
    ./local-go ../../instances/DIMACS_subset_ascii/brock200_2.clq 2 100000 10 > brock200_2
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat300-1.clq 1 100000 10 > p_hat300-1
    ./local-go ../../instances/DIMACS_subset_ascii/brock200_4.clq 2 100000 10 > brock200_4
    ./local-go ../../instances/DIMACS_subset_ascii/gen200_p0.9_44.clq 1 100000 10 > gen200_p0.9_44
    ./local-go ../../instances/DIMACS_subset_ascii/gen200_p0.9_55.clq 1 100000 10 > gen200_p0.9_55
    ./local-go ../../instances/DIMACS_subset_ascii/hamming8-4.clq 5 100000 10 > hamming8-4
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat300-2.clq 1 100000 10 > p_hat300-2
    ./local-go ../../instances/DIMACS_subset_ascii/C250.9.clq 1 100000 10 > C250.9
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat300-3.clq 1 100000 10 > p_hat300-3
    ./local-go ../../instances/DIMACS_subset_ascii/brock400_4.clq 23 100000 10 > brock400_4
    ./local-go ../../instances/DIMACS_subset_ascii/brock400_2.clq 23 100000 10 > brock400_2
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat700-1.clq 1 100000 10 > p_hat700-1
    ./local-go ../../instances/DIMACS_subset_ascii/DSJC500_5.clq 2 100000 10 > DSJC500_5
    ./local-go ../../instances/DIMACS_subset_ascii/MANN_a27.clq 3 100000 10 > MANN_a27
    ./local-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_55.clq 1 100000 10 > gen400_p0.9_55
    ./local-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_75.clq 1 100000 10 > gen400_p0.9_75
    ./local-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_65.clq 1 100000 10 > gen400_p0.9_65
    ./local-go ../../instances/DIMACS_subset_ascii/C500.9.clq 1 100000 10 > C500.9
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat700-2.clq 1 100000 10 > p_hat700-2
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat700-3.clq 1 100000 10 > p_hat700-3
    ./local-go ../../instances/DIMACS_subset_ascii/brock800_4.clq 45 100000 10 > brock800_4
    ./local-go ../../instances/DIMACS_subset_ascii/brock800_2.clq 45 100000 10 > brock800_2
    ./local-go ../../instances/DIMACS_subset_ascii/keller5.clq 1  100000 10 > keller5
    ./local-go ../../instances/DIMACS_subset_ascii/DSJC1000_5.clq 15 100000 10 > DSJC1000_5
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat1500-1.clq 1 100000 10 > p_hat1500-1
    ./local-go ../../instances/DIMACS_subset_ascii/hamming10-4.clq 5 100000 10 > hamming10-4
    ./local-go ../../instances/DIMACS_subset_ascii/C1000.9.clq 1 100000 10 > C1000.9
    ./local-go ../../instances/DIMACS_subset_ascii/MANN_a45.clq 3 100000 10 > MANN_a45
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat1500-2.clq 1 100000 10 > p_hat1500-2
    ./local-go ../../instances/DIMACS_subset_ascii/p_hat1500-3.clq 1 100000 10 > p_hat1500-3
    ./local-go ../../instances/DIMACS_subset_ascii/C2000.5.clq 1 100000 10 > C2000.5
    ./local-go ../../instances/DIMACS_subset_ascii/C2000.9.clq 1 100000 10 > C2000.9
    ./local-go ../../instances/DIMACS_subset_ascii/C4000.5.clq 1 100000 10 > C4000.5
    ./local-go ../../instances/DIMACS_subset_ascii/keller6.clq 1 100000 10 > keller6
    ./local-go ../../instances/DIMACS_subset_ascii/MANN_a81.clq 3 100000 10 > MANN_a81
done
