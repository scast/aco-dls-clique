#!/bin/bash
for i in {1..3}
do
    ./aco-go ../../instances/DIMACS_subset_ascii/C125.9.clq 0.01 6 0.99 1 1000 30 5 500 >> C125.9_5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/keller4.clq 0.01 6 0.99 1 1000 30 1 500 >> keller4_5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/brock200_2.clq 0.01 6 0.99 1 1000 30 2 1 500 >> brock200_2_5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat300-1.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat300-1-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/brock200_4.clq 0.01 6 0.99 1 1000 302 500 >> brock200-4-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/gen200_p0.9_44.clq 0.01 6 0.99 1 1000 30 1 500 >> gen200_p0.9_44-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/gen200_p0.9_55.clq 0.01 6 0.99 1 1000 30 1 500 >> gen200_p0.9_55-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/hamming8-4.clq 0.01 6 0.99 1 1000 30 5 500 >> hamming8-4-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat300-2.clq 0.01 6 0.99 1 1000 301 500 >> p_hat300-2-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/C250.9.clq 0.01 6 0.99 1 1000 30 1 500 >> C250.9-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat300-3.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat300-3-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/brock400_4.clq 0.01 6 0.99 1 1000 30 23 500 >> brock400_4-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/brock400_2.clq 0.01 6 0.99 1 1000 30 23 500 >> brock400_2-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat700-1.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat700-1-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/DSJC500_5.clq 0.01 6 0.99 1 1000 30 2 500 >> DSJC500_5-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/MANN_a27.clq 0.01 6 0.99 1 1000 30 3 500 >> MANN_a27-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_55.clq 0.01 6 0.99 1 1000 30 1 500 >> gen400_p0.9_55-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_75.clq 0.01 6 0.99 1 1000 30 1 500 >> gen400_p0.9_75-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/gen400_p0.9_65.clq 0.01 6 0.99 1 1000 30 1 500 >> gen400_p0.9_65-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/C500.9.clq 0.01 6 0.99 1 1000 30 1 500 >> C500.9-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat700-2.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat700-2-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat700-3.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat700-3-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/brock800_4.clq 0.01 6 0.99 1 1000 30 45 500 >> brock800_4-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/brock800_2.clq 0.01 6 0.99 1 1000 30 45 500 >> brock800_2-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/keller5.clq 0.01 6 0.99 1 1000 30 1  500 >> keller5-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/DSJC1000_5.clq 0.01 6 0.99 1 1000 30 15 500 >> DSJC1000_5-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat1500-1.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat1500-1-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/hamming10-4.clq 0.01 6 0.99 1 1000 30 5 500 >> hamming10-4-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/C1000.9.clq 0.01 6 0.99 1 1000 30 1 500 >> C1000.9-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/MANN_a45.clq 0.01 6 0.99 1 1000 30 3 500 >> MANN_a45-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat1500-2.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat1500-2-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/p_hat1500-3.clq 0.01 6 0.99 1 1000 30 1 500 >> p_hat1500-3-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/C2000.5.clq 0.01 6 0.99 1 1000 30 1 500 >> C2000.5-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/C2000.9.clq 0.01 6 0.99 1 1000 30 1 500 >> C2000.9-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/C4000.5.clq 0.01 6 0.99 1 1000 30 1 500 >> C4000.5-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/keller6.clq 0.01 6 0.99 1 1000 30 1 500 >> keller6-5.acoresult
    ./aco-go ../../instances/DIMACS_subset_ascii/MANN_a81.clq 0.01 6 0.99 1 1000 30 3 500 >> MANN_a81-5.acoresult
done
