# Earth mover distance in 1D

\text{EMD}(f, g) = \sum\_{i=1}^{k} \left\|\sum\_{j \leq i} f_j -
\sum\_{j \leq i} g_j\right\|

For 1D distributions, EMD is mathematically equivalent to the L1
distance between cumulative distribution functions:

\text{EMD}(f, g) = \int\_{-\infty}^{\infty} \|F(x) - G(x)\|, dx
