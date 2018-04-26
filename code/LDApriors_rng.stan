functions {
  matrix LDA_rng(int V, vector beta, int K, vector alpha, int M, int [] N) {
    vector[V] phi[K]; //declare phi to be a collection of K vectors of size V
    matrix[M, V] DTM = rep_matrix(0, M, V); // creates a matrix full of zeros that we'll fill up below
    for (k in 1:K) { // loop over each topic from k = 1 ... K
      phi[k] = dirichlet_rng(beta); // phi[k] is a vector of real numbers (size of K) drawn from Dir(beta) which is the per-topic word distribution
    }
    for (m in 1:M) { // loop over each document from m = 1 ... M
      vector[K] theta = dirichlet_rng(alpha); // theta is a vector of real numbers (size of K) drawn from Dir(alpha) which is the per-document topic distribution
      int z = categorical_rng(theta); // integer z is the topic k for the n-th word w in doc m drawn from Categorical(theta) which is the topic distribution for document m
      for (i in 1:N[m]) { // loop over i = 1 ... N which is the number of words N in the m-th document m
        int w = categorical_rng(phi[z]); // integer w is the specific word w drawn from Categorical(phi_{z_{mn}}) which is a topic distribution for each word in each document
        DTM[m, w] += 1; // In Stan, x += 1; is equivalent to x = x + 1
      }
    }
    return DTM;
  }
}
