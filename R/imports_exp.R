# Expression to calculate the probability of importing and infected animal from an infected herd
imports_exp <- quote({
  # Probability that an animal in an infected herd is infected (a = an animal)
  inf_a <- w_prev

  # Probability an animal is tested and is a false negative (test specificity assumed to be 100%)
  false_neg_a <- inf_a * test_origin * (1 - test_sensi)

  # Probability an animal is not tested
  no_test_a <- inf_a * (1 - test_origin)

  # Probability an animal is not detected
  no_detect_a <- false_neg_a + no_test_a
})
