context("test textstat_entropy")


test_that("textstat_entropy computation is correct", {
    
    test <- dfm(c(
        d1 = "a a a b c d e f f",
        d2 = "b b b c c d e f",
        d3 = "a a d d d f"
    ))
    
    expect_equal(textstat_entropy(test, method = 'ml', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method='ML', unit = "log2"))
    
    expect_equal(textstat_entropy(test, method = 'mm', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method='MM', unit = "log2"))
    
    expect_equal(textstat_entropy(test, method = 'jeffreys', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method ='Jeffreys', unit = "log2"))
    
    expect_equal(textstat_entropy(test, method = 'laplace', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method ='Laplace', unit = "log2"))
    
    expect_equal(textstat_entropy(test, method = 'sg', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method ='SG', unit = "log2"))
    
    expect_equal(textstat_entropy(test, method = 'minimax', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method ='minimax', unit = "log2"))
    
    expect_equal(textstat_entropy(test, method = 'cs', unit = 'log2'),
                 apply(test, 2, entropy::entropy, method ='CS', unit = "log2"))
    
})