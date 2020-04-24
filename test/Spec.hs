module Main where
    import Chapter1.Spec
    import Chapter2.Spec

    import Test.HUnit
    import System.Exit

    main :: IO ()
    main = do
        counts2 <- runTestTT (test [
                chapter1_tests,
                chapter2_tests
                ])
        if (errors counts2 + failures counts2 == 0)
            then exitSuccess
            else exitFailure
