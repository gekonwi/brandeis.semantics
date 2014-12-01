module UnitTest where

indent :: (Show a) => a -> String
indent x = "    " ++ ( filter (\c -> c /= '"') $ show x)


sepLine :: String -> String
sepLine c = (concat $ take 60 $ repeat c)

runTests :: (Show a, Eq a) => [(String, a, a)] -> ([String], Int)
runTests tests = (concat details, errorCount)
	where
		details = [ oneTestDetails | (name, actual, expected) <- tests,
			let header h1 h2 = [sepLine "-", h1, indent name, h2],
			let passedHead = header "Test Passed:" "expected == actual:",
			let failedHead = (header ">>>>>>> TEST FAILED:" "expected:") 
				++ [indent expected] ++ ["actual:"],
			let detailsHead = if actual == expected then passedHead else failedHead,
			let oneTestDetails = detailsHead ++ [indent actual]
			]

		errorCount = sum [boolNum | (_, actual, expected) <- tests,
			let boolNum = if actual == expected then 0 else 1
			]


showErrorCount :: Int -> String
showErrorCount count = 
	if count == 0 
	then "All tests passed :D"
	else ">>>>>>> FAILED TESTS: " ++ show count


showTestResults :: [([String], Int)] -> [String]
showTestResults results = (concat lineGroups) ++ lastLines
	where 
		lineGroups = [ header ++ details ++ errorCount | r <- results,
			let header = ["", "", "Starting test suite:"],
			let details = map indent $ fst r,
			let errorCount = [(sepLine "="), showErrorCount $ snd r]
			]

		lastLines = 
			["", "", "", sepLine "=", "Result of all test suites:"]
			++ [showErrorCount $ sum [ snd r | r <- results]]
			++ [sepLine "=", "", ""]


runUnitTests :: [([String], Int)] -> IO()
runUnitTests testSuites = mapM_ putStrLn $ showTestResults $ testSuites