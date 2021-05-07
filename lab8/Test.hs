import OccurrenceFunct
import Test.HUnit

test1 = TestCase (assertEqual "For pattern - a ; text - a" 1 (count "a" "a"))
test2 = TestCase (assertEqual "For pattern - abba ; text - abbasdasdabba" 2 (count "abba" "abbasdasdabba"))
test3 = TestCase (assertEqual "For pattern - sam ; text - sam jack john tim tom" 1 (count "sam" "sam jack john tim tom"))

test4 = TestCase (assertEqual "For empty strings" 1 (count "" ""))
test5 = TestCase (assertEqual "For empty pattern string" 6 (count "" "sadsa"))
test6 = TestCase (assertEqual "For empty pattern string" 3 (count "" "aa"))
test7 = TestCase (assertEqual "For empty text strings" 0 (count "asda" ""))


tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5,
                  TestLabel "test6" test6, TestLabel "test7" test7]

main = runTestTT tests
