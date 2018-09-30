import Test.HUnit
--Test the createTree function
testCreateTree1=(assertEquals "for (createTree [])" "cannot create tree from empty list" createTree [])
--Test the addToTree funtion

tests = TestList [TestLabel "testCreateTree1" testCreateTree1]
