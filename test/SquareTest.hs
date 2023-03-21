module SquareTest where
    import Square
    import Test.HUnit

    squareTests :: [Test]
    squareTests = [
        testSqStr,
        testMovement,
        testRangeDir]

    sq :: Square
    sq = Sq 4 4

    testSqStr :: Test
    testSqStr = "testSqStr" ~: TestList [show sq ~=? "d4"]

    testMovement :: Test
    testMovement = "testMovement" ~: TestList [
        moveXY sq (1, 1) ~=? Just (Sq 5 5),
        moveXY sq (-1, 1) ~=? Just (Sq 3 5),
        moveXY sq (1, -1) ~=? Just (Sq 5 3),
        moveXY sq (-1, -1) ~=? Just (Sq 3 3),
        moveXY (Sq 1 1) (-1, 1) ~=? Nothing,
        moveXY (Sq 8 4) (1, 1) ~=? Nothing]

    testRangeDir :: Test
    testRangeDir = "testRangeDir" ~: TestList [
        fullRangeDir N sq ~?= [
            Just (Sq 4 5),
            Just (Sq 4 6),
            Just (Sq 4 7),
            Just (Sq 4 8)
        ],
        fullRangeDir S sq ~?= [
        Just (Sq 4 3),
        Just (Sq 4 2),
        Just (Sq 4 1)
        ],
        fullRangeDir E sq ~?= [
            Just (Sq 5 4),
            Just (Sq 6 4),
            Just (Sq 7 4),
            Just (Sq 8 4)
            ],
        fullRangeDir W sq ~?= [
            Just (Sq 3 4),
            Just (Sq 2 4),
            Just (Sq 1 4)
            ]
        ]
    



