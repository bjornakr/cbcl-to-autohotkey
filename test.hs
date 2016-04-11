module CbclToAhk_Test where
    import Control.Exception
    import Test.HUnit
    import CbclToAhk

    toKeyString_emptyList_emptyString =
        TestCase $ assertEqual "toKeyString_emptyList_emptyString" "" (toKeyString [])

    toKeyString_nothing_keyDownString =
        TestCase $ assertEqual "toKeyString_nothing_keyDownString" "{down}" (toKeyString [Nothing])

    toKeyString_justNumber_numberString =
        TestCase $ assertEqual "toKeyString_justNumber_numberString" "1" (toKeyString [Just 1])

    toKeyString_responseSequence_ahkString =
        TestCase $ assertEqual "toKeyString_responseSequence_ahkString"
            "12{down}3"
            (toKeyString [Just 1, Just 2, Nothing, Just 3])
    
    toKeyString_testList = TestList [
        toKeyString_emptyList_emptyString, 
        toKeyString_nothing_keyDownString, 
        toKeyString_justNumber_numberString,
        toKeyString_responseSequence_ahkString
        ]

    

    toResponse_space_nothing =
        TestCase $ assertEqual "toResponse_space_nothing" Nothing (toResponse ' ')
    toResponse_digit_validResponse =
        TestCase $ assertEqual "toResponse_digit_validResponse" (Just 1) (toResponse '1')

    toResponse_testList = TestList [
        toResponse_space_nothing,
        toResponse_digit_validResponse
        ]


    toResponses_emptyString_emptyList =
        TestCase $ assertEqual "toResponses_emptyString_emptyList"
            [] (toResponses "")
    toResponses_singleSeparator_emptyList =
        TestCase $ assertEqual "toResponses_singleSeparator_emptyList"
            [] (toResponses ";")
    toResponses_twoSeparators_singleNothing =
        TestCase $ assertEqual "toResponses_twoSeparators_singleNothing"
            [Nothing] (toResponses ";;")
    toResponses_singleDigit_singleResponse =
        TestCase $ assertEqual "toResponses_singleDigit_singleResponse"
            [Just 1] (toResponses "1")
    toResponses_twoSeparatedDigits_twoValidResponses =
        TestCase $ assertEqual "toResponses_twoSeparatedDigits_twoValidResponses"
            [Just 1, Just 2] (toResponses "1;2")
    toResponses_digitAndSeparator_singleResponse =
        TestCase $ assertEqual "toResponses_digitAndSeparator_singleResponse"
            [Just 1] (toResponses "1;")
    toResponses_digitSeparatorSeparator_validResponseAndNothing =
        TestCase $ assertEqual "toResponses_digitSeparatorSeparator_validResponseAndNothing"
            [Just 1, Nothing] (toResponses "1;;")
    toResponses_digitSeparatorSeparatorDigit_validResponseNothingValidResponse =
        TestCase $ assertEqual "toResponses_digitSeparatorSeparatorDigit_validResponseNothingValidResponse"
            [Just 1, Nothing, Just 2] (toResponses "1;;2")

    toResponses_testList = TestList [
        toResponses_emptyString_emptyList,
        toResponses_singleSeparator_emptyList,
        toResponses_twoSeparators_singleNothing,
        toResponses_singleDigit_singleResponse,
        toResponses_twoSeparatedDigits_twoValidResponses,
        toResponses_digitAndSeparator_singleResponse,
        toResponses_digitSeparatorSeparator_validResponseAndNothing,
        toResponses_digitSeparatorSeparatorDigit_validResponseNothingValidResponse
        ]


    main = runTestTT $ TestList [
        toKeyString_testList,
        toResponse_testList,
        toResponses_testList
        ]
