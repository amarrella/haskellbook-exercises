ifEvenAdd2 n =
    case isEven of
        True -> n + 2
        False -> n
    where isEven = even n