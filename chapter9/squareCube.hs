mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuples = [(x,y) | 
            x <- mySqr, 
            y <- myCube
            ]

tuplesLT50 = [(x,y) | 
                x <- mySqr, 
                y <- myCube,
                x < 50,
                y < 50
                ]

nTuples ts = length ts