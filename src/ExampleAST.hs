import SLispParser

two :: Expression
two = Define "two" [Value 2]

twice :: Expression
twice =
  Define
    "twice"
    [Function ["n"] [Call "*" [Reference "n", Reference "two"]]]

factorial :: Expression
factorial =
  Define
    "factorial"
    [ Function
        ["n"]
        [ Call
            "if"
            [ Call "==" [Reference "n", Value 0],
              Value 1,
              Call
                "*"
                [ Reference "n",
                  Call
                    "factorial"
                    [Call "-" [Reference "n", Value 1]]
                ]
            ]
        ]
    ]

main' :: Expression
main' = Define "root" [two, twice, factorial]
