-- na podstawie prezentacji ze strony:
-- http://www.infoq.com/presentations/Functional-Languages-101

list_function_maker base rec_op =
    \ xs ->
        if xs == []
            then base
            else rec_op (head xs)
                        ((list_function_maker base rec_op) (tail xs))

square x = x * x
squares = list_function_maker [] (\ first results -> ((square first):results))

add1 x = x + 1
my_length = list_function_maker 0 (\ first result -> add1 result)
