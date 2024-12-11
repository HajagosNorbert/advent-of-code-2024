module [unwrap]

unwrap = \res, msg ->
    when res is
        Ok a -> a
        Err _ -> crash msg
