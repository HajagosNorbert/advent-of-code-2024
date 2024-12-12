module [unwrap, ident]

unwrap = \res ->
    when res is
        Ok a -> a
        Err e -> crash "unwrapped on $(Inspect.toStr e)"

ident = \x -> x
