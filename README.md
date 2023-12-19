
# Usage

## Pitches

Pitches can be represented as a vector of integers, or a comma-separated string as integers:

```

60:64 %>% trans(1)

60:64 %>% trans(-1)

"60,61,62" %>% trans(1)

# To pitch class:

60:65 %>% pc()

"60,61,62" %>% pc()

```


## Interval

```

# 60:65 %>% int()

# "60,61,62" %>% int()

```

## Fuzzified intervals

```

# 60:65 %>% int() %>% fuzzy()
# 60:65 %>% int() %>% parsons()

```

# TODO

